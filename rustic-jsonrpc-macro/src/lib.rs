use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::visit_mut::{visit_lifetime_mut, visit_type_reference_mut, VisitMut};
use syn::{
    parse_macro_input, parse_quote, Attribute, Error, FnArg, GenericParam, Ident, ItemFn, Lifetime,
    LitStr, Meta, Pat, ReturnType, Token, Type, TypeReference,
};

#[proc_macro]
pub fn method_ident(input: TokenStream) -> TokenStream {
    let ident = parse_macro_input!(input as Ident);
    let ident = format_method_ident(&ident);
    quote!(#ident).into()
}

#[proc_macro_attribute]
pub fn method(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as Attr);
    let mut func = parse_macro_input!(input as ItemFn);
    expand_method(&attr, &mut func).unwrap_or_else(|e| e.to_compile_error().into())
}

fn expand_method(attr: &Attr, func: &mut ItemFn) -> syn::Result<TokenStream> {
    check_generic(func)?;
    let args = collect_args(func)?;
    let params_struct = params_struct(&args);
    let mut args_gen = Vec::with_capacity(args.len());
    for arg in args {
        args_gen.push(match arg.kind {
            Kind::Param => {
                let name = arg.ident.unwrap();
                quote!(params.#name)
            }
            Kind::Inject  => match &*arg.ty {
                Type::Reference(TypeReference { elem: ty, .. }) => {
                    let name = arg.ident.unwrap();
                    let mut ty = ty.clone();
                    RemoveLifetime.visit_type_mut(&mut ty);
                    quote_spanned! {arg.ty.span()=>
                        container.get::<#ty>().ok_or(rustic_jsonrpc::InjectError::new::<#ty>(stringify!(#name)))?
                    }
                }
                _ => return Err(Error::new_spanned(arg.ty, "method: expected reference")),
            }
            Kind::From((ref ident, param_ty)) => {
                let ty = &arg.ty;
                quote_spanned! {ty.span()=>
                    <#ty as rustic_jsonrpc::FromArg::<#param_ty>>::from_arg(container, params.#ident).await?
                }
            }
        });
    }

    let ident = &func.sig.ident;
    let method_ident = format_method_ident(&func.sig.ident);
    let method_name = attr
        .method_name
        .clone()
        .unwrap_or_else(|| ident.to_string());

    let await_ = func.sig.asyncness.map(|_| quote!(.await));
    let handler = quote! {
        |container, params| {
            #params_struct
            use rustic_jsonrpc::serde_json::{from_str, to_value};
            Box::pin(async {
                match from_str::<Params>(params) {
                    Ok(params) => Ok(to_value(#ident(#(#args_gen),*)#await_?).expect("json serialize error")),
                    Err(err) => Err(rustic_jsonrpc::Error::new(rustic_jsonrpc::INVALID_PARAMS, err, None))?,
                }
            })
        }
    };

    let output_assert = output_assert(&func);
    func.block.stmts.insert(0, parse_quote!(#output_assert));
    let vis = &func.vis;
    let gen = quote! {
        #func
        #vis const #method_ident: rustic_jsonrpc::Method = rustic_jsonrpc::Method::new(#method_name, #handler);
    };
    Ok(gen.into())
}

fn format_method_ident(i: &Ident) -> Ident {
    format_ident!(
        "RUSTIC_JSONRPC_METHOD_{}",
        i.to_string().to_ascii_uppercase()
    )
}

fn check_generic(func: &ItemFn) -> syn::Result<()> {
    let generics = &func.sig.generics;
    for v in &generics.params {
        match v {
            GenericParam::Lifetime(_) => (),
            _ => {
                return Err(Error::new_spanned(
                    generics,
                    "method: generic type is not allowed",
                ));
            }
        }
    }
    Ok(())
}

struct ResetLifetime(bool);

impl ResetLifetime {
    fn new() -> Self {
        Self(false)
    }
}

impl VisitMut for ResetLifetime {
    fn visit_lifetime_mut(&mut self, i: &mut Lifetime) {
        self.0 = true;
        if i.ident != "param" {
            i.ident = Ident::new("param", i.ident.span())
        }
        visit_lifetime_mut(self, i);
    }

    fn visit_type_reference_mut(&mut self, i: &mut TypeReference) {
        if i.lifetime.is_none() {
            i.lifetime = Some(Lifetime::new("'param", Span::call_site()))
        }
        visit_type_reference_mut(self, i);
    }
}

struct RemoveLifetime;

impl VisitMut for RemoveLifetime {
    fn visit_type_reference_mut(&mut self, i: &mut TypeReference) {
        i.lifetime = None;
        visit_type_reference_mut(self, i);
    }
}

fn borrow(tpe: &Type) -> bool {
    match tpe {
        Type::Path(tpe) => match tpe.path.segments.last() {
            Some(v) => v.ident == "Cow",
            _ => false,
        },
        _ => false,
    }
}

fn params_struct(args: &[Argument]) -> proc_macro2::TokenStream {
    let mut ident = Vec::with_capacity(args.len());
    let mut ty = Vec::with_capacity(args.len());
    let mut attr: Vec<Option<Attribute>> = Vec::with_capacity(args.len());
    let mut reset_lifetime = ResetLifetime::new();
    for arg in args {
        let (arg_ident, arg_ty) = match arg.kind {
            Kind::Param => (arg.ident.as_ref().unwrap(), &arg.ty),
            Kind::Inject => continue,
            Kind::From((ref arg_ident, ref arg_ty)) => (arg_ident, arg_ty),
        };

        ident.push(arg_ident);
        let mut ty_clone = arg_ty.clone();
        reset_lifetime.visit_type_mut(&mut ty_clone);
        ty.push(ty_clone);

        if borrow(&arg.ty) {
            attr.push(Some(parse_quote!(#[serde(borrow)])));
        } else {
            attr.push(None)
        }
    }

    if reset_lifetime.0 {
        quote! {
            #[derive(rustic_jsonrpc::serde::Deserialize)]
            #[serde(crate = "rustic_jsonrpc::serde")]
            struct Params<'param> {
                #(#attr #ident: #ty,)*
            }
        }
    } else {
        quote! {
            #[derive(rustic_jsonrpc::serde::Deserialize)]
            #[serde(crate = "rustic_jsonrpc::serde")]
            struct Params {
                #(#ident: #ty,)*
            }
        }
    }
}

fn output_assert(item_fn: &ItemFn) -> proc_macro2::TokenStream {
    match item_fn.sig.output {
        ReturnType::Default => {
            Error::new_spanned(&item_fn.sig, "method: expected return type").to_compile_error()
        }
        ReturnType::Type(_, ref ty) => quote_spanned! {ty.span()=>
            { let _ = <#ty as rustic_jsonrpc::MethodResult>::ASSERT; }
        },
    }
}

enum Kind {
    Param,
    Inject,
    From((Ident, Box<Type>)),
}

struct Argument {
    ident: Option<Ident>,
    ty: Box<Type>,
    kind: Kind,
}

fn collect_args(func: &mut ItemFn) -> syn::Result<Vec<Argument>> {
    let mut args = Vec::with_capacity(func.sig.inputs.len());
    for arg in &mut func.sig.inputs {
        match arg {
            FnArg::Typed(arg) => {
                let kind = remove_arg_attr(&mut arg.attrs)?;
                match *arg.pat {
                    Pat::Ident(ref pat) => args.push(Argument {
                        ident: Some(pat.ident.clone()),
                        ty: arg.ty.clone(),
                        kind,
                    }),
                    Pat::Wild(_) if matches!(kind, Kind::From(_)) => args.push(Argument {
                        ident: None,
                        ty: arg.ty.clone(),
                        kind,
                    }),
                    _ => {
                        return Err(Error::new_spanned(
                            arg,
                            "method: non identifier pattern is not allowed",
                        ));
                    }
                };
            }
            FnArg::Receiver(_) => {
                return Err(Error::new_spanned(
                    arg,
                    "method: self parameter is not allowed",
                ));
            }
        }
    }
    Ok(args)
}

fn remove_arg_attr(attrs: &mut Vec<Attribute>) -> syn::Result<Kind> {
    let mut kind = Kind::Param;
    if attrs.is_empty() {
        return Ok(kind);
    }

    let mut delete = vec![];
    for i in 0..attrs.len() {
        match attrs[i].meta {
            Meta::Path(ref path) if path.is_ident("inject") => match kind {
                Kind::Param => {
                    kind = Kind::Inject;
                    delete.push(i);
                }
                _ => return Err(Error::new_spanned(path, "method: unexpected attribute")),
            },
            Meta::List(ref list) if list.path.is_ident("from") => {
                let from = list.parse_args::<From>()?;
                match kind {
                    Kind::Param => {
                        kind = Kind::From(from.0);
                        delete.push(i);
                    }
                    _ => return Err(Error::new_spanned(list, "method: unexpected attribute")),
                };
            }
            _ => (),
        }
    }

    for i in delete.into_iter().rev() {
        attrs.remove(i);
    }
    Ok(kind)
}

struct From((Ident, Box<Type>));

impl Parse for From {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse::<Box<Type>>()?;
        Ok(Self((ident, ty)))
    }
}

#[derive(Default)]
struct Attr {
    method_name: Option<String>,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut attr = Attr::default();
        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "name" => {
                    input.parse::<Token![=]>()?;
                    let name: LitStr = input.parse()?;
                    attr.method_name = Some(name.value());
                    if input.peek(Token![,]) {
                        input.parse::<Token![,]>()?;
                    }
                }
                _ => {
                    return Err(Error::new_spanned(
                        &ident,
                        format!("method: unknown attribute `{}`", ident),
                    ));
                }
            }
        }
        Ok(attr)
    }
}
