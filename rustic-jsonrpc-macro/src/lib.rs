use proc_macro::TokenStream;
use std::mem::take;

use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::visit_mut::{visit_lifetime_mut, visit_type_reference_mut, VisitMut};
use syn::{
    parse_macro_input, parse_quote, Attribute, Error, FnArg, GenericParam, Ident, ItemFn, Lifetime,
    LitStr, Meta, Pat, Path, ReturnType, Token, Type, TypeReference,
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
    match expand_method(&attr, &mut func) {
        Ok(v) => v,
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_method(attr: &Attr, func: &mut ItemFn) -> syn::Result<TokenStream> {
    check_generic(func)?;
    let args = collect_args(func)?;
    let params_assert = params_assert(&args);
    let params_struct = params_struct(&args);
    let mut args_gen = Vec::with_capacity(args.len());
    for arg in args {
        let name = arg.ident;
        args_gen.push(if arg.inject {
            match &*arg.ty {
                Type::Reference(TypeReference { elem: ty, .. }) => {
                    let mut ty = ty.clone();
                    RemoveLifetime.visit_type_mut(&mut ty);
                    quote_spanned! {arg.ty.span()=>
                        container.get::<#ty>().ok_or(rustic_jsonrpc::InjectError::new::<#ty>(stringify!(#name)))?
                    }
                }
                _ => return Err(Error::new_spanned(arg.ty, "method: expected reference")),
            }
        } else {
            quote!(params.#name)
        })
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
    func.block.stmts.insert(
        0,
        parse_quote! {
            {
                #(#params_assert)*
                #output_assert
            }
        },
    );
    let vis = &func.vis;
    let gen = quote! {
        #func
        #vis const #method_ident: rustic_jsonrpc::Method = rustic_jsonrpc::Method::new(#method_name, #handler);
    };
    Ok(gen.into())
}

fn format_method_ident(i: &Ident) -> Ident {
    format_ident!(
        "RUSTIC_JSONRRPC_METHOD_{}",
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
        if arg.inject {
            continue;
        }

        ident.push(&arg.ident);

        let mut ty_clone = arg.ty.clone();
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
            syn::Error::new_spanned(&item_fn.sig, "method: expected return type").to_compile_error()
        }
        ReturnType::Type(_, ref ty) => quote_spanned! {ty.span()=>
            { let _ = <#ty as rustic_jsonrpc::MethodResult>::ASSERT; }
        },
    }
}

fn params_assert(args: &[Argument]) -> Vec<proc_macro2::TokenStream> {
    let mut assert = Vec::with_capacity(args.len());
    assert.push(quote! {
        const fn assert<'de, T: rustic_jsonrpc::serde::Deserialize<'de>>() { }
    });
    let mut empty = true;
    for v in args {
        if v.inject {
            continue;
        }
        empty = false;
        let ty = &v.ty;
        assert.push(quote_spanned! {ty.span()=>
            assert::<#ty>();
        })
    }

    if empty {
        Vec::new()
    } else {
        assert
    }
}

struct Argument {
    ident: Ident,
    ty: Box<Type>,
    inject: bool,
}

fn collect_args(func: &mut ItemFn) -> syn::Result<Vec<Argument>> {
    let mut args = Vec::with_capacity(func.sig.inputs.len());
    for arg in &mut func.sig.inputs {
        match arg {
            FnArg::Typed(arg) => match *arg.pat {
                Pat::Ident(ref pat) => args.push(Argument {
                    ident: pat.ident.clone(),
                    ty: arg.ty.clone(),
                    inject: remove_inejct_attr(&mut arg.attrs),
                }),
                _ => {
                    return Err(Error::new_spanned(
                        arg,
                        "method: non identifier pattern is not allwoed",
                    ));
                }
            },
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

fn remove_inejct_attr(attrs: &mut Vec<Attribute>) -> bool {
    if attrs.is_empty() {
        return false;
    }

    fn meta_path(meta: &Meta) -> &Path {
        match meta {
            Meta::Path(v) => v,
            Meta::List(v) => &v.path,
            Meta::NameValue(v) => &v.path,
        }
    }

    let mut present = false;
    for v in &*attrs {
        if meta_path(&v.meta).is_ident("inject") {
            present = true;
            break;
        }
    }
    if !present {
        return false;
    }

    let mut filterd = Vec::new();
    for v in take(attrs) {
        if !meta_path(&v.meta).is_ident("inject") {
            filterd.push(v);
        }
    }
    *attrs = filterd;
    true
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
