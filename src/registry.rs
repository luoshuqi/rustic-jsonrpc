use std::any::type_name;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt::{Display, Formatter};
use std::future::Future;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;

use serde::Serialize;
use serde_json::{from_slice, Value};

use crate::{Container, Error, Id, Request, Response, METHOD_NOT_FOUND, PARSE_ERROR, SERVER_ERROR};

pub type BoxError = Box<dyn StdError + Send + Sync>;

/// Type alias for an RPC handler function.
pub type Handler = for<'a> fn(
    &'a Container,
    &'a str,
)
    -> Pin<Box<dyn Future<Output = Result<Value, BoxError>> + Send + 'a>>;

/// The `Registry` struct maintains a collection of RPC methods and a dependency injection container.
pub struct Registry {
    container: Container,
    methods: HashMap<&'static str, Handler>,
    post_call: Option<
        Box<
            dyn for<'a> Fn(
                    &'a Request<'a>,
                    &'a Result<Value, BoxError>,
                ) -> Pin<Box<dyn Future<Output = ()> + Send + 'a>>
                + Send
                + Sync,
        >,
    >,
}

impl Registry {
    /// Creates a new, empty `Registry`.
    pub fn new() -> Self {
        Self {
            container: Container::new(),
            methods: HashMap::new(),
            post_call: None,
        }
    }

    /// Registers a list of methods with the `Registry`.
    pub fn register(&mut self, methods: &[Method]) {
        for method in methods {
            assert!(
                self.methods.insert(method.name, method.handler).is_none(),
                "method `{}` exists",
                method.name,
            );
        }
    }

    /// Sets a function to be called after each RPC call.
    pub fn post_call(
        &mut self,
        func: impl for<'a> Fn(
                &'a Request<'a>,
                &'a Result<Value, BoxError>,
            ) -> Pin<Box<dyn Future<Output = ()> + Send + 'a>>
            + Send
            + Sync
            + 'static,
    ) {
        self.post_call = Some(Box::new(func))
    }

    /// Handles an incoming RPC request.
    pub async fn handle<'a>(&self, request: &'a [u8]) -> Option<Amount<Response<'a>>> {
        if is_object(request) {
            let response = match from_slice::<Request>(request) {
                Ok(v) => self.invoke(&v).await?,
                Err(e) => Response::error(Error::new(PARSE_ERROR, e, None), Id::Null),
            };
            return Some(Amount::One(response));
        }

        match from_slice::<Vec<Request>>(request) {
            Ok(batch) => {
                let mut response = Vec::with_capacity(batch.len());
                for r in &batch {
                    if let Some(v) = self.invoke(r).await {
                        response.push(v);
                    }
                }
                (!response.is_empty()).then_some(Amount::Batch(response))
            }
            Err(e) => Some(Amount::One(Response::error(
                Error::new(PARSE_ERROR, e, None),
                Id::Null,
            ))),
        }
    }

    async fn invoke<'a>(&self, req: &Request<'a>) -> Option<Response<'a>> {
        let handler = match self.methods.get(req.method) {
            Some(handler) => handler,
            None if matches!(req.id, Id::None) => return None,
            None => {
                let err = Error::new(
                    METHOD_NOT_FOUND,
                    format!("method `{}` not found", req.method),
                    None,
                );
                return Some(Response::error(err, req.id));
            }
        };

        let params = req.params.map(|v| v.get()).unwrap_or("{}");
        let result = handler(&self.container, params).await;
        if let Some(ref f) = self.post_call {
            f(req, &result).await;
        }

        if matches!(req.id, Id::None) {
            return None;
        }

        match result {
            Ok(v) => Some(Response::result(v, req.id)),
            Err(e) => match Error::cast(&*e) {
                Some(e) => Some(Response::error(e.clone(), req.id)),
                None => {
                    let e = Error::new(SERVER_ERROR, "server error", None);
                    Some(Response::error(e, req.id))
                }
            },
        }
    }

    /// Returns a list of registered method names.
    pub fn methods(&self) -> Vec<&'static str> {
        let mut methods = self.methods.keys().map(|v| *v).collect::<Vec<_>>();
        methods.sort();
        methods
    }
}

impl Deref for Registry {
    type Target = Container;

    fn deref(&self) -> &Self::Target {
        &self.container
    }
}

impl DerefMut for Registry {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.container
    }
}

/// Checks if the provided byte slice represents a JSON object.
fn is_object(s: &[u8]) -> bool {
    for v in s {
        if v.is_ascii_whitespace() {
            continue;
        }
        return *v == b'{';
    }
    false
}

/// Represents a response that can be a single response or a batch of responses.
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Amount<T> {
    One(T),
    Batch(Vec<T>),
}

/// Represents an RPC method.
pub struct Method {
    name: &'static str,
    handler: Handler,
}

impl Method {
    /// Creates a new `Method` with the given name and handler.
    pub const fn new(name: &'static str, handler: Handler) -> Self {
        Self { name, handler }
    }
}

/// Represents an error encountered while injecting dependencies.
#[derive(Debug)]
pub struct InjectError {
    name: &'static str,
    ty: &'static str,
}

impl InjectError {
    /// Creates a new `InjectError` for the specified argument name and type.
    pub fn new<T>(name: &'static str) -> Self {
        Self {
            name,
            ty: type_name::<T>(),
        }
    }
}

impl Display for InjectError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "error inject argument `{}: {}`", self.name, self.ty)
    }
}

impl StdError for InjectError {}

mod sealed {
    pub trait Sealed {}
}

/// Trait representing the result of an RPC method.
pub trait MethodResult: sealed::Sealed {
    const ASSERT: () = ();
}

impl<T, E> sealed::Sealed for Result<T, E>
where
    T: Serialize,
    E: Into<BoxError>,
{
}

impl<T, E> MethodResult for Result<T, E>
where
    T: Serialize,
    E: Into<BoxError>,
{
}

/// Trait for converting an argument from one type to another.
#[allow(async_fn_in_trait)]
pub trait FromArg<T>: Sized {
    type Error: StdError;

    async fn from_arg(container: &Container, arg: T) -> Result<Self, Self::Error>;
}
