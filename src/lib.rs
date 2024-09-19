#![doc = include_str!("../README.md")]

use std::error::Error as StdError;
use std::fmt::{Display, Formatter};

pub use serde;
use serde::de::{Error as SerdeError, Unexpected};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
pub use serde_json;
use serde_json::value::RawValue;
use serde_json::Value;

pub use container::Container;
pub use registry::*;

/// Method attribute macro.
pub use rustic_jsonrpc_macro::method;

/// Method identifier generation macro.
pub use rustic_jsonrpc_macro::method_ident;

/// Macro to generate an array of method identifiers.
#[macro_export]
macro_rules! methods {
    ($($name:ident),+ $(,)?) => {
        &[$(rustic_jsonrpc::method_ident!($name)),*]
    };
}

mod container;
mod registry;

/// Error code for parse errors in JSON-RPC.
const PARSE_ERROR: i32 = -32700;

/// Error code for method not found in JSON-RPC.
const METHOD_NOT_FOUND: i32 = -32601;

/// Error code for invalid parameters in JSON-RPC.
pub const INVALID_PARAMS: i32 = -32602;

/// Error code for server errors in JSON-RPC.
const SERVER_ERROR: i32 = -32000;

/// Represents a JSON-RPC request.
#[derive(Deserialize, Debug)]
pub struct Request<'a> {
    /// JSON-RPC version.
    #[allow(dead_code)]
    jsonrpc: V2_0,

    /// Method name.
    pub method: &'a str,

    /// Parameters.
    #[serde(borrow)]
    pub params: Option<&'a RawValue>,
    #[serde(borrow, default)]

    /// Request ID.
    pub id: Id<'a>,
}

/// Represents a JSON-RPC response.
#[derive(Serialize, Debug)]
pub struct Response<'a> {
    /// JSON-RPC version.
    jsonrpc: V2_0,

    /// Result of the method invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,

    /// Error occurred during method invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<Error>,

    /// Response ID.
    pub id: Id<'a>,
}

impl<'a> Response<'a> {
    /// Creates a successful JSON-RPC response.
    pub fn result(result: Value, id: Id<'a>) -> Self {
        Self {
            jsonrpc: V2_0,
            result: Some(result),
            error: None,
            id,
        }
    }

    /// Creates an error JSON-RPC response.
    pub fn error(err: Error, id: Id<'a>) -> Self {
        Self {
            jsonrpc: V2_0,
            result: None,
            error: Some(err),
            id,
        }
    }
}

/// Represents a JSON-RPC error.
#[derive(Serialize, Debug, Clone)]
pub struct Error {
    /// Error code.
    pub code: i32,

    /// Error message.
    pub message: String,

    /// Additional data associated with the error.
    pub data: Option<Value>,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl StdError for Error {}

impl Error {
    /// Creates a new JSON-RPC error.
    pub fn new(code: i32, message: impl ToString, data: Option<Value>) -> Self {
        Self {
            code,
            message: message.to_string(),
            data,
        }
    }

    /// Casts a general error trait object to a specific JSON-RPC error.
    pub fn cast<'a>(err: &'a (dyn StdError + 'static)) -> Option<&'a Self> {
        match err.downcast_ref() {
            Some(v) => return Some(v),
            None => (),
        };

        let mut err: &(dyn StdError + 'static) = err;
        while let Some(e) = err.source() {
            match e.downcast_ref::<Self>() {
                Some(e) => return Some(e),
                None => err = e,
            }
        }
        None
    }
}

/// Represents the JSON-RPC version.
#[derive(Copy, Clone, Debug)]
struct V2_0;

impl Serialize for V2_0 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str("2.0")
    }
}

impl<'a> Deserialize<'a> for V2_0 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        match <&str as Deserialize>::deserialize(deserializer)? {
            "2.0" => Ok(V2_0),
            v => Err(SerdeError::invalid_value(Unexpected::Str(v), &"\"2.0\"")),
        }
    }
}

/// Represents various forms of JSON-RPC request IDs.
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Id<'a> {
    /// String ID.
    Str(&'a str),

    /// Integer ID.
    Int(i64),

    /// Represents a null ID.
    Null,

    /// Represents an unspecified or missing ID.
    None,
}

impl<'a> Default for Id<'a> {
    fn default() -> Self {
        Self::None
    }
}
