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
pub use rustic_jsonrpc_macro::method;
pub use rustic_jsonrpc_macro::method_ident;

#[macro_export]
macro_rules! methods {
    ($($name:ident),+) => {
        &[$(rustic_jsonrpc::method_ident!($name)),*]
    };
}

mod container;
mod registry;

const PARSE_ERROR: i32 = -32700;

const METHOD_NOT_FOUND: i32 = -32601;

pub const INVALID_PARAMS: i32 = -32602;

const SERVER_ERROR: i32 = -32000;

/// Request Object
#[derive(Deserialize, Debug)]
pub struct Request<'a> {
    #[allow(dead_code)]
    jsonrpc: V2_0,
    pub method: &'a str,
    #[serde(borrow)]
    pub params: Option<&'a RawValue>,
    #[serde(borrow, default)]
    pub id: Id<'a>,
}

/// Response Object
#[derive(Serialize, Debug)]
pub struct Response<'a> {
    jsonrpc: V2_0,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<Error>,
    pub id: Id<'a>,
}

impl<'a> Response<'a> {
    /// Returns a success response
    pub fn result(result: Value, id: Id<'a>) -> Self {
        Self {
            jsonrpc: V2_0,
            result: Some(result),
            error: None,
            id,
        }
    }

    /// Returns a error response
    pub fn error(err: Error, id: Id<'a>) -> Self {
        Self {
            jsonrpc: V2_0,
            result: None,
            error: Some(err),
            id,
        }
    }
}

/// Error object
#[derive(Serialize, Debug, Clone)]
pub struct Error {
    pub code: i32,
    pub message: String,
    pub data: Option<Value>,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl StdError for Error {}

impl Error {
    pub fn new(code: i32, message: impl ToString, data: Option<Value>) -> Self {
        Self {
            code,
            message: message.to_string(),
            data,
        }
    }

    /// Returns some reference to the `err`'s inner value if `err` or `err`'s source is of type [Error], or
    /// `None` if it isn't.
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

/// Request Id
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Id<'a> {
    /// String id
    Str(&'a str),
    /// Number id
    Int(i64),
    /// Id is null
    Null,
    /// Id is not present
    Absent,
}

impl<'a> Default for Id<'a> {
    fn default() -> Self {
        Self::Absent
    }
}
