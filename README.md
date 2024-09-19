# rustic-jsonrpc

`rustic-jsonrpc` is a JSON-RPC 2.0 server library written in Rust. It provides a simple and efficient way to create JSON-RPC services.

## Features

- Easy to define and register RPC methods using procedural macros.
- Support for method injection and custom argument deserialization.

## Example

Here is a simple example demonstrating how to write RPC methods similar to Redis's `get` and `set` methods:

```rust
use std::collections::HashMap;
use std::convert::Infallible;
use std::future::Future;
use std::pin::Pin;
use std::sync::RwLock;

use rustic_jsonrpc::serde_json::{to_string, Value};
use rustic_jsonrpc::{method, methods, BoxError, Container, Error, FromArg, Registry, Request};

struct Auth {}

impl<'a> FromArg<&'a str> for Auth {
    type Error = Error;

    async fn from_arg(_container: &Container, password: &'a str) -> Result<Self, Self::Error> {
        if password == "123456" {
            Ok(Auth {})
        } else {
            Err(Error::new(-1, "invalid auth", None))
        }
    }
}

#[derive(Default)]
struct Storage {
    data: RwLock<HashMap<String, String>>,
}

#[method(name = "get")]
async fn get(#[inject] storage: &Storage, key: &str) -> Result<Option<String>, Infallible> {
    Ok(storage.data.read().unwrap().get(key).map(|v| v.to_string()))
}

#[method]
async fn set(
    #[inject] storage: &Storage,
    #[from(password: &str)] _auth: Auth,
    key: String,
    value: String,
) -> Result<(), Infallible> {
    storage.data.write().unwrap().insert(key, value);
    Ok(())
}

fn log_call<'a>(
    req: &'a Request<'a>,
    resp: &'a Result<Value, BoxError>,
) -> Pin<Box<dyn Future<Output = ()> + Send + 'a>> {
    Box::pin(async move {
        match resp {
            Ok(_) => println!("call method `{}` ok", req.method),
            Err(err) => println!("call method `{}` error: {}", req.method, err),
        }
    })
}

#[tokio::main]
async fn main() {
    let mut registry = Registry::new();
    registry.provide(Storage::default());
    registry.register(methods!(get, set));
    registry.post_call(log_call);

    // set foo bar
    let response = registry
        .handle(
            br#"{"jsonrpc":"2.0","method":"set","params":{"password": "123456","key": "foo", "value": "bar"}, "id":1}"#,
        )
        .await
        .unwrap();
    println!("{}", to_string(&response).unwrap());

    // get foo
    let response = registry
        .handle(br#"{"jsonrpc":"2.0","method":"get","params":{"key": "foo"}, "id":2}"#)
        .await
        .unwrap();
    println!("{}", to_string(&response).unwrap());
}
```

## Output

When the above example is run, it produces the following output:

```
call method `set` ok
{"jsonrpc":"2.0","result":null,"id":1}
call method `get` ok
{"jsonrpc":"2.0","result":"bar","id":2}
```
