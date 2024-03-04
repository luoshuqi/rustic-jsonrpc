# rustic-jsonrpc

JSON-RPC 2.0 server library written in Rust

# Example

Here is a simple example demonstrating how to write RPC methods similar to Redis's get and set method.

```rust
use std::collections::HashMap;
use std::convert::Infallible;
use std::future::Future;
use std::pin::Pin;
use std::sync::RwLock;

use rustic_jsonrpc::serde_json::{to_string, Value};
use rustic_jsonrpc::{method, methods, BoxError, Registry, Request};

#[derive(Default)]
struct Storage {
    data: RwLock<HashMap<String, String>>,
}

#[method(name = "get")]
async fn get(#[inject] storage: &Storage, key: &str) -> Result<Option<String>, Infallible> {
    Ok(storage.data.read().unwrap().get(key).map(|v| v.to_string()))
}

#[method]
async fn set(#[inject] storage: &Storage, key: String, value: String) -> Result<(), Infallible> {
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
            br#"{"jsonrpc":"2.0","method":"set","params":{"key": "foo", "value": "bar"}, "id":1}"#,
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

Output

```
call method `set` ok
{"jsonrpc":"2.0","result":null,"id":1}
call method `get` ok
{"jsonrpc":"2.0","result":"bar","id":2}
```

[Real world example](https://github.com/luoshuqi/passman)
