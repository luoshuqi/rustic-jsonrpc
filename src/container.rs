use std::any::{Any, TypeId};
use std::collections::HashMap;

/// Dependency Injection Container
pub struct Container {
    map: HashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl Container {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Returns some reference to it if the container holds a value of type `T`, or `None` if not
    #[inline]
    pub fn get<T>(&self) -> Option<&T>
    where
        T: Send + Sync + 'static,
    {
        self.map
            .get(&TypeId::of::<T>())
            .map(|v| v.downcast_ref().unwrap())
    }

    /// Put a value of type `T` to the container
    pub fn put<T>(&mut self, value: T) -> Option<Box<T>>
    where
        T: Send + Sync + 'static,
    {
        self.map
            .insert(TypeId::of::<T>(), Box::new(value))
            .map(|v| v.downcast().unwrap())
    }
}
