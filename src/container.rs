use std::any::{Any, TypeId};
use std::collections::HashMap;

/// A struct that acts as a container for storing and retrieving values of different types.
pub struct Container {
    map: HashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl Container {
    /// Creates a new, empty Container.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Retrieves a reference to a value of type T from the container, if it exists.
    #[inline]
    pub fn get<T>(&self) -> Option<&T>
    where
        T: Send + Sync + 'static,
    {
        self.map
            .get(&TypeId::of::<T>())
            .map(|v| v.downcast_ref().unwrap())
    }

    /// Inserts a value of type T into the container.
    /// Return the previous value of type T if it existed, or `None` if it did not.
    pub fn put<T>(&mut self, value: T) -> Option<Box<T>>
    where
        T: Send + Sync + 'static,
    {
        self.map
            .insert(TypeId::of::<T>(), Box::new(value))
            .map(|v| v.downcast().unwrap())
    }
}
