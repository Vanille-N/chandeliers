//! Stateful expressions

use crate::nillable::FirstIsNil;

/// A device that returns true exactly once and then always false.
pub struct Flip(bool);

impl Default for Flip {
    fn default() -> Self {
        Self(true)
    }
}

impl Flip {
    /// Test and set: returns `true` once and `false` permanently afterwards.
    pub fn tas(&mut self) -> bool {
        let res = self.0;
        self.0 = false;
        res
    }
}

/// A device that can store an arbitrary value in between executions.
pub struct Register<T> {
    inner: Option<T>,
    has_been_set: bool,
}

impl<T> Default for Register<T> {
    fn default() -> Self {
        Self {
            inner: None,
            has_been_set: false,
        }
    }
}

impl<T> Register<T> {
    /// Store a value that can then be obtained later exactly once.
    pub fn try_set(&mut self, t: T)
    where
        T: FirstIsNil,
    {
        if !t.first_is_nil() {
            self.inner = Some(t);
            self.has_been_set = true;
        }
    }

    /// Store the very first value of the register.
    pub fn try_initialize(&mut self, t: T)
    where
        T: FirstIsNil,
    {
        if !self.has_been_set {
            self.try_set(t)
        }
    }

    /// Extract the inner value.
    pub fn get(&mut self) -> T {
        self.inner.take().unwrap()
    }
}
