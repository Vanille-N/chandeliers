//! Stateful expressions

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
}

impl<T> Default for Register<T> {
    fn default() -> Self {
        Self { inner: None }
    }
}

impl<T> Register<T> {
    /// Store a value that can then be obtained later exactly once.
    pub fn set(&mut self, t: T) {
        self.inner = Some(t);
    }

    /// Extract the inner value.
    pub fn get(&mut self) -> T {
        self.inner.take().unwrap()
    }
}
