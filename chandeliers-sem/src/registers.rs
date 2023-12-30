//! Stateful expressions

/// A device that returns true exactly once and then always false.
pub struct Flip(bool);

impl Flip {
    /// Unique creation method.
    pub fn new() -> Self {
        Self(true)
    }

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

impl<T> Register<T> {
    /// Without a value.
    pub fn empty() -> Self {
        Self { inner: None }
    }

    /// Store a value that can then be obtained later exactly once.
    pub fn set(&mut self, t: T) {
        self.inner = Some(t);
    }

    /// Extract the inner value.
    pub fn get(&mut self) -> T {
        self.inner.take().unwrap()
    }
}
