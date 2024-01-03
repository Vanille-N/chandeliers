//! Stateful expressions

use crate::nillable::{AllNil, FirstIsNil};

/// A device that returns true exactly once and then always false.
pub struct Flip(bool);

impl Default for Flip {
    #[inline]
    fn default() -> Self {
        Self(true)
    }
}

impl Flip {
    /// Test and set: returns `true` once and `false` permanently afterwards.
    #[inline]
    pub fn tas(&mut self) -> bool {
        let res = self.0;
        self.0 = false;
        res
    }
}

/// A device that can store an arbitrary value in between executions.
pub struct Register<T> {
    /// Value saved for the next execution.
    inner: T,
    /// Not yet commited next value.
    next: T,
    /// Whether the clock is currently active.
    on_clock: bool,
}

impl<T> Default for Register<T>
where
    T: AllNil,
{
    #[inline]
    fn default() -> Self {
        Self {
            inner: T::auto_size(),
            next: T::auto_size(),
            on_clock: false,
        }
    }
}

impl<T> Register<T> {
    /// Store a value that can then be obtained later exactly once.
    #[inline]
    pub fn schedule(&mut self, t: T)
    where
        T: FirstIsNil,
    {
        if !t.first_is_nil() {
            self.next = t;
        }
    }

    /// Activate or not this register for this round.
    #[inline]
    pub fn with_clock(&mut self, clk: crate::nillable::Nillable<bool>) {
        self.on_clock = clk.is(crate::nillable::Nillable::Defined(true));
    }

    /// Store the very first value of the register.
    #[inline]
    pub fn try_initialize(&mut self, t: T)
    where
        T: FirstIsNil,
    {
        if self.inner.first_is_nil() {
            self.inner = t;
        }
    }

    /// Move `next` to become the current value.
    #[inline]
    pub fn commit(&mut self)
    where
        T: Clone,
    {
        self.inner = self.next.clone();
    }

    /// Extract the inner value.
    #[inline]
    pub fn get(&mut self) -> T
    where
        T: Clone + AllNil,
    {
        if self.on_clock {
            self.inner.clone()
        } else {
            T::auto_size()
        }
    }
}
