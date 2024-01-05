//! Defines the wrapper [`Transparent`] that trivially implements
//! equality and hashing on any type by making all instances indistinguishable
//! and forgeable.

use std::fmt;
use std::hash::{Hash, Hasher};

/// A type that transparently implements `PartialEq` and `Hash`, to be used
/// in structs that carry additional data that should not be relevant in comparisons.
///
/// It additionally supports creating dummy values and they will also compare
/// equal to all other and hash identically.
///
/// The use of a `Transparent<_>` as a field of a struct should be understood
/// to convey the following intent: this field is useful only for diagnostics
/// and is never relevant for decisions.
/// This makes `Transparent::forge` suitable for use in unit tests where said
/// diagnostic data may not be available and we still want the computation to
/// be unaffected.
///
/// In fact the computation is guaranteed to be unaffected because `Transparent`
/// provides no peeking method, and the only way to extract data is `unwrap`
/// that will panic if the inner value was forged.
#[derive(Clone, Copy)]
pub struct Transparent<T> {
    /// Payload.
    inner: Result<T, &'static str>,
}

impl<T> fmt::Debug for Transparent<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_")
    }
}

impl<T> Transparent<T> {
    /// Assert that the value was not forged and get its contents.
    /// # Panics
    /// fails if `self` was obtainedy by `Transparent::forge()`.
    pub fn unwrap(self) -> T {
        match self.inner {
            Ok(x) => x,
            Err(l) => crate::abort!(
                "Attempted to read a forged transparent value. Forged at {}",
                l
            ),
        }
    }

    /// Create a dummy value.
    ///
    /// It is recommended to use the `loc` parameter as a way to track the location
    /// where it was forged so that if it is accidentally unwrapped the error message
    /// is more readable, e.g. by using either an identifying string or even better
    /// the output of the `here!()` macro that gives the location in the source code.
    #[must_use]
    pub fn forge(loc: &'static str) -> Self {
        Self { inner: Err(loc) }
    }
}

impl<T> Transparent<Option<T>> {
    /// If there is not already data, add it.
    /// This is a noop if the contents were not forged.
    pub fn try_override_inner(&mut self, t: T) {
        match &mut self.inner {
            Ok(s) => {
                if s.is_none() {
                    *s = Some(t);
                }
            }
            Err(_) => self.inner = Ok(Some(t)),
        }
    }
}

impl<T> Transparent<Transparent<T>> {
    /// Remove the outer `Transparent`
    pub fn flatten(self) -> Transparent<T> {
        match self.inner {
            Ok(x1) => match x1.inner {
                Ok(x2) => Transparent { inner: Ok(x2) },
                Err(loc) => Transparent { inner: Err(loc) },
            },
            Err(loc) => Transparent { inner: Err(loc) },
        }
    }
}

impl<T> Transparent<Option<T>> {
    /// Remove the outer `Transparent`
    pub fn flatten(self) -> Option<Transparent<T>> {
        match self.inner {
            Ok(x1) => Some(Transparent { inner: Ok(x1?) }),
            Err(loc) => Some(Transparent { inner: Err(loc) }),
        }
    }
}

impl<T> Transparent<T> {
    /// Wrap in a transparent.
    pub fn from(inner: T) -> Self {
        Self { inner: Ok(inner) }
    }
}

impl<T> PartialEq for Transparent<T> {
    /// Trivial equality. All `Transparent` are indistinguishable
    /// from each other.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> Eq for Transparent<T> {}

impl<T> Hash for Transparent<T> {
    // Trivial hash. All `Transparent` are transparent to hashing.
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl<T: fmt::Display> fmt::Display for Transparent<T> {
    /// Almost transparent wrapper for display.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            Ok(v) => write!(f, "{v}"),
            Err(_) => write!(f, "<undef>"),
        }
    }
}

impl Transparent<proc_macro2::Span> {
    /// Map `join` to the inner `span`s
    #[must_use]
    pub fn join(self, other: Self) -> Option<Self> {
        let Ok(v1) = self.inner else {
            return Some(self);
        };
        let Ok(v2) = other.inner else {
            return Some(self);
        };
        Some(Self {
            inner: Ok(v1.join(v2)?),
        })
    }
}

impl<T: crate::TrySpan> crate::TrySpan for Transparent<T> {
    /// `Sp` always has a span, so `TrySpan` is guaranteed to succeed.
    fn try_span(&self) -> Option<crate::Span> {
        self.inner.try_span()
    }
}
