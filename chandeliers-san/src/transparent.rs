//! A mostly transparent wrapper around any type to indicate that it carries
//! a `Span` with it.
//!
//! The recommended pattern is that whenever you have some type `T` that
//! needs a method `foo(&self, span: Span)`, you should implement
//! `foo(&self)` for `Sp<T>` that gives its associated span to the inner call.
//! Macros are provided to facilitate this usage.
//!
//! Note: yous should generally prefer putting the `Sp` on the outside
//! in public function arguments, and on the inside on return values.
//! That is,
//! - use `Sp<&T>`, `Sp<Box<T>>`, `Sp<Result<T, E>>` in inputs and struct fields,
//! - use `&Sp<T>`, `Box<Sp<T>>`, `Result<Sp<T>, E>>` in outputs.
//! The reason is that `Sp` implements the following conversions:
//! - `&Sp<T> -> Sp<&T>` through `as_ref`,
//! - `&mut Sp<T> -> Sp<&mut T>` through `as_ref_mut`,
//! - `Box<Sp<T>> -> Sp<Box<T>>` through `boxed`,
//! - `Sp<Result<T, E>> -> Result<Sp<T>, E>` through `transpose`.
//! Thus a function that takes as input a `Sp<&T>` is more general than
//! a function that takes a `&Sp<T>`.
//!
//! The above does not necessarily apply to functions that are strictly for
//! internal use, since if all callers are known the benefit of the generality
//! is lessened, and passing `&Sp<T>` may reduce copies of `Span` that come with
//! each invocation of `as_ref`.

use std::fmt;
use std::hash::{Hash, Hasher};

use chandeliers_err as err;

use crate::sp::Span;

/// A type that transparently implements `PartialEq` and `Hash`, to be used
/// in structs that carry additional data that should not be relevant in comparisons.
#[derive(Debug, Clone, Copy)]
pub struct Transparent<T> {
    /// Payload.
    pub inner: T,
}

impl From<Transparent<Span>> for Span {
    fn from(val: Transparent<Span>) -> Self {
        val.inner
    }
}

impl From<Span> for Transparent<Span> {
    fn from(span: Span) -> Self {
        Self { inner: span }
    }
}

impl From<usize> for Transparent<usize> {
    fn from(i: usize) -> Self {
        Self { inner: i }
    }
}

impl<T> PartialEq for Transparent<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> Eq for Transparent<T> {}

impl<T> Hash for Transparent<T> {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl<T: fmt::Display> fmt::Display for Transparent<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl Transparent<Span> {
    /// Map `join` to the inner `span`s
    #[must_use]
    pub fn join(self, other: Self) -> Option<Self> {
        Some(Self {
            inner: self.inner.join(other.inner)?,
        })
    }
}

impl<T: err::TrySpan> err::TrySpan for Transparent<T> {
    /// `Sp` always has a span, so `TrySpan` is guaranteed to succeed.
    fn try_span(&self) -> Option<Span> {
        self.inner.try_span()
    }
}
