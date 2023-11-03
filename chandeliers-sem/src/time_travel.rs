//! Fixed-size streams of values.
//!
//! This module implements an abstraction of the notion of "past values of a
//! variable" by implementing a type that can represent a circular stream of
//! bounded size.
//!
//! Warning: pushing a new value is linear in the number of values registered.
//! This should be fine because it is only linear with the size of the type,
//! so for a fixed type all implementations are constant-time.
//!
//! Warning: this crate imposes very strict typing constraints to minimize
//! mistakes such as cloning expensive data or constructing nonsensical streams.
//! If you find yourself fighting the type checker on trait bounds, it probably
//! means that you are using this library in a manner that is too low-level:
//! consider using directly the macros from the module `candle` that will
//! only construct well-formed types and well-typed terms.

use std::fmt;

use crate::nillable::{Defined, Nillable};

/// Current value of type `T`.
#[derive(Debug, Clone, Copy, Default)]
pub struct O<T> {
    /// Latest value.
    pub current: Nillable<T>,
}

/// Current value of type `T`, and previous values.
///
/// Trait bounds ensure that any `S<N, T>` is actually more specifically
/// a `S<S<S<S<..., T>, T>, T>, T>`.
#[derive(Debug, Clone, Default)]
pub struct S<N, T> {
    /// Latest value.
    pub current: Nillable<T>,
    /// Stream of past values.
    pub previous: N,
}

impl<T: fmt::Display> fmt::Display for O<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.current)
    }
}
impl<N: fmt::Display, T: fmt::Display> fmt::Display for S<N, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}->{}", self.previous, self.current)
    }
}

/// This trait guarantees that you cannot create a stream of objects that
/// are not supported efficiently.
pub(crate) trait Sealed: Sized {}
impl Sealed for i64 {}
impl Sealed for f64 {}
impl Sealed for bool {}
impl Sealed for () {}

/// This trait guarantees that you cannot construct an `S<_, _>` that
/// isn't usable.
trait Counting<T>: Sized {}
impl<T> Counting<T> for O<T> where T: Sealed {}
impl<N, T> Counting<T> for S<N, T>
where
    N: Counting<T>,
    T: Sealed,
{
}

/// A value of `O<_>`.
///
/// Usage: `o!(true)`
macro_rules! o {
    ($e:expr) => {
        O { current: $e }
    };
}

/// A value of `S<_, _>`.
///
/// Usage: `s!(true, o!(false))`
macro_rules! s {
    ($e:expr, $n:expr) => {
        S {
            current: $e,
            previous: $n,
        }
    };
}

/// Fetch value from `dt` instants ago.
#[expect(private_bounds, reason = "Sealed trait pattern")]
pub trait Ago<T>: Counting<T> {
    /// Read a past value.
    fn ago(&self, dt: usize) -> &Nillable<T>;
}

impl<T> Ago<T> for O<T>
where
    T: Sealed,
{
    #[inline]
    #[track_caller]
    #[expect(clippy::panic, reason = "Panic at the semantic level")]
    fn ago(&self, dt: usize) -> &Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => &self.current,
            _ => panic!("Tried to look too much into the past"),
        }
    }
}

impl<N, T> Ago<T> for S<N, T>
where
    T: Sealed,
    N: Ago<T>,
{
    #[inline]
    #[track_caller]
    #[expect(clippy::panic, reason = "Panic at the semantic level")]
    fn ago(&self, dt: usize) -> &Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => &self.current,
            dt => self.previous.ago(dt - 1),
        }
    }
}

/// Forget the most ancient value to remove one level of `S<_, _>` to the type.
trait Downcast<N, T>: Counting<T> {
    /// Extract the contents.
    fn downcast(self) -> N;
}

impl<T> Downcast<O<T>, T> for S<O<T>, T>
where
    T: Sealed,
{
    #[inline]
    fn downcast(self) -> O<T> {
        o!(self.current)
    }
}

impl<N, T> Downcast<S<N, T>, T> for S<S<N, T>, T>
where
    T: Sealed,
    N: Counting<T>,
    S<N, T>: Downcast<N, T>,
{
    #[inline]
    fn downcast(self) -> S<N, T> {
        s!(self.current, self.previous.downcast())
    }
}

/// Prepend a new value to the history to add one level of `S<_, _>` to the type.
pub trait Extend<N, T>: Sized {
    /// Extend the history with the most recent value.
    fn extend(self, new: Nillable<T>) -> N;

    /// Extend the history with an undefined value.
    #[inline]
    fn extend_undefined(self) -> N {
        self.extend(Nillable::Nil)
    }
}

impl<T> Extend<S<O<T>, T>, T> for O<T> {
    #[inline]
    fn extend(self, new: Nillable<T>) -> S<O<T>, T> {
        s!(new, self)
    }
}

impl<N, T> Extend<S<S<N, T>, T>, T> for S<N, T>
where
    N: Extend<S<N, T>, T>,
{
    #[inline]
    fn extend(self, new: Nillable<T>) -> S<S<N, T>, T> {
        s!(new, self.previous.extend(self.current))
    }
}

/// Advance the history by 1.
///
/// We add a new value a the front and at the same time we forget an
/// old value from the back to keep the same size.
#[expect(private_bounds, reason = "Sealed trait pattern")]
pub trait Update<T>: Counting<T> {
    /// Prepend a new value.
    #[must_use]
    fn update(self, new: Nillable<T>) -> Self;

    /// Mutable prepend a new value in-place.
    #[inline]
    fn update_mut(&mut self, new: Nillable<T>) {
        replace_with::replace_with_or_abort(self, |s| s.update(new));
    }
}

impl<T> Update<T> for O<T>
where
    T: Sealed,
{
    #[inline]
    fn update(self, new: Nillable<T>) -> Self {
        Self { current: new }
    }
}

impl<N, T> Update<T> for S<N, T>
where
    T: Sealed,
    N: Counting<T>,
    S<N, T>: Downcast<N, T>,
{
    #[inline]
    fn update(self, new: Nillable<T>) -> Self {
        s!(new, self.downcast())
    }
}

/// Construct a new empty or singleton history of a given type.
#[expect(private_bounds, reason = "Sealed trait pattern")]
pub trait IntoHistory: Sealed {
    /// History of size one (always an `O<_>`).
    #[inline]
    #[must_use]
    fn into_history(self) -> O<Self> {
        o!(Defined(self))
    }

    /// History of size zero (also an `O<_>`, but this time it contains a `Nil`).
    #[inline]
    #[must_use]
    fn empty_history() -> O<Self> {
        o!(Nillable::Nil)
    }
}
impl<T> IntoHistory for T where T: Sealed {}
