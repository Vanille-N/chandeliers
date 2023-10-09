//! Possibly uninitialized values.
//!
//! This modules defines the `Nillable<T>` datatype which implements
//! values of type `T` with an extra element `Nil` that contaminates any
//! expression it is a part of.
//! Unlike `Option<T>`, `Nillable<T>` implements all the binary operators
//! that one can apply to `T`.
//!
//! E.g.
//! - `Defined(5) + Defined(6)` is `Defined(11)`
//! - `Defined(5) + Nil` is `Nil`
//! - `Nil + Defined(0.1)` is `Nil`

use std::fmt;

/// Values of `T` or `Nil`.
#[derive(Debug, Clone, Copy)]
pub enum Nillable<T> {
    Defined(T),
    Nil,
}

pub use Nillable::*;

/// The default value of a `Nillable` is always `Nil`.
impl<T> Default for Nillable<T> {
    #[inline(always)]
    fn default() -> Self {
        Nil
    }
}

impl<T: fmt::Display> fmt::Display for Nillable<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Defined(t) => write!(f, "{}", t),
        }
    }
}

impl<T> Nillable<T> {
    /// This function is the identity, but its type constraints
    /// can help the compiler determine the associated type `T` for `Nil`.
    pub fn with_type_of(self, _other: Self) -> Self {
        self
    }
}

impl<T> Nillable<T> {
    /// Extract the inner value.
    ///
    /// Panics: if `self` is `Nil`.
    #[inline(always)]
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            Defined(t) => t,
            Nil => panic!("Tried to unwrap a nil value"),
        }
    }
}

impl<T> Nillable<T> {
    /// Apply the given function to the inner value.
    #[inline(always)]
    pub fn map<F, U>(self, f: F) -> Nillable<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Defined(t) => Defined(f(t)),
            _ => Nil,
        }
    }
}
/// `Nillable` implements all standard binary operators (not comparison
/// operators though) by mapping them to the inner value if it exists.
macro_rules! nillable_impl_ops_binary {
    ($trait:ident, $func:ident) => {
        impl<T> std::ops::$trait for Nillable<T>
        where
            T: std::ops::$trait<Output = T>,
        {
            type Output = Self;

            #[inline(always)]
            fn $func(self, other: Self) -> Self {
                match (self, other) {
                    (Defined(lhs), Defined(rhs)) => Defined(lhs.$func(rhs)),
                    _ => Nil,
                }
            }
        }
    };
}

nillable_impl_ops_binary!(Add, add);
nillable_impl_ops_binary!(Mul, mul);
nillable_impl_ops_binary!(Sub, sub);
nillable_impl_ops_binary!(Div, div);
nillable_impl_ops_binary!(Rem, rem);
nillable_impl_ops_binary!(BitOr, bitor);
nillable_impl_ops_binary!(BitXor, bitxor);
nillable_impl_ops_binary!(BitAnd, bitand);

/// `Nillable` implements all standard unary operators by mapping them to the
/// inner value if it exists.
macro_rules! nillable_impl_ops_unary {
    ($trait:ident, $func:ident) => {
        impl<T> std::ops::$trait for Nillable<T>
        where
            T: std::ops::$trait<Output = T>,
        {
            type Output = Self;

            #[inline(always)]
            fn $func(self) -> Self {
                match self {
                    Defined(lhs) => Defined(lhs.$func()),
                    _ => Nil,
                }
            }
        }
    };
}

nillable_impl_ops_unary!(Not, not);
nillable_impl_ops_unary!(Neg, neg);

impl<T> Nillable<T>
where
    T: PartialEq,
{
    /// Equality test for `Nillable`.
    ///
    /// `Nillable` does not implement equality, because `Nil` contaminates
    /// all expressions it is a part of.
    /// The function `eq` implements only a partial equality that
    /// is not reflexive where `Nil` is not comparable to itself.
    #[inline(always)]
    pub fn eq(self, other: Self) -> Option<bool> {
        match (self, other) {
            (Defined(this), Defined(other)) => Some(this == other),
            _ => None,
        }
    }

    /// Identity test for `Nillable`.
    ///
    /// Determines whether `self` and `other` are identical.
    /// `Nil` is identical to itself, and two `Defined(_)` are
    /// identical if their inner values are `PartialEq`.
    #[inline(always)]
    pub fn is(&self, other: Self) -> bool {
        match (self, other) {
            (Defined(this), Defined(other)) => this == &other,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

/// Identity assertion.
///
/// Since `Nillable<T>` does not implement `PartialEq`,
/// the canonical way to perform equality assertions is
/// `assert_is!(a, b)` that panics if `a` and `b` are not identical.
#[macro_export]
macro_rules! assert_is {
    ($lhs:expr, $rhs:expr) => {
        if !$lhs.is($rhs) {
            panic!("{} is not identical to {}", $lhs, $rhs.with_type_of($lhs));
        }
    };
}

impl<T> Nillable<T>
where
    T: PartialOrd,
{
    /// Partial ordering of `Nillable`s.
    ///
    /// Similarly to equality, `Nillable` does not implement `PartialOrd`
    /// because `Nil` is incomparable with every other value.
    #[inline(always)]
    pub fn cmp(self, other: Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Defined(this), Defined(other)) => this.partial_cmp(&other),
            _ => None,
        }
    }
}