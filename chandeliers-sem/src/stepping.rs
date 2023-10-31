//! Some traits to make error messages of Candle less obscure.
//!
//! We define two traits that are identical in definition but opposite in
//! intent. `Embed` and `Trusted` describe conversions to and from
//! `Nillable` types, with tuple unpacking.
//!
//! `Step` is the general trait for all lustre nodes, and the inputs
//! and outputs of the node are described in terms of trusted and embed.

use crate::nillable::{Defined, Nillable};

/// Generalized conversion from a `T` to a `Nillable<T>`.
///
/// This distributes the `Nillable` to each element in the case of tuples.
/// Implementations are currently provided for tuples of length up to 10.
pub trait Embed {
    /// Wraped type.
    type Target;
    /// Conversion.
    fn embed(self) -> Self::Target;
}

/// Generalized conversion from a `Nillable<T>` to a `T`.
///
/// This distributes the `unwrap` to each element in the case of tuples.
/// Implementations are currently provided for tuples of length up to 10.
pub trait Trusted {
    /// Base type.
    type Target;
    /// Conversion.
    fn trusted(self) -> Self::Target;
}

impl<T> Embed for T
where
    T: crate::time_travel::Sealed,
{
    type Target = Nillable<T>;
    fn embed(self) -> Nillable<T> {
        Defined(self)
    }
}

impl<T> Embed for Nillable<T> {
    type Target = Self;
    fn embed(self) -> Self {
        self
    }
}

/// Implement `Embed` for a tuple of values by projecting `Embed` to each field.
/// This only applies to tuples of length 1 and above, since `()` is itself
/// nillable and handled separately.
macro_rules! embed_for_tuple {
    ( ( $( $i:tt : $T:ty ),* ) with $($decl:tt)*) => {
        impl$($decl)* Embed for ( $( $T, )* )
        where $( $T : Embed ),*
        {
            type Target = ( $( <$T>::Target, )* );
            #[allow(clippy::unused_unit)]
            fn embed(self) -> Self::Target {
                ( $( self.$i.embed(), )* )
            }
        }
    }
}

embed_for_tuple!((0: T0) with <T0>);
embed_for_tuple!((0: T0, 1: T1) with <T0, T1>);
embed_for_tuple!((0: T0, 1: T1, 2: T2) with <T0, T1, T2>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3) with <T0, T1, T2, T3>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4) with <T0, T1, T2, T3, T4>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5) with <T0, T1, T2, T3, T4, T5>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6)
    with <T0, T1, T2, T3, T4, T5, T6>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7)
    with <T0, T1, T2, T3, T4, T5, T6, T7>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9, 10: T10)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>);
// Who would ever need more than 10 arguments, right ?

impl<T> Trusted for Nillable<T> {
    type Target = T;
    fn trusted(self) -> T {
        self.unwrap()
    }
}

/// Implement `Trusted` for a tuple of values by projecting `Trusted` to each field.
/// This only applies to tuples of length 1 and above, since `()` is itself
/// nillable and handled separately.
macro_rules! trusted_for_tuple {
    ( ( $( $i:tt : $T:ty ),* ) with $($decl:tt)*) => {
        impl$($decl)* Trusted for ( $( $T, )* )
        where $( $T : Trusted ),*
        {
            type Target = ( $( <$T>::Target, )* );
            #[allow(clippy::unused_unit)]
            fn trusted(self) -> Self::Target {
                ( $( self.$i.trusted(), )* )
            }
        }
    }
}

trusted_for_tuple!((0: T0) with <T0>);
trusted_for_tuple!((0: T0, 1: T1) with <T0, T1>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2) with <T0, T1, T2>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3) with <T0, T1, T2, T3>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4) with <T0, T1, T2, T3, T4>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5) with <T0, T1, T2, T3, T4, T5>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6)
    with <T0, T1, T2, T3, T4, T5, T6>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7)
    with <T0, T1, T2, T3, T4, T5, T6, T7>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9, 10: T10)
    with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>);
// Who would ever need more than 10 arguments, right ?

#[test]
fn embed_trusted_inverse() {
    assert_eq!(5, 5.embed().trusted());
    assert_eq!((3, 4, true, 7), (3, 4, true, 7).embed().trusted());
}

/// Trait that represents a Lustre node.
///
/// Typically `Input` will be a tuple type of `T_i` if the node expects a tuple
/// of `Nillable<T_i>`, and `Output` will be a tuple of `U_i` if the node outputs
/// a tuple of `Nillable<U_i>`.
///
/// This interfaces well with the other traits `Embed` and `Trusted` in that
/// a node call will usually look like
/// `let (o1, o2) = n.step((i1, i2, i3).embed()).trusted();`
pub trait Step {
    /// Input tuple (expects "normal" rust types like `i64`, not `Nillable<i64>` or `O<i64>`.
    type Input: Embed;
    /// Output tuple (expects "normal" rust types like `i64`, not `Nillable<i64>` or `O<i64>`.
    type Output: Embed;
    /// Advance the node by one step.
    fn step(&mut self, input: <Self::Input as Embed>::Target) -> <Self::Output as Embed>::Target;
}
