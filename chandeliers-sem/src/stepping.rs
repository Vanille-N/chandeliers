//! Some traits to make error messages of Candle less obscure.
//!
//! We define two traits that are identical in definition but opposite in
//! intent. `Embed` and `Trusted` describe conversions to and from
//! `Nillable` types, with tuple unpacking.
//!
//! `Step` is the general trait for all lustre nodes, and the inputs
//! and outputs of the node are described in terms of trusted and embed.

use crate::nillable::*;

pub trait Embed {
    type Target;
    fn embed(self) -> Self::Target;
}

pub trait Trusted {
    type Target;
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

macro_rules! embed_for_tuple {
    ( ( $( $i:tt : $T:ty ),* ) with $($decl:tt)*) => {
        impl$($decl)* Embed for ( $( $T, )* )
        where $( $T : Embed ),*
        {
            type Target = ( $( <$T>::Target, )* );
            fn embed(self) -> Self::Target {
                ( $( self.$i.embed(), )* )
            }
        }
    }
}

embed_for_tuple!(() with);
embed_for_tuple!((0: T0) with <T0>);
embed_for_tuple!((0: T0, 1: T1) with <T0, T1>);
embed_for_tuple!((0: T0, 1: T1, 2: T2) with <T0, T1, T2>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3) with <T0, T1, T2, T3>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4) with <T0, T1, T2, T3, T4>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5) with <T0, T1, T2, T3, T4, T5>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6) with <T0, T1, T2, T3, T4, T5, T6>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7) with <T0, T1, T2, T3, T4, T5, T6, T7>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8) with <T0, T1, T2, T3, T4, T5, T6, T7, T8>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9) with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>);
embed_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9, 10: T10) with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>);
// Who would ever need more than 10 arguments, right ?

impl<T> Trusted for Nillable<T> {
    type Target = T;
    fn trusted(self) -> T {
        self.unwrap()
    }
}

macro_rules! trusted_for_tuple {
    ( ( $( $i:tt : $T:ty ),* ) with $($decl:tt)*) => {
        impl$($decl)* Trusted for ( $( $T, )* )
        where $( $T : Trusted ),*
        {
            type Target = ( $( <$T>::Target, )* );
            fn trusted(self) -> Self::Target {
                ( $( self.$i.trusted(), )* )
            }
        }
    }
}

trusted_for_tuple!(() with);
trusted_for_tuple!((0: T0) with <T0>);
trusted_for_tuple!((0: T0, 1: T1) with <T0, T1>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2) with <T0, T1, T2>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3) with <T0, T1, T2, T3>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4) with <T0, T1, T2, T3, T4>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5) with <T0, T1, T2, T3, T4, T5>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6) with <T0, T1, T2, T3, T4, T5, T6>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7) with <T0, T1, T2, T3, T4, T5, T6, T7>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8) with <T0, T1, T2, T3, T4, T5, T6, T7, T8>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9) with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>);
trusted_for_tuple!((0: T0, 1: T1, 2: T2, 3: T3, 4: T4, 5: T5, 6: T6, 7: T7, 8: T8, 9: T9, 10: T10) with <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>);
// Who would ever need more than 10 arguments, right ?

#[test]
fn embed_trusted_inverse() {
    assert_eq!(5, 5.embed().trusted());
    assert_eq!((3, 4, true, 7), (3, 4, true, 7).embed().trusted());
}

pub trait Step {
    type Input: Embed;
    type Output: Embed;
    fn step(&mut self, input: <Self::Input as Embed>::Target) -> <Self::Output as Embed>::Target;
}
