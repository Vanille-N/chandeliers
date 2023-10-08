#![feature(core_intrinsics)]
#![allow(unused_comparisons)]
//#![allow(unused_macros)]

#[cfg(test)]
pub mod tests;

pub mod nillable;

#[macro_use]
pub mod time_travel;

#[macro_use]
pub mod candle;

pub mod traits {
    pub use crate::time_travel::{Ago, Update};
}

pub mod macros {
    pub use crate::assert_is;
    pub use crate::{
        binop, cmp, float, ifx, lit, nil, node_trace, substep, then, tick, ty, unop, update, var,
    };
}
