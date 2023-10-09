//! Syntax and semantics of the Candle language.
//!
//! Candle is a shallow embedding of
//! [Lustre](https://en.wikipedia.org/wiki/Lustre_(programming_language))
//! in Rust.
//!
//! How to use the macros below:
//! A `struct` described using the constructs of Candle simulates a
//! Lustre `node` by
//! - deriving the `Default` trait,
//! - having a `__clock` field of type `usize`,
//! - (optional) having a `__nodes` field of type a tuple of all the subnodes
//!   that the node uses,
//! - (optional but recommended) having a `__trace` boolean field that enables
//!   priting debug information,
//! - having fields of a type provided by the `ty!(_)` macro that converts
//!   standard types into types with a memory of the past,
//! - providing an `update_mut` method of signature
//!   `(&mut self, ty!(...)) -> ty!(...)` that takes the node inputs,
//!   applies one step of computation, and returns the updated values of
//!   the node outputs.
//!
//! We provide here a thoroughly commented implementation of a simple integer
//! counter in Candle and Lustre side-by-side to showcase the general structure.
//! See more examples in `tests/impls/*.rs`
//!
//! ### Simple integer counter
//!
//! An integer counter has no inputs, and a single integer output that starts
//! at `0` and increments by one at each time step.
//!
//! A Lustre implementation would look like this:
//! ```ml
//! node counter() returns (n : int);
//! let
//!   n = 0 fby n + 1;
//! tel;
//! ```
//! In Candle we would define the same node like this:
//!
//! ```
//! #![feature(core_intrinsics)]
//! use chandeliers_sem::macros::*;
//!
//! #[allow(non_camel_case_types)]
//! #[derive(Default)]
//! pub struct counter {
//!     __clock: usize,
//!     __trace: bool,
//!     n: ty!(int+),
//!     __nodes: (),
//! }
//!
//! impl counter {
//!     pub fn update_mut(&mut self) -> ty!(int) {
//!         node_trace!(self, "() => counter(n={})", self.n);
//!         let n = later!(self <~ 0; lit!(0), var!(self <~ 1; n) + lit!(1));
//!         update!(self, n);
//!         tick!(self);
//!         node_trace!(self, "counter(n={}) => (n={})", self.n, self.n);
//!         n
//!     }
//! }
//!
//! fn usage() {
//!     let mut c = counter::default();
//!     for i in 0..10 {
//!         assert_is!(c.update_mut(), lit!(i));
//!     }
//! }
//! ```
//! You may notice that there are a lot of low level and error-prone details
//! here: the numbers in each `self <~ n` are carefully chosen, and we must
//! not forget to invoque `tick!(self)` to increment the clock
//! or `update!(self, n)` to make the update persistent.
//!
//! That is
//!
//! In the above code, the irreducible template that will be used for all
//! definitions is:
//! ```ignore
//! #[derive(Default)]
//! pub struct ... {
//!     // The names "__clock", "__trace", and "__nodes" are mandatory,
//!     // as they are hard-coded in some macros.
//!     __clock: usize,
//!     __trace: bool,
//!     // Here you should put the subnodes that you use, as a tuple.
//!     // If the node is primive and has no subnodes, this field is optional.
//!     __nodes: (...),
//!     ... // add any extra variables that you want to keep in-between
//!         // executions of `update_mut`.
//! }
//!
//! impl ... {
//!     pub fn update_mut(&mut self, ...) -> ... {
//!         node_trace!(self, ...); // Print any relevant information
//!         ...
//!         // You must always increment the clock once per call to `update_mut`.
//!         tick!(self);
//!         // And you must *never perform any computation* after incrementing the clock.
//!         node_trace!(self, ...); // Print any relevant information
//!         ... // (return the computed value(s) here)
//!     }
//! }
//! ```
//! Keep reading this document for explanations of the individual constructs.

/// Do not use explicitly, call `ty` instead.
/// If you don't know `S` and `O` and want to understand the purpose of
/// this macro, consult module `time_travel`.
///
/// This macro converts a scalar type into a stream of fixed size.
/// ```ignore
/// past_ty!(T,) ~ O<T>
/// past_ty!(T, +) ~ S<O<T>, T>
/// past_ty!(T, +++) ~ S<S<S<O<T>, T>, T>, T>
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! past_ty {
    ( $t:ty, ) => { $crate::time_travel::O<$t> };
    ( $t:ty, + $( $rest:tt )* ) => { $crate::time_travel::S<$crate::past_ty!($t, $($rest)*), $t> };
}

/// Do not use explicitly, call `ty` instead.
/// If you don't know `Nillable` and want to understand the purpose of
/// this macro, consult module `nillable`.
///
/// This macro converts a scalar type into a nillable type.
/// ```ignore
/// present_ty!(T) ~ Nillable<T>
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! present_ty {
    ( $t:ty ) => { $crate::nillable::Nillable<$t> };
}

/// Do not use explicitly, call `ty` instead.
///
/// Convert type names of Candle/Lustre to their internal Rust representation.
/// ```ignore
/// ty_mapping!(float) ~ f64
/// ty_mapping!(int) ~ i64
/// ty_mapping!(bool) ~ bool
/// ty_mapping!(T) ~ T
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! ty_mapping {
    ( float ) => {
        f64
    };
    ( int ) => {
        i64
    };
    ( bool ) => {
        bool
    };
    ( $other:ident ) => {
        $other
    };
}

/// Convert a Candle type into its internal Rust representation.
///
/// It builds upon the types provided by modules `nillable` and `time_travel`:
/// - `nillable` provides the `Nillable` monad that represents values extended
///   with a corrupted/uninitialized value.
/// - `time_travel` provides the `O<T>` and `S<N, T>` types that provide an
///   abstraction for looking into the past values of a variable:
///   `S(5, S(3, S(2, O(0))))` represents a value of which we know the 4
///   most recent values: 0 then 2 then 3 then 5 (most acient to most recent).
///
/// ```ignore
/// ty!(float) ~ Nillable<f64>
/// ty!(int+) ~ O<i64>
/// ty!(bool++++) ~ S<S<S<O<bool>, bool>, bool>, bool>
/// ```
#[macro_export]
macro_rules! ty {
    ( $t:ident ) => { $crate::present_ty!($crate::ty_mapping!($t)) };
    ( $t:ident + $($rest:tt)* ) => { $crate::past_ty!($crate::ty_mapping!($t), $($rest)*) };
}

/// Conditional debug printing. [pure]
///
/// Usage: `node_trace!(self, "debugging at step {}", self.__clock);` (statement)
/// Assumption: `self` has a field `__trace: bool`.
///
/// If the trace is enabled by means of `self.__trace`, pass all the
/// remaining arguments to `println!` for debugging.
#[macro_export]
macro_rules! node_trace {
    ($this:ident, $($fmt:expr),+) => {
        if $this.__trace {
            println!($($fmt),+);
        }
    }
}

/// Remember a variable for the next iteration.
///
/// Usage: `update!(self, $foo)` (statement)
/// Assumption: `self` has a field `$foo` AND `$foo` exists as a local variable.
///
/// Updates the internal field to reflect the new value of the variable in the
/// environment. This invoques `S<_, _>::update_mut` which shifts by one all
/// the past values of `$foo` (forgetting the most ancient one) and pushes
/// the new value as the most recent.
#[macro_export]
macro_rules! update {
    ($this:ident, $var:ident) => {{
        use $crate::traits::*;
        $this.$var.update_mut($var)
    };};
}

/// Fetch a variable from the environment. [pure]
///
/// Usage: `var!(self <~ $dt; $var)` (expression)
/// Assumption: if `$dt = 0` then `$var` exists as a local variable.
///             (`0` must appear textually, not as a variable)
/// Assumption: if `$dt > 0` then `$var` exists as a field of `self`.
///
/// Reads the value that `$var` had `$dt` steps ago.
/// This will either fetch `$var` from the current environment or
/// read it as a field of self.
#[macro_export]
macro_rules! var {
    ($this:ident <~ 0 ; $field:tt) => {
        $field
    };
    ($this:ident <~ $dt:expr ; $field:tt) => {{
        use $crate::traits::*;
        *$this.$field.ago($dt)
    }};
}

/// Wrap a value as a `Nillable`. [pure]
///
/// Usage: `lit!(true)`, `lit!(0.5)`, `lit!(42)` (expression)
///
/// Wraps a value of a compatible type (`bool`, `f64`, `i64`)
/// as a `Nillable`.
#[macro_export]
macro_rules! lit {
    ($lit:expr) => {
        $crate::nillable::Defined($lit)
    };
}

/// The uninitialized value. [pure]
///
/// Usage: `nil!()` (expression)
///
/// The value `Nil`.
#[macro_export]
macro_rules! nil {
    () => {
        $crate::nillable::Nil
    };
}

/// Increment the internal clock. [side-effects: only call once at the end]
///
/// Usage: `tick!(self)` (statement)
/// Assumption: `self` has a field `__clock: usize`.
///
/// Increments the clock by 1.
#[macro_export]
macro_rules! tick {
    ($this:ident) => {
        $this.__clock += 1
    };
}

/// Convert from `int` to `float`. [pure]
///
/// Usage: `float!($v)` (expression)
///
/// Converts a `Nillable<u64>` to a `Nillable<f64>`.
#[macro_export]
macro_rules! float {
    ($val:expr) => {
        $val.map(|i| i as f64)
    };
}

/// The `->` Lustre operator. [pure]
///
/// Usage: `later!(self <~ $dt; $lhs, $rhs)` (expression)
/// Assumption: `self` has a field `__clock: usize`
///
/// This performs a comparison against the clock:
/// for instants before `$dt` (inclusive) it will return the left value,
/// and for instants after `$dt` it will return the right value.
#[macro_export]
macro_rules! later {
    ($this:ident <~ $dt:expr ; $lhs:expr, $rhs:expr) => {
        if std::intrinsics::likely($this.__clock > $dt) {
            $rhs
        } else {
            $lhs
        }
    };
}

/// Invocation of subnodes. [side-effects: only call once for each node id]
///
/// Usage: `substep!(self <~ $dt; $id => { ... })` (expression)
/// Assumption: `self` has a `__clock` field.
/// Assumption: `self` has a tuple field `__nodes` of which the `$id`'th component
///             has a method `update_mut` of the correct arity, input and output types.
///
/// This advances the `$id`'th subnode by one step by providing it with the arguments to
/// its `update_mut` method (provide the comma-separated list of arguments between
/// the `{ ... }`) and gives the return value of said method.
/// This computation will not be performed if the clock is not at least `$dt`,
/// allowing delayed execution of subnodes.
#[macro_export]
macro_rules! substep {
    ($this:ident <~ $dt:expr ; $id:tt => { $( $arg:expr, )* } ) => {
        if std::intrinsics::likely($this.__clock >= $dt) {
            $this.__nodes.$id
                .update_mut(
                    $( $arg ),*
                )
        } else { nil!() }
    }
}

/// Conditional on `Nillable`s. [pure]
///
/// Usage: `ifx!(($b) then { $yes } else { $no })` (expression)
///
/// Will return `$yes` if `$b` holds (`true` and not `Nil`),
/// and `$no` if `$b` does not hold (`false` and not `Nil`).
/// A `Nil` test condition contaminates the entire expression.
#[macro_export]
macro_rules! ifx {
    ( ( $b:expr ) then { $yes:expr } else { $no:expr }) => {
        match $b {
            $crate::nillable::Defined(true) => $yes,
            $crate::nillable::Defined(false) => $no,
            $crate::nillable::Nil => $crate::nil!(),
        }
    };
}

/// Application of a binary operator. [pure]
///
/// Usage: `binop!(+; $lhs, $rhs)`, ... (expression)
/// Reminder: `Nillable` has wrapper implementations for `+`, `-`, `/`, `*`, `%`, `|`, `&`, `^`
///
/// If the two arguments properly implement the given binary operator,
/// this will apply it.
#[macro_export]
macro_rules! binop {
    ($op:tt ; $lhs:expr, $rhs:expr) => { $lhs $op $rhs };
}

/// Application of a unary operator. [pure]
///
/// Usage: `binop!(-; $val)`, ... (expression)
/// Reminder: `Nillable` has wrapper implementations for `-`, `!`
///
/// If the argument properly implements the given binary operator,
/// this will apply it.
#[macro_export]
macro_rules! unop {
    ($op:tt ; $e:expr) => { $op $e };
}

/// Do not use directly, call `cmp` instead.
#[doc(hidden)]
#[macro_export]
macro_rules! nillable_cmp_ord {
    ($ord:ident, $res:expr, $lhs:expr, $rhs:expr) => {
        match $lhs.cmp($rhs) {
            Some(std::cmp::Ordering::$ord) => lit!($res),
            Some(_) => lit!(!$res),
            None => nil!(),
        }
    };
}

/// Do not use directly, call `cmp` instead.
#[doc(hidden)]
#[macro_export]
macro_rules! nillable_cmp_eq {
    ($equal:expr, $lhs:expr, $rhs:expr) => {
        match $lhs.eq($rhs) {
            Some(true) => lit!($equal),
            Some(false) => lit!(!$equal),
            None => nil!(),
        }
    };
}

/// Comparison operators on `Nillable`. [pure]
///
/// Usage: `cmp!(<=; $lhs, $rhs)`, ... (expression)
/// Reminder: `Nillable` has wrapper implementations for `<=`, `>=`, `<`, `>`, `!=`, `==`
/// Warning: the pairs `<=` and `>`,  `>` and `<=`, `==` and `!=`,
///          are only opposites of each other as long as the arguments
///          are not `Nil`.
///
/// Evaluates the comparison of its arguments outputting a `Nillable<bool>`:
/// a `Nil` argument will contaminate the result.
#[macro_export]
macro_rules! cmp {
    (== ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_eq!(true, $lhs, $rhs)
    };
    (!= ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_eq!(false, $lhs, $rhs)
    };
    (< ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_ord!(Less, true, $lhs, $rhs)
    };
    (> ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_ord!(Greater, true, $lhs, $rhs)
    };
    (<= ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_ord!(Greater, false, $lhs, $rhs)
    };
    (>= ; $lhs:expr, $rhs:expr) => {
        $crate::nillable_cmp_ord!(Less, false, $lhs, $rhs)
    };
}