#![feature(core_intrinsics)]
#![allow(unused_comparisons)]
#![allow(unused_macros)]

use std::fmt;

#[cfg(test)]
pub mod sanity;
#[cfg(test)]
pub mod impls;

#[derive(Debug, Clone, Copy)]
pub enum Nillable<T> {
    Nil,
    Defined(T),
}

impl<T> Default for Nillable<T> {
    fn default() -> Self {
        Nillable::Nil
    }
}

impl<T: fmt::Display> fmt::Display for Nillable<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nillable::Nil => write!(f, "nil"),
            Nillable::Defined(t) => write!(f, "{}", t),
        }
    }
}

mod nillable_impl_ops {
    use super::Nillable::{self, *};
    use std::ops;
    use std::cmp;

    impl<T> Nillable<T> {
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

    impl<T> ops::Add for Nillable<T>
    where
        T: ops::Add<Output = T>,
    {
        type Output = Nillable<T>;
        fn add(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs + rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Mul for Nillable<T>
    where
        T: ops::Mul<Output = T>,
    {
        type Output = Nillable<T>;
        fn mul(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs * rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Sub for Nillable<T>
    where
        T: ops::Sub<Output = T>,
    {
        type Output = Nillable<T>;
        fn sub(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs - rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Div for Nillable<T>
    where
        T: ops::Div<Output = T>,
    {
        type Output = Nillable<T>;
        fn div(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs / rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Rem for Nillable<T>
    where
        T: ops::Rem<Output = T>,
    {
        type Output = Nillable<T>;
        fn rem(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs % rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::BitOr for Nillable<T>
    where
        T: ops::BitOr<Output = T>,
    {
        type Output = Nillable<T>;
        fn bitor(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs | rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::BitXor for Nillable<T>
    where
        T: ops::BitXor<Output = T>,
    {
        type Output = Nillable<T>;
        fn bitxor(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs ^ rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::BitAnd for Nillable<T>
    where
        T: ops::BitAnd<Output = T>,
    {
        type Output = Nillable<T>;
        fn bitand(self, other: Nillable<T>) -> Nillable<T> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs & rhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Not for Nillable<T>
    where
        T: ops::Not<Output = T>,
    {
        type Output = Nillable<T>;
        fn not(self) -> Nillable<T> {
            match self {
                Defined(lhs) => Defined(!lhs),
                _ => Nil,
            }
        }
    }

    impl<T> ops::Neg for Nillable<T>
    where
        T: ops::Neg<Output = T>,
    {
        type Output = Nillable<T>;
        fn neg(self) -> Nillable<T> {
            match self {
                Defined(lhs) => Defined(-lhs),
                _ => Nil,
            }
        }
    }

    // Important: Nil is never equal to a value
    impl<T> Nillable<T>
    where T: PartialEq {
        pub fn eq(self, other: Self) -> Option<bool> {
            use Nillable::*;
            match (self, other) {
                (Defined(this), Defined(other)) => Some(this == other),
                _ => None,
            }
        }

        pub fn is(&self, other: Self) -> bool {
            use Nillable::*;
            match (self, other) {
                (Defined(this), Defined(other)) => this == &other,
                (Nil, Nil) => true,
                _ => false,
            }
        }

    }

    impl<T> Nillable<T>
    where T: PartialOrd {
        pub fn cmp(self, other: Self) -> Option<cmp::Ordering> {
            match (self, other) {
                (Defined(this), Defined(other)) => this.partial_cmp(&other),
                _ => None,
            }
        }
    }

    impl Nillable<bool> {
        pub fn truth(self) -> bool {
            match self {
                Defined(true) => true,
                _ => false,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct O<T> {
    pub current: Nillable<T>,
}

#[derive(Debug, Clone, Default)]
pub struct S<N, T> {
    pub current: Nillable<T>,
    pub previous: N,
}

impl<T: fmt::Display> fmt::Display for O<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.current)
    }
}
impl<N: fmt::Display, T: fmt::Display> fmt::Display for S<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}->{}", self.previous, self.current)
    }
}

#[macro_export]
macro_rules! past_ty {
    ( $t:ty, ) => { $crate::O<$t> };
    ( $t:ty, + $( rest:tt )* ) => { $crate::S<past_ty!($t $($rest)*), $t> };
}

#[macro_export]
macro_rules! present_ty {
    ( $t:ty ) => { $crate::Nillable<$t> };
}

#[macro_export]
macro_rules! ty_mapping {
    ( float ) => { f64 };
    ( int ) => { i64 };
    ( bool ) => { bool };
    ( $other:ident ) => { $other };
}

#[macro_export]
macro_rules! ty {
    ( $t:ident ) => { $crate::present_ty!($crate::ty_mapping!($t)) };
    ( $t:ident + $($rest:tt)* ) => { $crate::past_ty!($crate::ty_mapping!($t), $($rest)*) };
}

#[macro_export]
macro_rules! o {
    ($e:expr) => { O { current: $e } };
}

#[macro_export]
macro_rules! s {
    ($e:expr, $n:expr) => {
        S { current: $e, previous: $n }
    };
}

pub trait Ago<T> {
    fn ago(&self, dt: usize) -> Nillable<T>;
}

impl<T: Clone> Ago<T> for O<T> {
    #[track_caller]
    fn ago(&self, dt: usize) -> Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => self.current.clone(),
            _ => panic!("Tried to look too much into the past"),
        }
    }
}

impl<N, T: Clone> Ago<T> for S<N, T>
where
    N: Ago<T>,
{
    #[track_caller]
    fn ago(&self, dt: usize) -> Nillable<T> {
        match dt {
            0 => panic!(
                "Cannot look into the past at distance 0, use the current environment instead"
            ),
            1 => self.current.clone(),
            dt => self.previous.ago(dt - 1),
        }
    }
}

// History: forget one step
trait Downcast<T> {
    fn downcast(self) -> T;
}

impl<T> Downcast<O<T>> for S<O<T>, T> {
    fn downcast(self) -> O<T> {
        o!(self.current)
    }
}

impl<N, T> Downcast<S<N, T>> for S<S<N, T>, T>
where
    S<N, T>: Downcast<N>,
{
    fn downcast(self) -> S<N, T> {
        s!(self.current, self.previous.downcast())
    }
}

// Prepend to history
pub trait Extend<N, T>: Sized {
    // Required methods
    fn extend(self, new: Nillable<T>) -> N;
    // Provided methods
    fn extend_undefined(self) -> N {
        self.extend(Nillable::Nil)
    }
}

impl<T> Extend<S<O<T>, T>, T> for O<T> {
    fn extend(self, new: Nillable<T>) -> S<O<T>, T> {
        s!(new, self)
    }
}

impl<N, T> Extend<S<S<N, T>, T>, T> for S<N, T>
where
    N: Extend<S<N, T>, T>,
{
    fn extend(self, new: Nillable<T>) -> S<S<N, T>, T> {
        s!(new, self.previous.extend(self.current))
    }
}

// Shift history by 1
pub trait Update<T>: Sized {
    // Required method
    fn update(self, new: Nillable<T>) -> Self;
    fn redefine(self, new: Nillable<T>) -> Self;
    // Provided methods
    fn update_mut(&mut self, new: Nillable<T>) {
        replace_with::replace_with_or_abort(self, |s| s.update(new));
    }
    fn update_undefined(self) -> Self {
        self.update(Nillable::Nil)
    }
    fn update_mut_undefined(&mut self) {
        self.update_mut(Nillable::Nil);
    }

    fn redefine_mut(&mut self, new: Nillable<T>) {
        replace_with::replace_with_or_abort(self, |s| s.redefine(new));
    }
}

impl<T> Update<T> for O<T> {
    fn update(self, new: Nillable<T>) -> Self {
        Self { current: new }
    }
    fn redefine(mut self, new: Nillable<T>) -> Self {
        self.current = new;
        self
    }
}

impl<N, T> Update<T> for S<N, T>
where
    S<N, T>: Downcast<N>,
{
    fn update(self, new: Nillable<T>) -> Self {
        s!(new, self.downcast())
    }
    fn redefine(mut self, new: Nillable<T>) -> Self {
        self.current = new;
        self
    }
}

// History of arbitrary values
pub trait IntoHistory: Sized {
    fn into_history(self) -> O<Self> {
        o!(Nillable::Defined(self))
    }
    fn empty_history() -> O<Self> {
        o!(Nillable::Nil)
    }
}
impl<T: Sized> IntoHistory for T {}

#[macro_export]
macro_rules! update {
    ($this:ident, $var:ident) => {
        $this.$var.update_mut($var);
    };
}

#[macro_export]
macro_rules! var {
    ($this:ident <~ 0 ; $field:tt) => {
        $field
    };
    ($this:ident <~ $dt:expr ; $field:tt) => {
        $this.$field.ago($dt)
    };
}

#[macro_export]
macro_rules! lit {
    ($lit:expr) => {
        $crate::Nillable::Defined($lit)
    };
}
#[macro_export]
macro_rules! nil {
    () => {
        $crate::Nillable::Nil
    };
}

#[macro_export]
macro_rules! tick {
    ($this:ident) => {
        $this.__clock += 1
    };
}

#[macro_export]
macro_rules! then {
    ($this:ident <~ $dt:expr ; $lhs:expr, $rhs:expr) => {
        if std::intrinsics::likely($this.__clock > $dt) {
            $rhs
        } else {
            $lhs
        }
    };
}
#[macro_export]
macro_rules! float {
    ($val:expr) => {
        $val.map(|i| i as f64)
    };
}
#[macro_export]
macro_rules! substep {
    ($this:ident <~ $dt:expr ; $id:tt => { $( $arg:expr, )* } ) => {
        if std::intrinsics::likely($this.__clock >= $dt) {
            $this.__nodes_blocks.$id
                .update_mut(
                    $( $arg ),*
                )
        } else { nil!() }
    }
}
#[macro_export]
macro_rules! ifx {
    ( ( $b:expr ) then { $yes:expr } else { $no:expr }) => {
        if $b.truth() {
            $yes
        } else {
            $no
        }
    }
}
#[macro_export]
macro_rules! binop {
    ($op:tt ; $lhs:expr, $rhs:expr) => { $lhs $op $rhs };
}
#[macro_export]
macro_rules! unop {
    ($op:tt ; $e:expr) => { $op $e };
}
#[macro_export]
macro_rules! cmp {
    (== ; $lhs:expr, $rhs:expr) => {{
        match $lhs.eq($rhs) {
            Some(true) => lit!(true),
            Some(false) => lit!(false),
            None => nil!(),
        }
    }};
    (!= ; $lhs:expr, $rhs:expr) => {{
        match $lhs.eq($rhs) {
            Some(true) => lit!(false),
            Some(false) => lit!(true),
            None => nil!(),
        }
    }};
    (< ; $lhs:expr, $rhs:expr) => {{
        use core::cmp::Ordering as O;
        match $lhs.cmp($rhs) {
            Some(O::Less) => lit!(true),
            Some(_) => lit!(false),
            _ => nil!(),
        }
    }};
    (> ; $lhs:expr, $rhs:expr) => {{
        use core::cmp::Ordering as O;
        match $lhs.cmp($rhs) {
            Some(O::Greater) => lit!(true),
            Some(_) => lit!(false),
            _ => nil!(),
        }
    }};
    (<= ; $lhs:expr, $rhs:expr) => {{
        use core::cmp::Ordering as O;
        match $lhs.cmp($rhs) {
            Some(O::Less | O::Equal) => lit!(true),
            Some(_) => lit!(false),
            _ => nil!(),
        }
    }};
    (>= ; $lhs:expr, $rhs:expr) => {{
        use core::cmp::Ordering as O;
        match $lhs.cmp($rhs) {
            Some(O::Greater | O::Equal) => lit!(true),
            Some(_) => lit!(false),
            _ => nil!(),
        }
    }};
}

pub mod macros {
    pub use super::{lit, binop, unop, cmp, nil, var, then, update, tick, substep, ty, float, ifx};
    pub use super::{Update, Ago};
}
