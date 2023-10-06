#![feature(core_intrinsics)]

use std::fmt;

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

impl<T: fmt::Display> fmt::Display for  Nillable<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nillable::Nil => write!(f, "nil"),
            Nillable::Defined(t) => write!(f, "{}", t),
        }
    }
}

mod nillable_impl_ops {
    use super::Nillable::{self, *};
    use std::ops::{Add, Div, Mul, Sub};

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

    impl<L, O> Add for Nillable<L>
    where
        L: Add<Output = O>,
    {
        type Output = Nillable<O>;
        fn add(self, other: Nillable<L>) -> Nillable<O> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs + rhs),
                _ => Nil,
            }
        }
    }

    impl<L, O> Mul for Nillable<L>
    where
        L: Mul<Output = O>,
    {
        type Output = Nillable<O>;
        fn mul(self, other: Nillable<L>) -> Nillable<O> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs * rhs),
                _ => Nil,
            }
        }
    }

    impl<L, O> Sub for Nillable<L>
    where
        L: Sub<Output = O>,
    {
        type Output = Nillable<O>;
        fn sub(self, other: Nillable<L>) -> Nillable<O> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs - rhs),
                _ => Nil,
            }
        }
    }

    impl<L, O> Div for Nillable<L>
    where
        L: Div<Output = O>,
    {
        type Output = Nillable<O>;
        fn div(self, other: Nillable<L>) -> Nillable<O> {
            match (self, other) {
                (Defined(lhs), Defined(rhs)) => Defined(lhs / rhs),
                _ => Nil,
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

macro_rules! ty {
    (float) => { O<f64> };
    (int) => { O<i64> };
    (bool) => { O<bool> };
    ($other:ident) => { O<$other> };
    (float + $( $rest:tt )*) => { S<ty!(float $($rest)*), f64> };
    (int + $( $rest:tt )*) => { S<ty!(int $($rest)*), i64> };
    (bool + $( $rest:tt )*) => { S<ty!(int $($rest)*), bool> };
    ($other:ident + $( $rest:tt )*) => { S<ty!($other $($rest)*), $other> };
}

macro_rules! o {
    ($e:expr) => { O { current: $e } };
}
macro_rules! s {
    ($e:expr, $n:expr) => { S { current: $e, previous: $n } };
}

trait Ago<T> {
    fn ago(&self, dt: usize) -> Nillable<T>;
}

impl<T: Clone> Ago<T> for O<T> {
    fn ago(&self, dt: usize) -> Nillable<T> {
        if dt == 0 {
            self.current.clone()
        } else {
            Nillable::Nil
        }
    }
}

impl<N, T: Clone> Ago<T> for S<N, T>
where N: Ago<T> {
    fn ago(&self, dt: usize) -> Nillable<T> {
        if dt == 0 {
            self.current.clone()
        } else {
            self.previous.ago(dt - 1)
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

#[test]
fn update_tests() {
    let def = |i| Nillable::Defined(i);
    let x = 1i64
        .into_history()
        .extend(def(3))
        .extend(def(5))
        .extend(def(7));
    dbg!(&x);
    let x = x.update(def(9)).update(def(11));
    dbg!(&x);
    //panic!();
}

// As a proof of concept we will implement here the following node
//
// node weighted_sum(x, y, weight : float) returns (sum : float);
// let
//   sum = weight * x + (1.0 - weight) * y;
// tel
//
// node cumul_avg(x : float) returns (avg : float);
// var n;
// let
//   n = 1 fby n + 1;
//   avg = weighted_sum(x, 0.0 fby avg, 1.0 / float(n));
// tel

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
struct weighted_sum {
    __clock: usize,
    // Inputs
    x: ty!(f64),
    y: ty!(f64),
    weight: ty!(f64),
    // Vars
    // (none)
    // Outputs
    sum: ty!(f64),
    // Subnodes
    // (none)
}

impl fmt::Display for weighted_sum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[ weighted_sum: (x={},y={},weight={}) => () => (sum={}) ]", self.x, self.y, self.weight, self.sum)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
struct cumul_avg {
    __clock: usize,
    // Inputs
    x: ty!(f64),
    // Vars
    n: ty!(i64+),
    // Outputs
    avg: ty!(f64+),
    // Subnodes
    __nodes_outputs: (ty!(f64),),
    __nodes_blocks: (weighted_sum,),
}

impl fmt::Display for cumul_avg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} <- {}", self.__nodes_outputs.0, self.__nodes_blocks.0)?;
        write!(f, "[ cumul_avg: (x={}) => (n={}) => (avg={}) ]", self.x, self.n, self.avg)
    }
}


macro_rules! expand {
    ($this:ident, $dt:expr, &, $val:expr) => { ($val)(&*$this, $dt) };
    ($this:ident, $dt:expr, ., $val:expr) => { $val };
}
macro_rules! input {
    ($field:ident := $val:expr) => {
        |this: &mut Self| {
            this.$field.update_mut($val);
        }
    };
}
macro_rules! advance {
    ($field:ident) => {
        |this: &mut Self| {
            this.$field.update_mut_undefined();
        }
    };
}
macro_rules! redefine {
    ($field:ident := <$v:tt> $val:expr) => {
        |this: &mut Self| {
            let val = expand!(this, 0, $v, $val);
            this.$field.redefine_mut(val);
        }
    };
}

macro_rules! add {
    (<$l:tt $r:tt> $lhs:expr, $rhs:expr) => {
        |this: &Self, dt: usize| {
            let lhs = expand!(this, dt, $l, $lhs);
            let rhs = expand!(this, dt, $r, $rhs);
            lhs + rhs
        }
    };
}
macro_rules! sub {
    (<$l:tt $r:tt> $lhs:expr, $rhs:expr) => {
        |this: &Self, dt: usize| {
            let lhs = expand!(this, dt, $l, $lhs);
            let rhs = expand!(this, dt, $r, $rhs);
            lhs - rhs
        }
    };
}
macro_rules! mul {
    (<$l:tt $r:tt> $lhs:expr, $rhs:expr) => {
        |this: &Self, dt: usize| {
            let lhs = expand!(this, dt, $l, $lhs);
            let rhs = expand!(this, dt, $r, $rhs);
            lhs * rhs
        }
    };
}
macro_rules! div {
    (<$l:tt $r:tt> $lhs:expr, $rhs:expr) => {
        |this: &Self, dt: usize| {
            let lhs = expand!(this, dt, $l, $lhs);
            let rhs = expand!(this, dt, $r, $rhs);
            lhs / rhs
        }
    };
}

macro_rules! field {
    ($field:ident) => {
        |this: &Self, dt: usize| this.$field.ago(dt)
    }
}
macro_rules! lit {
    ($lit:expr) => { Nillable::Defined($lit) }
}

macro_rules! tick {
    ($this:ident) => { $this.__clock += 1 };
}
macro_rules! ret {
    ($field:ident) => {
        |this: &Self| ( this.$field.current )
    }
}
macro_rules! fby {
    (<$l:tt $r:tt> $lhs:expr, $rhs:expr) => {
        |this: &Self, dt: usize| {
            if std::intrinsics::likely(this.__clock > dt) {
                expand!(this, dt+1, $r, $rhs)
            } else {
                expand!(this, dt, $l, $lhs)
            }
        }
    }
}
macro_rules! float {
    (<$v:tt> $val:expr) => {
        |this: &Self, dt: usize| {
            expand!(this, dt, $v, $val).map(|i| i as f64)
        }
    }
}
macro_rules! substep {
    ($id:tt, $( <$a:tt> $arg:expr, )*) => {
        |this: &mut Self| {
            this.__nodes_outputs.$id
                .update_mut(this.__nodes_blocks.$id
                    .update_mut(
                        $( expand!(this, 0, $a, $arg) ),*
                    )
                )
        }
    }
}

impl weighted_sum {
    fn update_mut(
        &mut self,
        x: Nillable<f64>,
        y: Nillable<f64>,
        weight: Nillable<f64>,
    ) -> Nillable<f64> {
        // Record inputs and step
        input!(x := x)(self);
        input!(y := y)(self);
        input!(weight := weight)(self);
        // Compute outputs
        advance!(sum)(self);
        redefine!(sum := <&>
            add!(<& &>
                mul!(<& &> field!(weight), field!(x)),
                mul!(<& &> sub!(<. &> lit!(1.0), field!(weight)), field!(y))
            )
        )(self);
        tick!(self);
        ret!(sum)(self)
    }
}

impl cumul_avg {
    fn update_mut(&mut self, x: Nillable<f64>) -> Nillable<f64> {
        // Record inputs and step
        input!(x := x)(self);
        // Initializations
        advance!(n)(self);
        redefine!(n := <&> fby!(<. &> lit!(1), add!(<& .> field!(n), lit!(1))))(self);
        // Step subnode
        advance!(avg)(self);
        substep!(0,
            <&> field!(x),
            <&> fby!(<. &> lit!(0.0), field!(avg)),
            <&> div!(<. &> lit!(1.0), float!(<&> field!(n))),
        )(self);
       // self.__nodes_outputs
        //    .0
        //    .update_mut(self.__nodes_blocks.0.update_mut(
        //        self.x.current,
        //        if std::intrinsics::likely(self.__clock > 0) {
        //            self.avg.current
        //        } else {
        //            Nillable::Defined(0.0)
        //        },
        //        Nillable::Defined(1.0)
        //            / self.n.current.map(|i| i as f64),
        //    ));
        // Compute outputs
        self.avg.redefine_mut(self.__nodes_outputs.0.current);
        self.__clock += 1;
        self.avg.current
    }
}

#[test]
fn cumul_avg_behavior() {
    let mut node = cumul_avg::default();
    println!("{}\n", &node);
    let v = node.update_mut(Nillable::Defined(0.5));
    println!("{}\n", &node);
    let v = node.update_mut(Nillable::Defined(1.0));
    println!("{}\n", &node);
    let v = node.update_mut(Nillable::Defined(0.3));
    println!("{}\n", &node);
    let v = node.update_mut(Nillable::Defined(0.2));
    println!("{}\n", &node);
    panic!();
}
