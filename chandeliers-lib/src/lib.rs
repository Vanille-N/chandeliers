#![feature(core_intrinsics)]

#[derive(Debug, Clone, Copy)]
enum Nillable<T> {
    Nil,
    Defined(T),
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

#[derive(Debug, Clone, Copy)]
struct O<T> {
    current: Nillable<T>,
}

#[derive(Debug, Clone)]
struct S<N, T> {
    current: Nillable<T>,
    previous: N,
}

// History: forget one step
trait Downcast<T> {
    fn downcast(self) -> T;
}

impl<T> Downcast<O<T>> for S<O<T>, T> {
    fn downcast(self) -> O<T> {
        O {
            current: self.current,
        }
    }
}

impl<N, T> Downcast<S<N, T>> for S<S<N, T>, T>
where
    S<N, T>: Downcast<N>,
{
    fn downcast(self) -> S<N, T> {
        S {
            current: self.current,
            previous: self.previous.downcast(),
        }
    }
}

// Prepend to history
trait Extend<N, T>: Sized {
    // Required methods
    fn extend(self, new: Nillable<T>) -> N;
    // Provided methods
    fn extend_undefined(self) -> N {
        self.extend(Nillable::Nil)
    }
}

impl<T> Extend<S<O<T>, T>, T> for O<T> {
    fn extend(self, new: Nillable<T>) -> S<O<T>, T> {
        S {
            current: new,
            previous: self,
        }
    }
}

impl<N, T> Extend<S<S<N, T>, T>, T> for S<N, T>
where
    N: Extend<S<N, T>, T>,
{
    fn extend(self, new: Nillable<T>) -> S<S<N, T>, T> {
        S {
            current: new,
            previous: self.previous.extend(self.current),
        }
    }
}

// Shift history by 1
trait Update<T>: Sized {
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
        Self {
            current: new,
            previous: self.downcast(),
        }
    }
    fn redefine(mut self, new: Nillable<T>) -> Self {
        self.current = new;
        self
    }
}

// History of arbitrary values
trait IntoHistory: Sized {
    fn into_history(self) -> O<Self> {
        O {
            current: Nillable::Defined(self),
        }
    }
    fn empty_history() -> O<Self> {
        O {
            current: Nillable::Nil,
        }
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
//   n = 0 fby n + 1;
//   avg = 0.0 fby weighted_sum(x, avg, 1.0 / float(n + 1));
// tel

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
struct weighted_sum {
    clock: usize,
    // Inputs
    x: O<f64>,
    y: O<f64>,
    weight: O<f64>,
    // Vars
    // (none)
    // Outputs
    sum: O<f64>,
    // Subnodes
    // (none)
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
struct cumul_avg {
    clock: usize,
    // Inputs
    x: O<f64>,
    // Vars
    n: S<O<i64>, i64>,
    // Outputs
    avg: S<O<f64>, f64>,
    // Subnodes
    nodes_outputs: (O<f64>,),
    nodes_blocks: (weighted_sum,),
}

impl weighted_sum {
    fn create() -> Self {
        Self {
            clock: 0,
            x: f64::empty_history(),
            y: f64::empty_history(),
            weight: f64::empty_history(),
            sum: f64::empty_history(),
        }
    }

    fn update_mut(
        &mut self,
        x: Nillable<f64>,
        y: Nillable<f64>,
        weight: Nillable<f64>,
    ) -> Nillable<f64> {
        // Record inputs and step
        self.x.update_mut(x);
        self.y.update_mut(y);
        self.weight.update_mut(weight);
        self.sum.update_mut_undefined();
        // Compute outputs
        self.sum.redefine_mut(
            self.weight.current * self.x.current
                + (Nillable::Defined(1.0) - self.weight.current) * self.y.current,
        );
        self.clock += 1;
        self.sum.current
    }
}

impl cumul_avg {
    fn create() -> Self {
        Self {
            clock: 0,
            avg: f64::empty_history().extend_undefined(),
            n: i64::empty_history().extend_undefined(),
            x: f64::empty_history(),
            nodes_outputs: (f64::empty_history(),),
            nodes_blocks: (weighted_sum::create(),),
        }
    }

    fn update_mut(&mut self, x: Nillable<f64>) -> Nillable<f64> {
        // Record inputs and step
        self.x.update_mut(x);
        // Initializations
        if std::intrinsics::unlikely(self.clock == 0) {
            self.avg.redefine_mut(Nillable::Defined(0.0));
            self.n.redefine_mut(Nillable::Defined(0));
        }
        self.n.update_mut(self.n.current + Nillable::Defined(1));
        // Step subnode
        self.nodes_outputs
            .0
            .update_mut(self.nodes_blocks.0.update_mut(
                self.x.current,
                self.avg.current,
                Nillable::Defined(1.0)
                    / (Nillable::Defined(1.0) + self.n.previous.current.map(|i| i as f64)),
            ));
        // Compute outputs
        self.avg.update_mut(self.nodes_outputs.0.current);
        self.clock += 1;
        self.avg.current
    }
}

#[test]
fn cumul_avg_behavior() {
    let mut node = cumul_avg::create();
    dbg!(&node);
    let v = node.update_mut(Nillable::Defined(0.5));
    dbg!(v, &node);
    let v = node.update_mut(Nillable::Defined(1.0));
    dbg!(v, &node);
    let v = node.update_mut(Nillable::Defined(0.3));
    dbg!(v, &node);
    let v = node.update_mut(Nillable::Defined(0.2));
    dbg!(v, &node);
    panic!();
}
