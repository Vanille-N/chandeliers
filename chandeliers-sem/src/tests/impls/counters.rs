use crate::macros::*;
use crate::traits::*;

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting {
    __trace: bool,
    __clock: usize,
    n: ty!(int+),
}

impl Step for counting {
    type Input = ();
    type Output = i64;
    fn step(&mut self, __inputs: ty!()) -> ty!(int) {
        implicit_clock!(__inputs);
        let n = later!(self <~ 0; lit!(0), var!(self <~ 1; n) + lit!(1));
        update!(self, n);
        tick!(self);
        n
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting_twice {
    __trace: bool,
    __clock: usize,
    b: ty!(bool+),
    __nodes: (counting, counting),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting_late {
    __trace: bool,
    __clock: usize,
    __nodes: (counting,),
}

impl Step for counting_twice {
    type Input = ();
    type Output = i64;
    fn step(&mut self, __inputs: ty!()) -> ty!(int) {
        implicit_clock!(__inputs);
        let b = later!(self <~ 0; lit!(true), ! var!(self <~ 1; b));
        update!(self, b);
        let res = ifx!((b) then {
            substep!(self; 0 => {lit!(())})
        } else {
            substep!(self; 1 => {lit!(())})
        });
        tick!(self);
        res
    }
}

impl Step for counting_late {
    type Input = ();
    type Output = i64;
    fn step(&mut self, __inputs: ty!()) -> ty!(int) {
        implicit_clock!(__inputs);
        let c =
            later!(self <~ 0; lit!(0), later!(self <~ 1; lit!(0), substep!(self; 0 => {lit!(())})));
        tick!(self);
        c
    }
}

#[test]
fn counting_twice_behavior() {
    let mut count = counting_twice::default();
    for i in 0..10 {
        let j = count.step(().embed());
        assert_is!(j, lit!(i));
    }
}

#[test]
fn counting_late_behavior() {
    let mut count = counting_late::default();
    for i in 0..10 {
        let j = count.step(().embed());
        let actual_i = 0.max(i - 2);
        assert_is!(j, lit!(actual_i));
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting_parallel {
    __trace: bool,
    __clock: usize,
    __nodes: (counting, counting),
}

impl Step for counting_parallel {
    type Input = ();
    type Output = (i64, i64);
    fn step(&mut self, __inputs: ty!()) -> (ty!(int), ty!(int)) {
        implicit_clock!(__inputs);
        let _0 = substep!(self; 0 => {lit!(())});
        let _1 = substep!(self; 1 => {lit!(())});
        let (a, b) = (_0, _1);
        tick!(self);
        (a, b)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting_parallel_tester {
    __trace: bool,
    __clock: usize,
    __nodes: (counting_parallel,),
}

impl Step for counting_parallel_tester {
    type Input = ();
    type Output = ();
    fn step(&mut self, __inputs: ty!()) -> ty!() {
        implicit_clock!(__inputs);
        let _0 = substep!(self; 0 => {lit!(())});
        tick!(self);
        lit!(())
    }
}

#[test]
fn counting_parallel_test() {
    let mut cpt = counting_parallel_tester::default();
    cpt.step(().embed());
    cpt.step(().embed());
    cpt.step(().embed());
}
