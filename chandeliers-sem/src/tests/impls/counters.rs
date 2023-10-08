use crate::macros::*;

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
pub struct counting {
    __trace: bool,
    __clock: usize,
    n: ty!(int+),
}

impl counting {
    pub fn update_mut(&mut self) -> ty!(int) {
        let n = then!(self <~ 0; lit!(0), var!(self <~ 1; n) + lit!(1));
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

impl counting_twice {
    pub fn update_mut(&mut self) -> ty!(int) {
        let b = then!(self <~ 0; lit!(true), ! var!(self <~ 1; b));
        update!(self, b);
        let _0 = substep!(self <~ 0; 0 => {});
        let _1 = substep!(self <~ 0; 1 => {});
        let res = ifx!((b) then { _0 } else { _1 });
        tick!(self);
        res
    }
}

impl counting_late {
    pub fn update_mut(&mut self) -> ty!(int) {
        let _0 = substep!(self <~ 2; 0 => {});
        let c = then!(self <~ 0; lit!(0), then!(self <~ 1; lit!(0), _0));
        tick!(self);
        c
    }
}

#[test]
fn counting_twice_behavior() {
    let mut count = counting_twice::default();
    for i in 0..10 {
        let j = count.update_mut();
        assert_is!(j, lit!(i));
    }
}

#[test]
fn counting_late_behavior() {
    let mut count = counting_late::default();
    for i in 0..10 {
        let j = count.update_mut();
        let actual_i = 0.max(i - 2);
        assert_is!(j, lit!(actual_i));
    }
}
