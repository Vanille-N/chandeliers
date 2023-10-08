use crate::macros::*;

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
    __trace: bool,
    __clock: usize,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
struct cumul_avg {
    __trace: bool,
    __clock: usize,
    n: ty!(int+),
    avg: ty!(float+),
    __nodes_blocks: (weighted_sum,),
}

impl weighted_sum {
    fn update_mut(&mut self, x: ty!(float), y: ty!(float), weight: ty!(float)) -> ty!(float) {
        if self.__trace {
            println!("(x={},y={},weight={}) => weighted_sum()", x, y, weight);
        }
        // == BEGIN ==
        let sum = binop!(+;
            binop!(*; var!(self <~ 0; weight), var!(self <~ 0; x)),
            binop!(*;
                binop!(-; lit!(1.0), var!(self <~ 0; weight)),
                var!(self <~ 0; y)
            )
        );
        // == END ==
        tick!(self);
        if self.__trace {
            println!("weighted_sum() => (sum={})", sum);
        }
        sum
    }
}

impl cumul_avg {
    fn update_mut(&mut self, x: ty!(float)) -> ty!(float) {
        if self.__trace {
            println!("(x={}) => cumul_avg(n={})", x, self.n);
        }
        // == BEGIN ==
        let n = then!(self <~ 0; lit!(1), var!(self <~ 1; n) + lit!(1));
        update!(self, n);
        let avg = substep!(
            self <~ 0;
            0 => {
                var!(self <~ 0; x),
                then!(self <~ 0; lit!(0.0), var!(self <~ 1; avg)),
                binop!(/; lit!(1.0), float!(var!(self <~ 0; n))),
            }
        );
        update!(self, avg);
        // == END ==
        tick!(self);
        if self.__trace {
            println!("cumul_avg(n={},avg={}) => (avg={})", self.n, self.avg, avg);
        }
        avg
    }
}

#[test]
fn cumul_avg_behavior() {
    let mut node = cumul_avg::default();
    node.__trace = true;
    node.__nodes_blocks.0.__trace = true;
    let v = node.update_mut(lit!(0.5));
    println!("{}\n", v);
    let v = node.update_mut(lit!(1.0));
    println!("{}\n", v);
    let v = node.update_mut(lit!(0.3));
    println!("{}\n", v);
    let v = node.update_mut(lit!(0.2));
    println!("{}\n", v);
    //panic!();
}

#[test]
fn exist_all_ops() {
    let i = lit!(5);
    let f = lit!(5.0);
    let b = lit!(true);
    let _ = binop!(+; i, i);
    let _ = binop!(-; f, f);
    let _ = unop!(-; i);
    let _ = binop!(%; i, i);
    let _ = binop!(*; f, f);
    let _ = binop!(/; i, i);
    let _ = unop!(!; b);
    let _ = cmp!(==; b, b);
    let _ = binop!(|; i, i);
    let _ = binop!(&; b, b);
    let _ = binop!(^; i, i);
    let _ = cmp!(<; i, i);
    let _ = cmp!(<=; f, f);
    let _ = cmp!(>; f, f);
    let _ = cmp!(>=; i, i);
    let _ = cmp!(!=; i, i);
}

const BNIL: ty!(bool) = nil!();

macro_rules! assert_is {
    ($lhs:expr, $rhs:expr) => {
        if !$lhs.is($rhs) {
            panic!("{} is not identical to {}", $lhs, $rhs);
        }
    }
}

#[test]
fn truth() {
    assert!(lit!(true).truth());
    assert!(!lit!(false).truth());
    assert!(!BNIL.truth());
}

#[test]
fn all_unop_correct() {
    assert!(unop!(!; lit!(true)).is(lit!(false)));
    assert!(unop!(!; lit!(false)).is(lit!(true)));
    assert!(unop!(!; BNIL).is(nil!()));
    assert!(unop!(-; lit!(5)).is(lit!(-5)));
}

#[test]
fn all_binop_correct() {
    // + -  % * /
    assert!(binop!(+; lit!(5), lit!(10)).is(lit!(15)));
    assert!(binop!(-; lit!(4), nil!()).is(nil!()));
    assert!(binop!(%; lit!(10), lit!(3)).is(lit!(1)));
    assert!(binop!(*; lit!(2), lit!(9)).is(lit!(18)));
    // |
    assert!(binop!(|; lit!(true), lit!(true)).is(lit!(true)));
    assert!(binop!(|; lit!(true), lit!(false)).is(lit!(true)));
    assert!(binop!(|; lit!(false), lit!(true)).is(lit!(true)));
    assert!(binop!(|; lit!(false), lit!(false)).is(lit!(false)));
    assert!(binop!(|; lit!(true), nil!()).is(nil!()));
    // &
    assert!(binop!(&; lit!(true), lit!(true)).is(lit!(true)));
    assert!(binop!(&; lit!(true), lit!(false)).is(lit!(false)));
    assert!(binop!(&; lit!(false), lit!(true)).is(lit!(false)));
    assert!(binop!(&; lit!(false), lit!(false)).is(lit!(false)));
    assert!(binop!(&; lit!(true), nil!()).is(nil!()));
    // ^
    assert!(binop!(^; lit!(true), lit!(true)).is(lit!(false)));
    assert!(binop!(^; lit!(true), lit!(false)).is(lit!(true)));
    assert!(binop!(^; lit!(false), lit!(true)).is(lit!(true)));
    assert!(binop!(^; lit!(false), lit!(false)).is(lit!(false)));
    assert!(binop!(^; lit!(true), nil!()).is(nil!()));

}

#[test]
fn all_cmp_correct() {
    assert!(cmp!(<; lit!(4), lit!(5)).is(lit!(true)));
    assert!(cmp!(<; lit!(4), lit!(4)).is(lit!(false)));
    assert!(cmp!(<; lit!(5), lit!(4)).is(lit!(false)));
    assert!(cmp!(<; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(<; BNIL, BNIL).is(nil!()));
    //---
    assert!(cmp!(>; lit!(4), lit!(5)).is(lit!(false)));
    assert!(cmp!(>; lit!(4), lit!(4)).is(lit!(false)));
    assert!(cmp!(>; lit!(5), lit!(4)).is(lit!(true)));
    assert!(cmp!(>; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(>; BNIL, BNIL).is(nil!()));
    //---
    assert!(cmp!(<=; lit!(4), lit!(5)).is(lit!(true)));
    assert!(cmp!(<=; lit!(4), lit!(4)).is(lit!(true)));
    assert!(cmp!(<=; lit!(5), lit!(4)).is(lit!(false)));
    assert!(cmp!(<=; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(<=; BNIL, BNIL).is(nil!()));
    //---
    assert!(cmp!(>=; lit!(4), lit!(5)).is(lit!(false)));
    assert!(cmp!(>=; lit!(4), lit!(4)).is(lit!(true)));
    assert!(cmp!(>=; lit!(5), lit!(4)).is(lit!(true)));
    assert!(cmp!(>=; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(>=; BNIL, BNIL).is(nil!()));
    //---
    assert!(cmp!(==; lit!(4), lit!(5)).is(lit!(false)));
    assert!(cmp!(==; lit!(4), lit!(4)).is(lit!(true)));
    assert!(cmp!(==; lit!(5), lit!(4)).is(lit!(false)));
    assert!(cmp!(==; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(==; BNIL, BNIL).is(nil!()));
    //---
    assert!(cmp!(!=; lit!(4), lit!(5)).is(lit!(true)));
    assert!(cmp!(!=; lit!(4), lit!(4)).is(lit!(false)));
    assert!(cmp!(!=; lit!(5), lit!(4)).is(lit!(true)));
    assert!(cmp!(!=; lit!(5), nil!()).is(nil!()));
    assert!(cmp!(!=; BNIL, BNIL).is(nil!()));
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
struct counting {
    __trace: bool,
    __clock: usize,
    n: ty!(int+),
}

impl counting {
    fn update_mut(&mut self) -> ty!(int) {
        let n = then!(self <~ 0; lit!(0), var!(self <~ 1; n) + lit!(1));
        update!(self, n);
        tick!(self);
        n
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
struct counting_twice {
    __trace: bool,
    __clock: usize,
    b: ty!(bool+),
    __nodes_blocks: (counting, counting,),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, Clone)]
struct counting_late {
    __trace: bool,
    __clock: usize,
    __nodes_blocks: (counting,),
}

impl counting_twice {
    fn update_mut(&mut self) -> ty!(int) {
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
    fn update_mut(&mut self) -> ty!(int) {
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
