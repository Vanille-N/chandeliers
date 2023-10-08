use crate::macros::*;

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


