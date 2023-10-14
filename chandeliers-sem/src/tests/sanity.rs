#![cfg(test)]

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

#[test]
fn assert_ok() {
    truth!(lit!(true), "Ok");
}

#[test]
#[should_panic]
fn assert_ko() {
    truth!(lit!(false), "Truth on false");
}

#[test]
#[should_panic]
fn assert_nil() {
    truth!(nil!(), "Truth on Nil");
}

const BNIL: ty!(bool) = nil!();

#[test]
fn all_unop_correct() {
    assert_is!(unop!(!; lit!(true)), lit!(false));
    assert_is!(unop!(!; lit!(false)), lit!(true));
    assert_is!(unop!(!; BNIL), BNIL);
    assert_is!(unop!(-; lit!(5)), lit!(-5));
}

#[test]
fn all_binop_correct() {
    // + -  % * /
    assert_is!(binop!(+; lit!(5), lit!(10)), lit!(15));
    assert_is!(binop!(-; lit!(4), nil!()), nil!());
    assert_is!(binop!(%; lit!(10), lit!(3)), lit!(1));
    assert_is!(binop!(*; lit!(2), lit!(9)), lit!(18));
    // |
    assert_is!(binop!(|; lit!(true), lit!(true)), lit!(true));
    assert_is!(binop!(|; lit!(true), lit!(false)), lit!(true));
    assert_is!(binop!(|; lit!(false), lit!(true)), lit!(true));
    assert_is!(binop!(|; lit!(false), lit!(false)), lit!(false));
    assert_is!(binop!(|; lit!(true), nil!()), nil!());
    // &
    assert_is!(binop!(&; lit!(true), lit!(true)), lit!(true));
    assert_is!(binop!(&; lit!(true), lit!(false)), lit!(false));
    assert_is!(binop!(&; lit!(false), lit!(true)), lit!(false));
    assert_is!(binop!(&; lit!(false), lit!(false)), lit!(false));
    assert_is!(binop!(&; lit!(true), nil!()), nil!());
    // ^
    assert_is!(binop!(^; lit!(true), lit!(true)), lit!(false));
    assert_is!(binop!(^; lit!(true), lit!(false)), lit!(true));
    assert_is!(binop!(^; lit!(false), lit!(true)), lit!(true));
    assert_is!(binop!(^; lit!(false), lit!(false)), lit!(false));
    assert_is!(binop!(^; lit!(true), nil!()), nil!());
}

#[test]
fn all_cmp_correct() {
    assert_is!(cmp!(<; lit!(4), lit!(5)), lit!(true));
    assert_is!(cmp!(<; lit!(4), lit!(4)), lit!(false));
    assert_is!(cmp!(<; lit!(5), lit!(4)), lit!(false));
    assert_is!(cmp!(<; lit!(5), nil!()), nil!());
    assert_is!(cmp!(<; BNIL, BNIL), nil!());
    //---
    assert_is!(cmp!(>; lit!(4), lit!(5)), lit!(false));
    assert_is!(cmp!(>; lit!(4), lit!(4)), lit!(false));
    assert_is!(cmp!(>; lit!(5), lit!(4)), lit!(true));
    assert_is!(cmp!(>; lit!(5), nil!()), nil!());
    assert_is!(cmp!(>; BNIL, BNIL), nil!());
    //---
    assert_is!(cmp!(<=; lit!(4), lit!(5)), lit!(true));
    assert_is!(cmp!(<=; lit!(4), lit!(4)), lit!(true));
    assert_is!(cmp!(<=; lit!(5), lit!(4)), lit!(false));
    assert_is!(cmp!(<=; lit!(5), nil!()), nil!());
    assert_is!(cmp!(<=; BNIL, BNIL), nil!());
    //---
    assert_is!(cmp!(>=; lit!(4), lit!(5)), lit!(false));
    assert_is!(cmp!(>=; lit!(4), lit!(4)), lit!(true));
    assert_is!(cmp!(>=; lit!(5), lit!(4)), lit!(true));
    assert_is!(cmp!(>=; lit!(5), nil!()), nil!());
    assert_is!(cmp!(>=; BNIL, BNIL), nil!());
    //---
    assert_is!(cmp!(==; lit!(4), lit!(5)), lit!(false));
    assert_is!(cmp!(==; lit!(4), lit!(4)), lit!(true));
    assert_is!(cmp!(==; lit!(5), lit!(4)), lit!(false));
    assert_is!(cmp!(==; lit!(5), nil!()), nil!());
    assert_is!(cmp!(==; BNIL, BNIL), nil!());
    //---
    assert_is!(cmp!(!=; lit!(4), lit!(5)), lit!(true));
    assert_is!(cmp!(!=; lit!(4), lit!(4)), lit!(false));
    assert_is!(cmp!(!=; lit!(5), lit!(4)), lit!(true));
    assert_is!(cmp!(!=; lit!(5), nil!()), nil!());
    assert_is!(cmp!(!=; BNIL, BNIL), nil!());
}
