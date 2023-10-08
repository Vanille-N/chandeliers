use crate::macros::*;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct fib {
    __trace: bool,
    __clock: usize,
    x: ty!(int++),
}

impl fib {
    pub fn update_mut(&mut self) -> ty!(int) {
        let x = then!(self <~ 0; lit!(0),
            then!(self <~ 1; lit!(1),
                binop!(+; var!(self <~ 2; x), var!(self <~ 1; x))
            )
        );
        update!(self, x);
        tick!(self);
        x
    }
}

#[test]
fn fib_behavior() {
    let mut fib = fib::default();
    let mut vals = vec![];
    for _ in 0..10 {
        vals.push(fib.update_mut().unwrap());
    }
    assert_eq!(&vals, &[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
}
