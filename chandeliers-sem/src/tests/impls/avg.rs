use crate::macros::*;
use crate::traits::*;

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
pub struct weighted_sum {
    __trace: bool,
    __clock: usize,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct cumul_avg {
    __trace: bool,
    __clock: usize,
    n: ty!(int+),
    avg: ty!(float+),
    __nodes: (weighted_sum,),
}

impl Step for weighted_sum {
    type Input = (f64, f64, f64);
    type Output = f64;
    fn step(&mut self, inputs: (ty!(float), ty!(float), ty!(float))) -> ty!(float) {
        let (x, y, weight) = inputs;
        node_trace!(
            self,
            "(x={},y={},weight={}) => weighted_sum()",
            x,
            y,
            weight
        );
        let sum = binop!(+;
            binop!(*; var!(self <~ 0; weight), var!(self <~ 0; x)),
            binop!(*;
                binop!(-; lit!(1.0), var!(self <~ 0; weight)),
                var!(self <~ 0; y)
            )
        );
        tick!(self);
        node_trace!(self, "weighted_sum() => (sum={})", sum);
        sum
    }
}

impl Step for cumul_avg {
    type Input = f64;
    type Output = f64;
    fn step(&mut self, x: ty!(float)) -> ty!(float) {
        node_trace!(self, "(x={}) => cumul_avg(n={})", x, self.n);
        let n = later!(self <~ 0; lit!(1), var!(self <~ 1; n) + lit!(1));
        update!(self, n);
        let avg = substep!(
            self <~ 0;
            0 => {(
                var!(self <~ 0; x),
                later!(self <~ 0; lit!(0.0), var!(self <~ 1; avg)),
                binop!(/; lit!(1.0), float!(var!(self <~ 0; n))),
            )}
        );
        tick!(self);
        update!(self, avg);
        node_trace!(
            self,
            "cumul_avg(n={},avg={}) => (avg={})",
            self.n,
            self.avg,
            avg
        );
        avg
    }
}

#[test]
fn cumul_avg_behavior() {
    let mut node = cumul_avg::default();
    node.__trace = true;
    node.__nodes.0.__trace = true;
    let v = node.step(lit!(0.5));
    println!("{}\n", v);
    let v = node.step(lit!(1.0));
    println!("{}\n", v);
    let v = node.step(lit!(0.3));
    println!("{}\n", v);
    let v = node.step(lit!(0.2));
    println!("{}\n", v);
    //panic!();
}
