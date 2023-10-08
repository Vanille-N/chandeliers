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

