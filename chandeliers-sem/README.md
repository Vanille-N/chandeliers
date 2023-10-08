# Chandeliers-Sem

Semantics of the Candle language.


## Candle

Candle is a language of macros that mimics the constructs of the
[Lustre](https://en.wikipedia.org/wiki/Lustre_(programming_language))
programming language.

Although some examples of hand-written Candle code are provided under
`src/tests/impl/`, Candle is not really a language designed for human use and many
of its constructs range anywhere from tricky to inhuman to use.

Instead Candle's main purpose it to be a _compilation target_ for Lustre
programs, in the sense that an automated translation from Lustre to Candle
should prove easy given the similarity of their structure.

Other than the example below, you can read the full specification of
Candle in `src/candle.rs`, which because it contains all the macro definitions
specifies both the syntax and the semantics of Candle.

### Example: counting the number of `true` in the input

Specification:
- Input: a stream of `bool`
- Output: a stream of `int`
- At all times, the output is the number of `true` seen so far in the input.

A typical Lustre implementation could look like this:
```lus
node counter(i: bool) returns (n : int);
let
  n = (0 fby n) + (if i then 1 else 0);
tel;
```

In Candle we would define the equivalent logic as follows:
```rs
#![feature(core_intrinsics)]
use chandeliers_sem::macros::*;

#[allow(non_camel_case_types)]
#[derive(Default)]
pub struct counter {
    __clock: usize,
    __trace: bool,
    n: ty!(int+),
    __nodes: (),
}

impl counter {
    pub fn update_mut(&mut self, i: ty!(bool)) -> ty!(int) {
        node_trace!(self, "(i={}) => counter(n={})", i, self.n);
        let n = binop!(+;
            then!(self <~ 0; lit!(0), var!(self <~ 1; n)),
            ifx!((var!(self <~ 0; i)) then { lit!(1) } else { lit!(0) })
        );
        update!(self, n);
        tick!(self);
        node_trace!(self, "counter(n={}) => (n={})", self.n, self.n);
        n
    }
}
```
You should see immediately from this simple example that Candle is much
more verbose and complex to read than Lustre, but also if one looks closer
at the program then it becomes apparent that the structure of the Lustre
program is very faithfully replicated in its Candle translation.

More examples (with varying degrees of explanations) are available
in `src/tests/impl/`, in particular `fib.rs` implements the fibonacci sequence
and `counters.rs` has various examples around the theme of integer counters).
