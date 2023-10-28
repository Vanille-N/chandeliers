# Chandeliers-Lus

Frondend of the Chandeliers suite.

---

This crate provides the `decl` macro that defines a deep embedding of Lustre into Rust.

## Usage

In this presentation of the usage of `decl` we assume a familiarity with the
Lustre language.

### Declaring constants and nodes

A self-contained Lustre program can usually be enclosed in a `decl` macro with barely
any change. Here are some examples:

```rs
chandeliers_lus::decl! {
    const ZERO: int = 0;
    const ONE: int = 1;

    node fibonacci() returns (n : int);
    let
        n = ZERO -> ONE -> (pre n + pre pre n);
    tel;

    node fibsum() returns (s : int);
    let
        s = fibonacci() + (0 fby s);
    tel;
}
```

```rs
chandeliers_lus::decl! {
    node selector(left, right : float; switch : bool) returns (out : float);
    let
        out = if switch then left else right end;
    tel;
}
```

### Importing external definitions

Chandeliers does not provide any builtin functions. This is intentional.

Instead a flexible and robust method is available for declaring extern definitions.
Any `extern` defined object will be assumed by the static analyzers of Chandeliers
to be defined, and if it is missing at codegen time then Rustc will emit an error.

`extern` covers
- declarations from another `decl` block (example below),
- nodes from the standard library (see crate `chandeliers-std`) or from another crate,
- custom nodes written directly in Candle (more information in `chandeliers-sem`,
  examples in `tests/pass/std` and in particular `tests/pass/std/rand.rs`).

Using a definition from another `decl` block:
```rs
chandeliers_lus::decl! {
    node counter() returns (n : int);
    let n = 0 fby (n + 1); tel;
}

// `decl` is local and cannot know that `counter` exists in another block.
// If we want to use it here, we need to redeclare it as an `extern node`.
chandeliers_lus::decl! {
    // any signature inconsistencies will produce type errors.
    extern node counter() returns (n : int);

    node cumul() returns (n : int);
    let n = counter() + (0 fby n); tel;
}
```

In the same way an
```rs
chandeliers_lus::decl! {
    extern const PI: float;
    ...
```
makes `PI` available to the rest of the block.

Using a definition from the standard library:
```rs
use chandeliers_std::float_of_int;

chandeliers_lus::decl! {
    extern node float_of_int(i : int) returns (f : float);
    ...
}
```

### How to write a `main` function

The `decl` macro never inserts a `main`, instead one should be defined manually
to interface with the environment.
This is made possible by tha fact that a `decl` block makes its definitions
publicly available under the same name.

A `main` could look something like this:
```rs
use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    node cumul(i : int) returns (s : int);
    let s = i + (0 fby s); tel;
}

fn main() {
    let mut cumul = cumul::default();
    for i in 0..100 {
        let s = cumul.step(i.embed()).trusted();
        println!("{s}");
    }
}
```

The above features are provided by the traits `Step`, `Default`, `Embed` and `Trusted`
which are implemented automatically for relevant objects either by the `decl` macro itself
or defined generically in `chandeliers-sem`.

Nodes produced by `decl` manipulate different types of values internally
(isomorphic to `Option<T>`), and the traits `Embed` and `Trusted` produce the
equivalent of `map(Some)` and `map(Option::unwrap)` on all tuple types.

More specifically, a tuple type `(T, U, V)` implements `Embed` with output type
`(Nillable<T>, Nillable<U>, Nillable<V>)`, and `Trusted` produces the reciprocal.
It is always safe to use `Embed`, but you should only use `Trusted` on values that
are not `Nil`. The static analysis performed inside `decl` guarantees that a node
whose inputs are all non-`Nil` will have non-`Nil` outputs, so only the implementation
of `extern node`s not produced by a `decl` block needs to be verified.


### Notable limitations and differences with standard Lustre

For various reasons -- including limited control over Rust syntax, and parser efficiency --
some Lustre programs may not be supported directly and require minor syntax adjustments.
Here are some that you may run into if you try to copy-paste Lustre code too eagerly:
- `node` definitions are required to end with a trailing `;`,
- tuple unpacking `(x, y, z) = foo()` requires the surrounding `(...)`,
- the Rust convention is used to determine in which contexts trailing commas are allowed:
  `1 = (1) <> (1,)` and `(1, 2) = (1, 2,)`,
- a few Rust reserved keywords (`self`, `super`, `move`, ...) cannot be used as identifiers,
- only Rust-style comments are available, i.e. `//` for line comments and `/* ... */` for block
  comments,

