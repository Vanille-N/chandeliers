# Chandeliers

A deep embedding of Lustre in Rust.

---

## Structure

The Chandeliers suite is split in many crates:
- public
    - `chandeliers-lus`: the main frontend of Chandeliers.
        This crate defines one macro: `decl`. This macro takes as input the
        source code of a Lustre program and generates equivalent Rust code.
        Examples of usage are shown below.
    - `chandeliers-sem`: semantics of the target language.
        This crate is useful if you want to interface Lustre with Rust.
        It defines the traits that Lustre nodes implement, conversion functions
        to and from the internal representation of Lustre, and some basic macros
        that are helpful if you want to write extern nodes.
    - `chandeliers-std`: standard library for Lustre.
        All nodes (`struct`s) defined in this crate can be imported and used as
        `extern node` to provide features that are not implementable directly
        in Lustre (typecasts, randomness, ...). See the crate documentation for
        all the items provided.
- internal
    - `chandeliers-err`: error message helpers.
    - `chandeliers-syn`: definition of the parsing AST, translation to the analysis AST.
    - `chandeliers-san`: static analysis, typechecking, codegen, etc.

![](deps.png)
The dependency graph of the Chandeliers suite, and the recommended crates
that you should use as direct dependencies.

You would typically depend on
- `chandeliers-sem` only if you want to write a library for Lustre,
- `chandeliers-lus` only if you want to write basic self-contained programs,
- `chandeliers-lus` and `chandeliers-std` if you need more advanced features,
- all of the above if you want to interface Lustre with Rust, e.g. to write a
  `main` function that isn't handled by a simple `#[main]`.

Under no circumstance should you find the need to have `chandeliers-syn`,
`chandeliers-san`, or `chandeliers-err` as direct dependencies. Their definitions
are `pub`, but their interface is very unstable and they are only useful when used
together anyway.

Note: all the crates of Chandeliers are published simultaneously and with the same
version number. As a result, `std` or `sem` may receive a version update even though
nothing in their source code has changed, just so that the version numbers do not
get out of sync with those of `syn` and `san` that change much more often.
It is not recommended to have different version numbers for the crates, as they
are only tested thoroughly with the same version number everywhere.


## Language features

Chandeliers implements a representative subset of Lustre, among which the
following constructs:

- base types `int`, `float`, `bool`, extensible with the generic types
  declared for the node.
- assertions `assert b`
- boolean connectives `b1 and b2`, `b1 or b2`, `not b`
- conditional expressions `if b then e1 else e2`
- temporal operators `e1 -> e2`, `pre e`, `e1 fby e2`
- limited support for clocked expressions `e when b`, `e whenot b`, and
  `merge b e1 e2`.

See syntax examples and notable limitations in `chandeliers-lus/README.md`.


## Compilation options

We adapt and regularize the Rust syntax for attributes to configure compilation
options. All attributes take the form `#[attribute_name[targets](params)]`
where `targets` is a comma-separated list of identifiers and `params` is a
comma-separated list of literals. For example `#[foo[bar, baz](1, "yes", 3)]`.
Sometimes the targets or parameters or both may be optional or unavailable,
in which case `#[attr[]()]` is equivalent to `#[attr]`.

The following options are available:
- `#[trace[file](in_format, out_format)]` (any `node`): print debug information after each step of the execution.
    Variants: all three of `file`, `in_format` and `out_format` are optional.
    See: [#formatting](#formatting).
- `#[export]` and `#[pub]` (non-`extern`): visibility specifiers for the environment
  outside of the macro invocation (we follow the Rust convention of private-by-default).
  See: [#visibility](#visibility).
- `#[trait]` (local `node`, requires `#[export]`): instead of an inherent impl,
  implement the trait `Step`. This may make the signature easier to read, but
  will interfere with dead code analysis.
- `#[main(nb_iter)]` (any `node`): generate a `main` function that will
  execute this node. This requires that the node have signature `() returns ()`.
  Variants: `nb_iter` is optional (defaults to 100).
- `#[test(nb_iter)]` (any `node`): generate a function annotated with `#[test]`
  that will execute this node during `cargo test`. This requires that the node
  have signature `() returns ()` and is incompatible with `#[main]`, `#[export]`,
  and `#[pub]`.
  Variants: `nb_iter` is optional (defaults to 100).
- `#[rustc_allow[attr]]` (any declaration): forward to Rustc as a
  `#[allow(attr)]`. Chandeliers itself inserts many such attributes, but if
  one is missing you can add it manually. This is useful with e.g. `dead_code`
  which Chandeliers intends to insert as little as possible to minimize false
  negatives.
- `#[doc("Message")]` (non-`extern`): insert documentation in the generated code.
  Can be specified multiple times and all messages will be concatenated in order.
- `#[generic[T, U]]` (any `node`): declare new opaque type variables.
    See: [#generics](#generics)

### Formatting

- full: `#[trace[file](in_format, out_format)]`
- minimal: `#[trace]`
- other useful examples:
    - `#[trace[stderr]]`
    - `#[trace("{_this} -> (out={out})\n")]`
    - `#[trace("{_this} <- (in={in})\n", "{_this} -> (out={out})\n")]`

`file` is one of `stderr` or `stdout` (defaults to `stdout` if unspecified).
It determines where the text will be printed.

`in_format` and `out_format` are standard Rust formatting strings with interpolation
variables `{x}`. For `in_format` the interpolation may use the input variables only.
For `out_format` the locals and outputs are additionally available.
Further strings are available:
- `_this` is interpolated to the name of the node
- `_ext` is interpolated to `"[ext]"` if the declaration is an `extern` one,
  and nothing otherwise,
- `_clk` is interpolated to the node's internal clock.

If only one format is specified, it will be interpreted as the `out_format`
and the `in_format` will be blank. `#[trace("foo\n")]` is equivalent to
`#[trace("", "foo\n")]`. Newlines are not inserted, but they are present in the
default formats.

If neither `in_format` nor `out_format` is specified, a default will be chosen
for both the input and output that looks like this:
for a node with signature `node foo(i, j : int) returns (a, b : float)`,
the default `#[trace]` produces the equivalent of
`#[trace("{_this} <- (i={i}, j={j})\n", "{_this} -> (a={a}, b={b})\n")]` which
will be rendered as
```
foo <- (i=0, j=1)
foo -> (a=0.1, b=4.2)
```

The default formatter is not stable and if you need a consistent output
you should write the format string yourself.

### Visibility

We do not implement any kind of module system for Lustre, as we prefer to just
defer to the existing Rust infrastructure.

The attributes that control visibility are `#[export]` and `#[pub]`.
By default a node is visible only within the macro it is defined in.
Adding `#[export]` makes it available to the current module, and
adding `#[pub]` further makes it `pub` in the Rust sense.

Nodes annotated `#[export]` or `#[pub]` should have unique names within their
module.

```rs
mod bar {
    chandeliers_lus::decl! {
        node foo() returns ();
        let tel;
        // `foo` is visible as `foo`
    }
    // `foo` is not visible
}
// `foo` is not visible
```

```rs
mod bar {
    chandeliers_lus::decl! {
        #[export]
        node foo() returns ();
        let tel;
        // `foo` is visible as `foo`
    }
    // `foo` is visible as `foo`
}
// `foo` is not visible
```

```rs
mod bar {
    chandeliers_lus::decl! {
        #[pub]
        node foo() returns ();
        let tel;
        // `foo` is visible as `foo`
    }
    // `foo` is visible as `foo`
}
// `foo` is visible as `bar::foo`
```

If you face name collisions between nodes you should circumvent them by
means of Rust techniques like enclosing them in modules and renaming them
by `use bar::foo as bar_foo;`.


### Generics

Chandeliers implements _System F_-style polymorphism with per-node type variables.
Values of a generic type cannot be inspected, but they can be manipulated by
any construct that is oblivious to type (`let _ = _`, `if b then _ else _`,
`pre _`, `_ -> _`, `_ fby _`), and nodes declared with generic types can be
instantiated with arbitrary concrete or generic types.

A standard example is `swap`:
```rs
#[generic[T, U]]
node swap(t0: T; u0: U) returns (u1: U; t1: T);
let
    t1 = t0;
    u1 = u0;
tel;
```
And you are then free to use `swap` with arguments of any type:
`swap(1, 0.5) ~> (0.5, 1)`, `swap(swap(true, false)) ~> (true, false)`.





## Examples

You may find self-contained examples of standalone executable programs in
the `examples` folder.

Below is an example taken from `examples/features` (somewhat contrived in
order to maximize the features it showcases) to get a feeling for the syntax:
```rs
use chandeliers_std::rand::random_float;

chandeliers_lus::decl! {
    #[trace[stderr]]
    extern node random_float() returns (f : float);

    #[export]
    #[rustc_allow[dead_code]]
    #[doc("Probability that this entire test will fail on the first execution.")]
    const PROBA_FAIL: float = 0.9;

    #[rustc_allow[unused_variables]] // We could also rename `i` into `_i`
    #[doc("Return `true` with probability `PROBA_FAIL`.")]
    #[doc("Note: the input is ignored")]
    node ignore_input_and_pick_random(i : int) returns (b : bool);
    let
        b = random_float() <= PROBA_FAIL;
    tel;

    #[export]
    #[doc("Increasing counter starting at 0 or 1 with a certain probability.")]
    #[doc("Has a certain probability of returning the same value twice in a row.")]
    node unreliable_counter() returns (n : int);
    let
        n = (if ignore_input_and_pick_random(42) then 1 else 0)
            + (0 fby n);
    tel;

    #[export]
    #[trait]
    #[trace]
    #[doc("Strictly increasing counter starting at 0.")]
    node reliable_counter() returns (n : int);
    let
        n = 0 fby n + 1;
    tel;
}

chandeliers_lus::decl! {
    #[trace]
    extern node unreliable_counter() returns (n : int);
    extern node reliable_counter() returns (n : int);

    #[main(15)]
    #[pub]
    #[doc("Verify that the strictly increasing counter is at least as big")]
    #[doc("as the increasing counter. There is currently a bug on purpose")]
    #[doc("in the implementation because there is a certain probability that")]
    #[doc("the unreliable counter starts at 1.")]
    #[doc("This results in a `PROBA_SUCCESS` probability of this node failing")]
    #[doc("its execution after one step, and a `1 - PROBA_SUCCESS` probability")]
    #[doc("of it running forever.")]
    node system() returns ();
    var c1, c2 : int;
    let
        c1 = unreliable_counter();
        c2 = reliable_counter();
        assert c1 <= c2;
    tel;
}
```
