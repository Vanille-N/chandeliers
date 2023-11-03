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


## Features

TODO


## Compilation options

Chandeliers follows the Rust-style syntax for specifying compilation options,
and the following are available:
- `#[trace]` (any `node`): print debug information after each step of the execution.
  The output is not stable and should not be relied on for tests.
- `#[export]` (non-`extern`): make the declaration visible to the environment
  outside of the macro invocation (we follow the Rust convention of private-by-default).
- `#[main]` or `#[main(42)]` (any `node`): generate a `main` function that will
  execute this node. This requires that the node have signature `() returns ()`.
- `#[rustc_allow[attr]]` (any declaration): forward to Rustc as a
  `#[allow(attr)]`. Chandeliers itself inserts many such attributes, but if
  one is missing you can add it manually. This is useful with e.g. `dead_code`
  which Chandeliers intends to insert as little as possible to hopefully
  minimize false negatives.
- `#[doc("Message")]` (non-`extern`): insert documentation in the generated code.


## Example

You may find self-contained examples of standalone executable programs in
the `examples` folder.

