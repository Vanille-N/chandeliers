# Chandeliers -- Dev guide

Project description for maintainers and code review.

---

## Version

This project was last tested with Rustc 1.77.0-nightly (f688dd684 2024-01-04).

It is expected to build with the latest nightly. If it does not please open an issue.

## Adding a feature

`./manage.sh` is the canonical way to run tests.
The dev cycle is
1. create new tests files in e.g. `chandeliers-lus/tests/compile_fail/foo`
2. if you created a new directory, schedule these tests for running in `chandeliers-lus/src/lib.rs` <br>
  by adding the line `compiling!(fail_foo with compile_fail in compile_fail/foo/);`. <br>
  Choose between `compile_fail` or `pass` depending on the desired behavior.
3. observe that `./manage test fail_foo` returns an invalid output,
4. implement the feature (with high likeliness in `chandeliers-san`)
5. if this is a `compile_fail` test, run `./manage.sh bless fail_foo` once
   the output is correct.
6. check that `./manage test` is all green
7. bump the crate version with `./manage.sh bump X.Y.Z`
8. `git commit` your changes
9. `./manage.sh publish` to push the update to [crates.io](https://crates.io)

## Where to find this project

- official repository [github.com:vanille-n/chandeliers](https://github.com/vanille-n/chandeliers)
- packaged crates on [crates.io](https://crates.io/search?q=chandeliers)
- rendered documentation on [docs.rs](https://docs.rs/releases/search?query=chandeliers&sort=relevance)
    - including private items: run `cargo doc --document-private-items --open` in the project root

## Directory tree

- `.`
    - `manage.sh` <br>
        Automatic runner for `cargo` commands on the entire project.
        This should be your main way of interacting with the project.
        - `./manage.sh check` runs `cargo check` on all subfolders
        - the above is also available for cargo commands `build`, `test`, `update`, `clippy`, `fmt` and `publish`,
        - `./manage.sh bump 1.0.0` update all subcrates to a new version,
        - `./manage.sh bless` run tests and set the current output as the expected output,
        - `./manage.sh lint` run `lint.sh`.
        - `./manage.sh gloss` build the glossary
        - `./manage.sh doc` open the documentation
    - `lint.sh` <br>
        - *DO NOT USE DIRECTLY*, use through `manage.sh lint` instead,
        - checks the current attributes `#![warn(...)]` and `#![feature(...)]` of all subcrates.
        - successful if the output is empty.
    - `src` <br>
        For debugging purposes, you can add any code to `lib.rs` or `main.rs` and
        run `cargo expand` or `cargo run` or `cargo test` to manually check that the output
        looks correct. *THIS IS NOT AUTHORITATIVE*, once the tests are done they should be
        added to `chandeliers-lus/tests/`.
    - `chandeliers-err` <br>
        Error message helpers.
        - `errors.rs` contains the error struct definitions that will be the main interface
          for implementing error messages. One error message is one (parameterized) struct.
        - `transparent.rs` defines the `Transparent` wrapper that is used in many other
          places of the project. Wrapping data in a `Transparent` declares that this data
          is not relevant for computation and is only preserved for user convenience.
          `Transparent` fields are not recursed into for equality tests or hashing.
        - `lib.rs` <br>
            - `abort` macros. We don't use `panic` directly but only through `err::abort`.
            - `here` macro that helpfully expands to the current location in the source code.
              Use with `Transparent::forge` and other panic messages.
            - `EAccum` is the accumulator of errors that the project uses. In order
              to be able to report several errors in parallel, most functions don't have
              a `Fn(...) -> Result<T, E>` signature but rather a `Fn(&mut EAccum, ...) -> Option<T>`.
    - `chandeliers-sem` <br>
        - `lib.rs` most importantly provides a structured reexport of all definitions in this crate,
          and defines the trait `Scalar` which is used for bounds on generic nodes.
        - `candle.rs` contains macro definitions for the Candle language that defines the semantics
          of the dialect of Lustre we are using.
        - `nillable.rs` provides the `Nillable` wrapper that works like `Option` but with
          more builtin trait implementations, making it suitable as the basic data container
          of all chandeliers.
        - `stepping.rs` defines the API that all compiled nodes follow, using `Embed`ed types
          for inputs and outputs and possibly implementing the `Step` trait.
        - `registers.rs` one way of handling past values, `Register` can update and save
          one value at a time of any type.
        - `time_travel.rs` is the other way of handling past values. An `S<...>` can save
          a fixed finite amount of past scalar values. Depending on the exact way you
          manipulate past values, either `Register` or `S<...>` may turn out to be more
          suited to your needs in terms of features and performance.
    - `chandeliers-san` <br>
        Code sanitizers and analysers.
        - `sp.rs` defines `Sp` which is an extremely pervasive type wrapper in the project.
          Because we want to know the provenance in the source code of all elements of the AST,
          everything is wrapped in `Sp`.
          The main ways that we make handling `Sp` less tedious is to define a lot of traits
          and usually have two traits for one, as such:
          ```rs
          trait FooSpan { fn foo_span(&self, span: Span) -> T; }
          trait Foo { fn foo(&self) -> Sp<T>; }
          impl<T: FooSpan> Foo for Sp<T> {
            fn foo(&self) -> Sp<T> {
              self.t.foo_span(self.span).with_span(self.span)
            }
          }
          ```
          this way we just have to implement `foo_span` for all elements of the
          AST that assumes the `Span` comes from somewhere, and we get for free
          the spanned version.
        - `ast` <br>
            Defines the AST used for analysis. This is not the parsing AST, there's a lot
            of preliminary work in `chandeliers-syn`.
            - `options.rs` handles specifically the part of the AST related to
              compiler options and code annotations.
        - `causality` <br>
            Sorts data according to the topological ordering and checks that there are no
            cyclic definitions.
            - `graph.rs` defines very generically the algorithm to detect cycles and sort if there
              are none.
            - `depends.rs` instanciates the definitions of `graph.rs` by defining the dependencies
              between and within items.
        - `typecheck.rs` regular type checking as well as type inference of the generic node parameters.
        - `clockcheck.rs` checks the consistency of clocks: that expressions go at the right speed
            to be used with each other and that we don't access uninitialized data by reading off clock.
        - `positivity.rs` checks that no variable is used before its initialization is possible,
            and determines how many past values of each variable we need to remember.
        - `codegen` <br>
            Code generation. This mostly (but not exclusively) just forwards constructs
            to Candle, though because Candle needs to respect the rules of sanitized macros
            and scoping, the name generation and structure of the nodes need to be handled here.
            - `options.rs` handles specifically the part of code generation related
              to compiler options and code annotations.
    - `chandeliers-syn` <br>
        From text to the `chandeliers-san` AST.
        - `ast.rs` defines the crate's own internal parsing AST which is self-parsing
          in that trait `#[derive(...)]`s make the definition of the structure of the AST
          also simultaneously define the syntax of the language.
        - `translate` <br>
            Converts between the parsing AST and the analysis AST. Apart from simple
            constructor replacement this mostly involves resolving associativity
            and precedence and extracting expressions with side-effects to their
            own statement.
            - `options.rs` handles the part of the translation that is specifically
              related to code annotations.
    - `chandeliers-lus` <br>
        The public API of Chandeliers through the `decl!` macro that invoques
        the full parsing, analysis and code generation.
        - `pipeline.rs` guarantees that it is impossible to execute the compiler
          passes in the wrong order by making them part of the type.
        - `tests` is where lies the end-to-end (not unit tests) test suite of
          Chandeliers.
    - `chandeliers-std` contains some node definitions and is intended as a form
       of standard library for Lustre, although with how many features it has it's
       more of a proof of concept currently.
    - `examples` are some self-contained full project examples using Chandeliers.
      Pick any of them to have a full view of how to integrate Lustre and Rust
      together in a small project.
    - `beamer` contains the sources for the oral presentation of the project.
      Uses Typst for rendering.











