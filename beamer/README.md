# Chandeliers -- Beamer

This folder contains the beamer for my presentation of Chandeliers for
[MPRI 2.23.1](https://wikimpri.dptinfo.ens-cachan.fr/doku.php?id=cours:c-2-23-1)
on January 12 2024 at ENS Ulm.

This beamer is written using [Typst 0.10.0](https://typst.app/) and
[polylux](https://github.com/andreasKroepelin/polylux).

Use `./run.sh c` to build the beamer from the source, and `./run.sh z` to
open it with the Zathura PDF viewer.
Several targets can be specified: e.g. the recommended dev setup
`./run.sh c z e w` will
- open an editor for the source
- open the compiled output
- watch and recompile on change

## Live code demo

As part of a live demo, the following elements are available:
- `assets/demo.lus` original lustre code,
- `live/` the environment (created with a simple `cargo new`) in which the demo will be carried out,
- `live.finished/` the expected end product.

This demo is intended to illustrate the process converting a pure Lustre code sample
into a macro invocation and surrounding Rust glue code to make an executable.
It puts focus on ease of use, quality of error messages, and -- if you have
[rust-analyzer](https://rust-analyzer.github.io/) installed -- IDE integration.

Here are the expected steps of the demo:

0. The folder for the demo is created using `cargo new live` (\*). \
   This step has already been completed in the commited state of the project.
1. Edit `Cargo.toml` to add the dependencies `chandeliers-{lus,std,sem}`, and build. \
   All of these have their latest version 1.0.0 published on [crates.io](https://crates.io)
   at the time of writing (2024-01-12).
2. Copy the lustre source file `cp ../assets/demo.lus src/main.rs`.
3. Edit `main.rs` to
    - add the surrounding macro invocation `chandeliers_lus::decl! { ... }` around the entire code
    - fix the syntax errors (comments need to be reformatted to Rust-style line comments)
    - import the required node from the standard library `use chandeliers_std::rand::random_bool;`
4. At this point there are no longer complaints from Chandeliers, but the project does not compile
   yet because it is missing a `main` function. There are also `dead_code` warnings.
   Add the annotation `#[main]` to `node system` to fix both.
5. You can now execute `cargo run` that will successfully print no output. \
   Next make the execution visible by adding `#[trace(...)]` invocations e.g. to `nondecreasing`,
   `random_bool`, and `cumulative_sum`.

This concludes the core demo but there are possible bonus extensions

- Intentionally insert errors to showcase error messages.
- Turn some comments into doc comments with `#[doc(...)]` and show the result of `cargo doc --open`. \
  You may need to also add `#[pub]` to the documented nodes.
- show `cargo expand`
- run `cargo build --release` then show `cargo asm live::main`


(\*) The folder may find itself in a transitory state where the source files `Cargo.toml` and `main.rs`
are exactly as `cargo new` provides them, but there are already cached libraries in `live/target/`.
This is partly accidental (unless one runs `cargo clean`, the `target` directory is not cleared),
and at the same time beneficial because it cuts down on compilation time during the live presentation.


