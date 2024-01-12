#import "@preview/polylux:0.3.1": *

#import themes.simple: *

#set text(
  font: "Inria Sans"
)

#show raw: text.with(font: "JetBrains Mono")
#set raw(
  theme: "Abyss.tmTheme",
  syntaxes: (
    "RustEnhanced.sublime-syntax",
  ),
)

#show: simple-theme.with(
  footer: [
    Chandeliers by #link("https://github.com/vanille-n/chandeliers")[`vanille-n`]]
)

#show heading.where(level: 2): it => [
  #set text(fill: aqua.darken(50%))
  #it
]

#title-slide[
  = Chandeliers

  A Lustre compiler in Rust
  #v(2em)

  Neven Villani

  2024-01-12

  #v(2em)
  #link("https://github.com/vanille-n/chandeliers")[`github.com:vanille-n/chandeliers`]
]

#slide[
  #grid(
    columns: (auto, auto),
    gutter: 3em,
    [
      Typical compilation steps are missing
      #list(
        marker: $dash.circle$,
        [file manipulation],
        [tokenization],
        [optimizations],
        [emitting assembly],
        [dependency management],
      )
    ],
    [
      Several nonstandard features
      #list(
        marker: $plus.circle$,
        [IDE integration],
        [documentation],
        [target language lints],
      )
    ],
  ) \
  And also...
  - target language is very relevant
  - comes as a _library_, not an executable
]

#title-slide[
  = Chandeliers

  A Lustre plugin for Rust
  #v(2em)

  Neven Villani

  2024-01-12

  #v(2em)
  #link("https://github.com/vanille-n/chandeliers")[`github.com:vanille-n/chandeliers`]
]


#focus-slide[
  = An introduction to Rust and Proc Macros
]

#slide[
  == About Rust

  - *compiled* language
  - strong *type system*
  - extensible via *macros*

  // Include image here
]

#slide[
  == Extending Rust with macros
  #box[
  - Custom parser,
  - Arbitrary code execution,
  - Unsanitized identifiers.
  ]

  What I did for Lustre is an instance of a more general fact:
  you can embed inside Rust *any language* if it "agrees" with Rust on
  - types, ownership and safety #sym.arrow language must be *memory-safe*
  - tokens and parentheses #sym.arrow macro expansion is *post-tokenization*
]

#slide[
  == The Rust ecosystem

  "crate" \~ library/package #sym.arrow published on `https://crates.io` \
  `rustc`: official compiler \
  `cargo`: package manager
]

#slide[
  == What is a macro ?

  Different invocations:
  - ```rsx #[derive(...)]```
  - ```rsx println!(...)```

  Different declarations:
  - ```rsx macro_rules!```
  - ```rsx #[proc_macro]```

  Common characteristic: mapping ```rsx Fn(TokenStream) -> TokenStream```
]

#slide[
  == A standard macro

  ```rsx
  use std::collections::HashMap;

  #[derive(Default)]
  struct Thing {
      n: usize,
      map: HashMap<char, f64>,
      label: Option<String>,
  }

  fn main() {}
```
]

#slide[
  == Expanded

  ```sh $ cargo expand```
  ```rsx
  impl ::core::default::Default for Thing {
      fn default() -> Thing {
          Thing {
              n: ::core::default::Default::default(),
              map: ::core::default::Default::default(),
              label: ::core::default::Default::default(),
          }
      }
  }
  ```
]

#slide[
  == In short

  - *Macros are regular functions* `TokenStream -> TokenStream`
  - Procedural Macros can execute *arbitrary code* at compile-time \
    ("proc macros")

  #v(2em)

  #sym.arrow Chandeliers consists of *one macro* that contains a parser,
  typechecker, and code generator
]

#focus-slide[
  = Chandeliers quick guide
]

#slide[
  == Structure of a program using Chandeliers

  ```toml
  # Cargo.toml
  [dependencies]
  chandeliers-lus = "0.5"
  ```
  ```rsx
  // main.rs
  use chandeliers_lus::decl;

  // Rust glue code
  decl! {
    // Lustre code -> expanded to equivalent Rust code
  }
  ```
]

#slide[
  == Example

  ```rsx
  // main.rs
  chandeliers_lus::decl! {
    node counting() returns (n : int);
    let
      n = 0 fby n + 1;
    tel;
  }
  // [...]
  ```

  Every `node` is expanded to (at least) one ```rsx struct``` with a `step` function.
]

#slide[
  == Annotations
  Rust-style attributes ```rsx #[...]```

  Some of the most useful:
  - ```rsx
  #[trace("foo({x}) = {y}")]
  node foo(x : int) returns (y : int);
  ```
  - ```rsx
  #[main(100)]
  node main() returns ();
  ```
  - ```rsx #[export]``` and ```rsx #[pub]``` levels of visibility
  - ```rsx #[doc("Add node documentation here")]```
  
]



#focus-slide[
  = Advantages
]

#slide[
  == What we get (almost) for free

  - good *performance* through LLVM
  - strong *typing guarantees* (hard to make mistakes in glue code)
  - good *error messages* and dead code analysis
  - *IDE* integration (rust-analyzer, clippy, ...)
  - glue code can *import crates*
  - Lustre libraries are Rust libraries
    - can be uploaded to `crates.io` and downloaded by `cargo`
    - documentation available on `docs.rs`
    - builtin test framework available (nodes annotated ```rsx #[test]```)
]

#slide[
  == A typical error message
  #text(size: 14pt)[
    ```rsx
    node foo(m : int) returns (f : float);
    let f = m; tel;
    ```
    ```
    error: Type mismatch between the left and right sides:
    Base types should be unifiable: expected float, got int
       --> src/lib.rs:605:13
        |
    605 |         let f = m; tel;
        |             ^^^^^
        |
    note: This element has type float
       --> src/lib.rs:605:13
        |
    605 |         let f = m; tel;
        |             ^
    note: While this element has type int
       --> src/lib.rs:605:17
        |
    605 |         let f = m; tel;
        |                 ^
    ```
  ]
]

#slide[
  == Error in glue code

  #text(size: 13pt)[
  ```rsx
  chandeliers_lus::decl! {
      #[export]
      node foo() returns (n: int);
      let n = 0; tel;
  }

  chandeliers_lus::decl! {
      extern node foo() returns (n: float);
  }
  ```
  ```
  error[E0308]: mismatched types
   --> src/lib.rs:609:21
    |
609 |         extern node foo() returns (n: float);
    |                     ^^^^^^^^^^^^^^^---------
    |                     |              |
    |                     |              expected `Nillable<f64>` because of return type
    |                     expected `Nillable<f64>`, found `Nillable<i64>`
    |
    = note: expected enum `Nillable<f64>`
               found enum `Nillable<i64>`
  ```
  ]
]

#slide[
  == Using external crates

  #text(size: 13pt)[
  ```rsx
  use rand::{rngs::ThreadRng, Rng};

  use chandeliers_sem::traits::{Embed, Step};
  use chandeliers_sem::{implicit_clock, ty};

  /// Lustre node that returns a random `int` uniformly between
  /// `i64::MIN` and `i64::MAX`.
  #[derive(Debug, Default, Clone)]
  pub struct random_int {
      /// Internal random number generator.
      rng: ThreadRng,
  }
  impl Step for random_int {
      type Input = ();
      type Output = i64;
      fn step(&mut self, __inputs: ty!()) -> ty!(int) {
          implicit_clock!(__inputs);
          self.rng.gen::<i64>().embed()
      }
  }
  ```
  ]
]

#focus-slide[
  = Difficulties encountered
]

#slide[
  == Parser limitations

  - no control over the tokenizer
    - program must be well-parenthesized (not an issue)
    - comments must be Rust-style: ```rsx // ...``` and ```rsx /* ... */```
    - Rust reserved keywords can't be used as Lustre variables

  - `syn` handles left recursion poorly
    - manual management of associativity and precedence
    - bugs in the parser lead to `rustc` stack overflow
]

#slide[
  == API constraints

  - macro output must be self-contained
  - 1 node = 1 step function (glue code requires stable API)
  - no `null` in Rust #sym.arrow Chandeliers works with ```rsx Option```
]


#slide[
  == Lack of test frameworks
  - most frameworks are optimized for unit tests, not proc macros
  - `trybuild`, the most complete, doesn't support `fail` tests \
    (only `compile-fail`)
]

#slide[
  == The bad side of error messages
  - Rust's error messages are optimized for the end user, not for the macro developers.
  - `panic` in macro execution prints a very vague error \
    ```rsx
    #[warn(clippy::expect_used,
           clippy::panic,
           clippy::unreachable,
           clippy::unwrap_used)]
    ```
  - getting the `Span`s to show the right location is sometimes quite brittle
]

#focus-slide[
  = A full example

  (coding demo)
]

#slide[
  == General porting procedure

  0. `cargo new`
  1. Create `Cargo.toml` and depend on `chandeliers-{std,sem,lus}`
  2. wrap code in ```rsx chandeliers_lus::decl! { ... }```
  3. rename variables if they conflict with Rust reserved keywords
  4. fix Chandeliers-specific semantic choices
  5. add annotations
    - ```rsx #[main]```, ```rsx #[test]``` on your toplevel functions
    - ```rsx #[trace(...)]``` everywhere relevant
]
