# Chandeliers-Syn

Syntax of Lustre.

---

In this crate we describe the parsing AST of Lustre programs.
This AST is not meant to be used directly, instead it should be translated
into one that is more convenient for static analysis.

## How to obtain a parsing AST and what to do with it

All types of the module `ast` implement `syn::parse::Parse`, so they
can be parsed from any stream of tokens that conforms to the structure
expected by `syn`.

The typical usage inside a proc macro would be like:
```rs
use chandeliers_syn::ast::Prog;
use proc_macro::TokenStream;

#[proc_macro]
pub fn get_and_handle_ast(input: TokenStream) -> TokenStream {
    let prog: Prog = syn::parse_macro_input!(input as Prog);
    // now you have a `Prog`, you can e.g.
    // `Prog::translate` it to get something that can be used by
    // Chandeliers' sanitizer.
}
```



