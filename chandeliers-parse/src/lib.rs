#![feature(proc_macro_diagnostic)]

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

use chandeliers_syntax as syntax;

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    unimplemented!();
    /*
    let prog = parse_macro_input!(i as syntax::Node);
    let mut toks = TokenStream::new();
    prog.to_tokens(&mut toks);
    dbg!(&toks);
    proc_macro::TokenStream::from(toks)
    */
}

#[proc_macro]
pub fn expr(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let prog = parse_macro_input!(i as syntax::Expr);
    let mut toks = TokenStream::new();
    prog.to_tokens(&mut toks);
    dbg!(&toks);
    proc_macro::TokenStream::from(toks)
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/ui/*.rs");
}

