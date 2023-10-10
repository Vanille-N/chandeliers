#![feature(proc_macro_diagnostic)]

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

use chandeliers_syn as syntax;
use chandeliers_san as sanitizer;
use chandeliers_sem as semantics;

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let prog: syntax::ast::AttrNode = parse_macro_input!(i as syntax::ast::AttrNode);
    let prog = match prog.translate() {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    dbg!(&prog);
    unimplemented!();
    //let prog: semantics::Node = prog.into();
    //let mut toks = TokenStream::new();
    //prog.to_tokens(&mut toks);
    //dbg!(&toks);
    //proc_macro::TokenStream::from(toks)
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/ui/*.rs");
}

