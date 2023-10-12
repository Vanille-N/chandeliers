#![feature(proc_macro_diagnostic)]

use syn::parse_macro_input;
use quote::ToTokens;

use chandeliers_syn as syntax;
use chandeliers_san as sanitizer;

use syntax::translate::SpanTranslate;
use sanitizer::causality::Causality;
use sanitizer::ast::Sp;

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut prog = parse_macro_input!(i as Sp<syntax::ast::Prog>);
    let prog = match prog.translate(()) {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    let prog = match prog.causality() {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    match prog.typecheck() {
        Ok(()) => {},
        Err(e) => return e.into(),
    }
    let mut toks = proc_macro2::TokenStream::new();
    prog.to_tokens(&mut toks);
    toks.into()
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/ui/*.rs");
}

