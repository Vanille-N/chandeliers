#![feature(proc_macro_diagnostic)]

use quote::ToTokens;
use syn::parse_macro_input;

use chandeliers_san as sanitizer;
use chandeliers_syn as syntax;

use sanitizer::ast::Sp;
use sanitizer::causality::Causality;
use sanitizer::positivity::MakePositive;
use syntax::translate::SpanTranslate;

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let prog = parse_macro_input!(i as Sp<syntax::ast::Prog>);
    let prog = match prog.translate(()) {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    let mut prog = match prog.causality() {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    match prog.typecheck() {
        Ok(()) => {}
        Err(e) => return e.into(),
    }
    match prog.make_positive() {
        Ok(()) => {}
        Err(e) => return e.into(),
    }
    let mut toks = proc_macro2::TokenStream::new();
    prog.to_tokens(&mut toks);
    toks.into()
}

macro_rules! ui_fail {
    ($fun:ident in $dir:ident) => {
        #[test]
        fn $fun() {
            let t = trybuild::TestCases::new();
            t.compile_fail(concat!("tests/fail/ui/", stringify!($dir), "/**/*.rs"));
        }
    };
}

ui_fail!(ui_fail_causality in causality);
ui_fail!(ui_fail_syn in syn);
ui_fail!(ui_fail_tc in tc);

#[test]
fn pass_ui() {
    let t = trybuild::TestCases::new();
    t.pass("tests/pass/ui/**/*.rs");
}
