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

macro_rules! compiling {
    ($fun:ident with $testing:ident in $($dir:ident / )*) => {
        #[test]
        fn $fun() {
            let t = trybuild::TestCases::new();
            t.$testing(concat!("tests/", $( concat!(stringify!($dir), "/") , )* "**/*.rs"));
        }
    };
}

compiling!(fail_ui_causality with compile_fail in fail/ui/causality/);
compiling!(fail_ui_syn with compile_fail in fail/ui/syn/);
compiling!(fail_ui_tc with compile_fail in fail/ui/tc/);

compiling!(pass_ui with pass in pass/ui/);
compiling!(pass_syn with pass in pass/syn/);
compiling!(pass_std with pass in pass/std/);
compiling!(pass_fromslides with pass in pass/fromslides/);
compiling!(pass_given with pass in pass/given/);
