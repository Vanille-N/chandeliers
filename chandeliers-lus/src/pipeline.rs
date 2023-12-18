//! Here we define a wrapper around the AST that makes it impossible for clients
//! to misuse it or call compiler passes in a wrong order.
//! The exact contents of this file are deeply connected with the internal
//! details of `chandeliers_san`.
//!
//! The order of compiler passes is enforced by a state machine with
//! transitions
//! - `[init] -> Post<Parsing>` given by the translation of the parsing AST.
//! - `Pre<Codegen> -> [end]` given by `ToTokens`.
//! - `Pre<S> -> Post<S>` for each `S` that implements `Apply`,
//!   typically by applying some transformation from `chandeliers_san`.
//! - `Post<S> -> Pre<T>` when `S: Finish<Next = T>`,
//!   prepare to apply the next pass.
//!
//! A full application of the macro will necessary go from `[init]` to `[end]`
//! through a predetermined path of the state machine herein defined.

use std::sync::atomic::{AtomicUsize, Ordering};

use chandeliers_err::{EAccum, Transparent};
use chandeliers_san::{self as sanitizer, sp::Sp};
use chandeliers_syn as syntax;

/// Compiler pass: parsed but completely unverified AST.
/// This is obtained through the `Translate` trait on `chandeliers_syn::Prog`
/// and gives access to the acyclicity check and topological sort (causality).
pub struct Parsing {}
/// Compiler pass: reorder AST to be without cycles.
/// Gives access to typechecking.
pub struct Causality {}
/// Compiler pass: check the types of the program.
/// Gives access to clockchecking.
pub struct Typecheck {}
/// Compiler pass: check the consistency of clocks.
/// Gives access to temporal depth computation.
pub struct Clockcheck {}
/// Compiler pass: determine how many past values of each variable are needed.
/// Last step before codegen.
pub struct Positivity {}
/// Final transformation of the AST by generation of the output token stream.
pub struct Codegen {}

/// Compiler pass status modifier.
pub struct Pre<T> {
    stage: std::marker::PhantomData<T>,
}
/// Compiler pass status modifier.
pub struct Post<T> {
    stage: std::marker::PhantomData<T>,
}

pub struct CompilerPass<Stage> {
    stage: std::marker::PhantomData<Stage>,
    prog: Sp<sanitizer::ast::decl::Prog>,
}

/// Generate unique identifiers for each macro invocation. We need this to avoid
/// name collisions in `extern node` and `extern const` declarations.
fn new_run_uid() -> usize {
    /// Mutable state to generate unique identifiers.
    static RUN_UID: AtomicUsize = AtomicUsize::new(0);
    RUN_UID.fetch_add(1, Ordering::SeqCst)
}

impl CompilerPass<Post<Parsing>> {
    pub fn new(eaccum: &mut EAccum, ast: Sp<syntax::Prog>) -> Option<Self> {
        use syntax::translate::SpanTranslate;
        let run_uid = new_run_uid();
        Some(CompilerPass::auto(ast.translate(
            eaccum,
            Transparent::from(run_uid),
            (),
        )?))
    }
}

impl<T> CompilerPass<T> {
    fn auto(prog: Sp<sanitizer::ast::decl::Prog>) -> Self {
        Self {
            stage: Default::default(),
            prog,
        }
    }
}

impl CompilerPass<Pre<Codegen>> {
    pub fn codegen(self) -> Option<proc_macro::TokenStream> {
        use quote::ToTokens;
        let mut toks = proc_macro2::TokenStream::new();
        self.prog.to_tokens(&mut toks);
        Some(toks.into())
    }
}

pub trait Apply: Sized {
    fn apply(
        eaccum: &mut EAccum,
        prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>>;
}

impl Apply for Causality {
    fn apply(
        eaccum: &mut EAccum,
        prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::causality::Causality;
        prog.causality(eaccum)
    }
}

impl Apply for Typecheck {
    fn apply(
        eaccum: &mut EAccum,
        mut prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        // FIXME: trait for this
        prog.typecheck(eaccum)?;
        Some(prog)
    }
}

impl Apply for Clockcheck {
    fn apply(
        eaccum: &mut EAccum,
        prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::clockcheck::ClockCheck;
        prog.clockcheck(eaccum)?;
        Some(prog)
    }
}

impl Apply for Positivity {
    fn apply(
        eaccum: &mut EAccum,
        mut prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::positivity::MakePositive;
        prog.make_positive(eaccum)?;
        Some(prog)
    }
}

pub trait Finish {
    type Next;
}

impl Finish for Parsing {
    type Next = Causality;
}
impl Finish for Causality {
    type Next = Typecheck;
}
impl Finish for Typecheck {
    type Next = Clockcheck;
}
impl Finish for Clockcheck {
    type Next = Positivity;
}
impl Finish for Positivity {
    type Next = Codegen;
}

impl<T, U> CompilerPass<Post<T>>
where
    T: Finish<Next = U>,
{
    pub fn finish(self) -> CompilerPass<Pre<U>> {
        CompilerPass::auto(self.prog)
    }
}

impl<T> CompilerPass<Pre<T>>
where
    T: Apply,
{
    pub fn apply(self, eaccum: &mut EAccum) -> Option<CompilerPass<Post<T>>> {
        Some(CompilerPass::auto(T::apply(eaccum, self.prog)?))
    }
}
