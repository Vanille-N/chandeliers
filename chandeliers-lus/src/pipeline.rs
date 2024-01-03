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

use std::marker::PhantomData;
use std::sync::atomic::{AtomicUsize, Ordering};

use chandeliers_err::{EAccum, Transparent};
use chandeliers_san::{self as sanitizer, sp::Sp};
use chandeliers_syn as syntax;

/// Listing of compiler passes.
mod stage {
    use std::marker::PhantomData;

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
        /// `stage` is the next thing to execute.
        stage: PhantomData<T>,
    }
    /// Compiler pass status modifier.
    pub struct Post<T> {
        /// `stage` has just finished executing.
        stage: PhantomData<T>,
    }
}
use stage::{Post, Pre};

/// Current status of the compiler's transformations.
pub struct CompilerPass<Stage> {
    /// Current operation being applied.
    /// `Stage` will typically be `Pre<stage::*>` or `Post<stage::*>`.
    stage: PhantomData<Stage>,
    /// Current status of the AST.
    prog: Sp<sanitizer::ast::decl::Prog>,
}

/// Generate unique identifiers for each macro invocation. We need this to avoid
/// name collisions in `extern node` and `extern const` declarations.
fn new_run_uid() -> usize {
    /// Mutable state to generate unique identifiers.
    static RUN_UID: AtomicUsize = AtomicUsize::new(0);
    RUN_UID.fetch_add(1, Ordering::SeqCst)
}

impl CompilerPass<Post<stage::Parsing>> {
    /// Begin the pipeline by wrapping `Prog` obtained through other means.
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
    /// Arbitrary wrapper to simplify the rest of the implementation.
    /// Very important that this is not public because it bypasses the
    /// mechanisms declared here.
    fn auto(prog: Sp<sanitizer::ast::decl::Prog>) -> Self {
        Self {
            stage: PhantomData {},
            prog,
        }
    }
}

impl CompilerPass<Pre<stage::Codegen>> {
    /// Final phase of the program transformation: there's nothing to do
    /// after `Codegen`, we have a `TokenStream` that we can return.
    #[expect(
        clippy::unnecessary_wraps,
        reason = "Option in return type for consistency with other passes"
    )]
    pub fn codegen(self) -> Option<proc_macro::TokenStream> {
        use quote::ToTokens;
        let mut toks = proc_macro2::TokenStream::new();
        self.prog.to_tokens(&mut toks);
        Some(toks.into())
    }
}

/// Execute a compiler pass.
/// This consists of any `(&mut EAccum, Sp<Prog>) -> Option<Sp<Prog>>`
/// function.
pub trait Apply: Sized {
    /// Check/transform the ast of the program and collect errors.
    fn apply(
        eaccum: &mut EAccum,
        prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>>;
}

impl Apply for stage::Causality {
    fn apply(
        eaccum: &mut EAccum,
        prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::causality::Causality;
        prog.causality(eaccum)
    }
}

impl Apply for stage::Typecheck {
    fn apply(
        eaccum: &mut EAccum,
        mut prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        // FIXME: trait for this
        prog.typecheck(eaccum)?;
        Some(prog)
    }
}

impl Apply for stage::Clockcheck {
    fn apply(
        eaccum: &mut EAccum,
        mut prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::clockcheck::ClockCheck;
        prog.clockcheck(eaccum)?;
        Some(prog)
    }
}

impl Apply for stage::Positivity {
    fn apply(
        eaccum: &mut EAccum,
        mut prog: Sp<sanitizer::ast::decl::Prog>,
    ) -> Option<Sp<sanitizer::ast::decl::Prog>> {
        use sanitizer::positivity::MakePositive;
        prog.make_positive(eaccum)?;
        Some(prog)
    }
}

/// Enforce the ordering of compiler passes.
pub trait Finish {
    /// The pass that comes next.
    type Next;
}

impl Finish for stage::Parsing {
    type Next = stage::Causality;
}
impl Finish for stage::Causality {
    type Next = stage::Typecheck;
}
impl Finish for stage::Typecheck {
    type Next = stage::Clockcheck;
}
impl Finish for stage::Clockcheck {
    type Next = stage::Positivity;
}
impl Finish for stage::Positivity {
    type Next = stage::Codegen;
}

impl<T, U> CompilerPass<Post<T>>
where
    T: Finish<Next = U>,
{
    /// Prepare for the next pass.
    pub fn finish(self) -> CompilerPass<Pre<U>> {
        CompilerPass::auto(self.prog)
    }
}

impl<T> CompilerPass<Pre<T>>
where
    T: Apply,
{
    /// Execute the current compiler pass and collect errors.
    pub fn apply(self, eaccum: &mut EAccum) -> Option<CompilerPass<Post<T>>> {
        Some(CompilerPass::auto(T::apply(eaccum, self.prog)?))
    }
}
