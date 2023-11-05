//! Verify the consistency of clocks within and between nodes.

use std::collections::HashMap;

use chandeliers_err::{self as err, Result};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, var};
use crate::sp::{Sp, Span, WithSpan};
use crate::transparent::Transparent;

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
/// A generic wrapper for objects that have two canonical `Span`s,
/// one at the definition site and one at the call site.
#[derive(Clone)]
#[expect(dead_code)] // FIXME
struct WithDefSite<T> {
    /// Payload.
    data: Sp<T>,
    /// Place where it was defined.
    def_site: Transparent<Option<Span>>,
}

impl<T> WithDefSite<T> {
    /// No relevant span to associate.
    fn without(data: Sp<T>) -> Self {
        Self {
            data,
            def_site: None.into(),
        }
    }

    /// Attach an additional span to an object.
    fn with_def_site<S: Into<Span>>(mut self, span: S) -> Self {
        self.def_site = Some(span.into()).into();
        self
    }
}

crate::sp::derive_with_span!(ClkMap);
/// Helper struct to map input clocks to output clocks.
struct ClkMap {}

/// Global clock context of clock mappings for each function.
#[derive(Default)]
struct ClkCtx {
    /// Maps a node to its clock transformer.
    signatures: HashMap<decl::NodeName, WithDefSite<ClkMap>>,
}

/// Local clock context, mapping local variables to their clocks.
struct ExprClkCtx {
    /// Known clocked variables.
    interface: HashMap<var::Local, WithDefSite<Clock>>,
}

impl ExprClkCtx {
    /// Fetch the clock of a local variable (global constants have an implicit clock that we never
    /// check).
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Clock> {
        self.interface
            .get(var)
            .unwrap_or_else(|| {
                err::abort!(
                "{var} is not in the context, which should have been detected during typechecking"
            )
            })
            .clone()
    }
}

/// Clock typing interface for expressions.
trait ClockCheckExpr {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<WithDefSite<Clock>>>;
}

/// Clock typing interface for statements.
trait ClockCheckStmt {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<()>;
}

/// Helper trait for `Sp` to implement `ClockCheckStmt`.
trait ClockCheckSpanStmt {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<()>>;
}

/// Clock typing interface of declarations.
trait ClockCheckDecl {
    /// Clock context (typically obtained by the `signature` on other toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<()>;
    /// Get input and output clocks to connect this declaration to the outside.
    fn signature(&self) -> ClkMap;
}

/// Helper trait for `Sp` to implement `ClockCheckDecl`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<()>>;
    /// Projection to the inner `signature`.
    fn signature(&self) -> Sp<ClkMap>;
}

/// Clock typing interface.
pub trait ClockCheck {
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self) -> Result<()>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, ctx: T::Ctx<'_>) -> Result<Sp<WithDefSite<Clock>>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckStmt> ClockCheckSpanStmt for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, ctx: T::Ctx<'_>) -> Result<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckDecl> ClockCheckSpanDecl for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: Self::Ctx<'_>) -> Result<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
    fn signature(&self) -> Sp<ClkMap> {
        self.as_ref().map(|_, t| t.signature())
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'i> = ();
    fn clockcheck(&self, span: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'i> = ();
    fn clockcheck(&self, span: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'i> = &'i mut ExprClkCtx;
    fn clockcheck(&self, _: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>> {
        Ok(ctx.clock_of(self))
    }
}

impl ClockCheckDecl for decl::Node {
    type Ctx<'i> = &'i mut ClkCtx;
    fn clockcheck(&self, _span: Span, _ctx: Self::Ctx<'_>) -> Result<()> {
        Ok(()) // FIXME
    }
    fn signature(&self) -> ClkMap {
        ClkMap {} // FIXME
    }
}

impl ClockCheckDecl for decl::ExtNode {
    type Ctx<'i> = ();
    fn clockcheck(&self, _: Span, (): ()) -> Result<()> {
        Ok(()) // FIXME
    }
    fn signature(&self) -> ClkMap {
        ClkMap {} // FIXME
    }
}

impl ClockCheck for Sp<decl::Prog> {
    fn clockcheck(&self) -> Result<()> {
        let mut ctx = ClkCtx::default();
        for decl in &self.t.decls {
            match &decl.t {
                decl::Decl::Const(_) | decl::Decl::ExtConst(_) => {}
                decl::Decl::Node(n) => {
                    n.clockcheck(&mut ctx)?;
                    let sig = n.signature();
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
                decl::Decl::ExtNode(n) => {
                    n.clockcheck(())?;
                    let sig = n.signature();
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
            }
        }
        Ok(())
    }
}
