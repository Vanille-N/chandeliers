//! Verify the consistency of clocks within and between nodes.

use std::collections::HashMap;

use chandeliers_err::{self as err, Result};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, var};
use crate::sp::{Sp, Span};

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
/// A generic wrapper for objects that have two canonical `Span`s,
/// one at the definition site and one at the call site.
#[derive(Clone)]
#[expect(dead_code)] // FIXME
struct WithDefSite<T> {
    /// Payload.
    data: T,
    /// Place where it was defined.
    def_site: Option<Span>,
}

impl<T> WithDefSite<T> {
    /// No relevant span to associate.
    fn without(data: T) -> Self {
        Self {
            data,
            def_site: None,
        }
    }
}

/// Clock context, mapping local variables to their clocks.
struct ExprClkCtx<'i> {
    /// Known clocked variables.
    interface: &'i HashMap<var::Local, WithDefSite<Clock>>,
}

impl ExprClkCtx<'_> {
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

/// Helper struct to map input clocks to output clocks.
pub struct ClkMap {}

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
    fn clockcheck(&self, span: Span) -> Result<()>;
    /// Get input and output clocks to connect this declaration to the outside.
    fn signature(&self) -> ClkMap;
}

/// Helper trait for `Sp` to implement `ClockCheckDecl`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self) -> Result<Sp<()>>;
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
    fn clockcheck<'i>(&self) -> Result<Sp<()>> {
        self.as_ref().map(|span, t| t.clockcheck(span)).transpose()
    }
    fn signature(&self) -> Sp<ClkMap> {
        self.as_ref().map(|_, t| t.signature())
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'i> = ();
    fn clockcheck(&self, _: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative))
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'i> = ();
    fn clockcheck(&self, _: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative))
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'i> = ExprClkCtx<'i>;
    fn clockcheck(&self, _: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>> {
        Ok(ctx.clock_of(self))
    }
}

impl ClockCheck for Sp<decl::Prog> {
    fn clockcheck(&self) -> Result<()> {
        Ok(()) // FIXME
    }
}
