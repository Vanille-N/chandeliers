//! Verify the consistency of clocks within and between nodes.

#![allow(missing_docs, clippy::missing_docs_in_private_items)] // FIXME

use std::collections::HashMap;

use chandeliers_err::{self as err, Result};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, var};
use crate::sp::{Sp, Span};

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
#[derive(Clone)]
#[expect(dead_code)] // FIXME
struct WithDefSite<T> {
    data: T,
    def_site: Option<Span>,
}

impl<T> WithDefSite<T> {
    fn without(data: T) -> Self {
        Self {
            data,
            def_site: None,
        }
    }
}

struct ExprClkCtx<'i> {
    interface: &'i HashMap<var::Local, WithDefSite<Clock>>,
}

impl ExprClkCtx<'_> {
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Clock> {
        self.interface
            .get(var)
            .unwrap_or_else(|| {
                err::panic!(
                "{var} is not in the context, which should have been detected during typechecking"
            )
            })
            .clone()
    }
}

pub struct ClkMap {}

trait ClockCheckExpr {
    type Ctx<'i>;
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>>;
}
trait ClockCheckSpanExpr {
    type Ctx<'i>;
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<WithDefSite<Clock>>>;
}
trait ClockCheckStmt {
    type Ctx<'i>;
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<()>;
}
trait ClockCheckSpanStmt {
    type Ctx<'i>;
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<()>>;
}
trait ClockCheckDecl {
    type Ctx<'i>;
    fn clockcheck(&self, span: Span) -> Result<()>;
    fn signature(&self) -> ClkMap;
}
trait ClockCheckSpanDecl {
    type Ctx<'i>;
    fn clockcheck(&self) -> Result<Sp<()>>;
    fn signature(&self) -> Sp<ClkMap>;
}
pub trait ClockCheck {
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
