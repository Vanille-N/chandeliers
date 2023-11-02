//! Verify the consistency of clocks within and between nodes.

#![allow(missing_docs)]

use std::collections::HashMap;

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, var};
use crate::sp::{Sp, Span};
use chandeliers_err::{self as err, Result};

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
#[derive(Clone)]
#[allow(dead_code)]
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
    fn clockcheck<'i>(&self, span: Span, ctx: Self::Ctx<'i>) -> Result<WithDefSite<Clock>>;
}
trait ClockCheckSpanExpr {
    type Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: Self::Ctx<'i>) -> Result<Sp<WithDefSite<Clock>>>;
}
trait ClockCheckStmt {
    type Ctx<'i>;
    fn clockcheck<'i>(&self, span: Span, ctx: Self::Ctx<'i>) -> Result<()>;
}
trait ClockCheckSpanStmt {
    type Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: Self::Ctx<'i>) -> Result<Sp<()>>;
}
trait ClockCheckDecl {
    type Ctx<'i>;
    fn clockcheck<'i>(&self, span: Span) -> Result<()>;
    fn signature(&self) -> ClkMap;
}
trait ClockCheckSpanDecl {
    type Ctx<'i>;
    fn clockcheck<'i>(&self) -> Result<Sp<()>>;
    fn signature(&self) -> Sp<ClkMap>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: T::Ctx<'i>) -> Result<Sp<WithDefSite<Clock>>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckStmt> ClockCheckSpanStmt for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: T::Ctx<'i>) -> Result<Sp<()>> {
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
    fn clockcheck<'i>(&self, _: Span, ctx: Self::Ctx<'i>) -> Result<WithDefSite<Clock>> {
        Ok(ctx.clock_of(&self))
    }
}

impl Sp<decl::Prog> {
    pub fn clockcheck(&self) -> Result<()> {
        Ok(())
    }
}
