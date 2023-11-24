//! Verify the consistency of clocks within and between nodes.
//!
//! The main challenge here is the handling of tuples and functions, because
//! we need to have clock substitutions.
//! As a first step, we DO NOT HANDLE non-homogeneous tuples, which means
//! that declarations such as this:
//! `node foo() returns (b; i when b)`
//! are not allowed. The codegen can handle them just fine, but they really
//! are a pain to verify because they make substitutions much harder.
//! Finer handling of clocks is something that I have already attempted, but
//! this version does not implement that.

#![allow(dead_code)] // FIXME

use std::collections::HashMap;
use std::fmt;

use chandeliers_err::{self as err, EAccum, Transparent};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, op, stmt, ty, var, Tuple};
use crate::sp::{Sp, Span, WithSpan};

/*
macro_rules! at {
    ($arr:expr, $idx:expr) => {
        match $arr.get($idx) {
            Some(x) => x,
            None => err::abort!(
                "Created a relative index of invalid size: index {} on tuple of length {}",
                $idx,
                $arr.len()
            ),
        }
    };
}
*/

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
/// A generic wrapper for objects that have two canonical `Span`s,
/// one at the definition site and one at the call site.
#[derive(Clone, PartialEq, Eq, Debug)]
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
            def_site: Transparent::from(None),
        }
    }

    /// Attach an additional span to an object.
    fn with_def_site(mut self, span: Span) -> Self {
        self.def_site = Transparent::from(Some(span));
        self
    }

    /// Transfer a def site from one object to another.
    fn with_def_site_of<Other>(mut self, other: &WithDefSite<Other>) -> Self {
        self.def_site = other.def_site;
        self
    }
}

crate::sp::derive_with_span!(AbsoluteClk);
#[derive(Debug, Clone, PartialEq, Eq)]
enum AbsoluteClk {
    Implicit,
    Adaptative,
    OnExplicit(WithDefSite<String>),
    OffExplicit(WithDefSite<String>),
}

impl fmt::Display for AbsoluteClk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => write!(f, "'self"),
            Self::Adaptative => write!(f, "'_"),
            Self::OnExplicit(c) => write!(f, "when '{}", c.data),
            Self::OffExplicit(c) => write!(f, "whenot '{}", c.data),
        }
    }
}

crate::sp::derive_with_span!(Named<T> where <T>);
#[derive(Debug, Clone, PartialEq, Eq)]
struct Named<T> {
    name: Option<Sp<String>>,
    data: Sp<T>,
}

impl Sp<&ty::Clock> {
    fn into_absolute(self) -> Sp<AbsoluteClk> {
        match &self.t {
            ty::Clock::Implicit => AbsoluteClk::Implicit,
            ty::Clock::Adaptative => AbsoluteClk::Adaptative,
            ty::Clock::Explicit { id, activation } => {
                let clk = WithDefSite::without(id.clone()).with_def_site(id.span);
                if *activation {
                    AbsoluteClk::OnExplicit(clk)
                } else {
                    AbsoluteClk::OffExplicit(clk)
                }
            }
        }
        .with_span(self.span)
        // FIXME: better def site
    }
}

/// Local clock context, mapping local variables to their clocks.
struct ExprClkCtx {
    /// Known clocked variables.
    local: HashMap<var::Local, WithDefSite<Clock>>,
}

impl ExprClkCtx {
    /// Fetch the clock of a local variable (global constants have an implicit clock that we never
    /// check).
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Clock> {
        self.local
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
    type Ctx<'node>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'node>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<Sp<AbsoluteClk>>;
}

/// Clock typing interface for statements.
trait ClockCheckDecl {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'node>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<()>;
}

/// Helper trait for `Sp` to implement `ClockCheckStmt`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'node>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<()>;
}

/// Clock typing interface.
pub trait ClockCheck {
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, eaccum: &mut EAccum) -> Option<()>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'node> = T::Ctx<'node>;
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: T::Ctx<'_>) -> Option<Sp<AbsoluteClk>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckExpr> ClockCheckExpr for Box<T> {
    type Ctx<'node> = T::Ctx<'node>;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: T::Ctx<'_>) -> Option<AbsoluteClk> {
        self.as_ref().clockcheck(eaccum, span, ctx)
    }
}

impl<T: ClockCheckDecl> ClockCheckSpanDecl for Sp<T> {
    type Ctx<'node> = T::Ctx<'node>;
    fn clockcheck<'i>(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<()> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()?;
        Some(())
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        _span: Span,
        _ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        Some(AbsoluteClk::Adaptative)
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        _span: Span,
        _ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        Some(AbsoluteClk::Adaptative)
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        let implicit = ctx.clock_of(self);
        let abs = implicit.data.as_ref().into_absolute();
        Some(abs.t)
    }
}

impl ClockCheckExpr for var::Past {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        // There is a big question here in how we handle `pre (e when b)`.
        // The current approach is that it will be blocked at the level of
        // `DummyPre`.
        let clk = self.var.clockcheck(eaccum, ctx)?.t;
        Some(clk)
    }
}

impl ClockCheckExpr for var::Reference {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        match self {
            Self::Global(g) => Some(g.clockcheck(eaccum, ctx)?.t),
            Self::Var(v) => Some(v.clockcheck(eaccum, ctx)?.t),
        }
    }
}

impl Sp<&AbsoluteClk> {
    fn assert_matches(self, eaccum: &mut EAccum, other: Self, whole: Span) -> Option<()> {
        match (&self.t, &other.t) {
            (AbsoluteClk::Adaptative, _)
            | (_, AbsoluteClk::Adaptative)
            | (AbsoluteClk::Implicit, AbsoluteClk::Implicit) => Some(()),
            (AbsoluteClk::OnExplicit(id1), AbsoluteClk::OnExplicit(id2))
            | (AbsoluteClk::OffExplicit(id1), AbsoluteClk::OffExplicit(id2)) => {
                if id1 == id2 {
                    Some(())
                } else {
                    eaccum.error(vec![(
                        format!(
                            "Mismatch between clocks: {} on the left but {} on the right",
                            self, other
                        ),
                        Some(whole),
                    )])
                }
            }
            _ => eaccum.error(vec![
                (
                    format!(
                        "Mismatch between clocks: {} on the left but {} on the right",
                        self, other
                    ),
                    Some(whole),
                ),
                (format!("This is clocked by {}", self), Some(self.span)),
                (format!("due to this"), Some(self.span)),
                (format!("This is clocked by {}", other), Some(other.span)),
                (format!("due to this"), Some(other.span)),
            ]),
        }
    }
}

trait Reclock: Sized {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self>;
}

impl<T: Reclock> Reclock for Sp<T> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        self.map(|_, t| t.reclock(eaccum, whole, new, clock_of_clock))
            .transpose()
    }
}

impl<T: Reclock, U: Reclock> Reclock for (T, U) {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        Some((
            self.0.reclock(eaccum, whole, new, clock_of_clock)?,
            self.1.reclock(eaccum, whole, new, clock_of_clock)?,
        ))
    }
}

impl Reclock for AbsoluteClk {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        span: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        clock_of_clock.assert_matches(eaccum, self.with_span(Span::forge()).as_ref(), span)?;
        Some(new.t.clone())
    }
}

impl<T: Reclock> Reclock for Named<T> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        Some(Self {
            name: None,
            data: self.data.reclock(eaccum, whole, new, clock_of_clock)?,
        })
    }
}

impl ClockCheckExpr for expr::Expr {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        match self {
            Self::Lit(l) => Some(l.clockcheck(eaccum, ctx)?.t),
            Self::Reference(r) => Some(r.clockcheck(eaccum, ctx)?.t),
            Self::DummyPre(e) => {
                let clk = e.clockcheck(eaccum, ctx)?;
                clk.as_ref().is_implicit(eaccum)?;
                Some(clk.t)
            }
            Self::Bin { op: _, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let lhs = lhs?;
                let rhs = rhs?;
                lhs.as_ref().assert_matches(eaccum, rhs.as_ref(), span)?;
                Some(lhs.t)
            }
            Self::Cmp { op: _, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let lhs = lhs?;
                let rhs = rhs?;
                lhs.as_ref().assert_matches(eaccum, rhs.as_ref(), span)?;
                Some(lhs.t)
            }
            Self::Un { op: _, inner } => {
                let inner = inner.clockcheck(eaccum, ctx)?;
                Some(inner.t)
            }
            Self::Later {
                delay: _,
                before,
                after,
            } => {
                let before = before.clockcheck(eaccum, ctx);
                let after = after.clockcheck(eaccum, ctx);
                let before = before?;
                let after = after?;
                before.as_ref().is_implicit(eaccum);
                after.as_ref().is_implicit(eaccum);
                Some(before.t)
            }
            Self::Clock {
                op,
                inner,
                activate,
            } => {
                let inner = inner.clockcheck(eaccum, ctx)?;
                let clock_of_clock = activate.clockcheck(eaccum, ctx)?;
                let activate = activate.into_clock(eaccum, *op)?;
                Some(
                    inner
                        .reclock(eaccum, span, activate.as_ref(), clock_of_clock.as_ref())?
                        .t,
                )
            }
            Self::Substep {
                delay: _,
                id: _,
                args,
            } => {
                let args = args.clockcheck(eaccum, ctx)?;
                Some(args.t)
            }
            Self::Tuple(tup) => Some(tup.clockcheck(eaccum, ctx)?.t),
            Self::Ifx { cond, yes, no } => {
                let cond = cond.clockcheck(eaccum, ctx);
                let yes = yes.clockcheck(eaccum, ctx);
                let no = no.clockcheck(eaccum, ctx);
                let mut cond = cond?;
                let yes = yes?;
                let no = no?;
                cond.refine(eaccum, yes)?;
                cond.refine(eaccum, no)?;
                Some(cond.t)
            }
            Self::Merge { switch, on, off } => {
                let expect_on = switch.into_clock(eaccum, op::Clock::When);
                let expect_off = switch.into_clock(eaccum, op::Clock::Whenot);
                let switch = switch.clockcheck(eaccum, ctx);
                let on = on.clockcheck(eaccum, ctx);
                let off = off.clockcheck(eaccum, ctx);
                let switch = switch?;
                let on = on?;
                let off = off?;
                on.as_ref()
                    .assert_matches(eaccum, expect_on?.as_ref(), span)?;
                off.as_ref()
                    .assert_matches(eaccum, expect_off?.as_ref(), span)?;
                Some(switch.t)
            }
        }
    }
}

impl Sp<Box<expr::Expr>> {
    fn into_clock(&self, _eaccum: &mut EAccum, op: op::Clock) -> Option<Sp<AbsoluteClk>> {
        match &*self.t {
            expr::Expr::Reference(r) => match &r.t {
                var::Reference::Var(v) => {
                    if op == op::Clock::When {
                        Some(
                            AbsoluteClk::OnExplicit(WithDefSite::without(v.t.var.t.repr.clone()))
                                .with_span(v.span),
                        )
                    } else {
                        Some(
                            AbsoluteClk::OffExplicit(WithDefSite::without(v.t.var.t.repr.clone()))
                                .with_span(v.span),
                        )
                    }
                }
                _ => panic!("Expected Var, got {:?}", r.t),
            },
            _ => panic!("Expected Reference, got {:?}", self.t),
        }
    }
}

impl Sp<&AbsoluteClk> {
    fn is_implicit(self, eaccum: &mut EAccum) -> Option<()> {
        match &self.t {
            AbsoluteClk::Implicit | AbsoluteClk::Adaptative => Some(()),
            AbsoluteClk::OnExplicit(e) | AbsoluteClk::OffExplicit(e) => eaccum.error(err::Basic {
                msg: format!("This clock is too slow (clocked by {})", e.data),
                span: self.span,
            }),
        }
    }
}

impl Sp<AbsoluteClk> {
    fn refine(&mut self, _eaccum: &mut EAccum, other: Self) -> Option<()> {
        use AbsoluteClk::*;
        match (&self.t, other.t) {
            (Implicit, Implicit | Adaptative) => Some(()),
            (Adaptative, any) => {
                self.t = any;
                Some(())
            }
            (OnExplicit(_) | OffExplicit(_), Adaptative) => Some(()),
            (Implicit, OnExplicit(_) | OffExplicit(_)) => panic!(),
            (OnExplicit(_) | OffExplicit(_), Implicit) => panic!(),
            (OnExplicit(_), OffExplicit(_)) | (OffExplicit(_), OnExplicit(_)) => panic!(),
            (OnExplicit(l), OnExplicit(r)) => {
                assert_eq!(l, &r);
                Some(())
            }
            (OffExplicit(l), OffExplicit(r)) => {
                assert_eq!(l, &r);
                Some(())
            }
        }
    }
}

impl<T> ClockCheckExpr for Tuple<T>
where
    T: for<'node> ClockCheckSpanExpr<Ctx<'node> = &'node mut ExprClkCtx>,
{
    type Ctx<'i> = &'i mut ExprClkCtx;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        let mut global = AbsoluteClk::Adaptative.with_span(span);
        for e in self.iter() {
            let clk = e.clockcheck(eaccum, &mut *ctx)?;
            global.refine(eaccum, clk)?;
        }
        Some(global.t)
    }
}

impl ClockCheckExpr for stmt::VarTuple {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<AbsoluteClk> {
        match self {
            Self::Single(v) => Some(v.clockcheck(eaccum, ctx)?.t),
            Self::Multiple(tup) => Some(tup.clockcheck(eaccum, ctx)?.t),
        }
    }
}

impl ClockCheckDecl for stmt::Statement {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<()> {
        match self {
            Self::Assert(e) => {
                let clk = e.clockcheck(eaccum, ctx)?;
                clk.as_ref().is_implicit(eaccum)?;
                Some(())
            }
            Self::Let { target, source } => {
                let target = target.clockcheck(eaccum, ctx)?;
                let source = source.clockcheck(eaccum, ctx)?;
                target
                    .as_ref()
                    .assert_matches(eaccum, source.as_ref(), span)?;
                Some(())
            }
        }
    }
}

impl ClockCheckDecl for decl::Node {
    type Ctx<'node> = ();
    fn clockcheck(&self, eaccum: &mut EAccum, _span: Span, _ctx: Self::Ctx<'_>) -> Option<()> {
        // First the interface must be homogeneous
        for vs in [&self.inputs.t, &self.outputs.t] {
            for v in vs.iter() {
                v.t.ty
                    .t
                    .ty
                    .t
                    .clk
                    .as_ref()
                    .into_absolute()
                    .as_ref()
                    .is_implicit(eaccum)?;
            }
        }
        // Then we can check the body in the context of the interface with added
        // locals
        let mut ectx = ExprClkCtx {
            local: HashMap::new(),
        };
        for vs in [&self.inputs.t, &self.outputs.t, &self.locals.t] {
            for v in vs.iter() {
                ectx.local.insert(
                    v.t.name.t.clone(),
                    WithDefSite::without(v.t.ty.t.ty.t.clk.clone()).with_def_site(v.span),
                );
            }
        }
        for stmt in &self.stmts {
            stmt.clockcheck(eaccum, &mut ectx)?;
        }
        Some(())
    }
}

impl ClockCheckDecl for decl::ExtNode {
    type Ctx<'i> = ();
    fn clockcheck(&self, eaccum: &mut EAccum, _: Span, (): ()) -> Option<()> {
        for vs in [&self.inputs.t, &self.outputs.t] {
            for v in vs.iter() {
                v.t.ty
                    .t
                    .ty
                    .t
                    .clk
                    .as_ref()
                    .into_absolute()
                    .as_ref()
                    .is_implicit(eaccum)?;
            }
        }
        Some(())
    }
}

impl ClockCheck for Sp<decl::Prog> {
    fn clockcheck(&self, eaccum: &mut EAccum) -> Option<()> {
        for decl in &self.t.decls {
            match &decl.t {
                decl::Decl::Const(_) | decl::Decl::ExtConst(_) => {}
                decl::Decl::Node(n) => {
                    n.clockcheck(eaccum, ())?;
                }
                decl::Decl::ExtNode(n) => {
                    n.clockcheck(eaccum, ())?;
                }
            }
        }
        Some(())
    }
}

/*
#[cfg(test)]
mod translation_tests {
    use super::*;

    macro_rules! sp {
        ( $x:expr ) => {
            $x.with_span(Span::forge())
        };
    }
    macro_rules! defsite {
        ( $x:expr ) => {
            WithDefSite::without($x)
        };
    }

    macro_rules! forge {
        ( & $T:tt : $($x:tt)* ) => { forge!($T:$($x)*).as_ref() };
        ( str : $s:expr ) => { String::from($s).with_span(Span::forge()) };
        ( relclk : = . ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(()))), sign: true } };
        ( relclk : = ? ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(AbsoluteClk::Implicit))), sign: true } };
        ( relclk : = $c:expr ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(AbsoluteClk::Explicit(defsite!(forge!(str:$c)))))), sign: true }.with_span(Span::forge()) };
        ( relclk : + $c:expr ) => { sp!(Clocked { clk: sp!(RelativeClk::Nth($c)), sign: true }) };
        ( relclk : - $c:expr ) => { sp!(Clocked { clk: sp!(RelativeClk::Nth($c)), sign: false }) };
        ( mapclk : = ) => { Clocked { clk: sp!(MappingClk::Implicit(sp!(()))), sign: true } };
        ( mapclk : <+ $c:expr ) => { Clocked { clk: sp!(MappingClk::NthIn($c)), sign: true } };
        ( mapclk : <- $c:expr ) => { Clocked { clk: sp!(MappingClk::NthIn($c)), sign: false } };
        ( mapclk : +> $c:expr ) => { Clocked { clk: sp!(MappingClk::NthOut($c)), sign: true } };
        ( mapclk : -> $c:expr ) => { Clocked { clk: sp!(MappingClk::NthOut($c)), sign: false } };
        ( tyclk : = ) => { sp!(ty::Clock::Implicit) };
        ( tyclk : + $s:expr ) => { sp!(ty::Clock::Explicit { id: forge!(str:$s), activation: true }) };
        ( tyclk : - $s:expr ) => { sp!(ty::Clock::Explicit { id: forge!(str:$s), activation: false }) };
        ( abs : + ? ) => { Clocked { clk: sp!(AbsoluteClk::Implicit), sign: true } };
        ( abs : + $name:expr ) => { Clocked { clk: sp!(AbsoluteClk::Explicit(defsite!(forge!(str:$name)))), sign: true } };
    }

    #[test]
    fn simple_io() {
        // Inserting `(t0, t1 when t0, t2 whenot t1, t3)`
        // And then we follow with the output tuple `(t4, t5 when t1, t6 when t4)`
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"t0"), forge!(&tyclk:=));
            map.new_input(forge!(&str:"t1"), forge!(&tyclk:+"t0"));
            map.new_input(forge!(&str:"t2"), forge!(&tyclk:-"t1"));
            map.new_input(forge!(&str:"t3"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"t4"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"t5"), forge!(&tyclk:+"t1"));
            map.new_output(forge!(&str:"t6"), forge!(&tyclk:+"t4"));
            map
        };
        assert_eq!(
            map.inputs,
            [
                sp!(forge!(relclk:=.)),
                sp!(forge!(relclk:+0)),
                sp!(forge!(relclk:-1)),
                sp!(forge!(relclk:=.))
            ]
        );
        assert_eq!(
            map.outputs,
            [
                sp!(forge!(mapclk:=)),
                sp!(forge!(mapclk:<+1)),
                sp!(forge!(mapclk:+>0))
            ]
        );
    }

    #[test]
    #[should_panic]
    fn undeclared_input() {
        // Trying to insert a clock that does not correspond to an already
        // declared variable.
        let _map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"t0"), forge!(&tyclk:+"t0"));
        };
    }

    #[test]
    fn empty_mapping() {
        // () -> ()
        let mut eaccum = EAccum::new();
        let map = {
            let map = ClkMap::default();
            map
        };
        // Preserves the implicit clock
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![],
            }
        );
        // Preserves an explicit clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![],
            }
        );
    }

    #[test]
    fn singleton_mapping() {
        let mut eaccum = EAccum::new();
        // (x) -> (y)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:=));
            map
        };
        // Recognizes a tuple element going at the right speed
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="c"))],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![(None, forge!(relclk:="c"))],
            }
        );
        // Preserves the implicit clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), sp!(forge!(relclk:=?)))],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![(None, sp!(forge!(relclk:=?)))],
            }
        );
        // Rejects a mismatch between the tuple's speed and the first element's speed
        let itup3 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="x"))],
        };
        assert!(map
            .translate(&mut eaccum, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn depends_on_input() {
        let mut eaccum = EAccum::new();
        // (x) -> (y when x)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:+"x"));
            map
        };
        // Transports whatever name `x` has.
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), sp!(forge!(relclk:=?)))],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Including if `x` itself is clocked.
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"z")),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="z"))],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+"z")),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Fails if the input is not named.
        let itup3 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(None, sp!(forge!(relclk:=?)))],
        };
        assert!(map
            .translate(&mut eaccum, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn transports_autoref() {
        let mut eaccum = EAccum::new();
        // (x1, x2 when x1) -> (y1, y2 whenot y1)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x1"), forge!(&tyclk:=));
            map.new_input(forge!(&str:"x2"), forge!(&tyclk:+"x1"));
            map.new_output(forge!(&str:"y1"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y2"), forge!(&tyclk:-"y1"));
            map
        };
        // Normal application.
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="c")),
                (Some(forge!(str:"a2")), forge!(relclk:+0)),
            ],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![(None, forge!(relclk:="c")), (None, forge!(relclk:-0)),],
            }
        );
        // Second element has the wrong clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"z")),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="z")),
                (Some(forge!(str:"a2")), forge!(relclk:="z")),
            ],
        };
        assert!(map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .is_none());
    }
}
*/
