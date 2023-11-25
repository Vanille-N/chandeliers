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
}

impl<T> err::TrySpan for WithDefSite<T> {
    fn try_span(&self) -> Option<Span> {
        self.data.try_span()
    }
}

impl<T> err::TryDefSite for WithDefSite<T> {
    fn try_def_site(&self) -> Option<Span> {
        Some(self.def_site.flatten()?.flatten())
    }
}

crate::sp::derive_with_span!(AbsoluteClk);
#[derive(Debug, Clone, PartialEq, Eq)]
enum AbsoluteClk {
    /// Variables that move at the same speed as the node. You can call temporal
    /// operators (`pre`, `fby`) on these.
    Implicit,
    /// Constants and global variables that because they have no state can be
    /// reinterpreted as having any clock. Can be implicitly cast to any of the
    /// other variants of this `enum`.
    Adaptative,
    /// Positive dependency on a clock (obtained by the `when` operator).
    /// Carries a value whenever the contained variable (whose name is the `String`)
    /// is `true`.
    OnExplicit(WithDefSite<String>),
    /// Negative dependency on a clock (obtained by the `whenot` operator).
    /// Carries a value whenever the contained variable (whose name is the `String`)
    /// is `false`.
    OffExplicit(WithDefSite<String>),
}

impl fmt::Display for AbsoluteClk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => write!(f, "'self"),
            Self::Adaptative => write!(f, "'_"),
            Self::OnExplicit(c) => write!(f, "'when {}", c.data),
            Self::OffExplicit(c) => write!(f, "'whenot {}", c.data),
        }
    }
}

impl err::TryDefSite for AbsoluteClk {
    fn try_def_site(&self) -> Option<Span> {
        match self {
            Self::Implicit | Self::Adaptative => None,
            Self::OnExplicit(c) | Self::OffExplicit(c) => c.try_def_site(),
        }
    }
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
            | (AbsoluteClk::OffExplicit(id1), AbsoluteClk::OffExplicit(id2))
                if id1 == id2 =>
            {
                Some(())
            }
            _ => {
                let mut es = vec![(
                    format!("Mismatch between clocks: {self} on the left but {other} on the right",),
                    Some(whole),
                )];
                err::clock_and_def_site(&mut es, &self);
                err::clock_and_def_site(&mut es, &other);
                eaccum.error(err::TmpRaw { es })
            }
        }
    }
}

trait Reclock: Sized {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        lhs_span: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self>;
}

impl<T: Reclock> Reclock for Sp<T> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        _lhs_span: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        self.map(|span, t| t.reclock(eaccum, whole, span, new, clock_of_clock))
            .transpose()
    }
}

impl Reclock for AbsoluteClk {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        lhs_span: Span,
        new: Sp<&AbsoluteClk>,
        clock_of_clock: Sp<&AbsoluteClk>,
    ) -> Option<Self> {
        self.with_span(lhs_span)
            .as_ref()
            .assert_matches(eaccum, clock_of_clock, whole)?;
        Some(new.t.clone())
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
                clk.as_ref().is_implicit(
                    eaccum,
                    "The pre operator operates under the nodes's implicit clock",
                )?;
                Some(clk.t)
            }
            Self::Bin { op: _, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let mut lhs = lhs?;
                let rhs = rhs?;
                lhs.refine(eaccum, rhs, span)?;
                Some(lhs.t)
            }
            Self::Cmp { op: _, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let mut lhs = lhs?;
                let rhs = rhs?;
                lhs.refine(eaccum, rhs, span)?;
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
                before.as_ref().is_implicit(
                    eaccum,
                    "The delay operator (-> / fby) operates under the nodes's implicit clock",
                );
                after.as_ref().is_implicit(
                    eaccum,
                    "The delay operator (-> / fby) operates under the nodes's implicit clock",
                );
                Some(before.t)
            }
            Self::Clock {
                op,
                inner,
                activate,
            } => {
                let inner = inner.clockcheck(eaccum, ctx)?;
                let clock_of_clock = activate.clockcheck(eaccum, ctx)?;
                let activate = activate.as_clock(eaccum, *op)?;
                Some(
                    inner
                        .reclock(
                            eaccum,
                            span,
                            span,
                            activate.as_ref(),
                            clock_of_clock.as_ref(),
                        )?
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
                cond.refine(eaccum, yes, span)?;
                cond.refine(eaccum, no, span)?;
                Some(cond.t)
            }
            Self::Merge { switch, on, off } => {
                let expect_on = switch.as_clock(eaccum, op::Clock::When);
                let expect_off = switch.as_clock(eaccum, op::Clock::Whenot);
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
    fn as_clock(&self, eaccum: &mut EAccum, op: op::Clock) -> Option<Sp<AbsoluteClk>> {
        match &*self.t {
            expr::Expr::Reference(r) => match &r.t {
                var::Reference::Var(v) => {
                    if op == op::Clock::When {
                        Some(
                            AbsoluteClk::OnExplicit(
                                WithDefSite::without(v.t.var.t.repr.clone())
                                    .with_def_site(self.span),
                            )
                            .with_span(v.span),
                        )
                    } else {
                        Some(
                            AbsoluteClk::OffExplicit(
                                WithDefSite::without(v.t.var.t.repr.clone())
                                    .with_def_site(self.span),
                            )
                            .with_span(v.span),
                        )
                    }
                }
                var::Reference::Global(_) => eaccum.error(err::TmpBasic {
                    msg: format!("Expression `{self}` cannot be interpreted as a clock."),
                    span: self.span,
                }),
            },
            _ => eaccum.error(err::TmpBasic {
                msg: format!("Expression `{self}` cannot be interpreted as a clock."),
                span: self.span,
            }),
        }
    }
}

impl Sp<&AbsoluteClk> {
    fn is_implicit(self, eaccum: &mut EAccum, help: &'static str) -> Option<()> {
        match &self.t {
            AbsoluteClk::Implicit | AbsoluteClk::Adaptative => Some(()),
            AbsoluteClk::OnExplicit(_) | AbsoluteClk::OffExplicit(_) => {
                let mut v = vec![(
                    format!(
                        "This clock is too slow: found {self}, expected the implicit clock 'self",
                    ),
                    Some(self.span),
                )];
                use err::TryDefSite;
                if let Some(def_site) = self.try_def_site() {
                    v.push(("due to this".to_owned(), Some(def_site)));
                }
                v.push((help.to_owned(), None));
                // FIXME: better suggestions
                v.push(("Delete the extra clock definition or put this in a separate node with its own clock".to_owned(), None));
                eaccum.error(err::TmpRaw { es: v })
            }
        }
    }
}

impl Sp<AbsoluteClk> {
    /// Specialize `self` to be at least as precise as `other`.
    /// This will check and accumulate compatibility, so to check that all clocks in a tuple
    /// are compatible you can do something to the effect of
    /// ```skip
    /// let mut current = AbsoluteClk::Implicit;
    /// for e in tup {
    ///     current.refine(e)?;
    /// }
    /// ```
    /// The rest is boilerplate for spans and accumulators.
    fn refine(&mut self, eaccum: &mut EAccum, other: Self, whole: Span) -> Option<()> {
        #[expect(clippy::enum_glob_use, reason = "Fine in local scope")]
        use AbsoluteClk::*;
        match (&self.t, &other.t) {
            (Adaptative, _) => {
                self.t = other.t;
                Some(())
            }
            (_, Adaptative) | (Implicit, Implicit) => Some(()),
            (Implicit, OnExplicit(_) | OffExplicit(_)) => {
                let mut v = vec![(
                    format!("This expression is too slow: expected {self} got {other}",),
                    Some(other.span),
                )];
                use err::TryDefSite;
                if let Some(def_site) = other.try_def_site() {
                    v.push((format!("Found {other} here"), Some(def_site)));
                }
                v.push((
                    "Expected because this expression moves at the implicit pace".to_owned(),
                    Some(self.span),
                ));
                eaccum.error(err::TmpRaw { es: v })
            }
            (OnExplicit(_) | OffExplicit(_), Implicit) => {
                let mut es = vec![(
                    format!("This expression is too slow: expected {other} got {self}",),
                    Some(self.span),
                )];
                use err::TryDefSite;
                if let Some(def_site) = self.try_def_site() {
                    es.push((format!("Found {self} here"), Some(def_site)));
                }
                es.push((
                    "Expected because this expression moves at the implicit pace".to_owned(),
                    Some(other.span),
                ));
                eaccum.error(err::TmpRaw { es })
            }
            (OnExplicit(l), OnExplicit(r)) if l == r => Some(()),
            (OffExplicit(l), OffExplicit(r)) if l == r => Some(()),
            _ => {
                let mut v = vec![(
                    format!(
                        "Two subexpressions have incomparable clocks: {other} and {self} are incompatible",
                    ),
                    Some(whole),
                )];
                err::clock_and_def_site(&mut v, &*self);
                err::clock_and_def_site(&mut v, &other);
                eaccum.error(err::TmpRaw { es: v })
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
            global.refine(eaccum, clk, span)?;
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
                clk.as_ref()
                    .is_implicit(eaccum, "Assert operates under the nodes's implicit clock")?;
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
                    .is_implicit(
                        eaccum,
                        "Node signature should use the implicit clock of the node",
                    )?;
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
        let mut scope = eaccum.scope();
        for stmt in &self.stmts {
            scope.compute(|eaccum| stmt.clockcheck(eaccum, &mut ectx));
        }
        scope.close()
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
                    .is_implicit(
                        eaccum,
                        "Node signature should use the implicit clock of the node",
                    )?;
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
