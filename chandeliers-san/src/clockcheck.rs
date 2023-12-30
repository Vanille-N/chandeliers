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

use chandeliers_err::{self as err, EAccum};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, op, stmt, ty, var, Tuple};
use crate::sp::{Sp, Span, WithDefSite, WithSpan};

crate::sp::derive_with_span!(Clk);
/// The clock of an expression.
/// This is usually `Implicit` for variables and `Adaptative` for constants,
/// but temporal operators produce other clocks.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Clk {
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
    OnExplicit(WithDefSite<Sp<String>>),
    /// Negative dependency on a clock (obtained by the `whenot` operator).
    /// Carries a value whenever the contained variable (whose name is the `String`)
    /// is `false`.
    OffExplicit(WithDefSite<Sp<String>>),
}

impl fmt::Display for Clk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => write!(f, "'self"),
            Self::Adaptative => write!(f, "'_"),
            Self::OnExplicit(c) => write!(f, "'when {}", c.data),
            Self::OffExplicit(c) => write!(f, "'whenot {}", c.data),
        }
    }
}

impl err::TryDefSite for Clk {
    fn try_def_site(&self) -> Option<Span> {
        match self {
            Self::Implicit | Self::Adaptative => None,
            Self::OnExplicit(c) | Self::OffExplicit(c) => c.try_def_site(),
        }
    }
}

impl Sp<&ty::Clock> {
    /// Turn a parsing clock into a `Clk`. This could almost have been part of `translate`
    /// in `syn`, but it's here because `Clk` is only an internal representation.
    fn into_clk(self) -> Sp<Clk> {
        match &self.t {
            ty::Clock::Implicit => Clk::Implicit,
            ty::Clock::Adaptative => Clk::Adaptative,
            ty::Clock::Explicit { id, activation } => {
                let clk = WithDefSite::new_with(id.clone(), id.span);
                if *activation {
                    Clk::OnExplicit(clk)
                } else {
                    Clk::OffExplicit(clk)
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
    local: HashMap<var::Local, WithDefSite<Sp<Clock>>>,
}

impl ExprClkCtx {
    /// Fetch the clock of a local variable (global constants have an implicit clock that we never
    /// check).
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Sp<Clock>> {
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
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<Clk>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'node>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<Sp<Clk>>;
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
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: T::Ctx<'_>) -> Option<Sp<Clk>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckExpr> ClockCheckExpr for Box<T> {
    type Ctx<'node> = T::Ctx<'node>;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: T::Ctx<'_>) -> Option<Clk> {
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
    fn clockcheck(&self, _eaccum: &mut EAccum, _span: Span, _ctx: Self::Ctx<'_>) -> Option<Clk> {
        Some(Clk::Adaptative)
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, _eaccum: &mut EAccum, _span: Span, _ctx: Self::Ctx<'_>) -> Option<Clk> {
        Some(Clk::Adaptative)
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, _eaccum: &mut EAccum, _span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
        let implicit = ctx.clock_of(self);
        let abs = implicit.data.as_ref().into_clk();
        Some(abs.t)
    }
}

impl ClockCheckExpr for var::Past {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, _span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
        // There is a big question here in how we handle `pre (e when b)`.
        // The current approach is that it will be blocked at the level of
        // `DummyPre`.
        let clk = self.var.clockcheck(eaccum, ctx)?.t;
        Some(clk)
    }
}

impl ClockCheckExpr for var::Reference {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, _span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
        match self {
            Self::Global(g) => Some(g.clockcheck(eaccum, ctx)?.t),
            Self::Var(v) => Some(v.clockcheck(eaccum, ctx)?.t),
        }
    }
}

/// Apply clock substitution.
trait Reclock: Sized {
    /// Change clock into `new`, after checking that the current clock
    /// is compatible with `clock_of_clock`.
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        lhs_span: Span,
        new: Sp<&Clk>,
        clock_of_clock: Sp<Clk>,
    ) -> Option<Self>;
}

impl<T: Reclock> Reclock for Sp<T> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        _lhs_span: Span,
        new: Sp<&Clk>,
        clock_of_clock: Sp<Clk>,
    ) -> Option<Self> {
        self.map(|span, t| t.reclock(eaccum, whole, span, new, clock_of_clock))
            .transpose()
    }
}

impl Reclock for Clk {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        whole: Span,
        lhs_span: Span,
        new: Sp<&Clk>,
        clock_of_clock: Sp<Clk>,
    ) -> Option<Self> {
        self.with_span(lhs_span)
            .refine(eaccum, clock_of_clock, whole)?;
        Some(new.t.clone())
    }
}

impl ClockCheckExpr for expr::Expr {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
        match self {
            Self::Lit(l) => Some(l.clockcheck(eaccum, ctx)?.t),
            Self::Reference(r) => Some(r.clockcheck(eaccum, ctx)?.t),
            Self::DummyParen(inner) => Some(inner.clockcheck(eaccum, ctx)?.t),
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
            // Flip serves the same role as Later, but it can operate on arbitrary
            // clocks as long as it's the same on both sides.
            Self::Flip {
                id: _,
                initial,
                continued,
            } => {
                // Rhs dictates the rythm, and lhs needs to be at least as fast.
                let lhs = initial.clockcheck(eaccum, ctx);
                let rhs = continued.clockcheck(eaccum, ctx);
                let rhs = rhs?;
                let lhs = lhs?;
                rhs.identical(eaccum, lhs, span)?;
                Some(rhs.t)
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
                        .reclock(eaccum, span, span, activate.as_ref(), clock_of_clock)?
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
                let mut on = on?;
                let mut off = off?;
                on.refine(eaccum, expect_on?, span)?;
                off.refine(eaccum, expect_off?, span)?;
                Some(switch.t)
            }
            Self::FetchRegister {
                id: _,
                dummy_init,
                dummy_followed_by,
            } => {
                let followed_by = dummy_followed_by.clockcheck(eaccum, ctx);
                let followed_by = followed_by?;
                if let Some(init) = dummy_init {
                    let init = init.clockcheck(eaccum, ctx)?;
                    followed_by.identical(eaccum, init, span)?;
                }
                Some(followed_by.t)
            }
        }
    }
}

impl Sp<Box<expr::Expr>> {
    /// Convert an expression into a clock.
    /// i.e. Check that it is a local variable (typecheck has already
    /// verified that it is a boolean), and apply the sign given by `op`.
    fn as_clock(&self, eaccum: &mut EAccum, op: op::Clock) -> Option<Sp<Clk>> {
        match &*self.t {
            expr::Expr::Reference(r) => match &r.t {
                var::Reference::Var(v) => {
                    if op == op::Clock::When {
                        Some(
                            Clk::OnExplicit(WithDefSite::new_with(
                                v.t.var.t.repr.clone(),
                                self.span,
                            ))
                            .with_span(v.span),
                        )
                    } else {
                        Some(
                            Clk::OffExplicit(WithDefSite::new_with(
                                v.t.var.t.repr.clone(),
                                self.span,
                            ))
                            .with_span(v.span),
                        )
                    }
                }
                var::Reference::Global(_) => eaccum.error(err::NotAClock { expr: self }),
            },
            _ => eaccum.error(err::NotAClock { expr: self }),
        }
    }
}

impl Sp<&Clk> {
    /// Check that the given clock is not explicitly set, i.e. that it is `Implicit` or
    /// `Adaptative`. This is required by `pre` and `->` who need their operands to
    /// move at the same pace as the node.
    fn is_implicit(self, eaccum: &mut EAccum, help: &'static str) -> Option<()> {
        match &self.t {
            Clk::Implicit | Clk::Adaptative => Some(()),
            Clk::OnExplicit(_) | Clk::OffExplicit(_) => {
                eaccum.error(err::ClkTooSlowExpectImplicit {
                    slow: self,
                    implicit: None::<Span>,
                    extra: &[
                        help,
                        "Delete the extra clock definition or put this in a separate node with its own clock",
                    ],
                })
            }
        }
    }
}

impl Sp<Clk> {
    /// Check that two clocks are identical.
    /// Whether we need identical or compatible clocks is up to each operator,
    /// as some are sensitive to when a value is actually defined or nil.
    fn identical(&self, eaccum: &mut EAccum, other: Self, whole: Span) -> Option<()> {
        #[expect(clippy::enum_glob_use, reason = "Fine in local scope")]
        use Clk::*;
        match (&self.t, &other.t) {
            (Adaptative | Implicit, Adaptative | Implicit) => Some(()),
            (OnExplicit(l), OnExplicit(r)) | (OffExplicit(l), OffExplicit(r)) if l == r => Some(()),
            _ => eaccum.error(err::ClkNotIdentical {
                first: &*self,
                second: &other,
                whole,
            }),
        }
    }

    /// Specialize `self` to be at least as precise as `other`.
    /// This will check and accumulate compatibility, so to check that all clocks in a tuple
    /// are compatible you can do something to the effect of
    /// ```skip
    /// let mut current = Clk::Implicit;
    /// for e in tup {
    ///     current.refine(e)?;
    /// }
    /// ```
    /// The rest is boilerplate for spans and accumulators.
    fn refine(&mut self, eaccum: &mut EAccum, other: Self, whole: Span) -> Option<()> {
        #[expect(clippy::enum_glob_use, reason = "Fine in local scope")]
        use Clk::*;
        match (&self.t, &other.t) {
            (Adaptative, _) => {
                self.t = other.t;
                Some(())
            }
            (_, Adaptative) | (Implicit, Implicit) => Some(()),
            (Implicit, OnExplicit(_) | OffExplicit(_)) => {
                eaccum.error(err::ClkTooSlowExpectImplicit {
                    slow: other,
                    implicit: &*self,
                    extra: None::<&str>,
                })
            }
            (OnExplicit(_) | OffExplicit(_), Implicit) => {
                eaccum.error(err::ClkTooSlowExpectImplicit {
                    slow: &*self,
                    implicit: other,
                    extra: None::<&str>,
                })
            }
            (OnExplicit(l), OnExplicit(r)) if l == r => Some(()),
            (OffExplicit(l), OffExplicit(r)) if l == r => Some(()),
            _ => eaccum.error(err::ClkNotComparable {
                first: &*self,
                second: &other,
                whole,
            }),
        }
    }
}

impl<T> ClockCheckExpr for Tuple<T>
where
    T: for<'node> ClockCheckSpanExpr<Ctx<'node> = &'node mut ExprClkCtx>,
{
    type Ctx<'i> = &'i mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
        let mut its = self.iter();
        let Some(fst) = its.next() else {
            return Some(Clk::Adaptative);
        };
        let global = fst.clockcheck(eaccum, &mut *ctx)?;
        for e in self.iter() {
            let clk = e.clockcheck(eaccum, &mut *ctx)?;
            global.identical(eaccum, clk, span)?;
        }
        Some(global.t)
    }
}

impl ClockCheckExpr for stmt::VarTuple {
    type Ctx<'node> = &'node mut ExprClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, _span: Span, ctx: Self::Ctx<'_>) -> Option<Clk> {
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
                let is_empty = target.t.is_empty();
                let target = target.clockcheck(eaccum, ctx)?;
                let source = source.clockcheck(eaccum, ctx)?;
                if is_empty {
                    Some(())
                } else {
                    target.identical(eaccum, source, span)?;
                    Some(())
                }
            }
            Self::PutRegister { .. } => {
                // All checks already completed by `FetchRegister`.
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
                v.t.ty.t.ty.t.clk.as_ref().into_clk().as_ref().is_implicit(
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
                    WithDefSite::new_with(v.t.ty.t.ty.t.clk.clone(), v.span),
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
                v.t.ty.t.ty.t.clk.as_ref().into_clk().as_ref().is_implicit(
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
        use decl::Decl;
        for decl in &self.t.decls {
            match &decl.t {
                Decl::Const(_) | Decl::ExtConst(_) => {}
                Decl::Node(n) => {
                    n.clockcheck(eaccum, ())?;
                }
                Decl::ExtNode(n) => {
                    n.clockcheck(eaccum, ())?;
                }
            }
        }
        Some(())
    }
}
