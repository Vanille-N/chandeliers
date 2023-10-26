//! Translator from the Lustre parsing AST to the Candle pre-analysis AST.
//!
//! Translating from Lustre to Candle is not a difficult task because the two
//! languages have mostly identical structures, but there are nevertheless
//! numerous traps and subtleties that although minor for a programmer are very
//! relevant for a systematic translator.
//!
//! We summarize here the differences between the two languages that constitute
//! the details to look out for in the implementations of this file.
//!
//!
//!
//! # Global vs Local name resolution
//!
//! Relevant `impl`: `VarExpr`.
//!
//! Lustre allows for local variables to shadow global constants, and any
//! use of a variable -- local or constant -- is syntactically identical.
//! Not so in Candle, where several technical restrictions require that the
//! Candle AST differentiate between the two:
//! - Rust does not allow global constant shadowing, so during codegen
//!   Candle must pick fresh identifiers for local variables that cannot
//!   collide with those of constants,
//! - global constants are given a Candle type that is not the same as that
//!   of local variables with the same Lustre type.
//!   Indeed the Candle type for variables of `int` is `Nillable<i64>`,
//!   and the Candle type for constants of `int` is a plain `i64`.
//! These thus require differences in the handling of variables and constants
//! at both definition site and call site, which means that the translator
//! must maintain a mutable context of currently shadowed variables.
//!
//!
//!
//! # Precedence
//!
//! Relevant `impl`s: all `expr::*`.
//!
//! The hierarchy of expressions exists only for parsing purposes to express
//! the precedence of operators, and in Candle there is no longer any notion
//! of `AtomicExpr`, all exressions are at the same level.
//! Candle has much fewer expression types (`Expr`) than the Lustre parsing
//! AST (`MulExpr`, `AddExpr`, `PreExpr`, `AtomicExpr`, ...), and this is
//! reflected in the translation that needs to create much more `Box`es
//! but in return has more translation functions with the same output type.
//!
//!
//!
//! # Associativity
//!
//! Relevant `impl`s: `AddExpr`, `OrExpr`, `AndExpr`, `FbyExpr`, `MulExpr`.
//!
//! FIXME.
//!
//!
//!
//! # Noassoc comparisons
//!
//! Relevant `impl`: `CmpExpr`.
//!
//! FIXME.
//!
//!
//!
//! # Size-1 tuple flattening
//!
//! Relevant `impl`s: `TargetExprTuple`, `TargetExpr`.
//!
//! FIXME.
//!
//!
//!
//! # Fby-expansion and pre-pushing
//!
//! Relevant `impl`s: `FbyExpr`, `PreExpr`, `VarExpr`.
//!
//! Lustre and Candle don't quite have the same temporal operators.
//! Both have a construct to fetch one value before a certain clock value
//! and another after (`->` in Lustre, `later!` in Candle), but
//! 1. only Lustre has `fby`, and
//! 2. only Lustre can apply `pre` to arbitrary expressions.
//! The first of these is easy: `e1 fby e2` expands to `e1 -> pre e2`,
//! but this still leaves the issue that `e2` could be an expression on which
//! Candle does not support `pre`. Indeed Candle's only other temporal operator
//! is `pre^n v` (written `var!(self <~ n; v)`) that fetches the value `n` instants
//! ago, but only for `v` a variable, not an arbitrary expression.
//!
//! The translator must thus push the `pre` to the leaves, according to the
//! following non-exhaustive list of rules:
//! - `pre n ~~> n` (`n` a literal or constant)
//! - `pre (e1 + e2) ~~> (pre e1) + (pre e2)`
//! - `pre (-e) ~~> -(pre e)`
//! - `pre float(e) ~~> float(pre e)`
//! - `pre (e1 or e2) ~~> (pre e1) or (pre e2)`
//! - `pre (e1 -> e2) ~~> (pre e1) -> (pre e2)`
//! - etc...
//!
//! To this end the translator carries in its context the current depth at
//! with the expression is evaluated. Usually "regular" (`+`, `-`, `or`, ...)
//! operators propagate the same depth to their arguments, and temporal operators
//! act upon the depth: `pre` increases it by one, `fby` evaluates its two arguments
//! on different depths, etc.
//!
//!
//!
//! # Substep extraction
//!
//! Relevant `impl`s: `Node`, all `expr::*` .
//!
//! Candle expressions are pure. This has the consequence that advancing by
//! one tick another node cannot be part of an expression, and must instead
//! be made part of a statement.
//! This is managed by the type `ExprCtx` and its accessor `ExprCtxView`:
//! when a call to another node is encountered, it is extracted to its own
//! statement to assign its output to a fresh binding that can be used as
//! a pure expression.

use std::collections::HashSet;

use super::ast as lus;
use super::translate::candle::Sp;

use chandeliers_err::{self as err, IntoError};
use chandeliers_san::ast as candle;

use proc_macro2::Span;

pub type TrResult<T> = Result<T, err::Error>;

type CandleExpr = candle::expr::Expr;

pub trait Translate {
    type Output;
    type Ctx<'i>;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<Self::Output>;
}

pub trait SpanTranslate {
    type Ctx<'i>;
    type Output;
    fn translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> TrResult<Sp<Self::Output>>;
    fn flat_translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> TrResult<Self::Output>;
}
impl<T> SpanTranslate for Sp<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> TrResult<Sp<Self::Output>> {
        let res = self.t.translate(run_uid, self.span, ctx)?;
        Ok(Sp {
            t: res,
            span: self.span,
        })
    }
    fn flat_translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> TrResult<Self::Output> {
        self.t.translate(run_uid, self.span, ctx)
    }
}

impl<T> Translate for Box<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<Self::Output> {
        (*self).translate(run_uid, span, ctx)
    }
}

impl Translate for lus::Prog {
    type Ctx<'i> = ();
    type Output = candle::decl::Prog;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<candle::decl::Prog> {
        let mut decls = Vec::new();
        for decl in self.decls.into_iter() {
            decls.push(decl.translate(run_uid, ())?);
        }
        Ok(candle::decl::Prog { decls })
    }
}

impl Translate for lus::AttrDecl {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        match self {
            Self::Tagged(_, n) => n.flat_translate(run_uid, ()),
            Self::Node(n) => n.flat_translate(run_uid, ()),
        }
    }
}

impl Translate for lus::Decl {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => candle::decl::Decl::Const(c.translate(run_uid, ())?),
            Self::Node(n) => candle::decl::Decl::Node(n.translate(run_uid, ())?),
            Self::Extern(e) => e.flat_translate(run_uid, ())?,
        })
    }
}

impl Translate for lus::Extern {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => candle::decl::Decl::ExtConst(c.translate(run_uid, ())?),
            Self::Node(n) => candle::decl::Decl::ExtNode(n.translate(run_uid, ())?),
        })
    }
}

impl Translate for lus::ExtConst {
    type Ctx<'i> = ();
    type Output = candle::decl::ExtConst;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            repr: Sp::new(name.to_string(), span),
            run_uid,
        });
        let ty = self.ty.translate(run_uid, ())?;
        Ok(candle::decl::ExtConst { name, ty })
    }
}

impl Translate for lus::ExtNode {
    type Ctx<'i> = ();
    type Output = candle::decl::ExtNode;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<candle::decl::ExtNode> {
        let name = self.name.map(|span, name| candle::decl::NodeName {
            repr: Sp::new(name.to_string(), span),
            run_uid,
        });
        let inputs = self.inputs.translate(run_uid, ())?;
        let outputs = self.outputs.translate(run_uid, ())?;
        Ok(candle::decl::ExtNode {
            name,
            inputs,
            outputs,
        })
    }
}

#[derive(Default)]
struct ExprCtx {
    blocks: Vec<Sp<candle::decl::NodeName>>,
    stmts: Vec<Sp<candle::stmt::Statement>>,
    shadow_glob: HashSet<String>,
}

pub struct ExprCtxView<'i> {
    blocks: &'i mut Vec<Sp<candle::decl::NodeName>>,
    stmts: &'i mut Vec<Sp<candle::stmt::Statement>>,
    shadow_glob: &'i HashSet<String>,
    depth: usize,
}

impl ExprCtx {
    fn view(&mut self) -> ExprCtxView<'_> {
        ExprCtxView {
            blocks: &mut self.blocks,
            stmts: &mut self.stmts,
            shadow_glob: &self.shadow_glob,
            depth: 0,
        }
    }
}

impl ExprCtxView<'_> {
    fn bump(self, n: usize) -> Self {
        Self {
            depth: self.depth + n,
            ..self
        }
    }

    fn incr(self) -> Self {
        self.bump(1)
    }
}

macro_rules! fork {
    ($this:ident) => {
        ExprCtxView {
            blocks: &mut *$this.blocks,
            stmts: &mut *$this.stmts,
            shadow_glob: &$this.shadow_glob,
            depth: $this.depth,
        }
    };
}

impl Translate for lus::Node {
    type Ctx<'i> = ();
    type Output = candle::decl::Node;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        let name = self.name.map(|span, name| candle::decl::NodeName {
            repr: Sp::new(name.to_string(), span),
            run_uid,
        });
        let inputs = self.inputs.translate(run_uid, ())?;
        let outputs = self.outputs.translate(run_uid, ())?;
        let locals = self.locals.translate(run_uid, ())?;
        let mut ectx = ExprCtx::default();
        for shadows in &[&inputs, &outputs, &locals] {
            for s in shadows.t.iter() {
                ectx.shadow_glob.insert(s.t.name.t.repr.t.clone());
            }
        }

        for def in self.defs.into_iter() {
            def.translate(run_uid, ectx.view())?;
        }
        let ExprCtx { blocks, stmts, .. } = ectx;
        Ok(candle::decl::Node {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        })
    }
}

impl Translate for lus::Const {
    type Ctx<'i> = ();
    type Output = candle::decl::Const;
    fn translate(self, run_uid: usize, span: Span, _: ()) -> TrResult<candle::decl::Const> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            repr: Sp::new(name.to_string(), span),
            run_uid,
        });
        let ty = self.ty.translate(run_uid, ())?;
        let mut ectx = ExprCtx::default();
        let value = self.value.translate(run_uid, ectx.view())?;
        if !ectx.stmts.is_empty() || !ectx.blocks.is_empty() {
            return Err(err::NotConst {
                site: span,
                what: "Function calls are",
            }
            .into_err());
        }
        Ok(candle::decl::Const { name, ty, value })
    }
}

impl Translate for lus::ArgsTys {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        _: (),
    ) -> TrResult<candle::Tuple<Sp<candle::decl::TyVar>>> {
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(run_uid, &mut vs)?;
        }
        Ok(vs)
    }
}

impl Translate for lus::ArgsTy {
    type Ctx<'i> = &'i mut candle::Tuple<Sp<candle::decl::TyVar>>;
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let ty = self.ty.translate(run_uid, ())?;
        self.args.translate(run_uid, (ctx, ty))?;
        Ok(())
    }
}

impl Translate for lus::Decls {
    type Ctx<'i> = (
        &'i mut candle::Tuple<Sp<candle::decl::TyVar>>,
        Sp<candle::ty::TyBase>,
    );
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, (vars, ty): Self::Ctx<'_>) -> TrResult<()> {
        for id in self.ids {
            vars.push(id.map(|span, id| {
                candle::decl::TyVar {
                    name: Sp::new(
                        candle::expr::LocalVar {
                            repr: Sp::new(id.to_string(), span),
                            run_uid,
                        },
                        span,
                    ),
                    ty: ty.map(|span, ty| {
                        candle::ty::Stream {
                            base: Sp::new(ty, span),
                            depth: candle::clock::Depth {
                                // Putting in a dummy value 0 for `dt`, don't forget to update
                                // it by depth propagation in the positivity check...
                                dt: 0,
                                span,
                            },
                        }
                    }),
                }
            }));
        }
        Ok(())
    }
}

impl Translate for lus::Type {
    type Ctx<'i> = ();
    type Output = candle::ty::TyBase;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        if !matches!(self.clock.t, lus::TypeClock::None) {
            unimplemented!("Translate for ClockType");
        }
        self.base.flat_translate(run_uid, ())
    }
}

impl Translate for lus::BaseType {
    type Ctx<'i> = ();
    type Output = candle::ty::TyBase;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        Ok(match self {
            Self::Int(_) => candle::ty::TyBase::Int,
            Self::Float(_) => candle::ty::TyBase::Float,
            Self::Bool(_) => candle::ty::TyBase::Bool,
        })
    }
}

impl Translate for lus::OptionalVarsDecl {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        match self {
            Self::Decls(d) => d.flat_translate(run_uid, ()),
            Self::None => Ok(candle::Tuple::default()),
        }
    }
}

impl Translate for lus::VarsDecl {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        self.decls.flat_translate(run_uid, ())
    }
}

impl Translate for lus::Statement {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        match self {
            Self::Assert(ass) => ass.flat_translate(run_uid, ctx),
            Self::Def(def) => def.flat_translate(run_uid, ctx),
        }
    }
}

impl Translate for lus::Assertion {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let e = self.expr.translate(run_uid, fork!(ctx))?;
        ctx.stmts
            .push(Sp::new(candle::stmt::Statement::Assert(e), span));
        Ok(())
    }
}

impl Translate for lus::Def {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let target = self.target.translate(run_uid, ())?;
        let source = self.source.translate(run_uid, fork!(ctx))?;
        ctx.stmts.push(Sp::new(
            candle::stmt::Statement::Let { target, source },
            span,
        ));
        Ok(())
    }
}

impl Translate for lus::TargetExpr {
    type Ctx<'i> = ();
    type Output = candle::stmt::VarTuple;
    fn translate(self, run_uid: usize, span: Span, _: ()) -> TrResult<Self::Output> {
        match self {
            Self::Var(i) => Ok(candle::stmt::VarTuple::Single(Sp::new(
                candle::expr::LocalVar {
                    repr: i.map(|_, t| t.to_string()),
                    run_uid,
                },
                span,
            ))),
            Self::Tuple(ts) => ts.flat_translate(run_uid, ()),
        }
    }
}

impl Translate for lus::TargetExprTuple {
    type Ctx<'i> = ();
    type Output = candle::stmt::VarTuple;
    fn translate(self, run_uid: usize, span: Span, _: ()) -> TrResult<Self::Output> {
        let mut vs = candle::Tuple::default();
        for t in self.fields {
            vs.push(t.translate(run_uid, ())?);
        }
        if vs.len() != 1 {
            Ok(candle::stmt::VarTuple::Multiple(Sp::new(vs, span)))
        } else {
            Ok(vs.into_iter().next().unwrap().t)
        }
    }
}

impl Translate for lus::Expr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        self.inner.flat_translate(run_uid, ctx)
    }
}

impl Translate for lus::expr::IfExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let cond = Box::new(self.cond.translate(run_uid, fork!(ctx))?);
        let yes = Box::new(self.yes.translate(run_uid, fork!(ctx))?);
        let no = Box::new(self.no.translate(run_uid, ctx)?);
        Ok(CandleExpr::Ifx { cond, yes, no })
    }
}

impl Translate for lus::expr::OrExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut or = it
            .next()
            .expect("OrExpr should have at least one member")
            .translate(run_uid, fork!(ctx))?;
        for e in it {
            let e = e.translate(run_uid, fork!(ctx))?;
            let span = e.span.join(or.span).expect("Faulty span");
            or = Sp::new(
                CandleExpr::BinOp {
                    op: candle::expr::BinOp::BitOr,
                    lhs: Box::new(or),
                    rhs: Box::new(e),
                },
                span,
            );
        }
        Ok(or.t)
    }
}

impl Translate for lus::expr::AndExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut and = it
            .next()
            .expect("AndExpr should have at least one member")
            .translate(run_uid, fork!(ctx))?;
        for e in it {
            let e = e.translate(run_uid, fork!(ctx))?;
            let span = e.span.join(and.span).expect("Faulty span");
            and = Sp::new(
                CandleExpr::BinOp {
                    op: candle::expr::BinOp::BitAnd,
                    lhs: Box::new(and),
                    rhs: Box::new(e),
                },
                span,
            );
        }
        Ok(and.t)
    }
}

impl Translate for lus::expr::NotExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(run_uid, ctx)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Not,
            inner,
        })
    }
}

impl Translate for lus::expr::CmpExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());
        let mut it = self.items.into_pairs();
        // We must have a first element
        let first = it.next().expect("CmpExpr should have at least one member");
        let second = match it.next() {
            Some(second) => second,
            None => {
                // If we don't have a second element then this is just dropping
                // to the level below.
                // The first one can't have punctuation
                let Pair::End(first) = first else {
                    unreachable!()
                };
                return first.flat_translate(run_uid, ctx);
            }
        };
        let Pair::Punctuated(lhs, op) = first else {
            unreachable!("We know that there is a second")
        };
        let lhs = Box::new(lhs.translate(run_uid, fork!(ctx))?);
        let op = op.translate(run_uid, span, ())?;
        // We must not have a third element.
        if let Some(third) = it.next() {
            // We're going to throw this path anyway, we might as well
            // make destructive changes to get a better error message.
            let Pair::Punctuated(second, oper2) = second else {
                unreachable!()
            };
            let second = second.translate(run_uid, fork!(ctx))?;
            let oper2 = oper2.translate(run_uid, span, ())?;
            let third = match third {
                Pair::Punctuated(third, _) => third,
                Pair::End(third) => third,
            };
            let third = third.translate(run_uid, fork!(ctx))?;
            return Err(err::CmpNotAssociative {
                site: span,
                first: lhs,
                oper1: op,
                oper2,
                second,
                third,
            }
            .into_err());
        }
        let Pair::End(rhs) = second else {
            unreachable!("We know there is no third")
        };
        let rhs = Box::new(rhs.translate(run_uid, ctx)?);
        Ok(CandleExpr::CmpOp { op, lhs, rhs })
    }
}

impl Translate for lus::expr::CmpOp {
    type Ctx<'i> = ();
    type Output = candle::expr::CmpOp;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> TrResult<Self::Output> {
        Ok(match self {
            Self::Le(_) => candle::expr::CmpOp::Le,
            Self::Lt(_) => candle::expr::CmpOp::Lt,
            Self::Ge(_) => candle::expr::CmpOp::Ge,
            Self::Gt(_) => candle::expr::CmpOp::Gt,
            Self::Eq(_) => candle::expr::CmpOp::Eq,
            Self::Ne(_) => candle::expr::CmpOp::Ne,
        })
    }
}

impl Translate for lus::expr::FbyExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (extra_depth, fby) = it.next().expect("FbyExpr should have at least one member");
        let mut fby = fby.translate(run_uid, fork!(ctx).bump(extra_depth))?;
        for (d, e) in it {
            let e = e.translate(run_uid, fork!(ctx).bump(d))?;
            let span = fby.span.join(e.span).expect("Faulty span");
            fby = Sp::new(
                CandleExpr::Later {
                    clk: candle::clock::Depth {
                        dt: ctx.depth + d,
                        span,
                    },
                    before: Box::new(e),
                    after: Box::new(fby),
                },
                span,
            );
        }
        Ok(fby.t)
    }
}

impl Translate for lus::expr::PreExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        self.inner.flat_translate(run_uid, ctx.incr())
    }
}

impl Translate for lus::expr::ThenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (_, then) = it.next().expect("ThenExpr should have at least one member");
        let mut then = then.translate(run_uid, fork!(ctx))?;
        for (d, e) in it {
            let e = e.translate(run_uid, fork!(ctx))?;
            let span = then.span.join(e.span).expect("Faulty span");
            then = Sp::new(
                CandleExpr::Later {
                    clk: candle::clock::Depth {
                        span,
                        dt: ctx.depth + d,
                    },
                    before: Box::new(e),
                    after: Box::new(then),
                },
                span,
            );
        }
        Ok(then.t)
    }
}

impl Translate for lus::expr::AddExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("AddExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(run_uid, fork!(ctx))?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.flat_translate(run_uid, ctx);
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(run_uid, fork!(ctx))?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(
                        CandleExpr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        span,
                    );
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(run_uid, fork!(ctx))?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(
                        CandleExpr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        span,
                    );
                    break;
                }
            }
        }
        Ok(lhs.t)
    }
}

impl lus::expr::AddOp {
    fn translate(self) -> TrResult<candle::expr::BinOp> {
        Ok(match self {
            Self::Add(_) => candle::expr::BinOp::Add,
            Self::Sub(_) => candle::expr::BinOp::Sub,
        })
    }
}

impl Translate for lus::expr::MulExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("MulExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(run_uid, fork!(ctx))?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.flat_translate(run_uid, fork!(ctx));
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(run_uid, fork!(ctx))?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(
                        CandleExpr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        span,
                    );
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(run_uid, fork!(ctx))?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(
                        CandleExpr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        span,
                    );
                    break;
                }
            }
        }
        Ok(lhs.t)
    }
}

impl Translate for lus::expr::ClockExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());
        let mut its = self.items.into_pairs();
        let Pair::End(e) = its
            .next()
            .expect("ClockExpr should have at least one member")
        else {
            unimplemented!("Translate for ClockExpr");
        };
        e.flat_translate(run_uid, ctx)
    }
}

impl lus::expr::MulOp {
    fn translate(self) -> TrResult<candle::expr::BinOp> {
        Ok(match self {
            Self::Mul(_) => candle::expr::BinOp::Mul,
            Self::Div(_) => candle::expr::BinOp::Div,
            Self::Rem(_) => candle::expr::BinOp::Rem,
        })
    }
}

impl Translate for lus::expr::NegExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(run_uid, ctx)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Neg,
            inner,
        })
    }
}

impl Translate for lus::expr::ParenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut es = candle::Tuple::default();
        for e in self.inner.into_iter() {
            es.push(e.translate(run_uid, fork!(ctx))?);
        }
        match es.len() {
            1 => Ok(es.into_iter().next().expect("ParenExpr cannot be empty").t),
            _ => Ok(CandleExpr::Tuple(Sp::new(es, span))),
        }
    }
}

impl Translate for lus::expr::CallExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let args_span = self._paren.span.join();
        let mut es = candle::Tuple::default();
        for e in self.args.into_iter() {
            es.push(e.translate(run_uid, fork!(ctx))?);
        }
        let repr = self.fun.map(|_, t| t.to_string());
        let id = Sp::new(
            candle::expr::NodeId {
                id: Sp::new(ctx.blocks.len(), span),
                repr: repr.clone(),
            },
            span,
        );
        ctx.blocks
            .push(Sp::new(candle::decl::NodeName { repr, run_uid }, span));
        let st = Sp::new(
            candle::stmt::Statement::Substep {
                clk: candle::clock::Depth {
                    dt: ctx.depth,
                    span,
                },
                id: id.clone(),
                args: Sp::new(es, args_span),
            },
            span,
        );
        ctx.stmts.push(st);
        Ok(CandleExpr::Reference(Sp::new(
            candle::expr::Reference::Node(id),
            span,
        )))
    }
}

impl Translate for lus::expr::AtomicExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        match self {
            Self::Lit(l) => l.translate(run_uid, span, ctx),
            Self::Call(c) => c.translate(run_uid, span, ctx),
            Self::Var(v) => v.translate(run_uid, span, ctx),
            Self::Paren(p) => p.translate(run_uid, span, ctx),
        }
    }
}

impl Translate for lus::expr::PositiveExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        match self {
            Self::If(i) => i.translate(run_uid, span, ctx),
            Self::Merge(_m) => unimplemented!("Translate for Merge"),
            Self::Pre(p) => p.translate(run_uid, span, ctx),
            Self::Neg(n) => n.translate(run_uid, span, ctx),
            Self::Not(n) => n.translate(run_uid, span, ctx),
            Self::Atomic(a) => a.translate(run_uid, span, ctx),
        }
    }
}

impl Translate for lus::expr::VarExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let repr = self.name.map(|_, t| t.to_string());
        if ctx.shadow_glob.contains(&repr.t) {
            Ok(CandleExpr::Reference(Sp::new(
                candle::expr::Reference::Var(Sp::new(
                    candle::expr::ClockVar {
                        var: Sp::new(candle::expr::LocalVar { repr, run_uid }, span),
                        depth: candle::clock::Depth {
                            span,
                            dt: ctx.depth,
                        },
                    },
                    span,
                )),
                span,
            )))
        } else {
            Ok(CandleExpr::Reference(Sp::new(
                candle::expr::Reference::Global(Sp::new(
                    candle::expr::GlobalVar { repr, run_uid },
                    span,
                )),
                span,
            )))
        }
    }
}

impl Translate for lus::expr::LitExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _run_uid: usize, span: Span, _ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::Lit;
        let lit = match self.lit.t {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().expect("Failed to parse Int")),
            Lit::Float(f) => {
                candle::expr::Lit::Float(f.base10_parse().expect("Failed to parse Float"))
            }
            _ => return Err(err::UnhandledLitType { site: span }.into_err()),
        };
        Ok(CandleExpr::Lit(Sp::new(lit, span)))
    }
}
