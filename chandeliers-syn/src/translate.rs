use std::collections::HashSet;

use super::ast as lus;
use super::translate::candle::Sp;

use chandeliers_san::ast as candle;
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

pub type TrResult<T> = Result<T, TokenStream>;

type CandleExpr = candle::expr::Expr;

pub trait Translate {
    type Output;
    type Ctx<'i>;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<Self::Output>;
}

pub trait SpanTranslate {
    type Ctx<'i>;
    type Output;
    fn translate(self, ctx: Self::Ctx<'_>) -> TrResult<Sp<Self::Output>>;
    fn flat_translate(self, ctx: Self::Ctx<'_>) -> TrResult<Self::Output>;
}
impl<T> SpanTranslate for Sp<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, ctx: Self::Ctx<'_>) -> TrResult<Sp<Self::Output>> {
        let res = self.t.translate(self.span, ctx)?;
        Ok(Sp {
            t: res,
            span: self.span,
        })
    }
    fn flat_translate(self, ctx: Self::Ctx<'_>) -> TrResult<Self::Output> {
        self.t.translate(self.span, ctx)
    }
}

impl<T> Translate for Box<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<Self::Output> {
        (*self).translate(span, ctx)
    }
}

impl Translate for lus::Prog {
    type Ctx<'i> = ();
    type Output = candle::decl::Prog;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::decl::Prog> {
        let mut decls = Vec::new();
        for decl in self.decls.into_iter() {
            decls.push(decl.translate(())?);
        }
        Ok(candle::decl::Prog { decls })
    }
}

impl Translate for lus::AttrDecl {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        match self {
            Self::Tagged(_, n) => n.flat_translate(()),
            Self::Node(n) => n.flat_translate(()),
        }
    }
}

impl Translate for lus::Decl {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => candle::decl::Decl::Const(c.translate(())?),
            Self::Node(n) => candle::decl::Decl::Node(n.translate(())?),
            Self::Extern(e) => e.flat_translate(())?,
        })
    }
}

impl Translate for lus::Extern {
    type Ctx<'i> = ();
    type Output = candle::decl::Decl;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => candle::decl::Decl::ExtConst(c.translate(())?),
            Self::Node(n) => candle::decl::Decl::ExtNode(n.translate(())?),
        })
    }
}

impl Translate for lus::ExtConst {
    type Ctx<'i> = ();
    type Output = candle::decl::ExtConst;
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            name: Sp::new(name.to_string(), span),
        });
        let ty = self.ty.translate(())?;
        Ok(candle::decl::ExtConst { name, ty })
    }
}

impl Translate for lus::ExtNode {
    type Ctx<'i> = ();
    type Output = candle::decl::ExtNode;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::decl::ExtNode> {
        let name = self
            .name
            .map(|span, name| candle::decl::NodeName(Sp::new(name.to_string(), span)));
        let inputs = self.inputs.translate(())?;
        let outputs = self.outputs.translate(())?;
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
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
        let name = self
            .name
            .map(|span, name| candle::decl::NodeName(Sp::new(name.to_string(), span)));
        let inputs = self.inputs.translate(())?;
        let outputs = self.outputs.translate(())?;
        let locals = self.locals.translate(())?;
        let mut ectx = ExprCtx::default();
        for shadows in &[&inputs, &outputs, &locals] {
            for s in shadows.t.iter() {
                ectx.shadow_glob.insert(s.t.name.t.name.t.clone());
            }
        }

        for def in self.defs.into_iter() {
            def.translate(ectx.view())?;
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
    fn translate(self, span: Span, _: ()) -> TrResult<candle::decl::Const> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            name: Sp::new(name.to_string(), span),
        });
        let ty = self.ty.translate(())?;
        let mut ectx = ExprCtx::default();
        let value = self.value.translate(ectx.view())?;
        if !ectx.stmts.is_empty() || !ectx.blocks.is_empty() {
            let s = format!("Expression {value} is not const: const expressions must not contain any function calls");
            return Err(quote_spanned! {span=>
                compile_error!(#s);
            });
        }
        Ok(candle::decl::Const { name, ty, value })
    }
}

impl Translate for lus::ArgsTys {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, _span: Span, _: ()) -> TrResult<candle::Tuple<Sp<candle::decl::TyVar>>> {
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(&mut vs)?;
        }
        Ok(vs)
    }
}

impl Translate for lus::ArgsTy {
    type Ctx<'i> = &'i mut candle::Tuple<Sp<candle::decl::TyVar>>;
    type Output = ();
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let ty = self.ty.translate(())?;
        self.args.translate((ctx, ty))?;
        Ok(())
    }
}

impl Translate for lus::Decls {
    type Ctx<'i> = (
        &'i mut candle::Tuple<Sp<candle::decl::TyVar>>,
        Sp<candle::ty::TyBase>,
    );
    type Output = ();
    fn translate(self, _span: Span, (vars, ty): Self::Ctx<'_>) -> TrResult<()> {
        for id in self.ids {
            vars.push(id.map(|span, id| {
                candle::decl::TyVar {
                    name: Sp::new(
                        candle::expr::LocalVar {
                            name: Sp::new(id.to_string(), span),
                        },
                        span,
                    ),
                    ty: ty.map(|span, ty| {
                        candle::ty::Stream {
                            base: Sp::new(ty, span),
                            depth: candle::clock::Depth {
                                // FIXME: don't forget to change this by depth propagation
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
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
        self.base.flat_translate(())
    }
}

impl Translate for lus::BaseType {
    type Ctx<'i> = ();
    type Output = candle::ty::TyBase;
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
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
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
        match self {
            Self::Decls(d) => d.flat_translate(()),
            Self::None => Ok(candle::Tuple::default()),
        }
    }
}

impl Translate for lus::VarsDecl {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
        self.decls.flat_translate(())
    }
}

impl Translate for lus::Statement {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        match self {
            Self::Assert(ass) => ass.flat_translate(ctx),
            Self::Def(def) => def.flat_translate(ctx),
        }
    }
}

impl Translate for lus::Assertion {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let e = self.expr.translate(fork!(ctx))?;
        ctx.stmts
            .push(Sp::new(candle::stmt::Statement::Assert(e), span));
        Ok(())
    }
}

impl Translate for lus::Def {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<()> {
        let target = self.target.translate(())?;
        let source = self.source.translate(fork!(ctx))?;
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
    fn translate(self, span: Span, _: ()) -> TrResult<Self::Output> {
        match self {
            Self::Var(i) => Ok(candle::stmt::VarTuple::Single(Sp::new(
                candle::expr::LocalVar {
                    name: i.map(|_, t| t.to_string()),
                },
                span,
            ))),
            Self::Tuple(ts) => ts.flat_translate(()),
        }
    }
}

impl Translate for lus::TargetExprTuple {
    type Ctx<'i> = ();
    type Output = candle::stmt::VarTuple;
    fn translate(self, span: Span, _: ()) -> TrResult<Self::Output> {
        let mut vs = candle::Tuple::default();
        for t in self.fields {
            vs.push(t.translate(())?);
        }
        if vs.len() != 1 {
            Ok(candle::stmt::VarTuple::Multiple(Sp::new(vs, span)))
        } else {
            Ok(vs.into_iter().next().unwrap().t)
        }
    }
}

impl<X, Y> Translate for lus::expr::ExprHierarchy<X, Y>
where
    X: Translate,
    for<'i> Y: Translate<Ctx<'i> = X::Ctx<'i>, Output = X::Output>,
{
    type Ctx<'i> = X::Ctx<'i>;
    type Output = X::Output;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<Self::Output> {
        match self {
            Self::Here(x) => x.translate(span, ctx),
            Self::Below(y) => y.translate(span, ctx),
        }
    }
}

impl Translate for lus::Expr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        self.inner.flat_translate(ctx)
    }
}

impl Translate for lus::expr::IfExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let cond = Box::new(self.cond.translate(fork!(ctx))?);
        let yes = Box::new(self.yes.translate(fork!(ctx))?);
        let no = Box::new(self.no.translate(ctx)?);
        Ok(CandleExpr::Ifx { cond, yes, no })
    }
}

impl Translate for lus::expr::OrExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut or = it
            .next()
            .expect("OrExpr should have at least one member")
            .translate(fork!(ctx))?;
        for e in it {
            let e = e.translate(fork!(ctx))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut and = it
            .next()
            .expect("AndExpr should have at least one member")
            .translate(fork!(ctx))?;
        for e in it {
            let e = e.translate(fork!(ctx))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(ctx)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Not,
            inner,
        })
    }
}

impl Translate for lus::expr::CmpExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
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
                return first.flat_translate(ctx);
            }
        };
        // We must not have a third element.
        if it.next().is_some() {
            return Err(quote_spanned! {span=>
                compile_error!("Comparisons are not associative");
            });
        }

        let Pair::Punctuated(lhs, op) = first else {
            unreachable!()
        };
        let Pair::End(rhs) = second else {
            unreachable!()
        };
        let op = op.translate(span, ())?;
        let lhs = Box::new(lhs.translate(fork!(ctx))?);
        let rhs = Box::new(rhs.translate(ctx)?);
        Ok(CandleExpr::CmpOp { op, lhs, rhs })
    }
}

impl Translate for lus::expr::CmpOp {
    type Ctx<'i> = ();
    type Output = candle::expr::CmpOp;
    fn translate(self, _span: Span, _: ()) -> TrResult<Self::Output> {
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (extra_depth, fby) = it.next().expect("FbyExpr should have at least one member");
        let mut fby = fby.translate(fork!(ctx).bump(extra_depth))?;
        for (d, e) in it {
            let e = e.translate(fork!(ctx).bump(d))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        self.inner.flat_translate(ctx.incr())
    }
}

impl Translate for lus::expr::ThenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (_, then) = it.next().expect("ThenExpr should have at least one member");
        let mut then = then.translate(fork!(ctx))?;
        for (d, e) in it {
            let e = e.translate(fork!(ctx))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("AddExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(fork!(ctx))?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.flat_translate(ctx);
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(fork!(ctx))?;
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
                    let rhs = e.translate(fork!(ctx))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("MulExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(fork!(ctx))?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.flat_translate(fork!(ctx));
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(fork!(ctx))?;
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
                    let rhs = e.translate(fork!(ctx))?;
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
    fn translate(self, _span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(ctx)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Neg,
            inner,
        })
    }
}

impl Translate for lus::expr::ParenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let mut es = candle::Tuple::default();
        for e in self.inner.into_iter() {
            es.push(e.translate(fork!(ctx))?);
        }
        match es.len() {
            0 => Err(quote_spanned! {span=>
                compile_error!("Tuple should have at least one element");
            }),
            1 => Ok(es.into_iter().next().expect("ParenExpr cannot be empty").t),
            _ => Ok(CandleExpr::Tuple(Sp::new(es, span))),
        }
    }
}

impl Translate for lus::expr::CallExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let args_span = self._paren.span.join();
        let mut es = candle::Tuple::default();
        for e in self.args.into_iter() {
            es.push(e.translate(fork!(ctx))?);
        }
        let s = self.fun.map(|_, t| t.to_string());
        match s.t.as_str() {
            "float" => {
                if es.len() != 1 {
                    Err(quote_spanned! {span=>
                        compile_error!("Builtin function `float` expects exactly one argument");
                    })
                } else {
                    Ok(CandleExpr::Builtin(Sp::new(
                        candle::expr::Builtin::Float(Box::new(
                            es.into_iter().next().expect("Float argument count failure"),
                        )),
                        span,
                    )))
                }
            }
            &_ => {
                let id = Sp::new(
                    candle::expr::NodeId {
                        id: Sp::new(ctx.blocks.len(), span),
                    },
                    span,
                );
                ctx.blocks.push(Sp::new(candle::decl::NodeName(s), span));
                let st = Sp::new(
                    candle::stmt::Statement::Substep {
                        clk: candle::clock::Depth {
                            dt: ctx.depth,
                            span,
                        },
                        id,
                        args: Sp::new(es, args_span),
                        nbret: Sp::new(None, span),
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
    }
}

impl Translate for lus::expr::AtomicExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        match self {
            Self::Lit(l) => l.translate(span, ctx),
            Self::Call(c) => c.translate(span, ctx),
            Self::Var(v) => v.translate(span, ctx),
            Self::Paren(p) => p.translate(span, ctx),
            Self::Pre(p) => p.translate(span, ctx),
            Self::Neg(n) => n.translate(span, ctx),
            Self::Not(n) => n.translate(span, ctx),
        }
    }
}

impl Translate for lus::expr::VarExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        let name = self.name.map(|_, t| t.to_string());
        if ctx.shadow_glob.contains(&name.t) {
            Ok(CandleExpr::Reference(Sp::new(
                candle::expr::Reference::Var(Sp::new(
                    candle::expr::ClockVar {
                        var: Sp::new(candle::expr::LocalVar { name }, span),
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
                candle::expr::Reference::Global(Sp::new(candle::expr::GlobalVar { name }, span)),
                span,
            )))
        }
    }
}

impl Translate for lus::expr::LitExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, span: Span, _ctx: Self::Ctx<'_>) -> TrResult<CandleExpr> {
        use syn::Lit;
        let lit = match self.lit.t {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().expect("Failed to parse Int")),
            Lit::Float(f) => {
                candle::expr::Lit::Float(f.base10_parse().expect("Failed to parse Float"))
            }
            _ => {
                return Err(quote_spanned! {span=>
                    compile_error!("Lustre only accepts bool/int/float literals");
                })
            }
        };
        Ok(CandleExpr::Lit(Sp::new(lit, span)))
    }
}
