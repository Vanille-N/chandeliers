use super::ast as lus;
use super::translate::candle::Sp;
use lus::InputSpan;

use chandeliers_san::candle::ast as candle;
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

#[must_use]
pub type TrResult<T> = Result<T, TokenStream>;

type BlockNames = Vec<Sp<candle::decl::NodeName>>;
type StmtList = Vec<Sp<candle::stmt::Statement>>;
type CandleExpr = candle::expr::Expr;
pub trait TranslateExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>>;
}

impl lus::AttrNode {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Node>> {
        match self {
            Self::Tagged(_, n) => n.translate(),
            Self::Node(n) => n.translate(),
        }
    }
}

impl lus::Node {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Node>> {
        let span = self.input_span();
        let name = self
            .name
            .map_ref_with_span(|_, name| candle::decl::NodeName(self.name.to_string()));
        let inputs = self.inputs.translate()?;
        let outputs = self.outputs.translate()?;
        let locals = self.locals.translate()?;
        let mut blocks = Vec::new();
        let mut stmts = Vec::new();
        for def in self.defs.into_iter() {
            def.translate(&mut blocks, &mut stmts)?;
        }
        Ok(Sp::new(
            candle::decl::Node {
                name,
                inputs,
                outputs,
                locals,
                blocks,
                stmts,
            },
            span,
        ))
    }
}

impl lus::ArgsTys {
    pub fn translate(self) -> TrResult<Sp<candle::Tuple<Sp<candle::decl::Var>>>> {
        let span = self.input_span();
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(&mut vs)?;
        }
        Ok(Sp::new(vs, span))
    }
}

impl lus::ArgsTy {
    pub fn translate(self, vars: &mut candle::Tuple<Sp<candle::decl::Var>>) -> TrResult<()> {
        let ty = self.ty.translate()?;
        self.args.translate(vars, ty)?;
        Ok(())
    }
}

impl lus::Decls {
    pub fn translate(
        self,
        vars: &mut candle::Tuple<Sp<candle::decl::Var>>,
        ty: Sp<candle::ty::TyBase>,
    ) -> TrResult<()> {
        for id in self.ids {
            vars.elems.push(id.map_with_span(|span, id| {
                candle::decl::Var {
                    name: id.map_with_span(|span, name| candle::expr::Var {
                        name: name.to_string(),
                    }),
                    ty: ty.map(|span, ty| {
                        candle::ty::Stream {
                            base: Sp::new(ty, span),
                            depth: candle::clock::Depth {
                                // FIXME: don't forget to change this by depth propagation
                                dt: 0,
                            },
                        }
                    }),
                }
            }));
        }
        Ok(())
    }
}

impl lus::Type {
    pub fn translate(self) -> TrResult<Sp<candle::ty::TyBase>> {
        self.base.translate()
    }
}

impl lus::BaseType {
    pub fn translate(self) -> TrResult<Sp<candle::ty::TyBase>> {
        Ok(self.map_with_span(|span, t| match t {
            Self::Int(_) => candle::ty::TyBase::Int,
            Self::Float(_) => candle::ty::TyBase::Float,
            Self::Bool(_) => candle::ty::TyBase::Bool,
        }))
    }
}

impl lus::OptionalVarsDecl {
    pub fn translate(self) -> TrResult<Sp<candle::Tuple<Sp<candle::decl::Var>>>> {
        match self {
            Self::Decls(d) => d.translate(),
            Self::None(m) => Ok(m.map_with_span(|_, _| candle::Tuple::default())),
        }
    }
}

impl lus::VarsDecl {
    pub fn translate(self) -> TrResult<Sp<candle::Tuple<Sp<candle::decl::Var>>>> {
        self.decls.translate()
    }
}

impl lus::Statement {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
    ) -> TrResult<()> {
        match self {
            Self::Assert(ass) => ass.translate(blocks, stmts),
            Self::Def(def) => def.translate(blocks, stmts),
        }
    }
}

impl lus::Assertion {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
    ) -> TrResult<()> {
        let span = self.input_span();
        let e = self.expr.translate(blocks, stmts, 0)?;
        stmts.push(Sp::new(candle::stmt::Statement::Assert(e), span));
        Ok(())
    }
}

impl lus::Def {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
    ) -> TrResult<()> {
        let span = self.input_span();
        let target = self.target.translate()?;
        let source = self.source.translate(blocks, stmts, 0)?;
        stmts.push(Sp::new(
            candle::stmt::Statement::Let { target, source },
            span,
        ));
        Ok(())
    }
}

impl lus::TargetExpr {
    pub fn translate(self) -> TrResult<Sp<candle::stmt::VarTuple>> {
        let span = self.input_span();
        match self {
            Self::Var(i) => Ok(Sp::new(
                candle::stmt::VarTuple::Single(Sp::new(
                    candle::expr::Var {
                        name: i.to_string(),
                    },
                    span,
                )),
                span,
            )),
            Self::Tuple(ts) => ts.translate(),
        }
    }
}

impl lus::TargetExprTuple {
    pub fn translate(self) -> TrResult<Sp<candle::stmt::VarTuple>> {
        let span = self.input_span();
        let mut vs = candle::Tuple::default();
        for t in self.fields {
            vs.elems.push(t.translate()?);
        }
        Ok(Sp::new(candle::stmt::VarTuple::Multiple(Sp::new(vs, span)), span))
    }
}

impl<X, Y> TranslateExpr for lus::expr::ExprHierarchy<X, Y>
where
    X: TranslateExpr,
    Y: TranslateExpr,
{
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        match self {
            Self::Here(x) => x.translate(blocks, stmts, depth),
            Self::Below(y) => y.translate(blocks, stmts, depth),
        }
    }
}

impl TranslateExpr for lus::Expr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        self.inner.translate(blocks, stmts, depth)
    }
}

impl TranslateExpr for lus::expr::IfExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let cond = Box::new(self.cond.translate(blocks, stmts, depth)?);
        let yes = Box::new(self.yes.translate(blocks, stmts, depth)?);
        let no = Box::new(self.no.translate(blocks, stmts, depth)?);
        Ok(Sp::new(CandleExpr::Ifx { cond, yes, no }, span))
    }
}

impl TranslateExpr for lus::expr::OrExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter();
        let mut or = it.next().unwrap().translate(blocks, stmts, depth)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth)?;
            let span = e.span.join(or.span).unwrap();
            or = Sp::new(CandleExpr::BinOp {
                op: candle::expr::BinOp::BitOr,
                lhs: Box::new(or),
                rhs: Box::new(e),
            }, span);
        }
        Ok(or)
    }
}

impl TranslateExpr for lus::expr::AndExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter();
        let mut or = it.next().unwrap().translate(blocks, stmts, depth)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth)?;
            let span = e.span.join(or.span).unwrap();
            or = Sp::new(CandleExpr::BinOp {
                op: candle::expr::BinOp::BitAnd,
                lhs: Box::new(or),
                rhs: Box::new(e),
            }, span);
        }
        Ok(or)
    }
}

impl TranslateExpr for lus::expr::NotExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let inner = Box::new(self.inner.translate(blocks, stmts, depth)?);
        Ok(Sp::new(CandleExpr::UnOp {
            op: candle::expr::UnOp::Not,
            inner,
        }, span))
    }
}

impl TranslateExpr for lus::expr::CmpExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());
        let mut it = self.items.into_pairs();
        // We must have a first element
        let first = it.next().unwrap();
        let second = match it.next() {
            Some(second) => second,
            None => {
                // If we don't have a second element then this is just dropping
                // to the level below.
                // The first one can't have punctuation
                let Pair::End(first) = first else {
                    unreachable!()
                };
                return first.translate(blocks, stmts, depth);
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
        let op = op.translate()?;
        let lhs = Box::new(lhs.translate(blocks, stmts, depth)?);
        let rhs = Box::new(rhs.translate(blocks, stmts, depth)?);
        Ok(Sp::new(CandleExpr::CmpOp { op, lhs, rhs }, span))
    }
}

impl lus::expr::CmpOp {
    fn translate(self) -> TrResult<candle::expr::CmpOp> {
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

impl TranslateExpr for lus::expr::FbyExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (extra_depth, fby) = it.next().unwrap();
        let mut fby = fby.translate(blocks, stmts, depth + extra_depth)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth + d)?;
            let span = fby.span.join(e.span).unwrap();
            fby = Sp::new(CandleExpr::Later {
                clk: candle::clock::Depth { dt: depth + d },
                before: Box::new(e),
                after: Box::new(fby),
            }, span);
        }
        Ok(fby)
    }
}

impl TranslateExpr for lus::expr::PreExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        self.inner.translate(blocks, stmts, depth + 1)
    }
}

impl TranslateExpr for lus::expr::ThenExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (_, then) = it.next().unwrap();
        let mut then = then.translate(blocks, stmts, depth)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth)?;
            let span = then.span.join(e.span).unwrap();
            then = Sp::new(CandleExpr::Later {
                clk: candle::clock::Depth { dt: depth + d },
                before: Box::new(e),
                after: Box::new(then),
            }, span);
        }
        Ok(then)
    }
}

impl TranslateExpr for lus::expr::AddExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut it = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match it.next().unwrap() {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(blocks, stmts, depth)?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.translate(blocks, stmts, depth);
            }
        }
        while let Some(it) = it.next() {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    let span = lhs.span.join(rhs.span).unwrap();
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    let span = lhs.span.join(rhs.span).unwrap();
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    break;
                }
            }
        }
        Ok(lhs)
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

impl TranslateExpr for lus::expr::MulExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut it = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match it.next().unwrap() {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(blocks, stmts, depth)?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.translate(blocks, stmts, depth);
            }
        }
        while let Some(it) = it.next() {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    let span = lhs.span.join(rhs.span).unwrap();
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    let span = lhs.span.join(rhs.span).unwrap();
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    break;
                }
            }
        }
        Ok(lhs)
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

impl TranslateExpr for lus::expr::NegExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let inner = Box::new(self.inner.translate(blocks, stmts, depth)?);
        Ok(Sp::new(CandleExpr::UnOp {
            op: candle::expr::UnOp::Neg,
            inner,
        }, span))
    }
}

impl TranslateExpr for lus::expr::ParenExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let mut es = candle::Tuple::default();
        for e in self.inner.into_iter() {
            es.elems.push(e.translate(blocks, stmts, depth)?);
        }
        match es.elems.len() {
            0 => {
                return Err(quote_spanned! {span=>
                    compile_error!("Tuple should have at least one element");
                })
            }
            1 => Ok(es.elems.pop().unwrap()),
            _ => Ok(Sp::new(CandleExpr::Tuple(Sp::new(es, span)), span)),
        }
    }
}

impl TranslateExpr for lus::expr::CallExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let args_span = self.args.input_span();
        let mut es = candle::Tuple::default();
        for e in self.args.into_iter() {
            es.elems.push(e.translate(blocks, stmts, depth)?);
        }
        let s = self.fun.to_string();
        match s.as_str() {
            "float" => {
                if es.elems.len() != 1 {
                    Err(quote_spanned! {span=>
                        compile_error!("Builtin function `float` expects exactly one argument");
                    })
                } else {
                    Ok(Sp::new(CandleExpr::Builtin(Sp::new(candle::expr::Builtin::Float(Box::new(
                        es.elems.pop().unwrap(),
                    )), span)), span))
                }
            }
            &_ => {
                let id = Sp::new(candle::expr::NodeId { id: blocks.len() }, span);
                blocks.push(Sp::new(candle::decl::NodeName(s), span));
                let st = Sp::new(candle::stmt::Statement::Substep {
                    clk: candle::clock::Depth { dt: depth },
                    id,
                    args: Sp::new(es, args_span),
                }, span);
                stmts.push(st);
                Ok(Sp::new(CandleExpr::Reference(Sp::new(candle::expr::Reference::Node(id), span)), span))
            }
        }
    }
}

impl TranslateExpr for lus::expr::VarExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        Ok(Sp::new(CandleExpr::Reference(Sp::new(candle::expr::Reference::Var(
            Sp::new(candle::expr::ClockVar {
                var: candle::expr::Var {
                    name: self.name.to_string(),
                },
                depth: candle::clock::Depth { dt: depth },
            }, span),
        ), span)), span))
    }
}

impl TranslateExpr for lus::expr::LitExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        use syn::Lit;
        let lit = match self.lit {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().unwrap()),
            Lit::Float(f) => candle::expr::Lit::Float(f.base10_parse().unwrap()),
            _ => {
                return Err(quote_spanned! {self.input_span()=>
                    compile_error!("Lustre only accepts bool/int/float literals");
                })
            }
        };
        Ok(Sp::new(CandleExpr::Lit(Sp::new(lit, span)), span))
    }
}
