use super::ast as lus;
use lus::InputSpan;

use quote::quote_spanned;
use proc_macro2::{Span, TokenStream};
use chandeliers_san::candle::ast as candle;

pub type TrResult<T> = Result<T, TokenStream>;

type BlockNames = Vec<candle::decl::NodeName>;
type StmtList = Vec<candle::stmt::Statement>;
type CandleExpr = candle::expr::Expr;
pub trait TranslateExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<CandleExpr>;
}

impl lus::AttrNode {
    pub fn translate(self) -> TrResult<candle::decl::Node> {
        match self {
            Self::Tagged(_, n) => n.translate(),
            Self::Node(n) => n.translate(),
        }
    }
}

impl lus::Node {
    pub fn translate(self) -> TrResult<candle::decl::Node> {
        let name = candle::decl::NodeName(self.name.to_string());
        let inputs = self.inputs.translate()?;
        let outputs = self.outputs.translate()?;
        let locals = self.locals.translate()?;
        let mut blocks = Vec::new();
        let mut stmts = Vec::new();
        for def in self.defs.into_iter() {
            def.translate(&mut blocks, &mut stmts)?;
        }
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

impl lus::ArgsTys {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(&mut vs)?;
        }
        Ok(vs)
    }
}

impl lus::ArgsTy {
    pub fn translate(self, vars: &mut candle::Tuple<candle::decl::Var>) -> TrResult<()> {
        let ty = self.ty.translate()?;
        self.args.translate(vars, ty)?;
        Ok(())
    }
}

impl lus::Decls {
    pub fn translate(
        self,
        vars: &mut candle::Tuple<candle::decl::Var>,
        ty: candle::ty::TyBase,
    ) -> TrResult<()> {
        for id in self.ids {
            vars.elems.push(candle::decl::Var {
                name: candle::expr::Var {
                    name: id.to_string(),
                },
                ty: candle::ty::Stream {
                    base: ty,
                    depth: candle::clock::Depth {
                        dt: 0, // FIXME
                    },
                },
            });
        }
        Ok(())
    }
}

impl lus::Type {
    pub fn translate(self) -> TrResult<candle::ty::TyBase> {
        self.base.translate()
    }
}

impl lus::BaseType {
    pub fn translate(self) -> TrResult<candle::ty::TyBase> {
        Ok(match self {
            Self::Int(_) => candle::ty::TyBase::Int,
            Self::Float(_) => candle::ty::TyBase::Float,
            Self::Bool(_) => candle::ty::TyBase::Bool,
        })
    }
}

impl lus::OptionalVarsDecl {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        match self {
            Self::Decls(d) => d.translate(),
            Self::None => Ok(candle::Tuple::default()),
        }
    }
}

impl lus::VarsDecl {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        self.decls.translate()
    }
}

impl lus::Statement {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
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
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
    ) -> TrResult<()> {
        let e = self.expr.translate(blocks, stmts, 0)?;
        stmts.push(candle::stmt::Statement::Assert(e));
        Ok(())
    }
}

impl lus::Def {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
    ) -> TrResult<()> {
        let target = self.target.translate()?;
        let source = self.source.translate(blocks, stmts, 0)?;
        stmts.push(candle::stmt::Statement::Let { target, source });
        Ok(())
    }
}

impl lus::TargetExpr {
    pub fn translate(
        self,
    ) -> TrResult<candle::stmt::VarTuple> {
        match self {
            Self::Var(i) => Ok(candle::stmt::VarTuple::Single(candle::expr::Var { name: i.to_string() })),
            Self::Tuple(ts) => ts.translate(),
        }
    }
}

impl lus::TargetExprTuple {
    pub fn translate(
        self,
    ) -> TrResult<candle::stmt::VarTuple> {
        let mut vs = candle::Tuple::default();
        for t in self.fields {
            vs.elems.push(t.translate()?);
        }
        Ok(candle::stmt::VarTuple::Multiple(vs))
    }
}

impl<X, Y> TranslateExpr for lus::expr::ExprHierarchy<X, Y>
where X: TranslateExpr,
      Y: TranslateExpr,
{
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
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
    ) -> TrResult<CandleExpr> {
        self.inner.translate(blocks, stmts, depth)
    }
}

impl TranslateExpr for lus::expr::IfExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<CandleExpr> {
        let cond = Box::new(self.cond.translate(blocks, stmts, depth)?);
        let yes = Box::new(self.yes.translate(blocks, stmts, depth)?);
        let no = Box::new(self.no.translate(blocks, stmts, depth)?);
        Ok(CandleExpr::Ifx { cond, yes, no })
    }
}

impl TranslateExpr for lus::expr::OrExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut or = it.next().unwrap().translate(blocks, stmts, depth)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth)?;
            or = CandleExpr::BinOp {
                op: candle::expr::BinOp::BitOr,
                lhs: Box::new(or),
                rhs: Box::new(e),
            }
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
    ) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter();
        let mut or = it.next().unwrap().translate(blocks, stmts, depth)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth)?;
            or = CandleExpr::BinOp {
                op: candle::expr::BinOp::BitAnd,
                lhs: Box::new(or),
                rhs: Box::new(e),
            }
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
    ) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(blocks, stmts, depth)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Not,
            inner,
        })
    }
}

impl TranslateExpr for lus::expr::CmpExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<CandleExpr> {
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
                let Pair::End(first) = first else { unreachable!() };
                return first.translate(blocks, stmts, depth);
            }
        };
        // We must not have a third element.
        if it.next().is_some() {
            return Err(quote_spanned! {span=>
                compile_error!("Comparisons are not associative");
            });
        }

        let Pair::Punctuated(lhs, op) = first else { unreachable!() };
        let Pair::End(rhs) = second else { unreachable!() };
        let op = op.translate()?;
        let lhs = Box::new(lhs.translate(blocks, stmts, depth)?);
        let rhs = Box::new(rhs.translate(blocks, stmts, depth)?);
        Ok(CandleExpr::CmpOp {
            op,
            lhs,
            rhs,
        })
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
    ) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (extra_depth, fby) = it.next().unwrap();
        let mut fby = fby.translate(blocks, stmts, depth + extra_depth)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth + d)?;
            fby = CandleExpr::Later {
                clk: candle::clock::Depth { dt: depth + d },
                before: Box::new(e),
                after: Box::new(fby),
            }
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
    ) -> TrResult<CandleExpr> {
        self.inner.translate(blocks, stmts, depth + 1)
    }
}

impl TranslateExpr for lus::expr::ThenExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
    ) -> TrResult<CandleExpr> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (_, then) = it.next().unwrap();
        let mut then = then.translate(blocks, stmts, depth)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth)?;
            then = CandleExpr::Later {
                clk: candle::clock::Depth { dt: depth + d },
                before: Box::new(e),
                after: Box::new(then),
            }
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
    ) -> TrResult<CandleExpr> {
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
                    lhs = CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    lhs = CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
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
    ) -> TrResult<CandleExpr> {
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
                    lhs = CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth)?;
                    lhs = CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
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
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
        let inner = Box::new(self.inner.translate(blocks, stmts, depth)?);
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Neg,
            inner,
        })
    }
}


impl TranslateExpr for lus::expr::ParenExpr {
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
        let span = self.input_span();
        let mut es = candle::Tuple::default();
        for e in self.inner.into_iter() {
            es.elems.push(e.translate(blocks, stmts, depth)?);
        }
        match es.elems.len() {
            0 => return Err(quote_spanned! {span=>
                compile_error!("Tuple should have at least one element");
            }),
            1 => Ok(es.elems.pop().unwrap()),
            _ => Ok(CandleExpr::Tuple(es)),
        }
    }
}

impl TranslateExpr for lus::expr::CallExpr {
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
        let span = self.input_span();
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
                    Ok(CandleExpr::Builtin(candle::expr::Builtin::Float(
                        Box::new(es.elems.pop().unwrap())
                    )))
                }
            }
            &_ => {
                let id = candle::expr::NodeId { id: blocks.len() };
                blocks.push(candle::decl::NodeName(s));
                let st = candle::stmt::Statement::Substep {
                    clk: candle::clock::Depth { dt: depth },
                    id,
                    args: es,
                };
                stmts.push(st);
                Ok(CandleExpr::Reference(candle::expr::Reference::Node(id)))
            }
        }
    }
}

impl TranslateExpr for lus::expr::VarExpr {
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
        Ok(CandleExpr::Reference(
                candle::expr::Reference::Var(
                    candle::expr::ClockVar {
                        var: candle::expr::Var { name: self.name.to_string() },
                        depth: candle::clock::Depth { dt: depth },
                    }
                )
        ))
    }
}

impl TranslateExpr for lus::expr::LitExpr {
    fn translate(self, blocks: &mut BlockNames, stmts: &mut StmtList, depth: usize) -> TrResult<CandleExpr> {
        use syn::Lit;
        let lit = match self.lit {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().unwrap()),
            Lit::Float(f) => candle::expr::Lit::Float(f.base10_parse().unwrap()),
            _ => return Err(quote_spanned! {self.input_span()=>
                compile_error!("Lustre only accepts bool/int/float literals");
            }),
        };
        Ok(CandleExpr::Lit(lit))
    }
}

