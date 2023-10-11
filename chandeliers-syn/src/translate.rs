use std::collections::HashSet;

use super::ast as lus;
use super::translate::candle::Sp;
use lus::InputSpan;

use chandeliers_san::ast as candle;
use proc_macro2::{TokenStream, Span};
use quote::quote_spanned;

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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>>;
}

impl lus::AttrDecl {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Decl>> {
        match self {
            Self::Tagged(_, n) => n.translate(),
            Self::Node(n) => n.translate(),
        }
    }
}

impl lus::Decl {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Decl>> {
        let span = self.input_span();
        match self {
            Self::Const(c) => Ok(Sp::new(candle::decl::Decl::Const(c.translate()?), span)),
            Self::Node(n) => Ok(Sp::new(candle::decl::Decl::Node(n.translate()?), span)),
            Self::Extern(lus::Extern::Const(c)) => Ok(Sp::new(candle::decl::Decl::ExtConst(c.translate()?), span)),
            Self::Extern(lus::Extern::Node(n)) => Ok(Sp::new(candle::decl::Decl::ExtNode(n.translate()?), span)),
        }
    }
}

impl lus::Node {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Node>> {
        let span = self.input_span();
        let name = self
            .name
            .map_ref_with_span(|_, name| candle::decl::NodeName(name.to_string()));
        let inputs = self.inputs.translate(self._inputs_paren.span.join())?;
        let outputs = self.outputs.translate(self._outputs_paren.span.join())?;
        let locals = self.locals.translate()?;
        let mut blocks = Vec::new();
        let mut stmts = Vec::new();
        let mut shadow_glob = HashSet::new();
        for shadows in &[&inputs, &outputs, &locals] {
            for s in &shadows.t.elems {
                shadow_glob.insert(s.t.name.t.name.clone());
            }
        }

        for def in self.defs.into_iter() {
            def.translate(&mut blocks, &mut stmts, &shadow_glob)?;
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

impl lus::Const {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Const>> {
        let span = self.input_span();
        let name = self.name.map_ref_with_span(|_, name| candle::expr::Var { name: name.to_string() });
        let ty = self.ty.translate()?;
        let mut v1 = Vec::new();
        let mut v2 = Vec::new();
        let value = self.value.translate(&mut v1, &mut v2, 0, &HashSet::new())?;
        if !v1.is_empty() || !v2.is_empty() {
            let s = format!("Expression {value} is not const: const expressions must not contain any function calls");
            return Err(quote_spanned! {span=>
                compile_error!(#s);
            });
        }
        Ok(Sp::new(candle::decl::Const { name, ty, value }, span))
    }
}

impl lus::ExtNode {
    pub fn translate(self) -> TrResult<Sp<candle::decl::ExtNode>> {
        let span = self.input_span();
        let name = self
            .name
            .map_ref_with_span(|_, name| candle::decl::NodeName(name.to_string()));
        let inputs = self.inputs.translate(self._inputs_paren.span.join())?;
        let outputs = self.outputs.translate(self._outputs_paren.span.join())?;
        Ok(Sp::new(
            candle::decl::ExtNode {
                name,
                inputs,
                outputs,
            },
            span,
        ))
    }
}

impl lus::ExtConst {
    pub fn translate(self) -> TrResult<Sp<candle::decl::ExtConst>> {
        let span = self.input_span();
        let name = self.name.map_ref_with_span(|_, name| candle::expr::Var { name: name.to_string() });
        let ty = self.ty.translate()?;
        Ok(Sp::new(candle::decl::ExtConst { name, ty }, span))
    }
}


impl lus::Prog {
    pub fn translate(self) -> TrResult<Sp<candle::decl::Prog>> {
        let span = self.input_span();
        let mut decls = Vec::new();
        for decl in self.decls.into_iter() {
            decls.push(decl.translate()?);
        }
        Ok(Sp::new(candle::decl::Prog { decls }, span))
    }
}

impl lus::ArgsTys {
    pub fn translate(self, span: Span) -> TrResult<Sp<candle::Tuple<Sp<candle::decl::Var>>>> {
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
            vars.elems.push(id.map_with_span(|_, id| {
                candle::decl::Var {
                    name: id.map_with_span(|_, name| candle::expr::Var {
                        name: name.to_string(),
                    }),
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

impl lus::Type {
    pub fn translate(self) -> TrResult<Sp<candle::ty::TyBase>> {
        self.base.translate()
    }
}

impl lus::BaseType {
    pub fn translate(self) -> TrResult<Sp<candle::ty::TyBase>> {
        Ok(self.map_with_span(|_, t| match t {
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
        let span = self.input_span();
        self.decls.translate(span)
    }
}

impl lus::Statement {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<()> {
        match self {
            Self::Assert(ass) => ass.translate(blocks, stmts, shadow_glob),
            Self::Def(def) => def.translate(blocks, stmts, shadow_glob),
        }
    }
}

impl lus::Assertion {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<()> {
        let span = self.input_span();
        let e = self.expr.translate(blocks, stmts, 0, shadow_glob)?;
        stmts.push(Sp::new(candle::stmt::Statement::Assert(e), span));
        Ok(())
    }
}

impl lus::Def {
    pub fn translate(
        self,
        blocks: &mut Vec<Sp<candle::decl::NodeName>>,
        stmts: &mut Vec<Sp<candle::stmt::Statement>>,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<()> {
        let span = self.input_span();
        let target = self.target.translate()?;
        let source = self.source.translate(blocks, stmts, 0, shadow_glob)?;
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
        if vs.elems.len() > 1 {
            Ok(Sp::new(candle::stmt::VarTuple::Multiple(Sp::new(vs, span)), span))
        } else {
            unimplemented!("Tuple to Single")
        }
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        match self {
            Self::Here(x) => x.translate(blocks, stmts, depth, shadow_glob),
            Self::Below(y) => y.translate(blocks, stmts, depth, shadow_glob),
        }
    }
}

impl TranslateExpr for lus::Expr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        self.inner.translate(blocks, stmts, depth, shadow_glob)
    }
}

impl TranslateExpr for lus::expr::IfExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let cond = Box::new(self.cond.translate(blocks, stmts, depth, shadow_glob)?);
        let yes = Box::new(self.yes.translate(blocks, stmts, depth, shadow_glob)?);
        let no = Box::new(self.no.translate(blocks, stmts, depth, shadow_glob)?);
        Ok(Sp::new(CandleExpr::Ifx { cond, yes, no }, span))
    }
}

impl TranslateExpr for lus::expr::OrExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter();
        let mut or = it.next().expect("OrExpr should have at least one member").translate(blocks, stmts, depth, shadow_glob)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth, shadow_glob)?;
            let span = e.span.join(or.span).expect("Faulty span");
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter();
        let mut or = it.next().expect("AndExpr should have at least one member").translate(blocks, stmts, depth, shadow_glob)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth, shadow_glob)?;
            let span = e.span.join(or.span).expect("Faulty span");
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let inner = Box::new(self.inner.translate(blocks, stmts, depth, shadow_glob)?);
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
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
                return first.translate(blocks, stmts, depth, shadow_glob);
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
        let lhs = Box::new(lhs.translate(blocks, stmts, depth, shadow_glob)?);
        let rhs = Box::new(rhs.translate(blocks, stmts, depth, shadow_glob)?);
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (extra_depth, fby) = it.next().expect("FbyExpr should have at least one member");
        let mut fby = fby.translate(blocks, stmts, depth + extra_depth, shadow_glob)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth + d, shadow_glob)?;
            let span = fby.span.join(e.span).expect("Faulty span");
            fby = Sp::new(CandleExpr::Later {
                clk: candle::clock::Depth {
                    dt: depth + d,
                    span,
                },
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        self.inner.translate(blocks, stmts, depth + 1, shadow_glob)
    }
}

impl TranslateExpr for lus::expr::ThenExpr {
    fn translate(
        self,
        blocks: &mut BlockNames,
        stmts: &mut StmtList,
        depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let mut it = self.items.into_iter().enumerate().rev();
        let (_, then) = it.next().expect("ThenExpr should have at least one member");
        let mut then = then.translate(blocks, stmts, depth, shadow_glob)?;
        for (d, e) in it {
            let e = e.translate(blocks, stmts, depth, shadow_glob)?;
            let span = then.span.join(e.span).expect("Faulty span");
            then = Sp::new(CandleExpr::Later {
                clk: candle::clock::Depth {
                    span,
                    dt: depth + d,
                },
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("AddExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.translate(blocks, stmts, depth, shadow_glob);
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        use syn::punctuated::Pair;
        assert!(!self.items.trailing_punct());

        let mut its = self.items.into_pairs();
        let mut lhs;
        let mut op;
        match its.next().expect("MulExpr should have at least one member") {
            Pair::Punctuated(e, o) => {
                lhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                op = o.translate()?;
            }
            Pair::End(e) => {
                return e.translate(blocks, stmts, depth, shadow_glob);
            }
        }
        for it in its {
            match it {
                Pair::Punctuated(e, o) => {
                    let rhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
                    lhs = Sp::new(CandleExpr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }, span);
                    op = o.translate()?;
                }
                Pair::End(e) => {
                    let rhs = e.translate(blocks, stmts, depth, shadow_glob)?;
                    let span = lhs.span.join(rhs.span).expect("Faulty span");
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let inner = Box::new(self.inner.translate(blocks, stmts, depth, shadow_glob)?);
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let mut es = candle::Tuple::default();
        for e in self.inner.into_iter() {
            es.elems.push(e.translate(blocks, stmts, depth, shadow_glob)?);
        }
        match es.elems.len() {
            0 => {
                Err(quote_spanned! {span=>
                    compile_error!("Tuple should have at least one element");
                })
            }
            1 => Ok(es.elems.pop().expect("ParenExpr cannot be empty")),
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
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let args_span = self._paren.input_span();
        let mut es = candle::Tuple::default();
        for e in self.args.into_iter() {
            es.elems.push(e.translate(blocks, stmts, depth, shadow_glob)?);
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
                        es.elems.pop().expect("Float argument count failure"),
                    )), span)), span))
                }
            }
            &_ => {
                let id = Sp::new(candle::expr::NodeId { id: blocks.len() }, span);
                blocks.push(Sp::new(candle::decl::NodeName(s), span));
                let st = Sp::new(candle::stmt::Statement::Substep {
                    clk: candle::clock::Depth {
                        dt: depth,
                        span,
                    },
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
        _blocks: &mut BlockNames,
        _stmts: &mut StmtList,
        depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        let name = self.name.to_string();
        if shadow_glob.contains(&name) {
            Ok(Sp::new(CandleExpr::Reference(Sp::new(candle::expr::Reference::Var(
                Sp::new(candle::expr::ClockVar {
                    var: Sp::new(
                             candle::expr::Var {
                                name,
                             }, span),
                    depth: candle::clock::Depth {
                        span,
                        dt: depth,
                    },
                }, span),
            ), span)), span))
        } else {
            Ok(Sp::new(CandleExpr::Reference(Sp::new(candle::expr::Reference::Global(
                    Sp::new(
                             candle::expr::Var {
                                name: self.name.to_string(),
                             }, span),
            ), span)), span))

        }
    }
}

impl TranslateExpr for lus::expr::LitExpr {
    fn translate(
        self,
        _blocks: &mut BlockNames,
        _stmts: &mut StmtList,
        _depth: usize,
        shadow_glob: &HashSet<String>,
    ) -> TrResult<Sp<CandleExpr>> {
        let span = self.input_span();
        use syn::Lit;
        let lit = match self.lit {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().expect("Failed to parse Int")),
            Lit::Float(f) => candle::expr::Lit::Float(f.base10_parse().expect("Failed to parse Float")),
            _ => {
                return Err(quote_spanned! {self.input_span()=>
                    compile_error!("Lustre only accepts bool/int/float literals");
                })
            }
        };
        Ok(Sp::new(CandleExpr::Lit(Sp::new(lit, span)), span))
    }
}
