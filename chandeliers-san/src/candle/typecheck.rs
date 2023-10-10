//! Type Checking of a Candle AST

use std::collections::HashMap;

use crate::candle::ast::{self, Sp};
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

pub type TcResult<T> = Result<T, TokenStream>;

use ast::ty::{TyBase, TyTuple};
use ast::Tuple;

type SpTyBaseTuple = Sp<Tuple<Sp<TyBase>>>;

#[derive(Debug, Default)]
pub struct TyCtx {
    vars: HashMap<ast::expr::Var, Sp<TyBase>>,
    nodes_in: HashMap<ast::expr::NodeId, SpTyBaseTuple>,
    nodes_out: HashMap<ast::expr::NodeId, SpTyBaseTuple>,
}

impl TyCtx {
    fn get_var(&self, var: Sp<&ast::expr::Var>) -> TcResult<Sp<TyTuple>> {
        match self.vars.get(var.t) {
            Some(ty) => Ok(ty.map(|span, ty| TyTuple::Single(Sp::new(ty, span)))),
            None => {
                let s = format!("Variable {var} was not found in the context");
                Err(quote_spanned! {var.span=>
                    compile_error!(#s);
                })
            },
        }
    }

    fn get_node_out(&self, node: Sp<&ast::expr::NodeId>) -> TcResult<Sp<TyTuple>> {
        match self.nodes_out.get(node.t) {
            Some(tup) => Ok(tup.as_flat_tytuple()),
            None => {
                unreachable!("TyCtx is improperly initialized: it does not know {node}");
            },
        }
    }
}

pub trait TypeCheckStmt {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<()>>;
}
pub trait TypeCheckExpr {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>>;
}

impl TypeCheckStmt for Sp<ast::stmt::Statement> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<()>> {
        use ast::stmt::Statement;
        self.as_ref()
            .map(|_, stmt| match stmt {
                Statement::Tick => Ok(()),
                Statement::Let { target, source } => {
                    let target_ty = target.typecheck(ctx)?;
                    let source_ty = source.typecheck(ctx)?;
                    target_ty.identical(&source_ty)
                }
                Statement::Update(_) => Ok(()),
                Statement::Trace { .. } => Ok(()),
                Statement::Assert(e) => {
                    let t = e.typecheck(ctx)?;
                    let t = t.is_primitive()?;
                    t.is_bool()?;
                    Ok(())
                }
                Statement::Substep { clk: _, id, args } => {
                    let Some(expected_tys) = ctx.nodes_out.get(&id.t) else {
                        unreachable!("Substep is malformed: {id} is not a block");
                    };
                    if expected_tys.t.elems.len() != args.t.elems.len() {
                        let s = format!(
                            "Block {} expects {} arguments but {} were given",
                            id,
                            expected_tys.t.elems.len(),
                            args.t.elems.len()
                        );
                        return Err(quote_spanned! {self.span=>
                            compile_error!(#s);//FIXME
                        });
                    }
                    for (i, arg) in args.t.elems.iter().enumerate() {
                        let actual_ty = arg.typecheck(ctx)?;
                        expected_tys
                            .as_ref()
                            .map(|_, tys| TyTuple::Single(tys.elems[i]))
                            .identical(&actual_ty)?;
                    }
                    Ok(())
                }
            })
            .transpose()
    }
}

impl TypeCheckExpr for Sp<ast::expr::Expr> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Expr;
        fn aux(span: Span, expr: &Expr, ctx: &TyCtx) -> TcResult<TyTuple> {
            match expr {
                Expr::Lit(l) => Ok(l.typecheck(ctx)?.t),
                Expr::Reference(r) => Ok(r.typecheck(ctx)?.t),
                Expr::Tuple(es) => {
                    es.as_ref()
                        .map(|span, es| {
                            let mut ts = Tuple::default();
                            for e in &es.elems {
                                ts.elems.push(e.typecheck(ctx)?);
                            }
                            Ok(TyTuple::Multiple(Sp::new(ts, span)))
                        })
                        .t
                }
                Expr::BinOp { op, lhs, rhs } => {
                    let left = lhs.typecheck(ctx)?;
                    let right = rhs.typecheck(ctx)?;
                    op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                    Ok(left.t)
                }
                Expr::UnOp { op, inner } => {
                    let inner = inner.typecheck(ctx)?;
                    op.accepts(inner.is_primitive()?)?;
                    Ok(inner.t)
                }
                Expr::CmpOp { op, lhs, rhs } => {
                    let left = lhs.typecheck(ctx)?;
                    let right = rhs.typecheck(ctx)?;
                    op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                    Ok(TyTuple::Single(Sp::new(TyBase::Bool, span)))
                }
                Expr::Builtin(f) => Ok(f.typecheck(ctx)?.t),
                Expr::Later {
                    clk: _,
                    before,
                    after,
                } => {
                    let left = before.typecheck(ctx)?;
                    let right = after.typecheck(ctx)?;
                    left.identical(&right)?;
                    Ok(left.t)
                }
                Expr::Ifx { cond, yes, no } => {
                    let cond = cond.typecheck(ctx)?;
                    let yes = yes.typecheck(ctx)?;
                    let no = no.typecheck(ctx)?;
                    let cond = cond.is_primitive()?;
                    cond.is_bool()?;
                    yes.identical(&no)?;
                    Ok(yes.t)
                }
            }
        }
        self.as_ref()
            .map(|span, expr| aux(span, expr, ctx))
            .transpose()
    }
}

impl TypeCheckExpr for Sp<ast::expr::Builtin> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Builtin;
        fn aux(span: Span, builtin: &Builtin, ctx: &TyCtx) -> TcResult<TyTuple> {
            match builtin {
                Builtin::Float(e) => match e.typecheck(ctx)?.t {
                    TyTuple::Single(_) => Ok(TyTuple::Single(Sp::new(TyBase::Float, span))),
                    TyTuple::Multiple(m) => {
                        let s = "Builtin float expects a single argument, not a tuple";
                        Err(quote_spanned! {m.span=>
                            compile_error!(#s);
                        })
                    },
                },
            }
        }
        self.as_ref()
            .map(|span, builtin| aux(span, builtin, ctx))
            .transpose()
    }
}

impl TypeCheckExpr for Sp<ast::expr::Lit> {
    fn typecheck(&self, _ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Lit;
        Ok(self.map(|span, lit| match lit {
            Lit::Int(_) => TyTuple::Single(Sp::new(TyBase::Int, span)),
            Lit::Float(_) => TyTuple::Single(Sp::new(TyBase::Float, span)),
            Lit::Bool(_) => TyTuple::Single(Sp::new(TyBase::Bool, span)),
        }))
    }
}

impl TypeCheckExpr for Sp<ast::expr::Reference> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Reference;
        fn aux(_span: Span, refer: &Reference, ctx: &TyCtx) -> TcResult<TyTuple> {
            Ok(match refer {
                Reference::Var(v) => ctx.get_var(v.as_ref().map(|_, v| &v.var))?.t,
                Reference::Node(n) => ctx.get_node_out(n.as_ref())?.t,
            })
        }
        self.as_ref()
            .map(|span, refer| aux(span, refer, ctx))
            .transpose()
    }
}

impl TypeCheckExpr for Sp<ast::stmt::VarTuple> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::stmt::VarTuple;
        fn aux_multiple(span: Span, vs: &Tuple<Sp<VarTuple>>, ctx: &TyCtx) -> TcResult<TyTuple> {
            let mut ts = Tuple::default();
            for v in vs.elems.iter() {
                ts.elems.push(v.typecheck(ctx)?);
            }
            Ok(TyTuple::Multiple(Sp::new(ts, span)))
        }
        fn aux(_span: Span, vartup: &VarTuple, ctx: &TyCtx) -> TcResult<TyTuple> {
            match vartup {
                VarTuple::Single(v) => Ok(ctx.get_var(v.as_ref())?.t),
                VarTuple::Multiple(vs) => vs.as_ref().map(|span, vs| aux_multiple(span, vs, ctx)).t,
            }
        }

        self.as_ref()
            .map(|span, vartup| aux(span, vartup, ctx))
            .transpose()
    }
}

impl ast::expr::BinOp {
    fn accepts(&self, left: Sp<TyBase>, right: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::BinOp::*;
        use TyBase::*;
        let span = left.span.join(right.span).unwrap();
        if left.t != right.t {
            let s = format!("Binary operator {self} expects arguments of the same type: got {left} and {right}");
            return Err(quote_spanned! {span=>
                compile_error!(#s);
            });
        }
        match (self, left.t) {
            (Add | Mul | Div | Sub | Rem, Bool) => {
                let s = format!("Operator {self} does not expect a bool argument");
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            },
            (Rem, Float) => {
                let s = format!("Operator {self} expects integer arguments: got floats");
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            },
            (BitAnd | BitOr | BitXor, Float) => {
                let s = "Cannot apply logical operators to float";
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            },
            _ => Ok(()),
        }
    }
}

impl ast::expr::UnOp {
    fn accepts(&self, inner: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::UnOp::*;
        use TyBase::*;
        match (self, inner.t) {
            (Neg, Bool) | (Not, Float) => {
                let s = format!("Operator {self} cannot be applied to {inner}");
                Err(quote_spanned! {inner.span=>
                    compile_error!(#s);
                })
            },
            _ => Ok(()),
        }
    }
}

impl ast::expr::CmpOp {
    fn accepts(&self, left: Sp<TyBase>, right: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::CmpOp::*;
        use TyBase::*;
        let span = left.span.join(right.span).unwrap();
        if left.t != right.t {
            let s = format!("Comparison operator {self} expects arguments of the same type: got {left} and {right}");
            return Err(quote_spanned! {span=>
                compile_error!(#s);
            });
        }
        match (self, left.t) {
            (Ne, Float) | (Eq, Float) => {
                let s = "Equality on float is not reliable";
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            },
            _ => Ok(()),
        }
    }
}

impl Sp<TyBase> {
    fn is_bool(self) -> TcResult<()> {
        match self.t {
            TyBase::Bool => Ok(()),
            _ => {
                let s = format!("Expected bool, got {self}");
                Err(quote_spanned! {self.span=>
                    compile_error!(#s);
                })
            }
        }
    }
}

impl Sp<TyTuple> {
    fn identical(&self, other: &Self) -> TcResult<()> {
        use TyTuple::*;
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => {
                if left.t != right.t {
                    let s = format!("Expected {left}, got {right}");
                    Err(quote_spanned! {other.span=>
                        compile_error!(#s);
                    })
                } else {
                    Ok(())
                }
            }
            (Multiple(ts), Multiple(us)) => {
                if ts.t.elems.len() != us.t.elems.len() {
                    let s = format!("Expected {self}, got {other} instead that does not have the same length");
                    return Err(quote_spanned! {other.span=>
                        compile_error!(#s);
                    });
                }
                for (t, u) in ts.t.elems.iter().zip(us.t.elems.iter()) {
                    t.identical(u)?;
                }
                Ok(())
            }
            (Multiple(_), Single(_)) => {
                let s = format!("Expected a tuple {}, got a scalar {}", self, other);
                Err(quote_spanned! {other.span=>
                    compile_error!(#s);
                })
            },
            (Single(_), Multiple(_)) => {
                let s = format!("Expected a scalar {}, got a tuple {}", self, other);
                Err(quote_spanned! {other.span=>
                    compile_error!(#s);
                })
            },
        }
    }

    fn is_primitive(&self) -> TcResult<Sp<TyBase>> {
        use TyTuple::*;
        if let Multiple(ts) = &self.t {
            assert!(ts.t.elems.len() != 1);
        }
        self.as_ref()
            .map(|_, t| match t {
                Single(t) => Ok(t.t),
                Multiple(_) => {
                    let s = "Expected a scalar type, got a tuple type";
                    Err(quote_spanned! {self.span=>
                        compile_error!(#s);
                    })
                },
            })
            .transpose()
    }
}

impl SpTyBaseTuple {
    fn as_flat_tytuple(&self) -> Sp<TyTuple> {
        self.as_ref().map(|span, tup| {
            TyTuple::Multiple(Sp::new(
                Tuple {
                    elems: tup
                        .elems
                        .iter()
                        .map(|t| t.map(|span, t| TyTuple::Single(Sp::new(t, span))))
                        .collect(),
                },
                span,
            ))
        })
    }
}

impl Sp<ast::decl::Node> {
    pub fn typecheck(
        &self,
        ext: HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
    ) -> TcResult<()> {
        let mut ctx = TyCtx::default();
        for vs in &[&self.t.inputs, &self.t.outputs, &self.t.locals] {
            for v in &vs.t.elems {
                if ctx
                    .vars
                    .insert(v.t.name.t.clone(), v.t.ty.as_ref().map(|_, t| t.base.t))
                    .is_some()
                {
                    let s = format!("Variable {} is declared twice in node {}", v.t.name, self.t.name);
                    return Err(quote_spanned! {v.span=>
                        compile_error!(#s);
                    });
                }
            }
        }
        for (id, blk) in self.t.blocks.iter().enumerate() {
            let Some((i, o)) = ext.get(&blk.t) else {
                let s = format!("Block {blk} is not defined");
                return Err(quote_spanned! {blk.span =>
                    compile_error!(#s);//FIXME
                });
            };
            let id = ast::expr::NodeId { id };
            ctx.nodes_in.insert(id, i.clone());
            ctx.nodes_out.insert(id, o.clone());
        }
        for st in &self.t.stmts {
            st.typecheck(&ctx)?;
        }
        Ok(())
    }

    pub fn signature(&self) -> TcResult<(SpTyBaseTuple, SpTyBaseTuple)> {
        let inputs = Tuple {
            elems: self
                .t
                .inputs
                .t
                .elems
                .iter()
                .map(|v| v.t.ty.as_ref().map(|_, t| t.base.t))
                .collect(),
        };
        let outputs = Tuple {
            elems: self
                .t
                .outputs
                .t
                .elems
                .iter()
                .map(|v| v.t.ty.as_ref().map(|_, t| t.base.t))
                .collect(),
        };
        Ok((
            Sp::new(inputs, self.t.inputs.span),
            Sp::new(outputs, self.t.outputs.span),
        ))
    }
}
