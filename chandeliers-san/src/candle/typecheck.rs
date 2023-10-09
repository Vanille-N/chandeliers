//! Type Checking of a Candle AST

use std::collections::HashMap;

use crate::candle::ast;

#[must_use]
#[derive(Debug, Clone, Default)]
pub struct TcError {
    msgs: Vec<String>,
}

impl TcError {
    fn new<T>() -> Result<T, Self> {
        Err(Self::default())
    }

    fn with(mut self, msg: String) -> Self {
        self.msgs.push(msg);
        self
    }
}

pub type TcResult<T> = Result<T, TcError>;

use ast::ty::{TyBase, TyTuple};
use ast::Tuple;

#[derive(Debug, Default)]
pub struct TyCtx {
    vars: HashMap<ast::expr::Var, TyBase>,
    nodes_in: HashMap<ast::expr::NodeId, TyTuple>,
    nodes_out: HashMap<ast::expr::NodeId, TyTuple>,
}
impl TyCtx {
    fn get_var(&self, v: &ast::expr::Var) -> TcResult<TyTuple> {
        match self.vars.get(v) {
            Some(t) => Ok(TyTuple::Single(*t)),
            None => TcError::new()
                .map_err(|e| e.with(format!("Variable {v} was not found in the context"))),
        }
    }

    fn get_node_out(&self, n: &ast::expr::NodeId) -> TcResult<TyTuple> {
        match self.nodes_out.get(n) {
            Some(t) => Ok(t.clone()),
            None => TcError::new().map_err(|e| e.with(format!("Node {n} does not exist"))),
        }
    }
}

pub trait TypeCheckStmt {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<()>;
}
pub trait TypeCheckExpr {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<TyTuple>;
}

impl TypeCheckStmt for ast::stmt::Statement {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<()> {
        match self {
            Self::Tick => Ok(()),
            Self::Let { target, source } => {
                let target_ty = target.typecheck(ctx)?;
                let source_ty = source.typecheck(ctx)?;
                target_ty.identical(&source_ty)
            }
            _ => todo!(),
        }
    }
}

impl TypeCheckExpr for ast::expr::Expr {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Lit(l) => l.typecheck(ctx),
            Self::Reference(r) => r.typecheck(ctx),
            Self::Tuple(es) => {
                let mut ts = Tuple::default();
                for e in &es.elems {
                    ts.elems.push(
                        e.typecheck(ctx).map_err(|e| {
                            e.with(format!("  while trying to recurse into {self}"))
                        })?,
                    );
                }
                Ok(TyTuple::Multiple(ts))
            }
            Self::BinOp { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(left)
            }
            Self::UnOp { op, inner } => {
                let inner = inner.typecheck(ctx)?;
                op.accepts(inner.is_primitive()?)?;
                Ok(inner)
            }
            Self::CmpOp { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(TyTuple::Single(TyBase::Bool))
            }
            Self::Builtin(f) => f.typecheck(ctx),
            Self::Later {
                clk: _,
                before,
                after,
            } => {
                let left = before.typecheck(ctx)?;
                let right = after.typecheck(ctx)?;
                left.identical(&right)?;
                Ok(left)
            }
            Self::Ifx { cond, yes, no } => {
                let cond = cond.typecheck(ctx)?;
                let yes = yes.typecheck(ctx)?;
                let no = no.typecheck(ctx)?;
                let cond = cond.is_primitive()?;
                cond.is_bool()?;
                yes.identical(&no)?;
                Ok(yes)
            }
        }
    }
}

impl TypeCheckExpr for ast::expr::Builtin {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Float(e) => match e.typecheck(ctx)? {
                TyTuple::Single(_) => Ok(TyTuple::Single(TyBase::Float)),
                TyTuple::Multiple(_) => TcError::new().map_err(|e| {
                    e.with(format!(
                        "Builtin float expects a single argument, not a tuple"
                    ))
                }),
            },
        }
    }
}

impl TypeCheckExpr for ast::expr::Lit {
    fn typecheck(&self, _ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Int(_) => Ok(TyTuple::Single(TyBase::Int)),
            Self::Float(_) => Ok(TyTuple::Single(TyBase::Float)),
            Self::Bool(_) => Ok(TyTuple::Single(TyBase::Bool)),
        }
    }
}

impl TypeCheckExpr for ast::expr::Reference {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Var(v) => ctx.get_var(&v.var),
            Self::Node(n) => ctx.get_node_out(n),
        }
    }
}

impl TypeCheckExpr for ast::stmt::VarTuple {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Single(v) => ctx.get_var(v),
            Self::Multiple(vs) => {
                let mut ts = Tuple::default();
                for v in vs.elems.iter() {
                    ts.elems.push(v.typecheck(ctx)?);
                }
                Ok(TyTuple::Multiple(ts))
            }
        }
    }
}

impl ast::expr::BinOp {
    fn accepts(&self, left: TyBase, right: TyBase) -> TcResult<()> {
        use ast::expr::BinOp::*;
        use TyBase::*;
        if left != right {
            TcError::new().map_err(|e| e.with(format!("Binary operator {self} expects arguments of the same type: got {left} and {right}")))?;
        }
        match (self, left) {
            (Add | Mul | Div | Sub | Rem, Bool) => TcError::new()
                .map_err(|e| e.with(format!("Operator {self} does not expect a bool argument"))),
            (Rem, Float) => TcError::new().map_err(|e| {
                e.with(format!(
                    "Operator {self} expects integer arguments: got floats"
                ))
            }),
            (BitAnd | BitOr | BitXor, Float) => TcError::new()
                .map_err(|e| e.with(format!("Cannot apply bitwise operators to float"))),
            _ => Ok(()),
        }
    }
}

impl ast::expr::UnOp {
    fn accepts(&self, inner: TyBase) -> TcResult<()> {
        use ast::expr::UnOp::*;
        use TyBase::*;
        match (self, inner) {
            (Neg, Bool) | (Not, Float) => TcError::new()
                .map_err(|e| e.with(format!("Operator {self} cannot be applied to {inner}"))),
            _ => Ok(()),
        }
    }
}

impl ast::expr::CmpOp {
    fn accepts(&self, left: TyBase, right: TyBase) -> TcResult<()> {
        use ast::expr::CmpOp::*;
        use TyBase::*;
        if left != right {
            TcError::new().map_err(|e| e.with(format!("Comparison operator {self} expects arguments of the same type: got {left} and {right}")))?;
        }
        match (self, left) {
            (Ne, Float) | (Eq, Float) => {
                TcError::new().map_err(|e| e.with(format!("Equality on float is not reliable")))
            }
            _ => Ok(()),
        }
    }
}

impl TyBase {
    fn always(self) -> TcResult<()> {
        Ok(())
    }

    fn is_numeric(self) -> TcResult<()> {
        match self {
            Self::Int | Self::Float => Ok(()),
            Self::Bool => TcError::new().map_err(|e| e.with(format!("bool is not a numeric type"))),
        }
    }

    fn is_bool(self) -> TcResult<()> {
        match self {
            Self::Bool => Ok(()),
            _ => TcError::new().map_err(|e| e.with(format!("Expected bool, got {self}"))),
        }
    }
}

impl TyTuple {
    fn identical(&self, other: &Self) -> TcResult<()> {
        if let Self::Multiple(ts) = self {
            assert!(ts.elems.len() != 1);
        }
        if let Self::Multiple(ts) = self {
            assert!(ts.elems.len() != 1);
        }
        match (self, other) {
            (Self::Single(t), Self::Single(u)) => {
                if t != u {
                    return TcError::new().map_err(|e| e.with(format!("Expected {t}, got {u}")));
                } else {
                    Ok(())
                }
            }
            (Self::Multiple(ts), Self::Multiple(us)) => {
                if ts.elems.len() != us.elems.len() {
                    return TcError::new().map_err(|e| {
                        e.with(format!(
                            "Tuple types {self} and {other} do not have the same length"
                        ))
                    });
                }
                for (t, u) in ts.elems.iter().zip(us.elems.iter()) {
                    t.identical(u).map_err(|e| {
                        e.with(format!("  while trying to match {self} with {other}"))
                    })?;
                }
                Ok(())
            }
            (Self::Multiple(_), Self::Single(_)) => TcError::new()
                .map_err(|e| e.with(format!("Left type is a tuple while right type is a scalar"))),
            (Self::Single(_), Self::Multiple(_)) => TcError::new()
                .map_err(|e| e.with(format!("Left type is a scalar while right type is a tuple"))),
        }
    }

    fn is_primitive(&self) -> TcResult<TyBase> {
        if let Self::Multiple(ts) = self {
            assert!(ts.elems.len() != 1);
        }
        match self {
            Self::Single(t) => Ok(*t),
            Self::Multiple(_) => TcError::new()
                .map_err(|e| e.with(format!("Expected a scalar type, got a tuple type"))),
        }
    }
}

impl ast::decl::Node {
    pub fn typecheck(
        &self,
        ext: HashMap<ast::decl::NodeName, (Tuple<TyBase>, Tuple<TyBase>)>,
    ) -> TcResult<(Tuple<TyBase>, Tuple<TyBase>)> {
        let mut ctx = TyCtx::default();
        for vs in &[self.inputs, self.outputs, self.locals] {
            for v in &vs.elems {
                if ctx.vars.insert(v.name, v.ty.base).is_some() {
                    TcError::new().map_err(|e| {
                        e.with(format!(
                            "Variable {} is declared twice in node {}",
                            v.name, self.name
                        ))
                    })?;
                }
            }
        }
        for (id, blk) in self.blocks.iter().enumerate() {
            let Some((i, o)) = ext.get(blk) else {
                return TcError::new().map_err(|e| {
                    e.with(format!(
                            "Block {} is not defined",
                            blk,
                    ))
                });
            };
            ctx.nodes_in.insert(NodeId { id }, i.as_flat_tytuple());
            ctx.nodes_out.insert(NodeId { id }, o.as_flat_tytuple());
        }
    }
}
