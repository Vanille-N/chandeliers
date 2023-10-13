//! Type Checking of a Candle AST

use std::collections::HashMap;

use crate::ast::{self, Sp};
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

pub type TcResult<T> = Result<T, TokenStream>;

use ast::ty::{TyBase, TyTuple};
use ast::Tuple;

/// This is the type of all interfaces (node inputs and outputs),
/// lets' give it a less confusing alias.
type SpTyBaseTuple = Sp<Tuple<Sp<TyBase>>>;

/// Context that the typechecking is done in.
#[derive(Debug)]
pub struct TyCtx<'i> {
    /// Global variables with their types (all scalar).
    global: &'i HashMap<ast::expr::GlobalVar, Sp<TyBase>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<ast::expr::LocalVar, Sp<TyBase>>,
    /// Known input and output types of nodes.
    /// Notice how these maps use `NodeId`s, not `NodeName`s:
    /// at the level at which typechecking on expressions is done, we have
    /// forgotten the name that blocks bind to and we only know their unique
    /// identifier.
    nodes_in: HashMap<ast::expr::NodeId, SpTyBaseTuple>,
    nodes_out: HashMap<ast::expr::NodeId, SpTyBaseTuple>,
}

/// Construct a fresh context with known global variables but no
/// locals or blocks.
impl<'i> TyCtx<'i> {
    fn from_ext(global: &'i HashMap<ast::expr::GlobalVar, Sp<TyBase>>) -> TyCtx<'i> {
        Self {
            global,
            vars: Default::default(),
            nodes_in: Default::default(),
            nodes_out: Default::default(),
        }
    }
}

impl TyCtx<'_> {
    /// Interpret a variable as a local variable and get its type if it exists.
    fn get_var(&self, var: Sp<&ast::expr::LocalVar>) -> TcResult<Sp<TyTuple>> {
        match self.vars.get(var.t) {
            Some(ty) => Ok(ty.map(|span, ty| TyTuple::Single(Sp::new(ty, span)))),
            None => {
                let s = format!(
                    "Variable {var} was not found in the context (assumed to be a local variable)"
                );
                Err(quote_spanned! {var.span=>
                    compile_error!(#s);
                })
            }
        }
    }

    /// Interpret a variable as a global variable and get its type if it exists.
    fn get_global(&self, var: Sp<&ast::expr::GlobalVar>) -> TcResult<Sp<TyTuple>> {
        match self.global.get(var.t) {
            Some(ty) => Ok(ty.map(|span, ty| TyTuple::Single(Sp::new(ty, span)))),
            None => {
                let s = format!(
                    "Variable {var} was not found in the context (assumed to be a global variable)"
                );
                Err(quote_spanned! {var.span=>
                    compile_error!(#s);
                })
            }
        }
    }

    /// Get the output tuple of a nade.
    fn get_node_out(&self, node: Sp<&ast::expr::NodeId>) -> TcResult<Sp<TyTuple>> {
        match self.nodes_out.get(node.t) {
            Some(tup) => Ok(tup.as_flat_tytuple()),
            None => {
                unreachable!("TyCtx is improperly initialized: it does not know {node}");
            }
        }
    }
}

// FIXME: we probably want the same pattern where Sp has its own trait that
// doesn't require a span, and normal types get a span for free in their argument.

/// Typechecking a statement is only a yes-or-no problem, as statements
/// do not introduce type constraints.
pub trait TypeCheckStmt {
    fn typecheck(&mut self, ctx: &TyCtx) -> TcResult<Sp<()>>;
}

/// Typechecking expressions is slightly more tricky, because we need to check
/// not only if the expression is internally consistent, but also we need to
/// compute its type to check it against the immediate context.
pub trait TypeCheckExpr {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>>;
    /// Also we want to detect as early as possible consrtucts that are
    /// not valid in const contexts.
    fn is_const(&self) -> TcResult<()>;
}

/// Typecheck a statement.
///
/// Warning: as indicated by the `&mut`, this is not pure,
/// the method may modify the statement in-place to update it with information
/// that was not available at translation time such as output types.
impl TypeCheckStmt for Sp<ast::stmt::Statement> {
    fn typecheck(&mut self, ctx: &TyCtx) -> TcResult<Sp<()>> {
        use ast::stmt::Statement;
        match &mut self.t {
            Statement::Let { target, source } => {
                // Let needs the target to have the same type as the source.
                let target_ty = target.typecheck(ctx)?;
                let source_ty = source.typecheck(ctx)?;
                Ok(Sp::new(target_ty.identical(&source_ty)?, self.span))
            }
            Statement::Trace { .. } => Ok(Sp::new((), self.span)),
            Statement::Assert(e) => {
                // Assert requires exactly one bool.
                let t = e.typecheck(ctx)?;
                let t = t.is_primitive()?;
                t.is_bool()?;
                Ok(Sp::new((), self.span))
            }
            Statement::Substep {
                clk: _,
                id,
                args,
                ref mut nbret,
            } => {
                let Some(expected_tys) = ctx.nodes_in.get(&id.t) else {
                    unreachable!("Substep is malformed: {id} is not a block");
                };
                // Here we find out the length of the tuple returned by the
                // node so that we can generate an `else` branch with the
                // correct size.
                let out_len = ctx.nodes_out.get(&id.t).unwrap().t.len();
                nbret.t = Some(out_len);
                if expected_tys.t.len() != args.t.len() {
                    let s = format!(
                        "Block {} expects {} arguments but {} were given",
                        id,
                        expected_tys.t.len(),
                        args.t.len()
                    );
                    return Err(quote_spanned! {self.span=>
                        compile_error!(#s);
                    });
                }
                // Then we check that the arguments actually have the correct
                // type at all positions.
                for (arg, expected) in args.t.iter().zip(expected_tys.t.iter()) {
                    let actual_ty = arg.typecheck(ctx)?;
                    expected
                        .map(|span, t| TyTuple::Single(Sp::new(t, span)))
                        .identical(&actual_ty)?;
                }
                Ok(Sp::new((), self.span))
            }
        }
    }
}

/// Most Expr cases are exactly recursing into all Expr fields
/// and checking that they are identical or in some other way compatible.
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
                            let ts = es.try_map(|e| e.typecheck(ctx))?;
                            assert!(ts.len() != 1, "Should be a var, not a Tuple");
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

    fn is_const(&self) -> TcResult<()> {
        use ast::expr::Expr;
        match &self.t {
            Expr::Lit(_) => Ok(()),
            Expr::Reference(_) => Ok(()),
            Expr::Tuple(_) => Ok(()),
            Expr::BinOp { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Expr::UnOp { inner, .. } => inner.is_const(),
            Expr::Builtin(b) => b.is_const(),
            Expr::CmpOp { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Expr::Later { before, after, .. } => {
                let span = before.span.join(after.span).expect("Faulty span");
                Err(quote_spanned! {span=>
                    compile_error!("Later is not valid in const contexts");
                })
            }
            Expr::Ifx { cond, yes, no } => {
                cond.is_const()?;
                yes.is_const()?;
                no.is_const()?;
                Ok(())
            }
        }
    }
}

/// Builtins have known number and types of arguments,
/// and their types can be more flexible (e.g. `float` is input polymorphic).
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
                    }
                },
            }
        }
        self.as_ref()
            .map(|span, builtin| aux(span, builtin, ctx))
            .transpose()
    }

    fn is_const(&self) -> TcResult<()> {
        Err(quote_spanned! {self.span=>
            compile_error!("Function calls are not valid in const contexts");
        })
    }
}

/// No surprises here: an Int has type int, a Bool has type bool, and a Float has type float.
impl TypeCheckExpr for Sp<ast::expr::Lit> {
    fn typecheck(&self, _ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Lit;
        Ok(self.map(|span, lit| match lit {
            Lit::Int(_) => TyTuple::Single(Sp::new(TyBase::Int, span)),
            Lit::Float(_) => TyTuple::Single(Sp::new(TyBase::Float, span)),
            Lit::Bool(_) => TyTuple::Single(Sp::new(TyBase::Bool, span)),
        }))
    }
    fn is_const(&self) -> TcResult<()> {
        Ok(())
    }
}

/// Typechecking references involves translation-time heuristics on whether
/// this should be assumed to be a local or a global variable.
/// This is not modifiable after generation and this function will only check
/// for one of the two.
impl TypeCheckExpr for Sp<ast::expr::Reference> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::expr::Reference;
        fn aux(_span: Span, refer: &Reference, ctx: &TyCtx) -> TcResult<TyTuple> {
            Ok(match refer {
                Reference::Var(v) => ctx.get_var(v.as_ref().map(|_, v| &v.var.t))?.t,
                Reference::Node(n) => ctx.get_node_out(n.as_ref())?.t,
                Reference::Global(v) => ctx.get_global(v.as_ref())?.t,
            })
        }
        self.as_ref()
            .map(|span, refer| aux(span, refer, ctx))
            .transpose()
    }
    fn is_const(&self) -> TcResult<()> {
        Ok(())
    }
}

impl TypeCheckExpr for Sp<ast::stmt::VarTuple> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        use ast::stmt::VarTuple;
        fn aux_multiple(span: Span, vs: &Tuple<Sp<VarTuple>>, ctx: &TyCtx) -> TcResult<TyTuple> {
            let ts = vs.try_map(|v| v.typecheck(ctx))?;
            assert!(ts.len() != 1, "This should be a Single, not a Multiple");
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

    fn is_const(&self) -> TcResult<()> {
        Ok(())
    }
}

impl ast::expr::BinOp {
    /// Determines if the binary operator can be applied to these arguments.
    fn accepts(&self, left: Sp<TyBase>, right: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::BinOp::*;
        use TyBase::*;
        let span = left.span.join(right.span).unwrap();
        if left.t != right.t {
            let s = format!(
                "Binary operator {self} expects arguments of the same type: got {left} and {right}"
            );
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
            }
            (Rem, Float) => {
                let s = format!("Operator {self} expects integer arguments: got floats");
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            }
            (BitAnd | BitOr | BitXor, Float) => {
                let s = "Cannot apply logical operators to float";
                Err(quote_spanned! {span=>
                    compile_error!(#s);
                })
            }
            _ => Ok(()),
        }
    }
}

impl ast::expr::UnOp {
    /// Determines if the unary operator can be applied to this argument.
    fn accepts(&self, inner: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::UnOp::*;
        use TyBase::*;
        match (self, inner.t) {
            (Neg, Bool) | (Not, Float) => {
                let s = format!("Operator {self} cannot be applied to {inner}");
                Err(quote_spanned! {inner.span=>
                    compile_error!(#s);
                })
            }
            _ => Ok(()),
        }
    }
}

impl ast::expr::CmpOp {
    /// Determines if the comparison operator can be applied to these arguments.
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
            }
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
    /// Check that two tuple types are identical:
    /// both tuples of the same length, or both scalars.
    ///
    /// This function *does not* identify `(T,)` with `T`: one is a
    /// size-1 `Multiple` while the other is a `Single`. If your language
    /// is such that `(T,)` and `T` are known to be isomorphic, you should
    /// compress `Multiple`s of size 1 earlier in the AST generation.
    fn identical(&self, other: &Self) -> TcResult<()> {
        use TyTuple::*;
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => {
                if left.t != right.t {
                    let s = format!("Expected {left}, got {right}");
                    Err(quote_spanned! {other.span=>
                        compile_error!(#s);//FIXME: must show both spans
                    })
                } else {
                    Ok(())
                }
            }
            (Multiple(ts), Multiple(us)) => {
                if ts.t.len() != us.t.len() {
                    let s = format!(
                        "Expected {self}, got {other} instead that does not have the same length"
                    );
                    return Err(quote_spanned! {other.span=>
                        compile_error!(#s);//FIXME: must show both spans
                    });
                }
                for (t, u) in ts.t.iter().zip(us.t.iter()) {
                    t.identical(u)?;
                }
                Ok(())
            }
            (Multiple(_), Single(_)) => {
                let s = format!("Expected a tuple {}, got a scalar {}", self, other);
                Err(quote_spanned! {other.span=>
                    compile_error!(#s);//FIXME: must show both spans
                })
            }
            (Single(_), Multiple(_)) => {
                let s = format!("Expected a scalar {}, got a tuple {}", self, other);
                Err(quote_spanned! {other.span=>
                    compile_error!(#s);//FIXME: must show both spans
                })
            }
        }
    }

    /// Whether this type is a `Single`.
    /// This function *does not* identify `(T,)` with `T`, the first will raise
    /// an error. If your language is such that `(T,)` is a valid scalar,
    /// you should compress `Multiple`s of size 1 earlier in the AST generation.
    fn is_primitive(&self) -> TcResult<Sp<TyBase>> {
        use TyTuple::*;
        self.as_ref()
            .map(|_, t| match t {
                Single(t) => Ok(t.t),
                Multiple(_) => {
                    let s = "Expected a scalar type, got a tuple type";
                    Err(quote_spanned! {self.span=>
                        compile_error!(#s);
                    })
                }
            })
            .transpose()
    }
}

impl SpTyBaseTuple {
    fn as_flat_tytuple(&self) -> Sp<TyTuple> {
        self.as_ref().map(|span, tup| {
            if tup.len() != 1 {
                TyTuple::Multiple(Sp::new(
                    tup.map_ref(|t| t.map(|span, t| TyTuple::Single(Sp::new(t, span)))),
                    span,
                ))
            } else {
                TyTuple::Single(Sp::new(tup.iter().last().expect("Length == 1").t, span))
            }
        })
    }
}

/// Typechecking a node involves first building the context that it makes available
/// to its statements, and then checking those.
impl Sp<ast::decl::Node> {
    pub fn typecheck(
        &mut self,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<ast::expr::GlobalVar, Sp<TyBase>>,
    ) -> TcResult<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // These are all the extra variables that we provide in addition
        // to `extvar`.
        for vs in &[&self.t.inputs, &self.t.outputs, &self.t.locals] {
            for v in vs.t.iter() {
                if ctx
                    .vars
                    .insert(v.t.name.t.clone(), v.t.ty.as_ref().map(|_, t| t.base.t))
                    .is_some()
                {
                    let s = format!(
                        "Variable {} is declared twice in node {}",
                        v.t.name, self.t.name
                    );
                    return Err(quote_spanned! {v.span=>
                        compile_error!(#s);
                    });
                }
            }
        }
        // Then also register the per-NodeId types of the blocks.
        for (id, blk) in self.t.blocks.iter().enumerate() {
            let Some((i, o)) = extfun.get(&blk.t) else {
                let s = format!("Block {blk} is not defined");
                return Err(quote_spanned! {blk.span =>
                    compile_error!(#s);//FIXME
                });
            };
            let id = ast::expr::NodeId {
                id: Sp::new(id, blk.span),
            };
            ctx.nodes_in.insert(id, i.clone());
            ctx.nodes_out.insert(id, o.clone());
        }
        for st in &mut self.t.stmts {
            st.typecheck(&ctx)?;
        }
        Ok(())
    }

    /// Fetch the input and output tuple types of this node.
    pub fn signature(&self) -> (SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .t
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.base.t));
        let outputs = self
            .t
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.base.t));
        (
            Sp::new(inputs, self.t.inputs.span),
            Sp::new(outputs, self.t.outputs.span),
        )
    }
}

/// Same signature as Node but we trust its type as there are no contents to check.
impl Sp<ast::decl::ExtNode> {
    pub fn signature(&self) -> (SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .t
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.base.t));
        let outputs = self
            .t
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.base.t));
        (
            Sp::new(inputs, self.t.inputs.span),
            Sp::new(outputs, self.t.outputs.span),
        )
    }
}

impl Sp<ast::decl::Const> {
    pub fn typecheck(&self, varctx: &HashMap<ast::expr::GlobalVar, Sp<TyBase>>) -> TcResult<()> {
        self.t.value.is_const()?;
        let e = self.t.value.typecheck(&TyCtx::from_ext(varctx))?;
        self.t
            .ty
            .map(|span, t| TyTuple::Single(Sp::new(t, span)))
            .identical(&e)?;
        Ok(())
    }

    /// The (name, type) pair that we need to add to the context.
    pub fn signature(&self) -> (Sp<ast::expr::GlobalVar>, Sp<TyBase>) {
        (self.t.name.clone(), self.t.ty)
    }
}

impl Sp<ast::decl::ExtConst> {
    /// The (name, type) pair that we need to add to the context.
    pub fn signature(&self) -> (Sp<ast::expr::GlobalVar>, Sp<TyBase>) {
        (self.t.name.clone(), self.t.ty)
    }
}

/// Iterate through declarations and iteratively build the context
/// to check subsequent declarations against.
///
/// Will also report duplicate definitions, although most redefinitions
/// should already be caught by the causality check.
impl Sp<ast::decl::Prog> {
    pub fn typecheck(&mut self) -> TcResult<()> {
        let mut varctx = HashMap::new();
        let mut functx = HashMap::new();
        for decl in &mut self.t.decls {
            match &mut decl.t {
                ast::decl::Decl::Const(c) => {
                    c.typecheck(&varctx)?;
                    let (name, ty) = c.signature();
                    if varctx.insert(name.t.clone(), ty).is_some() {
                        let s = format!("Redefinition of const {}", name);
                        return Err(quote_spanned! {c.span=>
                            compile_error!(#s);
                        });
                    }
                }
                ast::decl::Decl::Node(n) => {
                    n.typecheck(&functx, &varctx)?;
                    let (i, o) = n.signature();
                    if functx.insert(n.t.name.t.clone(), (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        return Err(quote_spanned! {n.span=>
                            compile_error!(#s);
                        });
                    }
                }
                ast::decl::Decl::ExtConst(c) => {
                    let (name, ty) = c.signature();
                    if varctx.insert(name.t.clone(), ty).is_some() {
                        let s = format!("Redefinition of const {}", name);
                        return Err(quote_spanned! {c.span=>
                            compile_error!(#s);
                        });
                    }
                }
                ast::decl::Decl::ExtNode(n) => {
                    let (i, o) = n.signature();
                    if functx.insert(n.t.name.t.clone(), (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        return Err(quote_spanned! {n.span=>
                            compile_error!(#s);
                        });
                    }
                }
            }
        }
        Ok(())
    }
}
