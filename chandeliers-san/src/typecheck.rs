//! Type Checking of a Candle AST

use std::collections::HashMap;

use crate::ast::{self, Sp};
use chandeliers_err::{self as err, IntoError};
use proc_macro2::Span;

pub type TcResult<T> = Result<T, err::Error>;

use ast::ty::{TyBase, TyTuple};
use ast::Tuple;

/// This is the type of all interfaces (node inputs and outputs),
/// lets' give it a less confusing alias.
type SpTyBaseTuple = Sp<Tuple<Sp<TyBase>>>;

#[derive(Debug)]
struct WithDefSite<T> {
    inner: Sp<T>,
    def_site: Span,
}

impl<T> WithDefSite<T> {
    fn map<F, U>(self, f: F) -> WithDefSite<U>
    where
        F: FnOnce(Span, T) -> U,
    {
        WithDefSite {
            inner: self.inner.map(f),
            def_site: self.def_site,
        }
    }

    fn as_ref(&self) -> WithDefSite<&T> {
        WithDefSite {
            inner: self.inner.as_ref(),
            def_site: self.def_site,
        }
    }
}

/// Context that the typechecking is done in.
#[derive(Debug)]
pub struct TyCtx<'i> {
    /// Global variables with their types (all scalar).
    global: &'i HashMap<ast::expr::GlobalVar, WithDefSite<TyBase>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<ast::expr::LocalVar, WithDefSite<TyBase>>,
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
    fn from_ext(global: &'i HashMap<ast::expr::GlobalVar, WithDefSite<TyBase>>) -> TyCtx<'i> {
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
    fn get_var(&self, var: Sp<&ast::expr::LocalVar>) -> TcResult<WithDefSite<TyTuple>> {
        match self.vars.get(var.t) {
            Some(vardef) => Ok(vardef
                .as_ref()
                .map(|span, ty| TyTuple::Single(Sp::new(ty.clone(), span)))),
            None => Err(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }
            .into_err()),
        }
    }

    /// Interpret a variable as a global variable and get its type if it exists.
    fn get_global(&self, var: Sp<&ast::expr::GlobalVar>) -> TcResult<WithDefSite<TyTuple>> {
        match self.global.get(var.t) {
            Some(vardef) => Ok(vardef
                .as_ref()
                .map(|span, ty| TyTuple::Single(Sp::new(ty.clone(), span)))),
            None => Err(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }
            .into_err()),
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

/// Typechecking a statement is only a yes-or-no problem, as statements
/// do not introduce type constraints.
pub trait TypeCheckStmt {
    fn typecheck(&mut self, span: Span, ctx: &TyCtx) -> TcResult<()>;
}
pub trait TypeCheckSpanStmt {
    fn typecheck(&mut self, ctx: &TyCtx) -> TcResult<Sp<()>>;
}

impl<T: TypeCheckStmt> TypeCheckSpanStmt for Sp<T> {
    fn typecheck(&mut self, ctx: &TyCtx) -> TcResult<Sp<()>> {
        self.as_ref_mut()
            .map(|span, t| t.typecheck(span, ctx))
            .transpose()
    }
}

/// Typechecking expressions is slightly more tricky, because we need to check
/// not only if the expression is internally consistent, but also we need to
/// compute its type to check it against the immediate context.
pub trait TypeCheckExpr {
    fn typecheck(&self, span: Span, ctx: &TyCtx) -> TcResult<TyTuple>;
    /// Also we want to detect as early as possible consrtucts that are
    /// not valid in const contexts.
    fn is_const(&self, span: Span) -> TcResult<()>;
}
pub trait TypeCheckSpanExpr {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>>;
    fn is_const(&self) -> TcResult<Sp<()>>;
}
impl<T: TypeCheckExpr> TypeCheckSpanExpr for Sp<T> {
    fn typecheck(&self, ctx: &TyCtx) -> TcResult<Sp<TyTuple>> {
        self.as_ref()
            .map(|span, t| t.typecheck(span, ctx))
            .transpose()
    }
    fn is_const(&self) -> TcResult<Sp<()>> {
        self.as_ref().map(|span, t| t.is_const(span)).transpose()
    }
}

/// Typecheck a statement.
///
/// Warning: as indicated by the `&mut`, this is not pure,
/// the method may modify the statement in-place to update it with information
/// that was not available at translation time such as output types.
impl TypeCheckStmt for ast::stmt::Statement {
    fn typecheck(&mut self, span: Span, ctx: &TyCtx) -> TcResult<()> {
        match self {
            Self::Let { target, source } => {
                // Let needs the target to have the same type as the source.
                let target_ty = target.typecheck(ctx)?;
                let source_ty = source.typecheck(ctx)?;
                Ok(target_ty.identical(&source_ty, span)?)
            }
            Self::Trace { .. } => Ok(()),
            Self::Assert(e) => {
                // Assert requires exactly one bool.
                let t = e.typecheck(ctx)?;
                let t = t.is_primitive()?;
                t.is_bool("The argument of assert", span)?;
                Ok(())
            }
            Self::Substep { clk: _, id, args } => {
                let Some(expected_tys) = ctx.nodes_in.get(&id.t) else {
                    unreachable!("Substep is malformed: {id} is not a block");
                };
                let actual_tys = args.typecheck(ctx)?;
                expected_tys
                    .as_flat_tytuple()
                    .identical(&actual_tys, span)?;
                Ok(())
            }
        }
    }
}

/// Most Expr cases are exactly recursing into all Expr fields
/// and checking that they are identical or in some other way compatible.
impl TypeCheckExpr for ast::expr::Expr {
    fn typecheck(&self, span: Span, ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Lit(l) => Ok(l.typecheck(ctx)?.t),
            Self::Reference(r) => Ok(r.typecheck(ctx)?.t),
            Self::Tuple(es) => {
                es.as_ref()
                    .map(|span, es| {
                        let ts = es.try_map(|e| e.typecheck(ctx))?;
                        Ok(TyTuple::Multiple(Sp::new(ts, span)))
                    })
                    .t
            }
            Self::BinOp { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(left.t)
            }
            Self::UnOp { op, inner } => {
                let inner = inner.typecheck(ctx)?;
                op.accepts(span, inner.is_primitive()?)?;
                Ok(inner.t)
            }
            Self::CmpOp { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(TyTuple::Single(Sp::new(TyBase::Bool, span)))
            }
            Self::Later {
                clk: _,
                before,
                after,
            } => {
                let left = before.typecheck(ctx)?;
                let right = after.typecheck(ctx)?;
                left.identical(&right, span)?;
                Ok(left.t)
            }
            Self::Ifx { cond, yes, no } => {
                let cond = cond.typecheck(ctx)?;
                let yes = yes.typecheck(ctx)?;
                let no = no.typecheck(ctx)?;
                let cond = cond.is_primitive()?;
                cond.is_bool("The condition of if", span)?;
                yes.identical(&no, span)?;
                Ok(yes.t)
            }
        }
    }

    fn is_const(&self, _span: Span) -> TcResult<()> {
        match self {
            Self::Lit(_) => Ok(()),
            Self::Reference(_) => Ok(()),
            Self::Tuple(_) => Ok(()),
            Self::BinOp { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Self::UnOp { inner, .. } => Ok(inner.is_const()?.t),
            Self::CmpOp { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Self::Later { before, after, .. } => {
                let span = before.span.join(after.span).expect("Faulty span");
                Err(err::NotConst {
                    what: "The later operator (-> / fby) is",
                    site: span,
                }
                .into_err())
            }
            Self::Ifx { cond, yes, no } => {
                cond.is_const()?;
                yes.is_const()?;
                no.is_const()?;
                Ok(())
            }
        }
    }
}

/// No surprises here: an Int has type int, a Bool has type bool, and a Float has type float.
impl TypeCheckExpr for ast::expr::Lit {
    fn typecheck(&self, span: Span, _ctx: &TyCtx) -> TcResult<TyTuple> {
        match self {
            Self::Int(_) => Ok(TyTuple::Single(Sp::new(TyBase::Int, span))),
            Self::Float(_) => Ok(TyTuple::Single(Sp::new(TyBase::Float, span))),
            Self::Bool(_) => Ok(TyTuple::Single(Sp::new(TyBase::Bool, span))),
        }
    }
    fn is_const(&self, _span: Span) -> TcResult<()> {
        Ok(())
    }
}

/// Typechecking references involves translation-time heuristics on whether
/// this should be assumed to be a local or a global variable.
/// This is not modifiable after generation and this function will only check
/// for one of the two.
impl TypeCheckExpr for ast::expr::Reference {
    fn typecheck(&self, _span: Span, ctx: &TyCtx) -> TcResult<TyTuple> {
        Ok(match self {
            Self::Var(v) => ctx.get_var(v.as_ref().map(|_, v| &v.var.t))?.inner.t,
            Self::Node(n) => ctx.get_node_out(n.as_ref())?.t,
            Self::Global(v) => ctx.get_global(v.as_ref())?.inner.t,
        })
    }

    fn is_const(&self, _span: Span) -> TcResult<()> {
        Ok(())
    }
}

impl TypeCheckExpr for ast::stmt::VarTuple {
    fn typecheck(&self, _span: Span, ctx: &TyCtx) -> TcResult<TyTuple> {
        use ast::stmt::VarTuple;
        fn aux_multiple(span: Span, vs: &Tuple<Sp<VarTuple>>, ctx: &TyCtx) -> TcResult<TyTuple> {
            let ts = vs.try_map(|v| v.typecheck(ctx))?;
            Ok(TyTuple::Multiple(Sp::new(ts, span)))
        }
        match self {
            VarTuple::Single(v) => Ok(ctx.get_var(v.as_ref())?.inner.t),
            VarTuple::Multiple(vs) => vs.as_ref().map(|span, vs| aux_multiple(span, vs, ctx)).t,
        }
    }

    fn is_const(&self, _span: Span) -> TcResult<()> {
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
            return Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "the same type",
                left: &left,
                right: &right,
            }
            .into_err());
        }
        match (self, left.t) {
            (Add | Mul | Div | Sub, Bool) => Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "type int or float, found bool",
                left: &left,
                right: &right,
            }
            .into_err()),
            (Rem, Bool | Float) => Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: format!("type int, found {left}"),
                left: &left,
                right: &right,
            }
            .into_err()),
            (BitAnd | BitOr | BitXor, Float) => Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "type int or bool, found float".to_string(),
                left: &left,
                right: &right,
            }
            .into_err()),
            _ => Ok(()),
        }
    }
}

impl ast::expr::UnOp {
    /// Determines if the unary operator can be applied to this argument.
    fn accepts(&self, span: Span, inner: Sp<TyBase>) -> TcResult<()> {
        use ast::expr::UnOp::*;
        use TyBase::*;
        match (self, inner.t) {
            (Neg, Bool) => Err(err::UnopMismatch {
                oper: self,
                site: span,
                expect: "type int or float, found bool",
                inner: &inner,
            }
            .into_err()),
            (Not, Float) => Err(err::UnopMismatch {
                oper: self,
                site: span,
                expect: "type bool or int, found float",
                inner: &inner,
            }
            .into_err()),
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
            return Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "the same type",
                left: &left,
                right: &right,
            }
            .into_err());
        }
        match (self, left.t) {
            (Ne | Eq, Float) => Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "type int or bool, not float because equality on float is not reliable",
                left: &left,
                right: &right,
            }
            .into_err()),
            _ => Ok(()),
        }
    }
}

impl Sp<TyBase> {
    fn is_bool(self, req: &str, span: Span) -> TcResult<()> {
        match self.t {
            TyBase::Bool => Ok(()),
            _ => Err(err::BoolRequired {
                actual: req,
                site: span,
                inner: &self,
            }
            .into_err()),
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
    fn identical(&self, other: &Self, source: Span) -> Result<(), err::Error> {
        use TyTuple::*;
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => {
                if left.t != right.t {
                    let msg =
                        format!("Base types should be identical: expected {left}, got {right}");
                    Err(err::TypeMismatch {
                        source,
                        left: self,
                        right: other,
                        msg,
                    }
                    .into_err())
                } else {
                    Ok(())
                }
            }
            (Multiple(ts), Multiple(us)) => {
                if ts.t.len() != us.t.len() {
                    let msg = format!(
                        "expected {self}, got {other} instead that does not have the same length"
                    );
                    return Err(err::TypeMismatch {
                        source,
                        left: self,
                        right: other,
                        msg,
                    }
                    .into_err());
                }
                for (t, u) in ts.t.iter().zip(us.t.iter()) {
                    t.identical(u, source)?;
                }
                Ok(())
            }
            (Multiple(_), Single(_)) => {
                let msg = format!("expected a tuple {self}, got a scalar {other}");
                Err(err::TypeMismatch {
                    source,
                    left: self,
                    right: other,
                    msg,
                }
                .into_err())
            }
            (Single(_), Multiple(_)) => {
                let msg = format!("expected a scalar {self}, got a tuple {other}");
                Err(err::TypeMismatch {
                    source,
                    left: self,
                    right: other,
                    msg,
                }
                .into_err())
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
                    let s = "expected a scalar type, got a tuple type".to_string();
                    Err(err::Basic {
                        span: self.span,
                        msg: s,
                    }
                    .into_err())
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
    fn typecheck(
        &mut self,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<ast::expr::GlobalVar, WithDefSite<TyBase>>,
    ) -> TcResult<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // These are all the extra variables that we provide in addition
        // to `extvar`.
        for vs in &[&self.t.inputs, &self.t.outputs, &self.t.locals] {
            for v in vs.t.iter() {
                if let Some(prior) = ctx.vars.get(&v.t.name.t) {
                    return Err(err::GraphUnitDeclTwice {
                        unit: &v.t.name,
                        prior: "a prior item",
                        new_site: v.span,
                        prior_site: prior.def_site,
                    }
                    .into_err());
                } else {
                    ctx.vars.insert(
                        v.t.name.t.clone(),
                        WithDefSite {
                            def_site: v.span,
                            inner: v.t.ty.as_ref().map(|_, t| t.base.t),
                        },
                    );
                }
            }
        }
        // Then also register the per-NodeId types of the blocks.
        for (id, blk) in self.t.blocks.iter().enumerate() {
            let Some((i, o)) = extfun.get(&blk.t) else {
                let s = format!("Block {blk} is not defined");
                return Err(err::Basic {
                    span: blk.span,
                    msg: s,
                }
                .into_err());
            };
            let id = ast::expr::NodeId {
                id: Sp::new(id, blk.span),
                repr: blk.t.repr.clone(),
            };
            ctx.nodes_in.insert(id.clone(), i.clone());
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
    fn typecheck(
        &self,
        varctx: &HashMap<ast::expr::GlobalVar, WithDefSite<TyBase>>,
    ) -> TcResult<()> {
        self.t.value.is_const()?;
        let e = self.t.value.typecheck(&TyCtx::from_ext(varctx))?;
        self.t
            .ty
            .map(|span, t| TyTuple::Single(Sp::new(t, span)))
            .identical(&e, self.span)?;
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
                    if varctx
                        .insert(
                            name.t.clone(),
                            WithDefSite {
                                def_site: name.span,
                                inner: ty,
                            },
                        )
                        .is_some()
                    {
                        let s = format!("Redefinition of const {}", name);
                        return Err(err::Basic {
                            span: c.span,
                            msg: s,
                        }
                        .into_err());
                    }
                }
                ast::decl::Decl::Node(n) => {
                    n.typecheck(&functx, &varctx)?;
                    let (i, o) = n.signature();
                    if functx.insert(n.t.name.t.clone(), (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        return Err(err::Basic {
                            span: n.span,
                            msg: s,
                        }
                        .into_err());
                    }
                }
                ast::decl::Decl::ExtConst(c) => {
                    let (name, ty) = c.signature();
                    if varctx
                        .insert(
                            name.t.clone(),
                            WithDefSite {
                                def_site: name.span,
                                inner: ty,
                            },
                        )
                        .is_some()
                    {
                        let s = format!("Redefinition of const {}", name);
                        return Err(err::Basic {
                            span: c.span,
                            msg: s,
                        }
                        .into_err());
                    }
                }
                ast::decl::Decl::ExtNode(n) => {
                    let (i, o) = n.signature();
                    if functx.insert(n.t.name.t.clone(), (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        return Err(err::Basic {
                            span: n.span,
                            msg: s,
                        }
                        .into_err());
                    }
                }
            }
        }
        Ok(())
    }
}
