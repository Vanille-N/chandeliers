//! Type Checking of a Candle AST

use std::collections::HashMap;

use crate::ast;
use crate::sp::{Sp, Span, WithSpan};
use chandeliers_err::{self as err, IntoError, Result};

use ast::options::usage;
use ast::ty;
use ast::Tuple;

/// This is the type of all interfaces (node inputs and outputs),
/// lets' give it a less confusing alias.
type SpTyBaseTuple = Sp<Tuple<Sp<ty::Base>>>;

/// Representation of an object that has two relevant `Span`s:
/// one where it is used, and one where it is defined.
#[derive(Debug)]
struct WithDefSite<T> {
    /// Object and its usage site.
    inner: Sp<T>,
    /// Where the object was first defined.
    def_site: Span,
}

impl<T> WithDefSite<T> {
    /// Apply a function to the inner contents.
    fn map<F, U>(self, f: F) -> WithDefSite<U>
    where
        F: FnOnce(Span, T) -> U,
    {
        WithDefSite {
            inner: self.inner.map(f),
            def_site: self.def_site,
        }
    }

    /// Swap `WithDefSite` with a `&` so that `map` can be applied
    /// without consuming the value.
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
    global: &'i HashMap<ast::var::Global, WithDefSite<ty::Base>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<ast::var::Local, WithDefSite<ty::Base>>,
    /// Known input and output types of nodes.
    /// Notice how these maps use `Node`s, not `NodeName`s:
    /// at the level at which typechecking on expressions is done, we have
    /// forgotten the name that blocks bind to and we only know their unique
    /// identifier.
    nodes_in: HashMap<ast::var::Node, SpTyBaseTuple>,
    /// Outputs, same as the inputs above.
    nodes_out: HashMap<ast::var::Node, SpTyBaseTuple>,
}

impl<'i> TyCtx<'i> {
    /// Construct a fresh context with known global variables but no
    /// locals or blocks.
    fn from_ext(global: &'i HashMap<ast::var::Global, WithDefSite<ty::Base>>) -> TyCtx<'i> {
        Self {
            global,
            vars: HashMap::default(),
            nodes_in: HashMap::default(),
            nodes_out: HashMap::default(),
        }
    }
}

impl TyCtx<'_> {
    /// Interpret a variable as a local variable and get its type if it exists.
    fn get_var(&self, var: Sp<&ast::var::Local>) -> Result<WithDefSite<ty::Tuple>> {
        match self.vars.get(var.t) {
            Some(vardef) => Ok(vardef
                .as_ref()
                .map(|span, ty| ty::Tuple::Single(ty.with_span(span)))),
            None => Err(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }
            .into_err()),
        }
    }

    /// Interpret a variable as a global variable and get its type if it exists.
    fn get_global(&self, var: Sp<&ast::var::Global>) -> Result<WithDefSite<ty::Tuple>> {
        match self.global.get(var.t) {
            Some(vardef) => Ok(vardef
                .as_ref()
                .map(|span, ty| ty::Tuple::Single(ty.with_span(span)))),
            None => Err(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }
            .into_err()),
        }
    }

    /// Get the output tuple of a nade.
    fn get_node_out(&self, node: Sp<&ast::var::Node>) -> Sp<ty::Tuple> {
        match self.nodes_out.get(node.t) {
            Some(tup) => tup.as_flat_tytuple(),
            None => {
                unreachable!("TyCtx is improperly initialized: it does not know {node}");
            }
        }
    }
}

/// Typechecking a statement is only a yes-or-no problem, as statements
/// do not introduce type constraints.
pub trait TypeCheckStmt {
    /// Verify internal consistency.
    /// # Errors
    /// Fails if one of the elements of the statement cannot be typed.
    fn typecheck(&mut self, span: Span, ctx: &TyCtx) -> Result<()>;
}

/// Helper trait for `Sp<T>` to project `TypeCheckStmt` to its contents.
pub trait TypeCheckSpanStmt {
    /// Verify that the inner content is consistent.
    /// # Errors
    /// Fails if one of the elements of the statement cannot be typed.
    fn typecheck(&mut self, ctx: &TyCtx) -> Result<Sp<()>>;
}

impl<T: TypeCheckStmt> TypeCheckSpanStmt for Sp<T> {
    fn typecheck(&mut self, ctx: &TyCtx) -> Result<Sp<()>> {
        self.as_ref_mut()
            .map(|span, t| t.typecheck(span, ctx))
            .transpose()
    }
}

/// Verify that the expression is internally consistent, and get the type
/// of the resulting value.
pub trait TypeCheckExpr {
    /// Typechecking expressions is slightly more tricky, because we need to check
    /// not only if the expression is internally consistent, but also we need to
    /// compute its type to check it against the immediate context.
    ///
    /// # Errors
    /// Fails if the expression is not internally consistent
    /// (e.g. operators applied to the wrong type, incompatible left- and right-
    /// hand side, ...)
    fn typecheck(&self, span: Span, ctx: &TyCtx) -> Result<ty::Tuple>;
    /// Verify that the expression is valid as a `const`.
    ///
    /// # Errors
    /// Fails when the expression contains some constructors that are not
    /// valid in const contexts, such as function calls or temporal operators.
    fn is_const(&self, span: Span) -> Result<()>;
}

/// Helper trait for `Sp<T>` to project `TypeCheckExpr` to its contents.
pub trait TypeCheckSpanExpr {
    /// Get the inner type.
    ///
    /// # Errors
    /// Fails if the expression is not internally consistent
    /// (e.g. operators applied to the wrong type, incompatible left- and right-
    /// hand side, ...)
    fn typecheck(&self, ctx: &TyCtx) -> Result<Sp<ty::Tuple>>;
    /// Verify that the inner contents are const computable.
    ///
    /// # Errors
    /// Fails when the expression contains some constructors that are not
    /// valid in const contexts, such as function calls or temporal operators.
    fn is_const(&self) -> Result<Sp<()>>;
}

impl<T: TypeCheckExpr> TypeCheckSpanExpr for Sp<T> {
    fn typecheck(&self, ctx: &TyCtx) -> Result<Sp<ty::Tuple>> {
        self.as_ref()
            .map(|span, t| t.typecheck(span, ctx))
            .transpose()
    }
    fn is_const(&self) -> Result<Sp<()>> {
        self.as_ref().map(|span, t| t.is_const(span)).transpose()
    }
}

impl<T: TypeCheckExpr> TypeCheckExpr for Box<T> {
    fn typecheck(&self, span: Span, ctx: &TyCtx) -> Result<ty::Tuple> {
        self.as_ref().typecheck(span, ctx)
    }
    fn is_const(&self, span: Span) -> Result<()> {
        self.as_ref().is_const(span)
    }
}

/// Typecheck a statement.
///
/// Warning: as indicated by the `&mut`, this is not pure,
/// the method may modify the statement in-place to update it with information
/// that was not available at translation time such as output types.
impl TypeCheckStmt for ast::stmt::Statement {
    fn typecheck(&mut self, span: Span, ctx: &TyCtx) -> Result<()> {
        match self {
            Self::Let { target, source } => {
                // Let needs the target to have the same type as the source.
                let target_ty = target.typecheck(ctx)?;
                let source_ty = source.typecheck(ctx)?;
                Ok(target_ty.identical(&source_ty, span)?)
            }
            Self::Assert(e) => {
                // Assert requires exactly one bool.
                let t = e.typecheck(ctx)?;
                let t = t.is_primitive()?;
                t.is_bool("The argument of assert", span)?;
                Ok(())
            }
        }
    }
}

/// Most Expr cases are exactly recursing into all Expr fields
/// and checking that they are identical or in some other way compatible.
impl TypeCheckExpr for ast::expr::Expr {
    fn typecheck(&self, span: Span, ctx: &TyCtx) -> Result<ty::Tuple> {
        match self {
            Self::Lit(l) => Ok(l.typecheck(ctx)?.t),
            Self::Reference(r) => Ok(r.typecheck(ctx)?.t),
            Self::Tuple(es) => {
                es.as_ref()
                    .map(|span, es| {
                        let ts = es.try_map(|e| e.typecheck(ctx))?;
                        Ok(ty::Tuple::Multiple(ts.with_span(span)))
                    })
                    .t
            }
            Self::Bin { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(left.t)
            }
            Self::Un { op, inner } => {
                let inner = inner.typecheck(ctx)?;
                op.accepts(span, inner.is_primitive()?)?;
                Ok(inner.t)
            }
            Self::Cmp { op, lhs, rhs } => {
                let left = lhs.typecheck(ctx)?;
                let right = rhs.typecheck(ctx)?;
                op.accepts(left.is_primitive()?, right.is_primitive()?)?;
                Ok(ty::Tuple::Single(ty::Base::Bool.with_span(span)))
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
            Self::Substep { clk: _, id, args } => {
                let Some(expected_tys) = ctx.nodes_in.get(&id.t) else {
                    unreachable!("Substep is malformed: {id} is not a block");
                };
                let actual_tys = args.typecheck(ctx)?;
                expected_tys
                    .as_flat_tytuple()
                    .identical(&actual_tys, span)?;
                Ok(ctx.get_node_out(id.as_ref()).t)
            }
            Self::Clock {
                op: _,
                inner,
                activate,
            } => {
                let activate = activate.typecheck(ctx)?;
                let inner = inner.typecheck(ctx)?;
                let activate = activate.is_primitive()?;
                activate.is_bool("A clock", span)?;
                Ok(inner.t)
            }
            Self::Merge { switch, on, off } => {
                let switch = switch.typecheck(ctx)?;
                let on = on.typecheck(ctx)?;
                let off = off.typecheck(ctx)?;
                let switch = switch.is_primitive()?;
                switch.is_bool("A clock", span)?;
                on.identical(&off, span)?;
                Ok(on.t)
            }
        }
    }

    fn is_const(&self, span: Span) -> Result<()> {
        match self {
            Self::Lit(_) | Self::Reference(_) | Self::Tuple(_) => Ok(()),
            Self::Bin { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Self::Un { inner, .. } => {
                inner.is_const()?;
                Ok(())
            }
            Self::Cmp { lhs, rhs, .. } => {
                lhs.is_const()?;
                rhs.is_const()?;
                Ok(())
            }
            Self::Later { .. } => Err(err::NotConst {
                what: "The later operator (-> / fby) is",
                site: span,
            }
            .into_err()),
            Self::Substep { .. } => Err(err::NotConst {
                what: "Function calls",
                site: span,
            }
            .into_err()),
            Self::Ifx { cond, yes, no } => {
                cond.is_const()?;
                yes.is_const()?;
                no.is_const()?;
                Ok(())
            }
            Self::Merge { .. } => Err(err::NotConst {
                what: "The merge builtin is",
                site: span,
            }
            .into_err()),
            Self::Clock { .. } => Err(err::NotConst {
                what: "Clock operators (when/whenot) are",
                site: span,
            }
            .into_err()),
        }
    }
}

/// No surprises here: an Int has type int, a Bool has type bool, and a Float has type float.
impl TypeCheckExpr for ast::expr::Lit {
    fn typecheck(&self, span: Span, _ctx: &TyCtx) -> Result<ty::Tuple> {
        Ok(ty::Tuple::Single(
            match self {
                Self::Int(_) => ty::Base::Int,
                Self::Float(_) => ty::Base::Float,
                Self::Bool(_) => ty::Base::Bool,
            }
            .with_span(span),
        ))
    }
    fn is_const(&self, _span: Span) -> Result<()> {
        Ok(())
    }
}

/// Typechecking references involves translation-time heuristics on whether
/// this should be assumed to be a local or a global variable.
/// This is not modifiable after generation and this function will only check
/// for one of the two.
impl TypeCheckExpr for ast::var::Reference {
    fn typecheck(&self, _span: Span, ctx: &TyCtx) -> Result<ty::Tuple> {
        Ok(match self {
            Self::Var(v) => ctx.get_var(v.as_ref().map(|_, v| &v.var.t))?.inner.t,
            Self::Global(v) => ctx.get_global(v.as_ref())?.inner.t,
        })
    }

    fn is_const(&self, _span: Span) -> Result<()> {
        Ok(())
    }
}

impl TypeCheckExpr for ast::stmt::VarTuple {
    fn typecheck(&self, _span: Span, ctx: &TyCtx) -> Result<ty::Tuple> {
        use ast::stmt::VarTuple;
        /// Recursion helper: applies `typecheck` to every element of the tuple.
        fn aux_multiple(span: Span, vs: &Tuple<Sp<VarTuple>>, ctx: &TyCtx) -> Result<ty::Tuple> {
            let ts = vs.try_map(|v| v.typecheck(ctx))?;
            Ok(ty::Tuple::Multiple(ts.with_span(span)))
        }
        match self {
            VarTuple::Single(v) => Ok(ctx.get_var(v.as_ref())?.inner.t),
            VarTuple::Multiple(vs) => vs.as_ref().map(|span, vs| aux_multiple(span, vs, ctx)).t,
        }
    }

    fn is_const(&self, _span: Span) -> Result<()> {
        Ok(())
    }
}

impl ast::op::Bin {
    /// Determines if the binary operator can be applied to these arguments.
    fn accepts(self, left: Sp<ty::Base>, right: Sp<ty::Base>) -> Result<()> {
        use ast::op::Bin;
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
            (Bin::Add | Bin::Mul | Bin::Div | Bin::Sub, ty::Base::Bool) => {
                Err(err::BinopMismatch {
                    oper: self,
                    site: span,
                    expect: "type int or float, found bool",
                    left: &left,
                    right: &right,
                }
                .into_err())
            }
            (Bin::Rem, ty::Base::Bool | ty::Base::Float) => Err(err::BinopMismatch {
                oper: self,
                site: span,
                expect: format!("type int, found {left}"),
                left: &left,
                right: &right,
            }
            .into_err()),
            (Bin::BitAnd | Bin::BitOr | Bin::BitXor, ty::Base::Float) => Err(err::BinopMismatch {
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

impl ast::op::Un {
    /// Determines if the unary operator can be applied to this argument.
    fn accepts(self, span: Span, inner: Sp<ty::Base>) -> Result<()> {
        use ast::op::Un;
        match (self, inner.t) {
            (Un::Neg, ty::Base::Bool) => Err(err::UnopMismatch {
                oper: self,
                site: span,
                expect: "type int or float, found bool",
                inner: &inner,
            }
            .into_err()),
            (Un::Not, ty::Base::Float) => Err(err::UnopMismatch {
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

impl ast::op::Cmp {
    /// Determines if the comparison operator can be applied to these arguments.
    fn accepts(self, left: Sp<ty::Base>, right: Sp<ty::Base>) -> Result<()> {
        use ast::op::Cmp;
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
            (Cmp::Ne | Cmp::Eq, ty::Base::Float) => Err(err::BinopMismatch {
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

impl Sp<ty::Base> {
    /// Verify that this is a boolean
    fn is_bool(self, req: &str, span: Span) -> Result<()> {
        match self.t {
            ty::Base::Bool => Ok(()),
            _ => Err(err::BoolRequired {
                actual: req,
                site: span,
                inner: &self,
            }
            .into_err()),
        }
    }
}

impl Sp<ty::Tuple> {
    /// Check that two tuple types are identical:
    /// both tuples of the same length, or both scalars.
    ///
    /// This function *does not* identify `(T,)` with `T`: one is a
    /// size-1 `Multiple` while the other is a `Single`. If your language
    /// is such that `(T,)` and `T` are known to be isomorphic, you should
    /// compress `Multiple`s of size 1 earlier in the AST generation.
    fn identical(&self, other: &Self, source: Span) -> Result<()> {
        use ty::Tuple::{Multiple, Single};
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => {
                if left.t == right.t {
                    Ok(())
                } else {
                    let msg =
                        format!("Base types should be identical: expected {left}, got {right}");
                    Err(err::TypeMismatch {
                        source,
                        left: self,
                        right: other,
                        msg,
                    }
                    .into_err())
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
    fn is_primitive(&self) -> Result<Sp<ty::Base>> {
        self.as_ref()
            .map(|_, t| match t {
                ty::Tuple::Single(t) => Ok(t.t),
                ty::Tuple::Multiple(_) => {
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
    /// Reinterpret a `Tuple<ty::Base>` as a non-nested `ty::Tuple`.
    fn as_flat_tytuple(&self) -> Sp<ty::Tuple> {
        self.as_ref().map(|span, tup| {
            if tup.len() == 1 {
                ty::Tuple::Single(tup.iter().last().expect("Length == 1").t.with_span(span))
            } else {
                ty::Tuple::Multiple(
                    tup.map_ref(|t| t.map(|span, t| ty::Tuple::Single(t.with_span(span))))
                        .with_span(span),
                )
            }
        })
    }
}

impl Sp<ast::decl::Node> {
    /// Projection of `typecheck`: verify the internal consistency of a node.
    fn typecheck(
        &mut self,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<ast::var::Global, WithDefSite<ty::Base>>,
    ) -> Result<()> {
        self.t.typecheck(extfun, extvar)
    }

    /// Projection of `signature`: get inputs and outputs of a node.
    fn signature(&self) -> (SpTyBaseTuple, SpTyBaseTuple) {
        self.t.signature()
    }
}

/// Typechecking a node involves first building the context that it makes available
/// to its statements, and then checking those.
impl ast::decl::Node {
    /// Verify inner consistency.
    fn typecheck(
        &mut self,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<ast::var::Global, WithDefSite<ty::Base>>,
    ) -> Result<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // FIXME: prettify
        if self.options.main.fetch::<usage::Typecheck>().is_some() {
            if !self.inputs.t.is_empty() {
                return Err(err::Basic {
                    msg: "Node declared as main should not have any inputs".to_string(),
                    span: self.inputs.span,
                }
                .into_err());
            }
            if !self.outputs.t.is_empty() {
                return Err(err::Basic {
                    msg: "Node declared as main should not have any outputs".to_string(),
                    span: self.inputs.span,
                }
                .into_err());
            }
        }
        // These are all the extra variables that we provide in addition
        // to `extvar`.
        for vs in &[&self.inputs, &self.outputs, &self.locals] {
            for v in vs.t.iter() {
                if let Some(prior) = ctx.vars.get(&v.t.name.t) {
                    return Err(err::GraphUnitDeclTwice {
                        unit: &v.t.name,
                        prior: "a prior item",
                        new_site: v.span,
                        prior_site: prior.def_site,
                    }
                    .into_err());
                }
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: v.span,
                        inner: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t),
                    },
                );
            }
        }
        // Then also register the per-var::Node types of the blocks.
        for (id, blk) in self.blocks.iter().enumerate() {
            let Some((i, o)) = extfun.get(&blk.t) else {
                let s = format!("Block {blk} is not defined");
                return Err(err::Basic {
                    span: blk.span,
                    msg: s,
                }
                .into_err());
            };
            let id = ast::var::Node {
                id: id.with_span(blk.span),
                repr: blk.t.repr.clone(),
            };
            ctx.nodes_in.insert(id.clone(), i.clone());
            ctx.nodes_out.insert(id, o.clone());
        }
        for st in &mut self.stmts {
            st.typecheck(&ctx)?;
        }
        Ok(())
    }

    /// Fetch the input and output tuple types of this node.
    #[must_use]
    pub fn signature(&self) -> (SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        let outputs = self
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        (
            inputs.with_span(self.inputs.span),
            outputs.with_span(self.outputs.span),
        )
    }
}

impl Sp<ast::decl::ExtNode> {
    /// Same signature as `Node` but we trust its type as there are no contents to check.
    /// We still checkt that there are no duplicate declarations of variables.
    fn typecheck(&mut self) -> Result<()> {
        let extvar = HashMap::default();
        let mut ctx = TyCtx::from_ext(&extvar);
        // FIXME: prettify
        if self.t.options.main.fetch::<usage::Typecheck>().is_some() {
            if !self.t.inputs.t.is_empty() {
                return Err(err::Basic {
                    msg: "Node declared as main should not have any inputs".to_string(),
                    span: self.t.inputs.span,
                }
                .into_err());
            }
            if !self.t.outputs.t.is_empty() {
                return Err(err::Basic {
                    msg: "Node declared as main should not have any outputs".to_string(),
                    span: self.t.inputs.span,
                }
                .into_err());
            }
        }
        // These are all the extra variables that we provide in addition
        // to `extvar`.
        for vs in &[&self.t.inputs, &self.t.outputs] {
            for v in vs.t.iter() {
                if let Some(prior) = ctx.vars.get(&v.t.name.t) {
                    return Err(err::GraphUnitDeclTwice {
                        unit: &v.t.name,
                        prior: "a prior item",
                        new_site: v.span,
                        prior_site: prior.def_site,
                    }
                    .into_err());
                }
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: v.span,
                        inner: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t),
                    },
                );
            }
        }
        Ok(())
    }

    /// Get the declared inputs and outputs of this node, assuming that
    /// they have already been checked to be internally consistent.
    #[must_use]
    fn signature(&self) -> (SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .t
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        let outputs = self
            .t
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        (
            inputs.with_span(self.t.inputs.span),
            outputs.with_span(self.t.outputs.span),
        )
    }
}

impl Sp<ast::decl::Const> {
    /// Projection of `typecheck`: verify internal consistency.
    fn typecheck(&self, varctx: &HashMap<ast::var::Global, WithDefSite<ty::Base>>) -> Result<()> {
        self.t.typecheck(self.span, varctx)
    }

    /// Projection of `signature`: get name and type of a global variable.
    fn signature(&self) -> (Sp<ast::var::Global>, Sp<ty::Base>) {
        self.t.signature()
    }
}
impl ast::decl::Const {
    /// Verify inner consistency.
    fn typecheck(
        &self,
        span: Span,
        varctx: &HashMap<ast::var::Global, WithDefSite<ty::Base>>,
    ) -> Result<()> {
        self.value.is_const()?;
        let e = self.value.typecheck(&TyCtx::from_ext(varctx))?;
        self.ty
            .map(|span, t| ty::Tuple::Single(t.with_span(span)))
            .identical(&e, span)?;
        Ok(())
    }

    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<ast::var::Global>, Sp<ty::Base>) {
        (self.name.clone(), self.ty)
    }
}

impl Sp<ast::decl::ExtConst> {
    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<ast::var::Global>, Sp<ty::Base>) {
        (self.t.name.clone(), self.t.ty)
    }
}

impl Sp<ast::decl::Prog> {
    /// Iterate through declarations and iteratively build the context
    /// to check subsequent declarations against.
    ///
    /// # Errors
    /// If any of the internal declarations are not consistent
    /// with their types, will return a typing error to be printed by the compiler.
    /// Will also report duplicate definitions.
    pub fn typecheck(&mut self) -> Result<()> {
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
                        let s = format!("Redefinition of const {name}");
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
                        let s = format!("Redefinition of const {name}");
                        return Err(err::Basic {
                            span: c.span,
                            msg: s,
                        }
                        .into_err());
                    }
                }
                ast::decl::Decl::ExtNode(n) => {
                    n.typecheck()?;
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
