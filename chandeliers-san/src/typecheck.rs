//! Type Checking of a Candle AST

use std::collections::HashMap;

use chandeliers_err::{self as err, EAccum, Transparent};

use crate::ast::{self, ty, var, Tuple};
use crate::sp::{Sp, Span, WithSpan};

use ast::options::usage::Typecheck as This;

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
struct TyCtx<'i> {
    /// Global variables with their types (all scalar).
    global: &'i HashMap<var::Global, WithDefSite<ty::Base>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<var::Local, WithDefSite<ty::Base>>,
    /// Known input and output types of nodes.
    /// Notice how these maps use `Node`s, not `NodeName`s:
    /// at the level at which typechecking on expressions is done, we have
    /// forgotten the name that blocks bind to and we only know their unique
    /// identifier.
    nodes_in: HashMap<var::Node, SpTyBaseTuple>,
    /// Outputs, same as the inputs above.
    nodes_out: HashMap<var::Node, SpTyBaseTuple>,
}

impl<'i> TyCtx<'i> {
    /// Construct a fresh context with known global variables but no
    /// locals or blocks.
    fn from_ext(global: &'i HashMap<var::Global, WithDefSite<ty::Base>>) -> TyCtx<'i> {
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
    fn get_var(&self, eaccum: &mut EAccum, var: Sp<&var::Local>) -> Option<WithDefSite<ty::Tuple>> {
        match self.vars.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_ref()
                    .map(|span, ty| ty::Tuple::Single(ty.with_span(span))),
            ),
            None => eaccum.error(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }),
        }
    }

    /// Try to get the type of a local variable, meant to be used during the
    /// typechecking of types so we don't have access to globals and we haven't
    /// yet declared all the local variables.
    fn get_var_during_ty(
        &self,
        eaccum: &mut EAccum,
        var: Sp<&var::Local>,
    ) -> Option<WithDefSite<ty::Tuple>> {
        match self.vars.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_ref()
                    .map(|span, ty| ty::Tuple::Single(ty.with_span(span))),
            ),
            None => eaccum.error(err::TyVarNotFound {
                var: &var,
                suggest: self.vars.keys(),
            }),
        }
    }

    /// Interpret a variable as a global variable and get its type if it exists.
    fn get_global(
        &self,
        eaccum: &mut EAccum,
        var: Sp<&var::Global>,
    ) -> Option<WithDefSite<ty::Tuple>> {
        match self.global.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_ref()
                    .map(|span, ty| ty::Tuple::Single(ty.with_span(span))),
            ),
            None => eaccum.error(err::VarNotFound {
                var: &var,
                suggest1: self.vars.keys(),
                suggest2: self.global.keys(),
            }),
        }
    }

    /// Get the output tuple of a nade.
    fn get_node_out(&self, node: Sp<&var::Node>) -> Sp<ty::Tuple> {
        match self.nodes_out.get(node.t) {
            Some(tup) => tup.as_flat_tytuple(),
            None => {
                err::abort!("The typing context is improperly initialized: either {node} is missing and it should have been caught during the causality check, or it was not added to the map.");
            }
        }
    }

    /// Get the input tuple of a nade.
    fn get_node_in(&self, node: Sp<&var::Node>) -> Sp<ty::Tuple> {
        match self.nodes_in.get(node.t) {
            Some(tup) => tup.as_flat_tytuple(),
            None => {
                err::abort!("The typing context is improperly initialized: either {node} is missing and it should have been caught during the causality check, or it was not added to the map.");
            }
        }
    }
}

/// Typechecking a statement is only a yes-or-no problem, as statements
/// do not introduce type constraints.
trait TypeCheckStmt {
    /// Verify internal consistency.
    /// # Errors
    /// Fails if one of the elements of the statement cannot be typed.
    fn typecheck(&mut self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<()>;
}

/// Helper trait for `Sp<T>` to project `TypeCheckStmt` to its contents.
trait TypeCheckSpanStmt {
    /// Verify that the inner content is consistent.
    /// # Errors
    /// Fails if one of the elements of the statement cannot be typed.
    fn typecheck(&mut self, eaccum: &mut EAccum, ctx: &TyCtx) -> Option<Sp<()>>;
}

impl<T: TypeCheckStmt> TypeCheckSpanStmt for Sp<T> {
    fn typecheck(&mut self, eaccum: &mut EAccum, ctx: &TyCtx) -> Option<Sp<()>> {
        self.as_ref_mut()
            .map(|span, t| t.typecheck(eaccum, span, ctx))
            .transpose()
    }
}

/// Verify that the expression is internally consistent, and get the type
/// of the resulting value.
trait TypeCheckExpr {
    /// Typechecking expressions is slightly more tricky, because we need to check
    /// not only if the expression is internally consistent, but also we need to
    /// compute its type to check it against the immediate context.
    ///
    /// # Errors
    /// Fails if the expression is not internally consistent
    /// (e.g. operators applied to the wrong type, incompatible left- and right-
    /// hand side, ...)
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<ty::Tuple>;
    /// Verify that the expression is valid as a `const`.
    ///
    /// # Errors
    /// Fails when the expression contains some constructors that are not
    /// valid in const contexts, such as function calls or temporal operators.
    fn is_const(&self, eaccum: &mut EAccum, span: Span) -> Option<()>;
}

/// Helper trait for `Sp<T>` to project `TypeCheckExpr` to its contents.
trait TypeCheckSpanExpr {
    /// Get the inner type.
    ///
    /// # Errors
    /// Fails if the expression is not internally consistent
    /// (e.g. operators applied to the wrong type, incompatible left- and right-
    /// hand side, ...)
    fn typecheck(&self, eaccum: &mut EAccum, ctx: &TyCtx) -> Option<Sp<ty::Tuple>>;
    /// Verify that the inner contents are const computable.
    ///
    /// # Errors
    /// Fails when the expression contains some constructors that are not
    /// valid in const contexts, such as function calls or temporal operators.
    fn is_const(&self, eaccum: &mut EAccum) -> Option<Sp<()>>;
}

impl<T: TypeCheckExpr> TypeCheckSpanExpr for Sp<T> {
    fn typecheck(&self, eaccum: &mut EAccum, ctx: &TyCtx) -> Option<Sp<ty::Tuple>> {
        self.as_ref()
            .map(|span, t| t.typecheck(eaccum, span, ctx))
            .transpose()
    }
    fn is_const(&self, eaccum: &mut EAccum) -> Option<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.is_const(eaccum, span))
            .transpose()
    }
}

impl<T: TypeCheckExpr> TypeCheckExpr for Box<T> {
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<ty::Tuple> {
        self.as_ref().typecheck(eaccum, span, ctx)
    }
    fn is_const(&self, eaccum: &mut EAccum, span: Span) -> Option<()> {
        self.as_ref().is_const(eaccum, span)
    }
}

/// Typecheck a statement.
///
/// Warning: as indicated by the `&mut`, this is not pure,
/// the method may modify the statement in-place to update it with information
/// that was not available at translation time such as output types.
impl TypeCheckStmt for ast::stmt::Statement {
    fn typecheck(&mut self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<()> {
        match self {
            Self::Let { target, source } => {
                // Let needs the target to have the same type as the source.
                let target_ty = target.typecheck(eaccum, ctx);
                let source_ty = source.typecheck(eaccum, ctx);
                Some(target_ty?.identical(eaccum, &source_ty?, span)?)
            }
            Self::Assert(e) => {
                // Assert requires exactly one bool.
                let t = e.typecheck(eaccum, ctx)?.is_primitive(eaccum)?;
                t.is_bool(eaccum, "The argument of assert", span)?;
                Some(())
            }
        }
    }
}

/// Most Expr cases are exactly recursing into all Expr fields
/// and checking that they are identical or in some other way compatible.
impl TypeCheckExpr for ast::expr::Expr {
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<ty::Tuple> {
        match self {
            Self::Lit(l) => Some(l.typecheck(eaccum, ctx)?.t),
            Self::Reference(r) => Some(r.typecheck(eaccum, ctx)?.t),
            Self::Tuple(es) => {
                es.as_ref()
                    .map(|span, es| {
                        let ts = es.try_map(eaccum, |eaccum, e: &Sp<ast::expr::Expr>| {
                            e.typecheck(eaccum, ctx)
                        })?;
                        Some(ty::Tuple::Multiple(ts.with_span(span)))
                    })
                    .t
            }
            Self::DummyPre(e) => Some(e.typecheck(eaccum, ctx)?.t),
            Self::Bin { op, lhs, rhs } => {
                let left = lhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let right = rhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let left = left?;
                op.accepts(eaccum, left, right?)?;
                Some(ty::Tuple::Single(left))
            }
            Self::Un { op, inner } => {
                let inner = inner.typecheck(eaccum, ctx)?.is_primitive(eaccum)?;
                op.accepts(eaccum, span, inner)?;
                Some(ty::Tuple::Single(inner))
            }
            Self::Cmp { op, lhs, rhs } => {
                let left = lhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let right = rhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                op.accepts(eaccum, left?, right?)?;
                Some(ty::Tuple::Single(ty::Base::Bool.with_span(span)))
            }
            Self::Later {
                delay: _,
                before,
                after,
            } => {
                let left = before.typecheck(eaccum, ctx);
                let right = after.typecheck(eaccum, ctx);
                let left = left?;
                left.identical(eaccum, &right?, span)?;
                Some(left.t)
            }
            Self::Ifx { cond, yes, no } => {
                let cond = cond
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let yes = yes.typecheck(eaccum, ctx);
                let no = no.typecheck(eaccum, ctx);
                cond?.is_bool(eaccum, "The condition of if", span)?;
                let yes = yes?;
                yes.identical(eaccum, &no?, span)?;
                Some(yes.t)
            }
            Self::Substep { delay: _, id, args } => {
                let expected_tys = ctx.get_node_in(id.as_ref());
                let actual_tys = args.typecheck(eaccum, ctx)?;
                expected_tys.identical(eaccum, &actual_tys, span)?;
                Some(ctx.get_node_out(id.as_ref()).t)
            }
            Self::Clock {
                op: _,
                inner,
                activate,
            } => {
                let activate = activate
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let inner = inner.typecheck(eaccum, ctx)?;
                activate?.is_bool(eaccum, "A clock", span)?;
                Some(inner.t)
            }
            Self::Merge { switch, on, off } => {
                let switch = switch.typecheck(eaccum, ctx)?;
                let on = on.typecheck(eaccum, ctx)?;
                let off = off.typecheck(eaccum, ctx)?;
                let switch = switch.is_primitive(eaccum)?;
                switch.is_bool(eaccum, "A clock", span)?;
                on.identical(eaccum, &off, span)?;
                Some(on.t)
            }
        }
    }

    fn is_const(&self, eaccum: &mut EAccum, span: Span) -> Option<()> {
        match self {
            Self::Lit(_) | Self::Reference(_) | Self::Tuple(_) => Some(()),
            Self::Bin { lhs, rhs, .. } => {
                let lhs = lhs.is_const(eaccum);
                let rhs = rhs.is_const(eaccum);
                lhs?;
                rhs?;
                Some(())
            }
            Self::Un { inner, .. } => {
                inner.is_const(eaccum)?;
                Some(())
            }
            Self::Cmp { lhs, rhs, .. } => {
                let lhs = lhs.is_const(eaccum);
                let rhs = rhs.is_const(eaccum);
                lhs?;
                rhs?;
                Some(())
            }
            Self::DummyPre(_) => eaccum.error(err::NotConst {
                what: "The temporal operator `pre` is",
                site: span,
            }),
            Self::Later { .. } => eaccum.error(err::NotConst {
                what: "The delay operator (-> / fby) is",
                site: span,
            }),
            Self::Substep { .. } => eaccum.error(err::NotConst {
                what: "Function calls",
                site: span,
            }),
            Self::Ifx { cond, yes, no } => {
                let cond = cond.is_const(eaccum);
                let yes = yes.is_const(eaccum);
                let no = no.is_const(eaccum);
                cond?;
                yes?;
                no?;
                Some(())
            }
            Self::Merge { .. } => eaccum.error(err::NotConst {
                what: "The merge builtin is",
                site: span,
            }),
            Self::Clock { .. } => eaccum.error(err::NotConst {
                what: "Clock operators (when/whenot) are",
                site: span,
            }),
        }
    }
}

/// No surprises here: an Int has type int, a Bool has type bool, and a Float has type float.
impl TypeCheckExpr for ast::expr::Lit {
    fn typecheck(&self, _eaccum: &mut EAccum, span: Span, _ctx: &TyCtx) -> Option<ty::Tuple> {
        Some(ty::Tuple::Single(
            match self {
                Self::Int(_) => ty::Base::Int,
                Self::Float(_) => ty::Base::Float,
                Self::Bool(_) => ty::Base::Bool,
            }
            .with_span(span),
        ))
    }
    fn is_const(&self, _eaccum: &mut EAccum, _span: Span) -> Option<()> {
        Some(())
    }
}

/// Typechecking references involves translation-time heuristics on whether
/// this should be assumed to be a local or a global variable.
/// This is not modifiable after generation and this function will only check
/// for one of the two.
impl TypeCheckExpr for var::Reference {
    fn typecheck(&self, eaccum: &mut EAccum, _span: Span, ctx: &TyCtx) -> Option<ty::Tuple> {
        Some(match self {
            Self::Var(v) => {
                ctx.get_var(eaccum, v.as_ref().map(|_, v| &v.var.t))?
                    .inner
                    .t
            }
            Self::Global(v) => ctx.get_global(eaccum, v.as_ref())?.inner.t,
        })
    }

    fn is_const(&self, _eaccum: &mut EAccum, _span: Span) -> Option<()> {
        Some(())
    }
}

impl TypeCheckExpr for ast::stmt::VarTuple {
    fn typecheck(&self, eaccum: &mut EAccum, _span: Span, ctx: &TyCtx) -> Option<ty::Tuple> {
        use ast::stmt::VarTuple;
        /// Recursion helper: applies `typecheck` to every element of the tuple.
        fn aux_multiple(
            eaccum: &mut EAccum,
            span: Span,
            vs: &Tuple<Sp<VarTuple>>,
            ctx: &TyCtx,
        ) -> Option<ty::Tuple> {
            let ts = vs.try_map(&mut *eaccum, |eaccum, v: &Sp<VarTuple>| {
                v.typecheck(eaccum, ctx)
            })?;
            Some(ty::Tuple::Multiple(ts.with_span(span)))
        }
        match self {
            VarTuple::Single(v) => Some(ctx.get_var(eaccum, v.as_ref())?.inner.t),
            VarTuple::Multiple(vs) => {
                vs.as_ref()
                    .map(|span, vs| aux_multiple(eaccum, span, vs, ctx))
                    .t
            }
        }
    }

    fn is_const(&self, _eaccum: &mut EAccum, _span: Span) -> Option<()> {
        Some(())
    }
}

impl ast::op::Bin {
    /// Determines if the binary operator can be applied to these arguments.
    fn accepts(self, eaccum: &mut EAccum, left: Sp<ty::Base>, right: Sp<ty::Base>) -> Option<()> {
        use ast::op::Bin;
        let span = left
            .span
            .join(right.span)
            .unwrap_or_else(|| err::abort!("Malformed span between {left:?} and {right:?}"));
        if left.t != right.t {
            eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "the same type",
                left: &left,
                right: &right,
            })?;
        }
        match (self, left.t) {
            (Bin::Add | Bin::Mul | Bin::Div | Bin::Sub, ty::Base::Bool) => {
                eaccum.error(err::BinopMismatch {
                    oper: self,
                    site: span,
                    expect: "type int or float, found bool",
                    left: &left,
                    right: &right,
                })
            }
            (Bin::Rem, ty::Base::Bool | ty::Base::Float) => eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: format!("type int, found {left}"),
                left: &left,
                right: &right,
            }),
            (Bin::BitAnd | Bin::BitOr | Bin::BitXor, ty::Base::Float) => {
                eaccum.error(err::BinopMismatch {
                    oper: self,
                    site: span,
                    expect: "type int or bool, found float".to_owned(),
                    left: &left,
                    right: &right,
                })
            }
            _ => Some(()),
        }
    }
}

impl ast::op::Un {
    /// Determines if the unary operator can be applied to this argument.
    fn accepts(self, eaccum: &mut EAccum, span: Span, inner: Sp<ty::Base>) -> Option<()> {
        use ast::op::Un;
        match (self, inner.t) {
            (Un::Neg, ty::Base::Bool) => eaccum.error(err::UnopMismatch {
                oper: self,
                site: span,
                expect: "type int or float, found bool",
                inner: &inner,
            }),
            (Un::Not, ty::Base::Float) => eaccum.error(err::UnopMismatch {
                oper: self,
                site: span,
                expect: "type bool or int, found float",
                inner: &inner,
            }),
            _ => Some(()),
        }
    }
}

impl ast::op::Cmp {
    /// Determines if the comparison operator can be applied to these arguments.
    fn accepts(self, eaccum: &mut EAccum, left: Sp<ty::Base>, right: Sp<ty::Base>) -> Option<()> {
        use ast::op::Cmp;
        let span = left
            .span
            .join(right.span)
            .unwrap_or_else(|| err::abort!("Malformed span between {left:?} and {right:?}"));
        if left.t != right.t {
            eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "the same type",
                left: &left,
                right: &right,
            })?;
        }
        match (self, left.t) {
            (Cmp::Ne | Cmp::Eq, ty::Base::Float) => eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "type int or bool, not float because equality on float is not reliable",
                left: &left,
                right: &right,
            }),
            _ => Some(()),
        }
    }
}

impl Sp<ty::Base> {
    /// Verify that this is a boolean
    fn is_bool(self, eaccum: &mut EAccum, req: &str, span: Span) -> Option<()> {
        match self.t {
            ty::Base::Bool => Some(()),
            _ => eaccum.error(err::BoolRequired {
                actual: req,
                site: span,
                inner: &self,
            }),
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
    fn identical(&self, eaccum: &mut EAccum, other: &Self, source: Span) -> Option<()> {
        use ty::Tuple::{Multiple, Single};
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => {
                if left.t == right.t {
                    Some(())
                } else {
                    let msg =
                        format!("Base types should be identical: expected {left}, got {right}");
                    eaccum.error(err::TypeMismatch {
                        source,
                        left: self,
                        right: other,
                        msg,
                    })
                }
            }
            (Multiple(ts), Multiple(us)) => {
                if ts.t.len() != us.t.len() {
                    let msg = format!(
                        "expected {self}, got {other} instead that does not have the same length"
                    );
                    eaccum.error(err::TypeMismatch {
                        source,
                        left: self,
                        right: other,
                        msg,
                    })?;
                }
                let mut scope = eaccum.scope();
                for (t, u) in ts.t.iter().zip(us.t.iter()) {
                    scope.compute(|eaccum| t.identical(eaccum, u, source));
                }
                scope.close()
            }
            (Multiple(_), Single(_)) => {
                let msg = format!("expected a tuple {self}, got a scalar {other}");
                eaccum.error(err::TypeMismatch {
                    source,
                    left: self,
                    right: other,
                    msg,
                })
            }
            (Single(_), Multiple(_)) => {
                let msg = format!("expected a scalar {self}, got a tuple {other}");
                eaccum.error(err::TypeMismatch {
                    source,
                    left: self,
                    right: other,
                    msg,
                })
            }
        }
    }

    /// Whether this type is a `Single`.
    /// This function *does not* identify `(T,)` with `T`, the first will raise
    /// an error. If your language is such that `(T,)` is a valid scalar,
    /// you should compress `Multiple`s of size 1 earlier in the AST generation.
    fn is_primitive(&self, eaccum: &mut EAccum) -> Option<Sp<ty::Base>> {
        self.as_ref()
            .map(|_, t| match t {
                ty::Tuple::Single(t) => Some(t.t),
                ty::Tuple::Multiple(_) => {
                    let s = "expected a scalar type, got a tuple type".to_owned();
                    eaccum.error(err::Basic {
                        span: self.span,
                        msg: s,
                    })
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
                ty::Tuple::Single(
                    tup.iter()
                        .last()
                        .unwrap_or_else(|| err::malformed!())
                        .t
                        .with_span(span),
                )
            } else {
                ty::Tuple::Multiple(
                    tup.map_ref(|t| t.map(|span, t| ty::Tuple::Single(t.with_span(span))))
                        .with_span(span),
                )
            }
        })
    }
}

/// Typing interface for toplevel declarations (nodes and constants).
trait TypeCheckDecl {
    /// Public interface of the object.
    type Signature;

    /// Verify that the object is internally consistent.
    ///
    /// May depend on `extfun` and `extvar` the contexts of typed function and global
    /// names respectively.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        span: Span,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()>;

    /// Extract the public interface of the item, typically its name and input/output types
    /// when relevant.
    fn signature(&self) -> Self::Signature;
}

/// Helper trait for `Sp<T>` to implement `TypeCheckDecl`.
trait TypeCheckSpanDecl {
    /// Projection to the inner `Signature`.
    type Signature;
    /// Projection to the inner `typecheck` with an added span available.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()>;
    /// Projection to the inner `signature`.
    fn signature(&self) -> Self::Signature;
}

impl<T: TypeCheckDecl> TypeCheckSpanDecl for Sp<T> {
    type Signature = T::Signature;
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()> {
        self.t.typecheck(eaccum, self.span, extfun, extvar)
    }
    fn signature(&self) -> Self::Signature {
        self.t.signature()
    }
}

/// Typechecking a node involves first building the context that it makes available
/// to its statements, and then checking those.
impl TypeCheckDecl for ast::decl::Node {
    type Signature = (Sp<ast::decl::NodeName>, SpTyBaseTuple, SpTyBaseTuple);
    /// Verify inner consistency.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        _span: Span,
        extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // FIXME: prettify
        let is_main = self.options.main.fetch::<This>().is_some();
        let is_test = self.options.test.fetch::<This>().is_some();
        if is_main || is_test {
            if !self.inputs.t.is_empty() {
                eaccum.error(err::Basic {
                    msg: "Node declared as executable (applies to `#[main]` and `#[test]`) should not have any inputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
            if !self.outputs.t.is_empty() {
                eaccum.error(err::Basic {
                    msg: "Node declared as executable (applies to `#[main]` and `#[test]`) should not have any outputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
        }

        // These are all the extra variables that we provide in addition
        // to `extvar`.
        let mut scope = eaccum.scope();
        for vs in &[&self.inputs, &self.outputs, &self.locals] {
            for v in vs.t.iter() {
                if let Some(prior) = ctx.vars.get(&v.t.name.t) {
                    scope.error(err::GraphUnitDeclTwice {
                        unit: &v.t.name,
                        prior: "a prior item",
                        new_site: v.span,
                        prior_site: prior.def_site,
                    });
                }
                // Don't forget to typecheck the type.
                scope.compute(|eaccum| v.t.ty.t.ty.t.clk.typecheck(eaccum, &ctx).map(|_| ()));
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: v.span,
                        inner: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t),
                    },
                );
            }
        }
        scope.close()?;
        // Then also register the per-var::Node types of the blocks.
        for (id, blk) in self.blocks.iter().enumerate() {
            let Some((i, o)) = extfun.get(&blk.t) else {
                let s = format!("Block {blk} is not defined");
                return eaccum.error(err::Basic {
                    span: blk.span,
                    msg: s,
                });
            };
            let id = var::Node {
                id: id.with_span(blk.span),
                repr: blk.t.repr.clone(),
            };
            ctx.nodes_in.insert(id.clone(), i.clone());
            ctx.nodes_out.insert(id, o.clone());
        }
        let mut scope = eaccum.scope();
        for st in &mut self.stmts {
            scope.compute(|eaccum| st.typecheck(eaccum, &ctx).map(|_| ()));
        }
        scope.close()
    }

    /// Fetch the input and output tuple types of this node.
    #[must_use]
    fn signature(&self) -> (Sp<ast::decl::NodeName>, SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        let outputs = self
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        (
            self.name.clone(),
            inputs.with_span(self.inputs.span),
            outputs.with_span(self.outputs.span),
        )
    }
}

impl TypeCheckDecl for ast::decl::ExtNode {
    type Signature = (Sp<ast::decl::NodeName>, SpTyBaseTuple, SpTyBaseTuple);
    /// Same signature as `Node` but we trust its type as there are no contents to check.
    /// We still check that there are no duplicate declarations of variables.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        _span: Span,
        _extfun: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        _extvar: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()> {
        let extvar = HashMap::default();
        let mut ctx = TyCtx::from_ext(&extvar);
        // FIXME: prettify
        if self.options.main.fetch::<This>().is_some() {
            if !self.inputs.t.is_empty() {
                eaccum.error(err::Basic {
                    msg: "Node declared as main should not have any inputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
            if !self.outputs.t.is_empty() {
                eaccum.error(err::Basic {
                    msg: "Node declared as main should not have any outputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
        }
        // These are all the extra variables that we provide in addition
        // to `extvar`.
        // The order in which we handle them is relevant for the clocks.
        let mut scope = eaccum.scope();
        for vs in &[&self.inputs, &self.outputs] {
            for v in vs.t.iter() {
                if let Some(prior) = ctx.vars.get(&v.t.name.t) {
                    scope.error(err::GraphUnitDeclTwice {
                        unit: &v.t.name,
                        prior: "a prior item",
                        new_site: v.span,
                        prior_site: prior.def_site,
                    });
                }
                // Don't forget to typecheck the type.
                scope.compute(|eaccum| v.t.ty.t.ty.t.clk.typecheck(eaccum, &ctx).map(|_| ()));
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: v.span,
                        inner: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t),
                    },
                );
            }
        }
        scope.close()
    }

    /// Get the declared inputs and outputs of this node, assuming that
    /// they have already been checked to be internally consistent.
    #[must_use]
    fn signature(&self) -> (Sp<ast::decl::NodeName>, SpTyBaseTuple, SpTyBaseTuple) {
        let inputs = self
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        let outputs = self
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t));
        (
            self.name.clone(),
            inputs.with_span(self.inputs.span),
            outputs.with_span(self.outputs.span),
        )
    }
}

impl TypeCheckExpr for ty::Clock {
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &TyCtx) -> Option<ty::Tuple> {
        match self {
            Self::Implicit | Self::Adaptative => {
                Some(ty::Tuple::Multiple(Tuple::default().with_span(span)))
            }
            Self::Explicit { id, .. } => {
                // FIXME: we should not be using the same error message
                // as for normal variables because there are weird interactions.
                // See test `when-self.rs` for example.
                let ty = ctx.get_var_during_ty(
                    eaccum,
                    id.as_ref()
                        .map(|span, repr| var::Local {
                            repr: repr.clone().with_span(span),
                            run_uid: Transparent::forge(err::here!()), // We can forge the `run_uid` since it's not relevant in
                                                                       // the `impl PartialEq` and `impl Hash`.
                        })
                        .as_ref(),
                )?;
                let ty = ty.inner.is_primitive(eaccum)?;
                ty.is_bool(eaccum, "Clock", span)?;
                Some(ty::Tuple::Single(ty))
            }
        }
    }
    fn is_const(&self, _eaccum: &mut EAccum, _: Span) -> Option<()> {
        err::abort!("We shouldn't even attempt to use a clock in a const context")
    }
}

impl TypeCheckDecl for ast::decl::Const {
    type Signature = (Sp<var::Global>, Sp<ty::Base>);
    /// Verify inner consistency.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        span: Span,
        _functx: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        varctx: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()> {
        self.value.is_const(eaccum)?;
        let e = self.value.typecheck(eaccum, &TyCtx::from_ext(varctx))?;
        self.ty
            .map(|span, t| ty::Tuple::Single(t.with_span(span)))
            .identical(eaccum, &e, span)?;
        Some(())
    }

    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<var::Global>, Sp<ty::Base>) {
        (self.name.clone(), self.ty)
    }
}

impl TypeCheckDecl for ast::decl::ExtConst {
    type Signature = (Sp<var::Global>, Sp<ty::Base>);
    fn typecheck(
        &mut self,
        _eaccum: &mut EAccum,
        _span: Span,
        _functx: &HashMap<ast::decl::NodeName, (SpTyBaseTuple, SpTyBaseTuple)>,
        _varctx: &HashMap<var::Global, WithDefSite<ty::Base>>,
    ) -> Option<()> {
        Some(())
    }

    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<var::Global>, Sp<ty::Base>) {
        (self.name.clone(), self.ty)
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
    pub fn typecheck(&mut self, eaccum: &mut EAccum) -> Option<()> {
        let mut varctx = HashMap::new();
        let mut functx = HashMap::new();
        let mut scope = eaccum.scope();
        for decl in &mut self.t.decls {
            match &mut decl.t {
                ast::decl::Decl::Const(c) => {
                    scope.compute(|eaccum| c.typecheck(eaccum, &functx, &varctx));
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
                        scope.error(err::Basic {
                            span: c.span,
                            msg: s,
                        });
                    }
                }
                ast::decl::Decl::Node(n) => {
                    scope.compute(|eaccum| n.typecheck(eaccum, &functx, &varctx));
                    let (name, i, o) = n.signature();
                    if functx.insert(name.t, (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        scope.error(err::Basic {
                            span: n.span,
                            msg: s,
                        });
                    }
                }
                ast::decl::Decl::ExtConst(c) => {
                    scope.compute(|eaccum| c.typecheck(eaccum, &functx, &varctx));
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
                        scope.error(err::Basic {
                            span: c.span,
                            msg: s,
                        });
                    }
                }
                ast::decl::Decl::ExtNode(n) => {
                    scope.compute(|eaccum| n.typecheck(eaccum, &functx, &varctx));
                    let (name, i, o) = n.signature();
                    if functx.insert(name.t, (i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        scope.error(err::Basic {
                            span: n.span,
                            msg: s,
                        });
                    }
                }
            }
        }
        scope.close()
    }
}
