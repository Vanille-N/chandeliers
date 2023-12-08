//! Type Checking of a Candle AST

use std::collections::HashMap;

use chandeliers_err::{self as err, EAccum, Transparent};

use crate::ast::{self, ty, var, Tuple};
use crate::sp::{Sp, Span, WithDefSite, WithSpan};

use ast::options::usage::Typecheck as This;

/// This is the type of all interfaces (node inputs and outputs),
/// lets' give it a less confusing alias.
type SpTyBaseTuple = Sp<Tuple<Sp<ty::Base>>>;

type GenericParams = Vec<Sp<String>>;
type GenericInstances = Vec<Sp<ty::Base>>;

/// Context that the typechecking is done in.
#[derive(Debug)]
struct TyCtx<'i> {
    /// Global variables with their types (all scalar).
    global: &'i HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<var::Local, WithDefSite<Sp<ty::Base>>>,
    /// Known input and output types of nodes.
    /// Notice how these maps use `Node`s, not `NodeName`s:
    /// at the level at which typechecking on expressions is done, we have
    /// forgotten the name that blocks bind to and we only know their unique
    /// identifier.
    nodes_in: HashMap<var::Node, SpTyBaseTuple>,
    /// Outputs, same as the inputs above.
    nodes_out: HashMap<var::Node, SpTyBaseTuple>,
    nodes_generics: HashMap<var::Node, (GenericParams, Option<GenericInstances>)>,
}

impl<'i> TyCtx<'i> {
    /// Construct a fresh context with known global variables but no
    /// locals or blocks.
    fn from_ext(global: &'i HashMap<var::Global, WithDefSite<Sp<ty::Base>>>) -> TyCtx<'i> {
        Self {
            global,
            vars: HashMap::default(),
            nodes_in: HashMap::default(),
            nodes_out: HashMap::default(),
            nodes_generics: HashMap::default(),
        }
    }
}

impl TyCtx<'_> {
    /// Interpret a variable as a local variable and get its type if it exists.
    fn get_var(
        &self,
        eaccum: &mut EAccum,
        var: Sp<&var::Local>,
    ) -> Option<WithDefSite<Sp<ty::Tuple>>> {
        match self.vars.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_sp_ref()
                    .sp_map(|span, ty| ty::Tuple::Single(ty.clone().with_span(span))),
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
    ) -> Option<WithDefSite<Sp<ty::Tuple>>> {
        match self.vars.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_sp_ref()
                    .sp_map(|span, ty| ty::Tuple::Single(ty.clone().with_span(span))),
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
    ) -> Option<WithDefSite<Sp<ty::Tuple>>> {
        match self.global.get(var.t) {
            Some(vardef) => Some(
                vardef
                    .as_sp_ref()
                    .sp_map(|span, ty| ty::Tuple::Single(ty.clone().with_span(span))),
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

    fn get_node_generics(&self, node: Sp<&var::Node>) -> GenericParams {
        match self.nodes_generics.get(node.t) {
            Some(tup) => tup.0.clone(),
            None => {
                err::abort!("The typing context is improperly initialized: either {node} is missing and it should have been caught during the causality check, or it was not added to the map.");
            }
        }
    }

    fn instantiate(&mut self, node: Sp<&var::Node>, unifier: &TyUnifier) {
        let gen = self.nodes_generics.get_mut(node.t).unwrap();
        match unifier {
            TyUnifier::Mapping { idx: _, subst } => {
                gen.1 = Some(subst.iter().map(|v| v.clone().unwrap()).collect());
            }
            _ => {
                gen.1 = Some(vec![]);
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
    fn typecheck(&mut self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<()>;
}

/// Helper trait for `Sp<T>` to project `TypeCheckStmt` to its contents.
trait TypeCheckSpanStmt {
    /// Verify that the inner content is consistent.
    /// # Errors
    /// Fails if one of the elements of the statement cannot be typed.
    fn typecheck(&mut self, eaccum: &mut EAccum, ctx: &mut TyCtx) -> Option<Sp<()>>;
}

impl<T: TypeCheckStmt> TypeCheckSpanStmt for Sp<T> {
    fn typecheck(&mut self, eaccum: &mut EAccum, ctx: &mut TyCtx) -> Option<Sp<()>> {
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
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple>;
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
    fn typecheck(&self, eaccum: &mut EAccum, ctx: &mut TyCtx) -> Option<Sp<ty::Tuple>>;
    /// Verify that the inner contents are const computable.
    ///
    /// # Errors
    /// Fails when the expression contains some constructors that are not
    /// valid in const contexts, such as function calls or temporal operators.
    fn is_const(&self, eaccum: &mut EAccum) -> Option<Sp<()>>;
}

impl<T: TypeCheckExpr> TypeCheckSpanExpr for Sp<T> {
    fn typecheck(&self, eaccum: &mut EAccum, ctx: &mut TyCtx) -> Option<Sp<ty::Tuple>> {
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
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple> {
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
    fn typecheck(&mut self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<()> {
        match self {
            Self::Let { target, source } => {
                // Let needs the target to have the same type as the source.
                let target_ty = target.typecheck(eaccum, ctx);
                let source_ty = source.typecheck(eaccum, ctx);
                Some(target_ty?.identical(eaccum, &mut TyUnifier::Identity, &source_ty?, span)?)
            }
            Self::Assert(e) => {
                // Assert requires exactly one bool.
                let t = e.typecheck(eaccum, ctx)?.is_primitive(eaccum)?;
                t.as_ref().is_bool(eaccum, "The argument of assert", span)?;
                Some(())
            }
        }
    }
}

/// Most Expr cases are exactly recursing into all Expr fields
/// and checking that they are identical or in some other way compatible.
impl TypeCheckExpr for ast::expr::Expr {
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple> {
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
                op.accepts(eaccum, left.as_ref(), right?.as_ref())?;
                Some(ty::Tuple::Single(left))
            }
            Self::Un { op, inner } => {
                let inner = inner.typecheck(eaccum, ctx)?.is_primitive(eaccum)?;
                op.accepts(eaccum, span, inner.as_ref())?;
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
                left.identical(eaccum, &mut TyUnifier::Identity, &right?, span)?;
                Some(left.t)
            }
            Self::Ifx { cond, yes, no } => {
                let cond = cond
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let yes = yes.typecheck(eaccum, ctx);
                let no = no.typecheck(eaccum, ctx);
                cond?
                    .as_ref()
                    .is_bool(eaccum, "The condition of if", span)?;
                let yes = yes?;
                yes.identical(eaccum, &mut TyUnifier::Identity, &no?, span)?;
                Some(yes.t)
            }
            Self::Substep { delay: _, id, args } => {
                let expected_tys = ctx.get_node_in(id.as_ref());
                let generics = ctx.get_node_generics(id.as_ref());
                let actual_tys = args.typecheck(eaccum, ctx)?;
                let mut unifier = TyUnifier::from_generics(eaccum, generics)?;
                expected_tys.identical(eaccum, &mut unifier, &actual_tys, span)?;
                ctx.instantiate(id.as_ref(), &unifier);
                Some(ctx.get_node_out(id.as_ref()).t.substitute(&unifier))
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
                activate?.as_ref().is_bool(eaccum, "A clock", span)?;
                Some(inner.t)
            }
            Self::Merge { switch, on, off } => {
                let switch = switch.typecheck(eaccum, ctx)?;
                let on = on.typecheck(eaccum, ctx)?;
                let off = off.typecheck(eaccum, ctx)?;
                let switch = switch.is_primitive(eaccum)?;
                switch.as_ref().is_bool(eaccum, "A clock", span)?;
                on.identical(eaccum, &mut TyUnifier::Identity, &off, span)?;
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
    fn typecheck(&self, _eaccum: &mut EAccum, span: Span, _ctx: &mut TyCtx) -> Option<ty::Tuple> {
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
    fn typecheck(&self, eaccum: &mut EAccum, _span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple> {
        Some(match self {
            Self::Var(v) => ctx.get_var(eaccum, v.as_ref().map(|_, v| &v.var.t))?.data.t,
            Self::Global(v) => ctx.get_global(eaccum, v.as_ref())?.data.t,
        })
    }

    fn is_const(&self, _eaccum: &mut EAccum, _span: Span) -> Option<()> {
        Some(())
    }
}

impl TypeCheckExpr for ast::stmt::VarTuple {
    fn typecheck(&self, eaccum: &mut EAccum, _span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple> {
        use ast::stmt::VarTuple;
        /// Recursion helper: applies `typecheck` to every element of the tuple.
        fn aux_multiple(
            eaccum: &mut EAccum,
            span: Span,
            vs: &Tuple<Sp<VarTuple>>,
            ctx: &mut TyCtx,
        ) -> Option<ty::Tuple> {
            let ts = vs.try_map(&mut *eaccum, |eaccum, v: &Sp<VarTuple>| {
                v.typecheck(eaccum, ctx)
            })?;
            Some(ty::Tuple::Multiple(ts.with_span(span)))
        }
        match self {
            VarTuple::Single(v) => Some(ctx.get_var(eaccum, v.as_ref())?.data.t),
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
    fn accepts(self, eaccum: &mut EAccum, left: Sp<&ty::Base>, right: Sp<&ty::Base>) -> Option<()> {
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
                    expect: "type int or bool, found float",
                    left: &left,
                    right: &right,
                })
            }
            (Bin::Add | Bin::Mul | Bin::Sub | Bin::Div, ty::Base::Int | ty::Base::Float) => Some(()),
            (Bin::Rem, ty::Base::Int) => Some(()),
            (Bin::BitAnd | Bin::BitOr | Bin::BitXor, ty::Base::Int | ty::Base::Bool) => Some(()),
            (_, ty::Base::Other(t)) => eaccum.error(err::BinopMismatch {
                    oper: self,
                    site: span,
                    expect: format!("a concrete type, found type variable {t}"),
                    left: &left,
                    right: &right,
                })

        }
    }
}

impl ast::op::Un {
    /// Determines if the unary operator can be applied to this argument.
    fn accepts(self, eaccum: &mut EAccum, span: Span, inner: Sp<&ty::Base>) -> Option<()> {
        use ast::op::Un;
        match (self, &inner.t) {
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
            (Un::Neg, ty::Base::Int | ty::Base::Float) => Some(()),
            (Un::Not, ty::Base::Int | ty::Base::Bool) => Some(()),
            (_, ty::Base::Other(t)) => eaccum.error(err::UnopMismatch {
                oper: self,
                site: span,
                expect: format!("a concrete type, found type variable {t}"),
                inner: &inner,
            }),

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
        match (self, &left.t) {
            (Cmp::Le | Cmp::Ge | Cmp::Lt | Cmp::Gt, ty::Base::Int | ty::Base::Bool | ty::Base::Float)
                | (Cmp::Ne | Cmp::Eq, ty::Base::Int | ty::Base::Bool) => Some(()),
            (Cmp::Ne | Cmp::Eq, ty::Base::Float) => eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: "type int or bool, not float because equality on float is not reliable",
                left: &left,
                right: &right,
            }),
            (_, ty::Base::Other(t)) => eaccum.error(err::BinopMismatch {
                oper: self,
                site: span,
                expect: format!("a concrete type, found type variable {t}"),
                left: &left,
                right: &right,
            }),

        }
    }
}

impl Sp<&ty::Base> {
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

enum TyUnifier {
    Null,
    Identity,
    Mapping {
        idx: HashMap<Sp<String>, usize>,
        subst: Vec<Option<Sp<ty::Base>>>,
    },
}

impl TyUnifier {
    fn from_generics(_eaccum: &mut EAccum, generics: Vec<Sp<String>>) -> Option<Self> {
        let mut idx = HashMap::new();
        let len = generics.len();
        for (i, gen) in generics.into_iter().enumerate() {
            if idx.insert(gen, i).is_some() {
                err::abort!("Failed to catch duplicate type variable.")
            }
        }
        Some(Self::Mapping {
            idx,
            subst: vec![None; len],
        })
    }
}

impl TyUnifier {
    fn image(&self, s: Sp<String>) -> ty::Base {
        match self {
            Self::Mapping { idx, subst } => {
                if let Some(i) = idx.get(&s) {
                    subst[*i]
                        .clone()
                        .unwrap_or_else(|| err::abort!("Mapping has the wrong length"))
                        .t
                } else {
                    ty::Base::Other(s)
                }
            }
            _ => ty::Base::Other(s),
        }
    }
    fn require_identical(
        &mut self,
        eaccum: &mut EAccum,
        left: &Sp<ty::Base>,
        right: &Sp<ty::Base>,
        source: Span,
    ) -> Option<()> {
        use ty::Base::*;
        match &left.t {
            l @ (Int | Float | Bool) => {
                if l == &right.t {
                    return Some(());
                }
            }
            Other(l) => match self {
                Self::Mapping { idx, subst } => {
                    if let Some(i) = idx.get(&l) {
                        if subst[*i].as_ref() == Some(right) {
                            return Some(());
                        } else if subst[*i] == None {
                            subst[*i] = Some(right.clone());
                            return Some(());
                        } else {
                            eaccum.error(err::TmpBasic {
                                msg: format!("A previous argument requires {} := {}, it cannot also equal {}", &l, subst[*i].as_ref().unwrap(), right),
                                span: right.span,
                            })?;
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => {
                    if left == right {
                        return Some(());
                    }
                }
            },
        }
        let msg = format!("Base types should be unifiable: expected {left}, got {right}");
        eaccum.error(err::TypeMismatch {
            source,
            left,
            right,
            msg,
        })
    }
}

trait TySubstitute {
    fn substitute(self, unifier: &TyUnifier) -> Self;
}

impl TySubstitute for ty::Base {
    fn substitute(self, unifier: &TyUnifier) -> Self {
        match self {
            Self::Other(s) => unifier.image(s),
            _ => self,
        }
    }
}

impl<T: TySubstitute> TySubstitute for ast::Tuple<T> {
    fn substitute(self, unifier: &TyUnifier) -> Self {
        self.map(|t| t.substitute(unifier))
    }
}

impl<T: TySubstitute> TySubstitute for Sp<T> {
    fn substitute(self, unifier: &TyUnifier) -> Self {
        self.map(|_, t| t.substitute(unifier))
    }
}

impl TySubstitute for ty::Tuple {
    fn substitute(self, unifier: &TyUnifier) -> Self {
        match self {
            Self::Single(t) => Self::Single(t.substitute(unifier)),
            Self::Multiple(t) => Self::Multiple(t.substitute(unifier)),
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
    fn identical(
        &self,
        eaccum: &mut EAccum,
        unifier: &mut TyUnifier,
        other: &Self,
        source: Span,
    ) -> Option<()> {
        use ty::Tuple::{Multiple, Single};
        match (&self.t, &other.t) {
            (Single(left), Single(right)) => unifier.require_identical(eaccum, left, right, source),
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
                    scope.compute(|eaccum| t.identical(eaccum, unifier, u, source));
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
                ty::Tuple::Single(t) => Some(t.t.clone()),
                ty::Tuple::Multiple(_) => {
                    let s = "expected a scalar type, got a tuple type".to_owned();
                    eaccum.error(err::TmpBasic {
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
                        .clone()
                        .with_span(span),
                )
            } else {
                ty::Tuple::Multiple(
                    tup.map_ref(|t| {
                        t.clone()
                            .map(|span, t| ty::Tuple::Single(t.with_span(span)))
                    })
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
        extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
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
        extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()>;
    /// Projection to the inner `signature`.
    fn signature(&self) -> Self::Signature;
}

impl<T: TypeCheckDecl> TypeCheckSpanDecl for Sp<T> {
    type Signature = T::Signature;
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
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
    type Signature = (
        Sp<ast::decl::NodeName>,
        GenericParams,
        SpTyBaseTuple,
        SpTyBaseTuple,
    );
    /// Verify inner consistency.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        _span: Span,
        extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // FIXME: prettify
        let is_main = self.options.main.fetch::<This>().is_some();
        let is_test = self.options.test.fetch::<This>().is_some();
        if is_main || is_test {
            if !self.inputs.t.is_empty() {
                eaccum.error(err::TmpBasic {
                    msg: "Node declared as executable (applies to `#[main]` and `#[test]`) should not have any inputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
            if !self.outputs.t.is_empty() {
                eaccum.error(err::TmpBasic {
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
                scope.compute(|eaccum| v.t.ty.t.ty.t.clk.typecheck(eaccum, &mut ctx).map(|_| ()));
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: Transparent::from(Some(v.span)),
                        data: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()),
                    },
                );
            }
        }
        scope.close()?;
        // Then also register the per-var::Node types of the blocks.
        for (id, blk) in self.blocks.iter().enumerate() {
            let Some((gen, i, o)) = extfun.get(&blk.name.t) else {
                let s = format!("Block {} is not defined", &blk.name);
                return eaccum.error(err::TmpBasic {
                    span: blk.name.span,
                    msg: s,
                });
            };
            let id = var::Node {
                id: id.with_span(blk.name.span),
                repr: blk.name.t.repr.clone(),
            };
            ctx.nodes_in.insert(id.clone(), i.clone());
            ctx.nodes_out.insert(id.clone(), o.clone());
            ctx.nodes_generics.insert(id, (gen.clone(), None));
        }
        let mut scope = eaccum.scope();
        for st in &mut self.stmts {
            scope.compute(|eaccum| st.typecheck(eaccum, &mut ctx).map(|_| ()));
        }
        // Finally instanciate the learned generic parameters
        for (id, gens) in ctx.nodes_generics.into_iter() {
            self.blocks[id.id.t].generics = gens.1;
        }
        scope.close()
    }

    /// Fetch the input and output tuple types of this node.
    #[must_use]
    fn signature(
        &self,
    ) -> (
        Sp<ast::decl::NodeName>,
        GenericParams,
        SpTyBaseTuple,
        SpTyBaseTuple,
    ) {
        let generics = self.options.generics.fetch::<This>();
        let inputs = self
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()));
        let outputs = self
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()));
        (
            self.name.clone(),
            generics.clone(),
            inputs.with_span(self.inputs.span),
            outputs.with_span(self.outputs.span),
        )
    }
}

impl TypeCheckDecl for ast::decl::ExtNode {
    type Signature = (
        Sp<ast::decl::NodeName>,
        GenericParams,
        SpTyBaseTuple,
        SpTyBaseTuple,
    );
    /// Same signature as `Node` but we trust its type as there are no contents to check.
    /// We still check that there are no duplicate declarations of variables.
    fn typecheck(
        &mut self,
        eaccum: &mut EAccum,
        _span: Span,
        _extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        _extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        let extvar = HashMap::default();
        let mut ctx = TyCtx::from_ext(&extvar);
        // FIXME: prettify
        if self.options.main.fetch::<This>().is_some() {
            if !self.inputs.t.is_empty() {
                eaccum.error(err::TmpBasic {
                    msg: "Node declared as main should not have any inputs".to_owned(),
                    span: self.inputs.span,
                })?;
            }
            if !self.outputs.t.is_empty() {
                eaccum.error(err::TmpBasic {
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
                scope.compute(|eaccum| v.t.ty.t.ty.t.clk.typecheck(eaccum, &mut ctx).map(|_| ()));
                ctx.vars.insert(
                    v.t.name.t.clone(),
                    WithDefSite {
                        def_site: Transparent::from(Some(v.span)),
                        data: v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()),
                    },
                );
            }
        }
        scope.close()
    }

    /// Get the declared inputs and outputs of this node, assuming that
    /// they have already been checked to be internally consistent.
    #[must_use]
    fn signature(
        &self,
    ) -> (
        Sp<ast::decl::NodeName>,
        GenericParams,
        SpTyBaseTuple,
        SpTyBaseTuple,
    ) {
        let generics = self.options.generics.fetch::<This>();
        let inputs = self
            .inputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()));
        let outputs = self
            .outputs
            .t
            .map_ref(|v| v.t.ty.as_ref().map(|_, t| t.ty.t.inner.t.clone()));
        (
            self.name.clone(),
            generics.clone(),
            inputs.with_span(self.inputs.span),
            outputs.with_span(self.outputs.span),
        )
    }
}

impl TypeCheckExpr for ty::Clock {
    fn typecheck(&self, eaccum: &mut EAccum, span: Span, ctx: &mut TyCtx) -> Option<ty::Tuple> {
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
                let ty = ty.data.is_primitive(eaccum)?;
                ty.as_ref().is_bool(eaccum, "Clock", span)?;
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
        _functx: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        varctx: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        self.value.is_const(eaccum)?;
        let mut unifier = TyUnifier::Null;
        let e = self.value.typecheck(eaccum, &mut TyCtx::from_ext(varctx))?;
        self.ty
            .clone()
            .map(|span, t| ty::Tuple::Single(t.with_span(span)))
            .identical(eaccum, &mut unifier, &e, span)?;
        Some(())
    }

    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<var::Global>, Sp<ty::Base>) {
        (self.name.clone(), self.ty.clone())
    }
}

impl TypeCheckDecl for ast::decl::ExtConst {
    type Signature = (Sp<var::Global>, Sp<ty::Base>);
    fn typecheck(
        &mut self,
        _eaccum: &mut EAccum,
        _span: Span,
        _functx: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        _varctx: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        Some(())
    }

    /// The (name, type) pair that we need to add to the context.
    #[must_use]
    fn signature(&self) -> (Sp<var::Global>, Sp<ty::Base>) {
        (self.name.clone(), self.ty.clone())
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
                                def_site: Transparent::from(Some(name.span)),
                                data: ty,
                            },
                        )
                        .is_some()
                    {
                        let s = format!("Redefinition of const {name}");
                        scope.error(err::TmpBasic {
                            span: c.span,
                            msg: s,
                        });
                    }
                }
                ast::decl::Decl::Node(n) => {
                    scope.compute(|eaccum| n.typecheck(eaccum, &functx, &varctx));
                    let (name, g, i, o) = n.signature();
                    if functx.insert(name.t, (g, i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        scope.error(err::TmpBasic {
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
                                def_site: Transparent::from(Some(name.span)),
                                data: ty,
                            },
                        )
                        .is_some()
                    {
                        let s = format!("Redefinition of const {name}");
                        scope.error(err::TmpBasic {
                            span: c.span,
                            msg: s,
                        });
                    }
                }
                ast::decl::Decl::ExtNode(n) => {
                    scope.compute(|eaccum| n.typecheck(eaccum, &functx, &varctx));
                    let (name, g, i, o) = n.signature();
                    if functx.insert(name.t, (g, i, o)).is_some() {
                        let s = format!("Redefinition of node {}", n.t.name);
                        scope.error(err::TmpBasic {
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
