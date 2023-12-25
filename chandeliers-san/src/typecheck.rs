//! Type Checking of a Candle AST

use std::collections::HashMap;

use chandeliers_err::{self as err, EAccum, Transparent};

use crate::ast::{self, ty, var, Tuple};
use crate::sp::{Sp, Span, WithDefSite, WithSpan};

use ast::options::usage::Typecheck as This;

/// This is the type of all interfaces (node inputs and outputs),
/// lets' give it a less confusing alias.
type SpTyBaseTuple = Sp<Tuple<Sp<ty::Base>>>;

/// Ordered sequence of names of type variables.
type GenericParams = Vec<Sp<String>>;
/// Ordered sequence of instanciations of type variables.
/// Goes hand in hand with `GenericParams`: you should read
/// a `(k: GenericParams, v: GenericInstances)` as a mapping where the
/// value associated with `k[i]` is `v[i]`.
type GenericInstances = Vec<Sp<ty::Base>>;

/// Immutable accessor for an array.
/// Catches out of bounds with a nice error message.
macro_rules! at {
    ( $arr:expr, $idx:expr ) => {
        $arr.get($idx).unwrap_or_else(|| {
            err::abort!(
                "Index {} out of bounds of array {}.",
                $idx,
                stringify!($arr),
            )
        })
    };
}

/// Mutable accessor for an array.
/// Catches out of bounds with a nice error message.
macro_rules! at_mut {
    ( $arr:expr, $idx:expr ) => {
        $arr.get_mut($idx).unwrap_or_else(|| {
            err::abort!(
                "Index {} out of bounds of array {}.",
                $idx,
                stringify!($arr),
            )
        })
    };
}

/// Context that the typechecking is done in.
#[derive(Debug)]
struct TyCtx<'i> {
    /// Global variables with their types (all scalar).
    global: &'i HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    /// Local variables (both inputs/outputs and hidden locals).
    vars: HashMap<var::Local, WithDefSite<Sp<ty::Base>>>,
    /// Registers storing delayed values.
    registers: HashMap<Sp<var::Register>, Sp<ty::Tuple>>,
    /// Known input and output types of nodes.
    /// Notice how these maps use `Node`s, not `NodeName`s:
    /// at the level at which typechecking on expressions is done, we have
    /// forgotten the name that blocks bind to and we only know their unique
    /// identifier.
    nodes_in: HashMap<var::Node, SpTyBaseTuple>,
    /// Outputs, same as the inputs above.
    nodes_out: HashMap<var::Node, SpTyBaseTuple>,
    /// Generic variables and their instanciations for each node.
    nodes_generics: HashMap<var::Node, (GenericParams, Option<GenericInstances>)>,
}

impl<'i> TyCtx<'i> {
    /// Construct a fresh context with known global variables but no
    /// locals or blocks.
    fn from_ext(global: &'i HashMap<var::Global, WithDefSite<Sp<ty::Base>>>) -> TyCtx<'i> {
        Self {
            global,
            registers: HashMap::default(),
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
                suggest1: err::Suggest {
                    available: self.vars.keys(),
                },
                suggest2: err::Suggest {
                    available: self.global.keys(),
                },
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
                suggest: err::Suggest {
                    available: self.vars.keys(),
                },
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
                suggest1: err::Suggest {
                    available: self.vars.keys(),
                },
                suggest2: err::Suggest {
                    available: self.global.keys(),
                },
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

    /// Get the (ordered) sequence of type variables that the node declares.
    fn get_node_generics(&self, node: Sp<&var::Node>) -> GenericParams {
        match self.nodes_generics.get(node.t) {
            Some(tup) => tup.0.clone(),
            None => {
                err::abort!("The typing context is improperly initialized: either {node} is missing and it should have been caught during the causality check, or it was not added to the map.");
            }
        }
    }

    /// A `Node` introduces a hole in the context to eventually put the actual
    /// values of type variables that the node declaration introduces.
    /// This function fills in that hole from a computed unifier.
    fn instantiate(&mut self, node: Sp<&var::Node>, unifier: &TyUnifier) {
        let gen = at_mut!(self.nodes_generics, node.t);
        match unifier {
            TyUnifier::Mapping {
                site: _,
                idx: _,
                subst,
            } => {
                gen.1 = Some(
                    subst
                        .iter()
                        .map(|v| {
                            v.clone().unwrap_or_else(|| {
                                err::abort!("Substitution is not fully instanciated")
                            })
                        })
                        .collect(),
                );
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
            Self::PutRegister { .. } => {
                // This whole thing has already been verified by `TypeCheckExpr`
                // for `FetchRegister`
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
            // Transparent containers
            Self::Lit(inner) => Some(inner.typecheck(eaccum, ctx)?.t),
            Self::Reference(inner) => Some(inner.typecheck(eaccum, ctx)?.t),
            Self::DummyPre(inner) => Some(inner.typecheck(eaccum, ctx)?.t),
            Self::DummyParen(inner) => Some(inner.typecheck(eaccum, ctx)?.t),
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
            // Both sides must be equal and accepted by `op`.
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
            // Must be accepted by `op`.
            Self::Un { op, inner } => {
                let inner = inner.typecheck(eaccum, ctx)?.is_primitive(eaccum)?;
                op.accepts(eaccum, span, inner.as_ref())?;
                Some(ty::Tuple::Single(inner))
            }
            // Both sides must be equal and accepted by `op`.
            Self::Cmp { op, lhs, rhs } => {
                let left = lhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                let right = rhs
                    .typecheck(eaccum, ctx)
                    .and_then(|ty| ty.is_primitive(eaccum));
                op.accepts(eaccum, left?.as_ref(), right?.as_ref())?;
                Some(ty::Tuple::Single(ty::Base::Bool.with_span(span)))
            }
            // Both sides must be equal.
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
            // Both branches must be equal. Condition must be a boolean.
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
            // All arguments need to have the right input type.
            Self::Substep { delay: _, id, args } => {
                let expected_tys = ctx.get_node_in(id.as_ref());
                let generics = ctx.get_node_generics(id.as_ref());
                let actual_tys = args.typecheck(eaccum, ctx)?;
                // It's a bit more subtle than plain equality between `expected_tys` and
                // `actual_tys` because there might be subtyping or substitutions.
                let mut unifier = TyUnifier::from_generics(generics, expected_tys.span);
                expected_tys.identical(eaccum, &mut unifier, &actual_tys, span)?;
                ctx.instantiate(id.as_ref(), &unifier); // The node might contain generics that we
                                                        // now learned about.
                Some(ctx.get_node_out(id.as_ref()).t.substitute(&unifier))
            }
            // Clocked by a boolean always.
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
            // Merge operates on a boolean, and both branches must have the same type.
            Self::Merge { switch, on, off } => {
                let switch = switch.typecheck(eaccum, ctx)?;
                let on = on.typecheck(eaccum, ctx)?;
                let off = off.typecheck(eaccum, ctx)?;
                let switch = switch.is_primitive(eaccum)?;
                switch.as_ref().is_bool(eaccum, "A clock", span)?;
                on.identical(eaccum, &mut TyUnifier::Identity, &off, span)?;
                Some(on.t)
            }
            Self::FetchRegister {
                id,
                dummy_init,
                dummy_followed_by,
                step_immediately: _,
            } => {
                let followed_by = dummy_followed_by.typecheck(eaccum, ctx)?;
                if let Some(init) = dummy_init {
                    let init = init.typecheck(eaccum, ctx)?;
                    followed_by.identical(eaccum, &mut TyUnifier::Identity, &init, span)?;
                }
                ctx.registers.insert(*id, followed_by.clone());
                Some(followed_by.t)
            }
        }
    }

    fn is_const(&self, eaccum: &mut EAccum, span: Span) -> Option<()> {
        match self {
            // Base cases
            Self::Lit(inner) => {
                inner.is_const(eaccum)?;
                Some(())
            }
            Self::Reference(inner) => {
                inner.is_const(eaccum)?;
                Some(())
            }
            // Transparent containers (i.e. all Rust-comptime-computable
            // operations)
            Self::Bin { lhs, rhs, .. } => {
                let lhs = lhs.is_const(eaccum);
                let rhs = rhs.is_const(eaccum);
                lhs?;
                rhs?;
                Some(())
            }
            Self::DummyParen(inner) => {
                inner.is_const(eaccum)?;
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
            Self::FetchRegister { .. } => eaccum.error(err::NotConst {
                what: "Register operations",
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
            // Currently unsupported, but not outrageous to add support in the future.
            Self::Tuple(_) => eaccum.error(err::NotConst {
                what: "Tuples are currently",
                site: span,
            }),
            // Unambiguous failures: not only are they not computable at compile-time by Rust,
            // it just doesn't make sense to put the temporal operators in a constant.
            Self::DummyPre(_) => eaccum.error(err::NotConst {
                what: "The temporal operator `pre` is",
                site: span,
            }),
            Self::Later { .. } => eaccum.error(err::NotConst {
                what: "The delay operator (-> / fby) is",
                site: span,
            }),
            Self::Substep { .. } => eaccum.error(err::NotConst {
                what: "Function calls are",
                site: span,
            }),
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
            (Bin::Add | Bin::Mul | Bin::Sub | Bin::Div, ty::Base::Int | ty::Base::Float)
            | (Bin::Rem, ty::Base::Int)
            | (Bin::BitAnd | Bin::BitOr | Bin::BitXor, ty::Base::Int | ty::Base::Bool) => Some(()),
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
            (Un::Neg, ty::Base::Int | ty::Base::Float)
            | (Un::Not, ty::Base::Int | ty::Base::Bool) => Some(()),
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
    fn accepts(self, eaccum: &mut EAccum, left: Sp<&ty::Base>, right: Sp<&ty::Base>) -> Option<()> {
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
            (
                Cmp::Le | Cmp::Ge | Cmp::Lt | Cmp::Gt,
                ty::Base::Int | ty::Base::Bool | ty::Base::Float,
            )
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
                what: req,
                site: span,
                inner: &self,
            }),
        }
    }
}

/// We implement unification via mapping because the language is simple
/// enough that there is always one of the two tuples that we need to unify
/// that is fully instanciated.
///
/// For example
/// - (T, U, V) ~ (T, int, T) introduces { T => T; V => T; U => int },
/// - (A, B) ~ (B, float) introduces { A => B; B => float },
/// - (A, A) ~ (int, float) is impossible.
///
/// In particular there is no relation between type variables of the same
/// name on the left or right side, and all that we need is for every variable
/// on the left to be uniquely mapped to a variable or type on the right.
///
/// The variants of this enum provide different behaviors towards type variables.
#[derive(Debug)]
enum TyUnifier {
    /// Trivial mapping. Every variable is unchanged.
    Identity,
    /// Absence of a mapping.
    /// This panics if there is a type variable.
    Null,
    /// Description of a `Type -> Type` substitution.
    /// Preserves order so that we can reproduce the correct sequence
    /// of instanciated type variables.
    Mapping {
        /// Function call that introduced this mapping.
        site: Span,
        /// `Type -> id` part of the full mapping `Type -> Type`.
        idx: HashMap<Sp<String>, usize>,
        /// `id -> Type` part of the full mapping `Type -> Type`.
        subst: Vec<Option<Sp<ty::Base>>>,
    },
}

impl TyUnifier {
    /// Fresh mapping unifier.
    /// `generics` gives the variables that the mapping should have as keys
    /// (requires no duplicates).
    ///
    /// `site` will be shown as the "thing that introduced this mapping" in
    /// error messages, so you should typically set it to the location of
    /// the full function call or other overapproximation of the statement
    /// that introduces a mapping.
    fn from_generics(generics: Vec<Sp<String>>, site: Span) -> Self {
        let mut idx = HashMap::new();
        let len = generics.len();
        // Each type variable is a key. A duplicate means that the caller didn't properly
        // check the uniqueness of type variables.
        for (i, gen) in generics.into_iter().enumerate() {
            if idx.insert(gen, i).is_some() {
                err::abort!("Failed to catch duplicate type variable.")
            }
        }
        Self::Mapping {
            site,
            idx,
            subst: vec![None; len],
        }
    }

    /// Extract the type that `s` should be substituted with.
    ///
    /// For non-mapping `TyUnifier`s this is the identity, otherwise this
    /// is quite straightforwardly the value in the `HashMap` that is associated
    /// with the key `s`. This requires that the map be fully instanciated, so
    /// make sure to check somewhere that all type variables introduced by the
    /// declaration are actually bound by the declarations.
    fn image(&self, s: Sp<String>) -> ty::Base {
        match self {
            Self::Mapping {
                site: _,
                idx,
                subst,
            } => {
                if let Some(i) = idx.get(&s) {
                    at!(subst, *i)
                        .clone()
                        .unwrap_or_else(|| err::abort!("Mapping is not fully instanciated"))
                        .t
                } else {
                    ty::Base::Other(s)
                }
            }
            Self::Identity => ty::Base::Other(s),
            Self::Null => err::abort!("This context should never contain generics"),
        }
    }

    /// Add a new type substitution `T => U`.
    /// If any substitution `T => V` already exists, it must be for `V = U`.
    /// It is the *left* argument that gets coerced to the right one, so
    /// `require_identical(T, int)` will produce `T => int` but
    /// `require_identical(int, T)` will always fail.
    fn require_identical(
        &mut self,
        eaccum: &mut EAccum,
        left: Sp<&ty::Base>,
        right: Sp<&ty::Base>,
        source: Span,
    ) -> Option<()> {
        use ty::Base as B;
        match &left.t {
            // Fully instanciated types don't get substituted obviously,
            // so they just need to compare identical.
            l @ (B::Int | B::Float | B::Bool) => {
                if l == &right.t {
                    return Some(());
                }
            }
            B::Other(l) => match self {
                Self::Mapping { site, idx, subst } => {
                    let Some(i) = idx.get(l) else {
                        unimplemented!()
                        // We get here if we try to use a malformed signature.
                        // This occurs when the signature contains an undeclared
                        // generic argument.
                        //
                        // We can just let this one go and the error will be
                        // caught by someone else.
                        // As a side effect, this will produce extraneous
                        // TypeMismatch errors, but better that than risk
                        // being unsound.
                    };

                    return match at_mut!(subst, *i) {
                        x @ None => {
                            // Not substituted yet.
                            *x = Some(right.cloned());
                            Some(())
                        }
                        Some(ref mut v) => {
                            if v.as_ref() == right {
                                // Already has some compatible substitution.
                                Some(())
                            } else {
                                // Already has another substitution.
                                eaccum.error(err::UnsatGenericConstraint {
                                    variable: &l,
                                    previous: v.as_ref(),
                                    new: right,
                                    context: &*site,
                                })
                            }
                        }
                    };
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

/// Apply a type substitution given by a `TyUnifier`.
trait TySubstitute {
    /// If `TyUnifier` is a `Mapping`, replace every occurence of a type with
    /// its image by the substitution.
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
            (Single(left), Single(right)) => unifier.require_identical(
                eaccum,
                left.as_ref().with_span(self.span),
                right.as_ref().with_span(other.span),
                source,
            ),
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
                ty::Tuple::Multiple(_) => eaccum.error(err::ScalarNotTuple { typ: self }),
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
        span: Span,
        extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        let mut ctx = TyCtx::from_ext(extvar);
        // Special case for functions declared as toplevel executables:
        // they must have empty inputs and outputs.
        let is_main = self.options.main.fetch::<This>().is_some();
        let is_test = self.options.test.fetch::<This>().is_some();
        let inputs_nonempty = !self.inputs.t.is_empty();
        let outputs_nonempty = !self.outputs.t.is_empty();
        if (is_main || is_test) && (inputs_nonempty || outputs_nonempty) {
            eaccum.error(err::ExecutableNodeSig {
                reason: if is_main {
                    "annotation #[main]"
                } else {
                    "annotation #[test]"
                },
                inputs_nonempty,
                outputs_nonempty,
                site: span,
                inputs: &self.inputs,
                outputs: &self.outputs,
            })?;
        }

        // Check that the generic parameters are all inferrable from the inputs only.
        // That is
        // - all type variables of the declaration appear in the inputs, and
        // - all type variables in the inputs, outputs, or locals are declared.
        {
            let mut declared_generics = std::collections::HashSet::new();
            for g in self.options.generics.fetch::<This>() {
                declared_generics.insert(g);
            }

            let mut unused_generics = declared_generics.clone();
            for i in self.inputs.t.iter() {
                if let ty::Base::Other(t) = &i.t.ty.t.ty.t.inner.t {
                    if !declared_generics.contains(&t) {
                        eaccum.error(err::UndeclaredGeneric {
                            undeclared: &i.t.ty.t.ty.t.inner,
                        })?;
                    }
                    // Inputs count towards being used
                    unused_generics.remove(&t);
                }
            }
            for i in self.outputs.t.iter().chain(self.locals.t.iter()) {
                if let ty::Base::Other(t) = &i.t.ty.t.ty.t.inner.t {
                    // Outputs and locals don't count towards usage
                    // and only need to be declared.
                    if !declared_generics.contains(&t) {
                        eaccum.error(err::UndeclaredGeneric {
                            undeclared: &i.t.ty.t.ty.t.inner,
                        })?;
                    }
                }
            }

            for unused in unused_generics {
                eaccum.error(err::UnusedGeneric {
                    unused,
                    inputs: &self.inputs,
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
                return eaccum.error(err::FunNotFound { fun: &blk.name });
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
        scope.close()?;
        // Finally instanciate the learned generic parameters
        for (id, gens) in ctx.nodes_generics {
            at_mut!(self.blocks, id.id.t).generics = gens.1;
        }
        // And propagate upwards the learned types of registers
        for reg in &mut self.registers {
            reg.typ = ctx.registers.get(&reg.id).cloned();
        }
        Some(())
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
        span: Span,
        _extfun: &HashMap<ast::decl::NodeName, (GenericParams, SpTyBaseTuple, SpTyBaseTuple)>,
        _extvar: &HashMap<var::Global, WithDefSite<Sp<ty::Base>>>,
    ) -> Option<()> {
        let extvar = HashMap::default();
        let mut ctx = TyCtx::from_ext(&extvar);
        // Special case for functions declared as toplevel executables:
        // they must have empty inputs and outputs.
        // Extern nodes cannot be marked #[test] so we don't need to handle that.
        let is_main = self.options.main.fetch::<This>().is_some();
        let inputs_nonempty = !self.inputs.t.is_empty();
        let outputs_nonempty = !self.outputs.t.is_empty();
        if is_main && (inputs_nonempty || outputs_nonempty) {
            eaccum.error(err::ExecutableNodeSig {
                reason: "annotation #[main]",
                inputs_nonempty,
                outputs_nonempty,
                site: span,
                inputs: &self.inputs,
                outputs: &self.outputs,
            })?;
        }

        // Check that the generic parameters are all inferrable from the inputs only.
        // That is
        // - all type variables of the declaration appear in the inputs, and
        // - all type variables in the inputs, outputs, or locals are declared.
        {
            let mut declared_generics = std::collections::HashSet::new();
            for g in self.options.generics.fetch::<This>() {
                declared_generics.insert(g);
            }

            let mut unused_generics = declared_generics.clone();
            for i in self.inputs.t.iter() {
                if let ty::Base::Other(t) = &i.t.ty.t.ty.t.inner.t {
                    if !declared_generics.contains(&t) {
                        eaccum.error(err::UndeclaredGeneric {
                            undeclared: &i.t.ty.t.ty.t.inner,
                        })?;
                    }
                    // Inputs count towards being used
                    unused_generics.remove(&t);
                }
            }
            for i in self.outputs.t.iter() {
                if let ty::Base::Other(t) = &i.t.ty.t.ty.t.inner.t {
                    // Outputs and locals don't count towards usage
                    // and only need to be declared.
                    if !declared_generics.contains(&t) {
                        eaccum.error(err::UndeclaredGeneric {
                            undeclared: &i.t.ty.t.ty.t.inner,
                        })?;
                    }
                }
            }

            for unused in unused_generics {
                eaccum.error(err::UnusedGeneric {
                    unused,
                    inputs: &self.inputs,
                })?;
            }
        }

        // These are all the extra variables that we provide in addition
        // to `extvar`.
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
                        err::abort!(
                            "Duplicate const definition should have been caught during causality check"
                        );
                    }
                }
                ast::decl::Decl::Node(node) => {
                    scope.compute(|eaccum| node.typecheck(eaccum, &functx, &varctx));
                    let (name, tyvars, ins, outs) = node.signature();
                    if functx.insert(name.t, (tyvars, ins, outs)).is_some() {
                        err::abort!(
                            "Duplicate node definition should have been caught during causality check"
                        );
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
                        err::abort!(
                            "Duplicate const definition should have been caught during causality check"
                        );
                    }
                }
                ast::decl::Decl::ExtNode(node) => {
                    scope.compute(|eaccum| node.typecheck(eaccum, &functx, &varctx));
                    let (name, tyvars, ins, outs) = node.signature();
                    if functx.insert(name.t, (tyvars, ins, outs)).is_some() {
                        err::abort!(
                            "Duplicate node definition should have been caught during causality check"
                        );
                    }
                }
            }
        }
        scope.close()
    }
}
