//! Translator from the Lustre parsing AST to the Candle pre-analysis AST.
//!
//! Translating from Lustre to Candle is not a difficult task because the two
//! languages have mostly identical structures, but there are nevertheless
//! numerous traps and subtleties that although minor for a programmer are very
//! relevant for a systematic translator.
//!
//! We summarize here the differences between the two languages that constitute
//! the details to look out for in the implementations of this file.
//!
//!
//!
//! # Global vs Local name resolution
//!
//! Relevant `impl`: `VarExpr`.
//!
//! Lustre allows for local variables to shadow global constants, and any
//! use of a variable -- local or constant -- is syntactically identical.
//! Not so in Candle, where several technical restrictions require that the
//! Candle AST differentiate between the two:
//! - Rust does not allow global constant shadowing, so during codegen
//!   Candle must pick fresh identifiers for local variables that cannot
//!   collide with those of constants,
//! - global constants are given a Candle type that is not the same as that
//!   of local variables with the same Lustre type.
//!   Indeed the Candle type for variables of `int` is `Nillable<i64>`,
//!   and the Candle type for constants of `int` is a plain `i64`.
//! These thus require differences in the handling of variables and constants
//! at both definition site and call site, which means that the translator
//! must maintain a mutable context of currently shadowed variables.
//!
//!
//!
//! # Precedence
//!
//! Relevant `impl`s: all `expr::*`.
//!
//! The hierarchy of expressions exists only for parsing purposes to express
//! the precedence of operators, and in Candle there is no longer any notion
//! of `AtomicExpr`, all exressions are at the same level.
//! Candle has much fewer expression types (`Expr`) than the Lustre parsing
//! AST (`MulExpr`, `AddExpr`, `PreExpr`, `AtomicExpr`, ...), and this is
//! reflected in the translation that needs to create much more `Box`es
//! but in return has more translation functions with the same output type.
//!
//!
//!
//! # Associativity
//!
//! Relevant `impl`s: `AddExpr`, `OrExpr`, `AndExpr`, `FbyExpr`, `MulExpr`.
//!
//! The parser cannot handle associativity, and instead associative expressions
//! are returned as a flat vector. For example `1 + 2 + 3 + 4` is parsed as a
//! vector of pairs `[Pair(1, +), Pair(2, +), Pair(3, +), End(4)]`, and we
//! want to resolve it into
//! ```skip
//! Bin {
//!     lhs: Bin {
//!         lhs: Bin {
//!             lhs: 1,
//!             op: +,
//!             rhs: 2,
//!         },
//!         op: +,
//!         rhs: 3,
//!     },
//!     op: +,
//!     rhs: 4,
//! }
//! ```
//! Symetrically, `1 fby 2 fby 3 fby 4` is parsed as
//! `[Pair(1, fby), Pair(2, fby), Pair(3, fby), End(4)]` and should be resolved as
//! ```skip
//! Fby {
//!     depth: 0,
//!     before: 1,
//!     after: Fby {
//!         depth: 1,
//!         before: 2,
//!         after: Fby {
//!             depth: 2,
//!             before: 3,
//!             after: 4,
//!         },
//!     },
//! }
//! ```
//! To this end we provide helpers in `assoc` to efficiently handle binary associative
//! operators.
//!
//!
//!
//! # Noassoc comparisons
//!
//! Relevant `impl`: `CmpExpr`.
//!
//! A complementary problem to the above issue is the fact that comparisons are not
//! associative, but the parser accepts them as such. `1 <= 2 < 3 <> 4` is parsed
//! as `[Pair(1, <=), Pair(2, <), Pair(3, <>), End(4)]` but must be rejected on
//! the account of `<=` not being associative with `<`.
//!
//!
//!
//! # Size-1 tuple flattening
//!
//! Relevant `impl`s: `TargetExprTuple`, `TargetExpr`, `CallExpr`.
//!
//! For Lustre we adapt and generalize the convention of Rust on what is a tuple
//! or simply a parenthesized expression, and where trailing commas are allowed.
//!
//! For tuples, we employ exactly the Rust convention, where
//! - `()`, `(())`, `((()))`, ... are all equal to the unit tuple,
//! - `1`, `(1)`, `((1))`, ... are all equal to the integer `1`,
//! - `(1, 2)`, `(1, 2,)`, `((1), (2))`, `((1, 2))`, ... are all tuples with two elements of `int`
//!   (i.e. up to one trailing comma and an unlimited amount of surrounding parentheses are allowed),
//! - however, specifically for size 1, a trailing comma changes the meaning:
//!   - `(1) <> (1,)` (left is of type `int`, right is of type `(int)` a 1-element tuple)
//!   - `(()) <> ((),)` (left is of type `unit`, right is a size-1 tuple containing `unit`)
//!
//! For function calls however we differ slightly from Rust, because the arguments of
//! a function are not a `Tuple<Expr>` but a plain `Expr` that just might happen to
//! contain a tuple. This has the following notable consequences:
//! - A node defined with `node foo(_, _, _ : int)` may accept interchangeably
//!   - `foo(1, 2, 3)`,
//!   - `foo((1, 2, 3))`,
//!   - or even `foo(bar())` if `node bar returns(_, _, _ : int)`.
//! - A node without any arguments can be given one unit argument
//!   - `node count() returns (n : int)` can accept `count(())`
//! - trailing commas are allowed except when the size is exactly one:
//!   - `foo(1, 2, 3,) = foo(1, 2, 3)`
//!   - `foo(1) <> foo(1,)`
//!   - `foo(()) <> foo((),)`
//!
//!
//!
//! # Fby-expansion and pre-pushing
//!
//! Relevant `impl`s: `FbyExpr`, `PreExpr`, `VarExpr`.
//!
//! Lustre and Candle don't quite have the same temporal operators.
//! Both have a construct to fetch one value before a certain clock value
//! and another after (`->` in Lustre, `later!` in Candle), but
//! 1. only Lustre has `fby`, and
//! 2. only Lustre can apply `pre` to arbitrary expressions.
//! The first of these is easy: `e1 fby e2` expands to `e1 -> pre e2`,
//! but this still leaves the issue that `e2` could be an expression on which
//! Candle does not support `pre`. Indeed Candle's only other temporal operator
//! is `pre^n v` (written `var!(self <~ n; v)`) that fetches the value `n` instants
//! ago, but only for `v` a variable, not an arbitrary expression.
//!
//! The translator must thus push the `pre` to the leaves, according to the
//! following non-exhaustive list of rules:
//! - `pre n ~~> n` (`n` a literal or constant)
//! - `pre (e1 + e2) ~~> (pre e1) + (pre e2)`
//! - `pre (-e) ~~> -(pre e)`
//! - `pre float(e) ~~> float(pre e)`
//! - `pre (e1 or e2) ~~> (pre e1) or (pre e2)`
//! - `pre (e1 -> e2) ~~> (pre e1) -> (pre e2)`
//! - etc...
//!
//! To this end the translator carries in its context the current depth at
//! with the expression is evaluated. Usually "regular" (`+`, `-`, `or`, ...)
//! operators propagate the same depth to their arguments, and temporal operators
//! act upon the depth: `pre` increases it by one, `fby` evaluates its two arguments
//! on different depths, etc.

use std::collections::HashSet;

use super::ast as lus;

use chandeliers_err::{self as err, IntoError, Result};
use chandeliers_san::ast as candle;
use chandeliers_san::sp::{Sp, Span, Spanned};

type CandleExpr = candle::expr::Expr;

/// Current options being applied to the node.
#[derive(Clone, Default, Debug)]
pub struct DeclOptions {
    trace: bool,
    export: bool,
    main: Option<usize>,
    rustc_allow: Vec<syn::Ident>,
}

impl DeclOptions {
    fn for_const(self) -> Result<candle::decl::ConstOptions> {
        /* FIXME: errors */
        let Self {
            trace: _,
            export,
            main: _,
            rustc_allow,
        } = self;
        Ok(candle::decl::ConstOptions {
            export,
            rustc_allow,
        })
    }

    fn for_node(self) -> Result<candle::decl::NodeOptions> {
        let Self {
            trace,
            export,
            main,
            rustc_allow,
        } = self;
        /* FIXME: errors */
        Ok(candle::decl::NodeOptions {
            trace,
            export,
            main,
            rustc_allow,
        })
    }

    fn for_ext_const(self) -> Result<candle::decl::ExtConstOptions> {
        /* FIXME: errors */
        let Self {
            trace: _,
            export: _,
            main: _,
            rustc_allow,
        } = self;
        Ok(candle::decl::ExtConstOptions { rustc_allow })
    }

    fn for_ext_node(self) -> Result<candle::decl::ExtNodeOptions> {
        let Self {
            trace,
            export: _,
            main,
            rustc_allow,
        } = self;
        /* FIXME: errors */
        Ok(candle::decl::ExtNodeOptions {
            trace,
            main,
            rustc_allow,
        })
    }

    fn with(mut self, attr: Sp<lus::Attribute>) -> Result<Self> {
        let params = attr.t.attr.t.params.map(|_, t| t.flatten());
        let targets = attr.t.attr.t.targets.map(|_, t| t.flatten());
        use syn::Lit;
        match (
            attr.t.attr.t.action.t.inner.to_string().as_str(),
            &params.t[..],
            &targets.t[..],
        ) {
            ("trace", [], []) => self.trace = true,
            ("export", [], []) => self.export = true,
            ("main", [], []) => self.main = Some(100),
            ("main", [], [Lit::Int(i)]) => self.main = Some(i.base10_parse().unwrap()),
            ("rustc_allow", [attr], []) => {
                self.rustc_allow.push(syn::Ident::new(attr, params.span))
            }
            (attr, _, _) => {
                unimplemented!("Unknown attribute {:?}", attr,)
            }
        }
        Ok(self)
    }
}

/// Trait to translate from the parsing AST to the analysis AST.
pub trait Translate {
    /// Corresponding item in the analysis AST.
    type Output;
    /// Context that the function needs (e.g. known variables, type of the context, ...)
    type Ctx<'i>;
    /// Convert from one AST node to another.
    /// - `run_uid`: used in order to generate unique identifiers that will not
    ///   collide with those of other invocations of the toplevel macro.
    /// - `span`: provided by the wrapper, the `Span` of the entire node.
    ///   You probably won't need it unless internal components need the same span
    ///   as their parent.
    /// - `ctx`: you may ask for any immutable state to be provided as context.
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<Self::Output>;
}

/// Helper trait to translate from the parsing AST to the analysis AST.
/// Only `Sp<T>` is expected to implement this trait.
pub trait SpanTranslate {
    /// Corresponding item in the analysis AST.
    type Output;
    /// Context that the function needs (e.g. known variables, type of the context, ...)
    type Ctx<'i>;
    /// Convert from one AST node to another.
    /// - `run_uid`: used in order to generate unique identifiers that will not
    ///   collide with those of other invocations of the toplevel macro.
    /// - `span`: provided by the wrapper, the `Span` of the entire node.
    ///   You probably won't need it unless internal components need the same span
    ///   as their parent.
    /// - `ctx`: you may ask for any immutable state to be provided as context.
    fn translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> Result<Sp<Self::Output>>;
    /// Same as `translate`, but do now wrap in a span.
    /// This helps for very nested structures that don't want a `Sp<_>` at every level.
    fn flat_translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> Result<Self::Output>;
}

/// `Sp<_>` is transparently translatable, but it makes its span available to the inner translation
/// function.
impl<T> SpanTranslate for Sp<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> Result<Sp<Self::Output>> {
        let res = self.t.translate(run_uid, self.span, ctx)?;
        Ok(Sp {
            t: res,
            span: self.span,
        })
    }
    fn flat_translate(self, run_uid: usize, ctx: Self::Ctx<'_>) -> Result<Self::Output> {
        self.t.translate(run_uid, self.span, ctx)
    }
}

/// `Box<_>` is transparently translatable, *and it consumes the `Box<_>`*.
///
/// The output is not wrapped by default because the parsing AST and the analysis
/// AST require boxes in different places.
impl<T> Translate for Box<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<Self::Output> {
        (*self).translate(run_uid, span, ctx)
    }
}

impl Translate for lus::Prog {
    type Ctx<'i> = ();
    type Output = candle::decl::Prog;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> Result<candle::decl::Prog> {
        let mut decls = Vec::new();
        for decl in self.decls.into_iter() {
            decls.push(decl.translate(run_uid, DeclOptions::default())?);
        }
        Ok(candle::decl::Prog { decls })
    }
}

impl Translate for lus::AttrDecl {
    type Ctx<'i> = DeclOptions;
    type Output = candle::decl::Decl;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: DeclOptions,
    ) -> Result<candle::decl::Decl> {
        match self {
            Self::Tagged(attr, n) => n.flat_translate(run_uid, options.with(attr)?),
            Self::Node(n) => n.flat_translate(run_uid, options),
        }
    }
}

impl Translate for lus::Decl {
    type Ctx<'i> = DeclOptions;
    type Output = candle::decl::Decl;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: DeclOptions,
    ) -> Result<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => {
                candle::decl::Decl::Const(c.translate(run_uid, options.for_const()?)?)
            }
            Self::Node(n) => candle::decl::Decl::Node(n.translate(run_uid, options.for_node()?)?),
            Self::Extern(e) => e.flat_translate(run_uid, options)?,
        })
    }
}

impl Translate for lus::Extern {
    type Ctx<'i> = DeclOptions;
    type Output = candle::decl::Decl;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: DeclOptions,
    ) -> Result<candle::decl::Decl> {
        Ok(match self {
            Self::Const(c) => {
                candle::decl::Decl::ExtConst(c.translate(run_uid, options.for_ext_const()?)?)
            }
            Self::Node(n) => {
                candle::decl::Decl::ExtNode(n.translate(run_uid, options.for_ext_node()?)?)
            }
        })
    }
}

impl Translate for lus::ExtConst {
    type Ctx<'i> = candle::decl::ExtConstOptions;
    type Output = candle::decl::ExtConst;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: candle::decl::ExtConstOptions,
    ) -> Result<Self::Output> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            repr: name.to_string().spanned(span),
            run_uid,
        });
        let ty = self.ty.translate(run_uid, ())?;
        Ok(candle::decl::ExtConst {
            name,
            ty: ty.t.inner,
            options,
        })
    }
}

impl Translate for lus::ExtNode {
    type Ctx<'i> = candle::decl::ExtNodeOptions;
    type Output = candle::decl::ExtNode;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: candle::decl::ExtNodeOptions,
    ) -> Result<candle::decl::ExtNode> {
        let name = self.name.map(|span, name| candle::decl::NodeName {
            repr: name.to_string().spanned(span),
            run_uid,
        });
        let inputs = self.inputs.translate(run_uid, ())?;
        let outputs = self.outputs.translate(run_uid, ())?;
        Ok(candle::decl::ExtNode {
            name,
            inputs,
            outputs,
            options,
        })
    }
}

/// Helper to construct an expression.
///
/// This exists to extract additional information as a byproduct of constructing
/// the expression.
#[derive(Default)]
struct ExprCtx {
    blocks: Vec<Sp<candle::decl::NodeName>>,
    stmts: Vec<Sp<candle::stmt::Statement>>,
    shadow_glob: HashSet<String>,
}

/// Accessor for `ExprCtx`.
/// This is not `Copy` or even `Clone`, so if you need to duplicate it
/// you should use the `fork!` macro that will reborrow a fresh copy.
pub struct ExprCtxView<'i> {
    blocks: &'i mut Vec<Sp<candle::decl::NodeName>>,
    stmts: &'i mut Vec<Sp<candle::stmt::Statement>>,
    shadow_glob: &'i HashSet<String>,
    depth: usize,
}

impl ExprCtx {
    /// Create a new accessor`
    fn view(&mut self) -> ExprCtxView<'_> {
        ExprCtxView {
            blocks: &mut self.blocks,
            stmts: &mut self.stmts,
            shadow_glob: &self.shadow_glob,
            depth: 0,
        }
    }
}

impl ExprCtxView<'_> {
    /// Increase the depth of the analysis. This affects the temporal operators
    /// `pre`, `fby`, and `->`.
    fn bump(self, n: usize) -> Self {
        Self {
            depth: self.depth + n,
            ..self
        }
    }

    /// Increase the depth by 1.
    fn incr(self) -> Self {
        self.bump(1)
    }
}

macro_rules! fork {
    ($this:ident) => {
        ExprCtxView {
            blocks: &mut *$this.blocks,
            stmts: &mut *$this.stmts,
            shadow_glob: &$this.shadow_glob,
            depth: $this.depth,
        }
    };
}

impl Translate for lus::Node {
    type Ctx<'i> = candle::decl::NodeOptions;
    type Output = candle::decl::Node;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        options: candle::decl::NodeOptions,
    ) -> Result<Self::Output> {
        let name = self.name.map(|span, name| candle::decl::NodeName {
            repr: name.to_string().spanned(span),
            run_uid,
        });
        let inputs = self.inputs.translate(run_uid, ())?;
        let outputs = self.outputs.translate(run_uid, ())?;
        let locals = self.locals.translate(run_uid, ())?;
        let mut ectx = ExprCtx::default();
        for shadows in &[&inputs, &outputs, &locals] {
            for s in shadows.t.iter() {
                ectx.shadow_glob.insert(s.t.name.t.repr.t.clone());
            }
        }

        for def in self.defs.into_iter() {
            def.translate(run_uid, ectx.view())?;
        }
        let ExprCtx { blocks, stmts, .. } = ectx;
        Ok(candle::decl::Node {
            options,
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        })
    }
}

impl Translate for lus::Const {
    type Ctx<'i> = candle::decl::ConstOptions;
    type Output = candle::decl::Const;
    fn translate(
        self,
        run_uid: usize,
        span: Span,
        options: candle::decl::ConstOptions,
    ) -> Result<candle::decl::Const> {
        let name = self.name.map(|span, name| candle::expr::GlobalVar {
            repr: name.to_string().spanned(span),
            run_uid,
        });
        let ty = self.ty.translate(run_uid, ())?;
        let mut ectx = ExprCtx::default();
        let value = self.value.translate(run_uid, ectx.view())?;
        if !ectx.stmts.is_empty() || !ectx.blocks.is_empty() {
            return Err(err::NotConst {
                site: span,
                what: "Function calls are",
            }
            .into_err());
        }
        Ok(candle::decl::Const {
            options,
            name,
            ty: ty.t.inner,
            value,
        })
    }
}

impl Translate for lus::ArgsTys {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(
        self,
        run_uid: usize,
        _span: Span,
        _: (),
    ) -> Result<candle::Tuple<Sp<candle::decl::TyVar>>> {
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(run_uid, &mut vs)?;
        }
        Ok(vs)
    }
}

impl Translate for lus::ArgsTy {
    type Ctx<'i> = &'i mut candle::Tuple<Sp<candle::decl::TyVar>>;
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<()> {
        let ty = self.ty.translate(run_uid, ())?;
        self.args.translate(run_uid, (ctx, ty))?;
        Ok(())
    }
}

impl Translate for lus::Decls {
    type Ctx<'i> = (
        &'i mut candle::Tuple<Sp<candle::decl::TyVar>>,
        Sp<candle::ty::Clocked<candle::ty::TyBase>>,
    );
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, (vars, ty): Self::Ctx<'_>) -> Result<()> {
        for id in self.ids {
            vars.push(id.map(|span, id| {
                candle::decl::TyVar {
                    name: candle::expr::LocalVar {
                        repr: id.to_string().spanned(span),
                        run_uid,
                    }
                    .spanned(span),
                    ty: ty.clone().map(|span, ty| {
                        candle::ty::Stream {
                            ty: ty.spanned(span),
                            depth: candle::past::Depth {
                                // Putting in a dummy value 0 for `dt`, don't forget to update
                                // it by depth propagation in the positivity check...
                                dt: 0,
                            }
                            .spanned(span),
                        }
                    }),
                }
            }));
        }
        Ok(())
    }
}

impl Translate for lus::Type {
    type Ctx<'i> = ();
    type Output = candle::ty::Clocked<candle::ty::TyBase>;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        // FIXME: translate the clock
        let inner = self.base.translate(run_uid, ())?;
        let clk = self.clock.translate(run_uid, ())?;
        Ok(candle::ty::Clocked { inner, clk })
    }
}

impl Translate for lus::BaseType {
    type Ctx<'i> = ();
    type Output = candle::ty::TyBase;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        Ok(match self {
            Self::Int(_) => candle::ty::TyBase::Int,
            Self::Float(_) => candle::ty::TyBase::Float,
            Self::Bool(_) => candle::ty::TyBase::Bool,
        })
    }
}

impl Translate for lus::TypeClock {
    type Ctx<'i> = ();
    type Output = candle::ty::Clock;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        match self {
            Self::When(w) => w.flat_translate(run_uid, ()),
            Self::Whenot(w) => w.flat_translate(run_uid, ()),
            Self::None => Ok(candle::ty::Clock::Implicit),
        }
    }
}

impl Translate for lus::WhenClock {
    type Ctx<'i> = ();
    type Output = candle::ty::Clock;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        Ok(candle::ty::Clock::Explicit {
            activation: true,
            id: self.clock.map(|_, c| format!("{c}")),
        })
    }
}

impl Translate for lus::WhenotClock {
    type Ctx<'i> = ();
    type Output = candle::ty::Clock;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        Ok(candle::ty::Clock::Explicit {
            activation: false,
            id: self.clock.map(|_, c| format!("{c}")),
        })
    }
}

impl Translate for lus::OptionalVarsDecl {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        match self {
            Self::Decls(d) => d.flat_translate(run_uid, ()),
            Self::None => Ok(candle::Tuple::default()),
        }
    }
}

impl Translate for lus::VarsDecl {
    type Ctx<'i> = ();
    type Output = candle::Tuple<Sp<candle::decl::TyVar>>;
    fn translate(self, run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
        self.decls.flat_translate(run_uid, ())
    }
}

impl Translate for lus::Statement {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<()> {
        match self {
            Self::Assert(ass) => ass.flat_translate(run_uid, ctx),
            Self::Def(def) => def.flat_translate(run_uid, ctx),
        }
    }
}

impl Translate for lus::Assertion {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<()> {
        let e = self.expr.translate(run_uid, fork!(ctx))?;
        ctx.stmts
            .push(candle::stmt::Statement::Assert(e).spanned(span));
        Ok(())
    }
}

impl Translate for lus::Def {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<()> {
        let target = self.target.translate(run_uid, ())?;
        let source = self.source.translate(run_uid, fork!(ctx))?;
        ctx.stmts
            .push(candle::stmt::Statement::Let { target, source }.spanned(span));
        Ok(())
    }
}

impl Translate for lus::TargetExpr {
    type Ctx<'i> = ();
    type Output = candle::stmt::VarTuple;
    fn translate(self, run_uid: usize, span: Span, _: ()) -> Result<Self::Output> {
        match self {
            Self::Var(i) => Ok(candle::stmt::VarTuple::Single(
                candle::expr::LocalVar {
                    repr: i.map(|_, t| t.to_string()),
                    run_uid,
                }
                .spanned(span),
            )),
            Self::Tuple(ts) => ts.flat_translate(run_uid, ()),
        }
    }
}

impl Translate for lus::TargetExprTuple {
    type Ctx<'i> = ();
    type Output = candle::stmt::VarTuple;
    fn translate(self, run_uid: usize, span: Span, _: ()) -> Result<Self::Output> {
        let mut vs = candle::Tuple::default();
        let trail = self.fields.trailing_punct();
        for t in self.fields {
            vs.push(t.translate(run_uid, ())?);
        }
        if vs.len() == 1 && !trail {
            Ok(vs.into_iter().next().unwrap().t)
        } else {
            Ok(candle::stmt::VarTuple::Multiple(vs.spanned(span)))
        }
    }
}

impl Translate for lus::Expr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        self.inner.flat_translate(run_uid, ctx)
    }
}

impl Translate for lus::expr::IfExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let cond = self.cond.translate(run_uid, fork!(ctx))?.boxed();
        let yes = self.yes.translate(run_uid, fork!(ctx))?.boxed();
        let no = self.no.translate(run_uid, ctx)?.boxed();
        Ok(CandleExpr::Ifx { cond, yes, no })
    }
}

/// Helpers to resolve associativity.
///
/// The parsing AST has associativity that the analysis AST does not:
/// - `+` and `-`, `*` and `/` and `%`, `or`, `and` are left associative,
/// - `fby`, `->` are right associative,
/// - `<` and `>` and `<=` and `>=` and `<>` and `=` are not associative,
/// but all are parsed as a `Punctuated<_, _>`.
///
/// This module offers functions that resolve associativity (or the absence
/// of associativity) to transform e.g.
/// `Punctuated [ (a, +), (b, +), (c, -), (d, +), (e, -), (f) ]`
/// into
/// `Bin { Bin { Bin { Bin { Bin { a, +, b }, +, c }, -, d }, +, e }, -, f }`
///
/// This translation is done here as generically as possible, including
/// differentiating between `Accum` and `Item` even though they will eventually
/// both be instanciated with `Expr`, to guarantee the absence of errors.
mod assoc {
    use super::{Result, Sp, Spanned};
    use std::fmt::Display;
    use syn::punctuated::{Pair, Punctuated};

    /// Descriptor for a translation.
    ///
    /// `label`: text to print if there is an error. Typically the name of the type
    /// being translated.
    ///
    /// `convert`: how to embed the type of elements in expressions.
    /// `compose`: how to combine two expressions into one.
    pub struct Descr<Label, Convert, Compose> {
        pub label: Label,
        pub convert: Convert,
        pub compose: Compose,
    }

    impl<Label, Convert, Compose> Descr<Label, Convert, Compose>
    where
        Label: Display,
    {
        /// Build the tree of left associative operations from a flat representation.
        ///
        /// Notice the type of `Compose`: the accumulator is to the left.
        pub fn left_associative<Elem, Punct, Accum, Item>(
            &mut self,
            elems: Punctuated<Sp<Elem>, Punct>,
        ) -> Result<Accum>
        where
            Convert: FnMut(Sp<Elem>, usize) -> Result<Sp<Item>>,
            Compose: FnMut(Sp<Accum>, Punct, usize, Sp<Item>) -> Accum,
            Accum: Spanned<Output = Sp<Accum>>,
            Item: Into<Accum>,
        {
            // We always assume that there is a trailing _element_, not a trailing punctuation.
            assert!(
                !elems.trailing_punct(),
                "Bug in the parser: {} should not accept trailing punctuation",
                self.label
            );
            let mut pairs = elems.into_pairs().enumerate();
            let mut oper: Punct;
            let mut accum: Sp<Accum>;
            // If the first element is a `Punctuated` we can initialize
            // our accumulator. Otherwise this is a transparent expression
            // and we just defer directly to the translation for the inner type.
            let (depth, fst) = pairs.next().unwrap();
            match fst {
                Pair::Punctuated(elem, punct) => {
                    accum = (self.convert)(elem, depth)?.map(|_, i| i.into());
                    oper = punct;
                }
                Pair::End(elem) => {
                    return Ok((self.convert)(elem, depth)?.t.into());
                }
            }
            // Looping case.
            for (depth, pair) in pairs {
                match pair {
                    // One more element: add it to the accumulator and record
                    // the punctuation to be used in the next iteration.
                    Pair::Punctuated(elem, punct) => {
                        let expr = (self.convert)(elem, depth)?;
                        let span = expr.span.join(accum.span).unwrap();
                        accum = (self.compose)(accum, oper, depth, expr).spanned(span);
                        oper = punct;
                    }
                    // End: combine now then return the accumulator.
                    Pair::End(elem) => {
                        let expr = (self.convert)(elem, depth)?;
                        return Ok((self.compose)(accum, oper, depth, expr));
                    }
                }
            }
            unreachable!("Must encounter a Pair::End")
        }

        /// Build the tree of right associative operations from a flat representation.
        ///
        /// Notice the type of `Compose`: the accumulator is to the right.
        pub fn right_associative<Elem, Punct, Accum, Item>(
            &mut self,
            elems: Punctuated<Sp<Elem>, Punct>,
        ) -> Result<Accum>
        where
            Accum: Spanned<Output = Sp<Accum>>,
            Convert: FnMut(Sp<Elem>, usize) -> Result<Sp<Item>>,
            Compose: FnMut(Sp<Item>, Punct, usize, Sp<Accum>) -> Accum,
            Item: Into<Accum>,
        {
            // We always assume that there is a trailing _element_, not a trailing punctuation.
            assert!(
                !elems.trailing_punct(),
                "Bug in the parser: {} should not accept trailing punctuation",
                self.label
            );
            let mut pairs = elems.into_pairs().enumerate().rev();
            let mut accum: Sp<Accum>;
            // Because we've reversed the iterator, the first element is always
            // and `End`.
            let (depth, last) = pairs.next().unwrap();
            let Pair::End(elem) = last else {
                unreachable!()
            };
            accum = (self.convert)(elem, depth)?.map(|_, i| i.into());
            // Looping case.
            // WARNING: contrary to the left associative case, `End` is not
            // the end! In fact `End` is unreachable because it has already
            // been seen in the handling of `last`.
            for (depth, pair) in pairs {
                let Pair::Punctuated(elem, punct) = pair else {
                    unreachable!()
                };
                let expr = (self.convert)(elem, depth)?;
                let span = expr.span.join(accum.span).unwrap();
                accum = (self.compose)(expr, punct, depth, accum).spanned(span);
            }
            Ok(accum.t)
        }
    }
}

impl Translate for lus::expr::OrExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        assoc::Descr {
            label: "OrExpr",
            convert: |elem: Sp<lus::expr::AndExpr>, _depth| elem.translate(run_uid, fork!(ctx)),
            compose: |lhs: Sp<CandleExpr>, _op, _depth, rhs: Sp<CandleExpr>| CandleExpr::BinOp {
                op: candle::expr::BinOp::BitOr,
                lhs: lhs.boxed(),
                rhs: rhs.boxed(),
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for lus::expr::AndExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        assoc::Descr {
            label: "AndExpr",
            convert: |elem: Sp<lus::expr::CmpExpr>, _depth| elem.translate(run_uid, fork!(ctx)),
            compose: |lhs: Sp<CandleExpr>, _op, _depth, rhs: Sp<CandleExpr>| CandleExpr::BinOp {
                op: candle::expr::BinOp::BitAnd,
                lhs: lhs.boxed(),
                rhs: rhs.boxed(),
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for lus::expr::NotExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let inner = self.inner.translate(run_uid, ctx)?.boxed();
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Not,
            inner,
        })
    }
}

impl Translate for lus::expr::CmpExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
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
                return first.flat_translate(run_uid, ctx);
            }
        };
        let Pair::Punctuated(lhs, op) = first else {
            unreachable!("We know that there is a second")
        };
        let lhs = lhs.translate(run_uid, fork!(ctx))?.boxed();
        let op = op.translate(run_uid, span, ())?;
        // We must not have a third element.
        if let Some(third) = it.next() {
            // We're going to throw this path anyway, we might as well
            // make destructive changes to get a better error message.
            let Pair::Punctuated(second, oper2) = second else {
                unreachable!()
            };
            let second = second.translate(run_uid, fork!(ctx))?;
            let oper2 = oper2.translate(run_uid, span, ())?;
            let third = match third {
                Pair::Punctuated(third, _) => third,
                Pair::End(third) => third,
            };
            let third = third.translate(run_uid, fork!(ctx))?;
            return Err(err::CmpNotAssociative {
                site: span,
                first: lhs,
                oper1: op,
                oper2,
                second,
                third,
            }
            .into_err());
        }
        let Pair::End(rhs) = second else {
            unreachable!("We know there is no third")
        };
        let rhs = rhs.translate(run_uid, ctx)?.boxed();
        Ok(CandleExpr::CmpOp { op, lhs, rhs })
    }
}

impl Translate for lus::expr::CmpOp {
    type Ctx<'i> = ();
    type Output = candle::expr::CmpOp;
    fn translate(self, _run_uid: usize, _span: Span, _: ()) -> Result<Self::Output> {
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

impl Translate for lus::expr::FbyExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        assoc::Descr {
            label: "FbyExpr",
            convert: |elem: Sp<lus::expr::ThenExpr>, depth| {
                elem.translate(run_uid, fork!(ctx).bump(depth))
            },
            compose: |before: Sp<CandleExpr>, _op, depth, after: Sp<CandleExpr>| {
                CandleExpr::Later {
                    clk: candle::past::Depth {
                        dt: ctx.depth + depth,
                    }
                    .spanned(before.span.join(after.span).expect("Faulty span")),
                    before: before.boxed(),
                    after: after.boxed(),
                }
            },
        }
        .right_associative(self.items)
    }
}

impl Translate for lus::expr::PreExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        self.inner.flat_translate(run_uid, ctx.incr())
    }
}

impl Translate for lus::expr::ThenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        // This looks similar to `FbyExpr`, but notice how we aren't using `depth`
        // in the same way.
        assoc::Descr {
            label: "ThenExpr",
            convert: |elem: Sp<lus::expr::AddExpr>, _depth| {
                elem.translate(run_uid, fork!(ctx) /* DO NOT BUMP */)
            },
            compose: |before: Sp<CandleExpr>, _op, depth, after: Sp<CandleExpr>| {
                CandleExpr::Later {
                    clk: candle::past::Depth {
                        dt: ctx.depth + depth,
                    }
                    .spanned(before.span.join(after.span).expect("Faulty span")),
                    before: before.boxed(),
                    after: after.boxed(),
                }
            },
        }
        .right_associative(self.items)
    }
}

impl Translate for lus::expr::AddExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        // Back to normal left assaciative stuff that doesn't care about the depth,
        // but this time we have to handle the fact that there are multiple possible operators.
        assoc::Descr {
            label: "AddExpr",
            convert: |elem: Sp<lus::expr::MulExpr>, _depth| elem.translate(run_uid, fork!(ctx)),
            compose: |lhs: Sp<CandleExpr>, op: lus::expr::AddOp, _depth, rhs: Sp<CandleExpr>| {
                CandleExpr::BinOp {
                    op: op.translate(),
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl lus::expr::AddOp {
    fn translate(self) -> candle::expr::BinOp {
        match self {
            Self::Add(_) => candle::expr::BinOp::Add,
            Self::Sub(_) => candle::expr::BinOp::Sub,
        }
    }
}

impl lus::expr::MulOp {
    fn translate(self) -> candle::expr::BinOp {
        match self {
            Self::Mul(_) => candle::expr::BinOp::Mul,
            Self::Div(_) => candle::expr::BinOp::Div,
            Self::Rem(_) => candle::expr::BinOp::Rem,
        }
    }
}

impl Translate for lus::expr::MulExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        assoc::Descr {
            label: "MulExpr",
            convert: |elem: Sp<lus::expr::ClockExpr>, _depth| elem.translate(run_uid, fork!(ctx)),
            compose: |lhs: Sp<CandleExpr>, op: lus::expr::MulOp, _depth, rhs: Sp<CandleExpr>| {
                CandleExpr::BinOp {
                    op: op.translate(),
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl lus::expr::ClockOp {
    fn translate(self) -> candle::expr::ClockOp {
        match self {
            Self::When(_) => candle::expr::ClockOp::When,
            Self::Whenot(_) => candle::expr::ClockOp::Whenot,
        }
    }
}

impl Translate for lus::expr::ClockExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        assoc::Descr {
            label: "ClockExpr",
            convert: |elem: Sp<lus::expr::PositiveExpr>, _depth| {
                elem.translate(run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<CandleExpr>, op: lus::expr::ClockOp, _depth, rhs: Sp<CandleExpr>| {
                CandleExpr::ClockOp {
                    op: op.translate(),
                    inner: lhs.boxed(),
                    activate: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for lus::expr::NegExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let inner = self.inner.translate(run_uid, ctx)?.boxed();
        Ok(CandleExpr::UnOp {
            op: candle::expr::UnOp::Neg,
            inner,
        })
    }
}

impl Translate for lus::expr::ParenExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let mut es = candle::Tuple::default();
        let trail = self.inner.trailing_punct();
        for e in self.inner.into_iter() {
            es.push(e.translate(run_uid, fork!(ctx))?);
        }
        if es.len() == 1 && !trail {
            Ok(es.into_iter().next().expect("ParenExpr cannot be empty").t)
        } else {
            Ok(CandleExpr::Tuple(es.spanned(span)))
        }
    }
}

impl Translate for lus::expr::CallExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let args = self.args.translate(run_uid, fork!(ctx))?;
        let repr = self.fun.map(|_, t| t.to_string());
        let id = candle::expr::NodeId {
            id: ctx.blocks.len().spanned(span),
            repr: repr.clone(),
        }
        .spanned(span);
        ctx.blocks
            .push(candle::decl::NodeName { repr, run_uid }.spanned(span));
        Ok(CandleExpr::Substep {
            clk: ctx.depth,
            id: id.clone(),
            args: args.boxed(),
        })
    }
}

impl Translate for lus::expr::AtomicExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        match self {
            Self::Lit(l) => l.flat_translate(run_uid, ctx),
            Self::Var(v) => v.flat_translate(run_uid, ctx),
            Self::Paren(p) => p.flat_translate(run_uid, ctx),
        }
    }
}

impl Translate for lus::expr::PositiveExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        match self {
            Self::If(i) => i.flat_translate(run_uid, ctx),
            Self::Merge(m) => m.flat_translate(run_uid, ctx),
            Self::Call(c) => c.flat_translate(run_uid, ctx),
            Self::Pre(p) => p.flat_translate(run_uid, ctx),
            Self::Neg(n) => n.flat_translate(run_uid, ctx),
            Self::Not(n) => n.flat_translate(run_uid, ctx),
            Self::Atomic(a) => a.flat_translate(run_uid, ctx),
        }
    }
}

impl Translate for lus::expr::MergeExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, _span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let switch = self.clk.translate(run_uid, fork!(ctx))?.boxed();
        let on = self.on.translate(run_uid, fork!(ctx))?.boxed();
        let off = self.off.translate(run_uid, fork!(ctx))?.boxed();
        Ok(CandleExpr::Merge { switch, on, off })
    }
}

impl Translate for lus::expr::VarExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, run_uid: usize, span: Span, ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        let repr = self.name.map(|_, t| t.to_string());
        if ctx.shadow_glob.contains(&repr.t) {
            Ok(CandleExpr::Reference(
                candle::expr::Reference::Var(
                    candle::expr::PastVar {
                        var: candle::expr::LocalVar { repr, run_uid }.spanned(span),
                        depth: candle::past::Depth { dt: ctx.depth }.spanned(span),
                    }
                    .spanned(span),
                )
                .spanned(span),
            ))
        } else {
            Ok(CandleExpr::Reference(
                candle::expr::Reference::Global(
                    candle::expr::GlobalVar { repr, run_uid }.spanned(span),
                )
                .spanned(span),
            ))
        }
    }
}

impl Translate for lus::expr::LitExpr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = CandleExpr;
    fn translate(self, _run_uid: usize, span: Span, _ctx: Self::Ctx<'_>) -> Result<CandleExpr> {
        use syn::Lit;
        let lit = match self.lit.t {
            Lit::Bool(b) => candle::expr::Lit::Bool(b.value()),
            Lit::Int(i) => candle::expr::Lit::Int(i.base10_parse().expect("Failed to parse Int")),
            Lit::Float(f) => {
                candle::expr::Lit::Float(f.base10_parse().expect("Failed to parse Float"))
            }
            _ => return Err(err::UnhandledLitType { site: span }.into_err()),
        };
        Ok(CandleExpr::Lit(lit.spanned(span)))
    }
}
