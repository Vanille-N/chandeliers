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
//! - `pre (e1 or e2) ~~> (pre e1) or (pre e2)`
//! - `pre (e1 ->{n} e2) ~~> (pre e1) ->{n-1} (pre e2)`
//! - etc...
//!
//! To this end the translator carries in its context the current depth at
//! with the expression is evaluated. Usually "regular" (`+`, `-`, `or`, ...)
//! operators propagate the same depth to their arguments, and temporal operators
//! act upon the depth: `pre` increases it by one, `fby` evaluates its two arguments
//! on different depths, etc.
//!
//! Alternatively, temporal operators are translated differently when
//! the `universal_pre` option is set, and this generates `Register` and `Flip`
//! instead of `Pre` and `Then`.

use std::collections::HashSet;

use chandeliers_err::{self as err, EAccum, Transparent};
use chandeliers_san::ast as tgt;
use chandeliers_san::sp::{Sp, Span, WithSpan};

use crate::ast as src;

mod options;

/// Current compiler pass for options manager (see [`chandeliers_san::ast::options`] for details).
type This = tgt::options::usage::Parsing;

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
    ///
    /// # Errors
    /// The contents cannot be translated.
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output>;
}

/// Helper trait to translate from the parsing AST to the analysis AST.
/// Only `Sp<T>` is expected to implement this trait.
#[expect(
    clippy::module_name_repetitions,
    reason = "False positive: we want the name to match the `Translate` trait."
)]
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
    ///
    /// # Errors
    /// The contents cannot be translated.
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        ctx: Self::Ctx<'_>,
    ) -> Option<Sp<Self::Output>>;
    /// Same as `translate`, but do now wrap in a span.
    /// This helps for very nested structures that don't want a `Sp<_>` at every level.
    ///
    /// # Errors
    /// The contents cannot be translated.
    fn flat_translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output>;
}

/// `Sp<_>` is transparently translatable, but it makes its span available to the inner translation
/// function.
impl<T> SpanTranslate for Sp<T>
where
    T: Translate,
{
    type Ctx<'i> = T::Ctx<'i>;
    type Output = T::Output;

    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        ctx: Self::Ctx<'_>,
    ) -> Option<Sp<Self::Output>> {
        let res = self.t.translate(eaccum, run_uid, self.span, ctx)?;
        Some(Sp {
            t: res,
            span: self.span,
        })
    }

    fn flat_translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        self.t.translate(eaccum, run_uid, self.span, ctx)
    }
}

/// Accumulator of detected dependent types.
///
/// We find these in the input and output clocks and we're going
/// to insert sode code to make them not appear as unused variables.
type DepTyAccum<'i> = &'i mut Vec<Sp<tgt::var::Local>>;

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
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        (*self).translate(eaccum, run_uid, span, ctx)
    }
}

impl Translate for src::Prog {
    type Ctx<'i> = ();
    type Output = tgt::decl::Prog;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        (): (),
    ) -> Option<tgt::decl::Prog> {
        let mut decls = Vec::new();
        for decl in self.decls {
            decls.push(decl.translate(eaccum, run_uid, options::Decl::default())?);
        }
        Some(tgt::decl::Prog { decls })
    }
}

impl Translate for src::AttrDecl {
    type Ctx<'i> = options::Decl;
    type Output = tgt::decl::Decl;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: options::Decl,
    ) -> Option<tgt::decl::Decl> {
        match self {
            Self::Tagged(attr, n) => {
                let options = options.with(eaccum, attr.map(|_, x| *x))?;
                n.flat_translate(eaccum, run_uid, options)
            }
            Self::Node(n) => n.flat_translate(eaccum, run_uid, options),
        }
    }
}

impl Translate for src::Decl {
    type Ctx<'i> = options::Decl;
    type Output = tgt::decl::Decl;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: options::Decl,
    ) -> Option<tgt::decl::Decl> {
        Some(match self {
            Self::Const(c) => {
                let options = options.for_const(eaccum)?;
                tgt::decl::Decl::Const(c.translate(eaccum, run_uid, options)?)
            }
            Self::Node(n) => {
                let options = options.for_node(eaccum)?;
                tgt::decl::Decl::Node(n.translate(eaccum, run_uid, options)?)
            }
            Self::Extern(e) => e.flat_translate(eaccum, run_uid, options)?,
        })
    }
}

impl Translate for src::Extern {
    type Ctx<'i> = options::Decl;
    type Output = tgt::decl::Decl;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: options::Decl,
    ) -> Option<tgt::decl::Decl> {
        Some(match self {
            Self::Const(c) => {
                let options = options.for_ext_const(eaccum)?;
                tgt::decl::Decl::ExtConst(c.translate(eaccum, run_uid, options)?)
            }
            Self::Node(n) => {
                let options = options.for_ext_node(eaccum)?;
                tgt::decl::Decl::ExtNode(n.translate(eaccum, run_uid, options)?)
            }
        })
    }
}

impl Translate for src::ExtConst {
    type Ctx<'i> = tgt::options::ExtConst;
    type Output = tgt::decl::ExtConst;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: tgt::options::ExtConst,
    ) -> Option<Self::Output> {
        let name = self.name.map(|span, name| tgt::var::Global {
            repr: name.to_string().with_span(span),
            run_uid,
        });
        let ty = self.ty.translate(eaccum, run_uid, &mut Vec::new())?;
        Some(tgt::decl::ExtConst {
            name,
            ty: ty.t.inner,
            options,
        })
    }
}

impl Translate for src::ExtNode {
    type Ctx<'i> = tgt::options::ExtNode;
    type Output = tgt::decl::ExtNode;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: tgt::options::ExtNode,
    ) -> Option<tgt::decl::ExtNode> {
        let name = self.name.map(|span, name| tgt::decl::NodeName {
            repr: name.to_string().with_span(span),
            run_uid,
        });
        let inputs = self.inputs.translate(eaccum, run_uid, &mut Vec::new())?;
        let outputs = self.outputs.translate(eaccum, run_uid, &mut Vec::new())?;
        Some(tgt::decl::ExtNode {
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
    /// List of blocks to append to whenever there is a function call.
    blocks: Vec<tgt::decl::NodeInstance>,
    /// List of statements to append to whenever a new statement is complete.
    stmts: Vec<Sp<tgt::stmt::Statement>>,
    /// Local variable names that might shadow global variables.
    shadow_glob: HashSet<String>,
    /// Are we using the temporal operators with buffers or registers ?
    use_registers: bool,
    /// List of registers, if any.
    /// A register is a struct that stores a value from one iteration to the next.
    registers: Vec<tgt::decl::RegisterInstance>,
    /// List of flips, if any.
    /// A flip is a 0-then-1 boolean to perform an initialization exactly once.
    flips: Vec<tgt::decl::FlipInstance>,
}

/// Accessor for `ExprCtx`.
/// This is not `Copy` or even `Clone`, so if you need to duplicate it
/// you should use the `fork!` macro that will reborrow a fresh copy.
pub struct ExprCtxView<'i> {
    /// List of blocks to append to whenever there is a function call.
    blocks: &'i mut Vec<tgt::decl::NodeInstance>,
    /// List of statements to append to whenever a new statement is complete.
    stmts: &'i mut Vec<Sp<tgt::stmt::Statement>>,
    /// Local variable names that might shadow global variables.
    shadow_glob: &'i HashSet<String>,
    /// Current depth of the translation (i.e. number of `pre` in front),
    /// influences the production of `var::Past` with the current value.
    depth: usize,
    /// Are we using the temporal operators with buffers or registers ?
    use_registers: bool,
    /// List of registers, if any.
    registers: &'i mut Vec<tgt::decl::RegisterInstance>,
    /// List of flips, if any.
    flips: &'i mut Vec<tgt::decl::FlipInstance>,
}

impl ExprCtx {
    /// Create a new accessor.
    fn view(&mut self) -> ExprCtxView<'_> {
        ExprCtxView {
            blocks: &mut self.blocks,
            stmts: &mut self.stmts,
            shadow_glob: &self.shadow_glob,
            depth: 0,
            use_registers: self.use_registers,
            registers: &mut self.registers,
            flips: &mut self.flips,
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

/// Reborrow a view on an `ExprCtx`.
macro_rules! fork {
    ($this:expr) => {
        crate::translate::ExprCtxView {
            blocks: &mut *$this.blocks,
            stmts: &mut *$this.stmts,
            shadow_glob: &*$this.shadow_glob,
            registers: &mut *$this.registers,
            flips: &mut *$this.flips,
            depth: $this.depth,
            use_registers: $this.use_registers,
        }
    };
}

impl Translate for src::Node {
    type Ctx<'i> = tgt::options::Node;
    type Output = tgt::decl::Node;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        options: tgt::options::Node,
    ) -> Option<Self::Output> {
        let name = self.name.map(|span, name| tgt::decl::NodeName {
            repr: name.to_string().with_span(span),
            run_uid,
        });
        let mut deptys = Vec::default();
        let inputs = self.inputs.translate(eaccum, run_uid, &mut deptys)?;
        let outputs = self.outputs.translate(eaccum, run_uid, &mut deptys)?;
        let locals = self.locals.translate(eaccum, run_uid, &mut deptys)?;
        let mut ectx = ExprCtx {
            use_registers: *options.universal_pre.fetch::<This>(),
            ..Default::default()
        };
        for shadows in &[&inputs, &outputs, &locals] {
            for s in shadows.t.iter() {
                ectx.shadow_glob.insert(s.t.name.t.repr.t.clone());
            }
        }

        for def in self.defs {
            def.translate(eaccum, run_uid, ectx.view())?;
        }
        let ExprCtx {
            blocks,
            stmts,
            registers,
            flips,
            ..
        } = ectx;
        Some(tgt::decl::Node {
            name,
            options,
            inputs,
            outputs,
            locals,
            blocks,
            deptys,
            stmts,
            registers,
            flips,
        })
    }
}

impl Translate for src::Const {
    type Ctx<'i> = tgt::options::Const;
    type Output = tgt::decl::Const;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        options: tgt::options::Const,
    ) -> Option<tgt::decl::Const> {
        let name = self.name.map(|span, name| tgt::var::Global {
            repr: name.to_string().with_span(span),
            run_uid,
        });
        let ty = self.ty.translate(eaccum, run_uid, ())?;
        let mut ectx = ExprCtx::default();
        let value = self.value.translate(eaccum, run_uid, ectx.view())?;
        if !ectx.stmts.is_empty() || !ectx.blocks.is_empty() {
            eaccum.error(err::NotConst {
                site: span,
                what: "Function calls are",
            })?;
        }
        Some(tgt::decl::Const {
            name,
            options,
            ty,
            value,
        })
    }
}

impl Translate for src::ArgsTys {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::Tuple<Sp<tgt::decl::TyVar>>;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::Tuple<Sp<tgt::decl::TyVar>>> {
        let mut vs = tgt::Tuple::default();
        for item in self.items {
            item.translate(eaccum, run_uid, (&mut vs, ctx))?;
        }
        Some(vs)
    }
}

impl Translate for src::ArgsTy {
    type Ctx<'i> = (&'i mut tgt::Tuple<Sp<tgt::decl::TyVar>>, DepTyAccum<'i>);
    type Output = ();
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<()> {
        let ty = self.ty.translate(eaccum, run_uid, ctx.1)?;
        self.args.translate(eaccum, run_uid, (ctx.0, ty))?;
        Some(())
    }
}

impl Translate for src::Decls {
    type Ctx<'i> = (
        &'i mut tgt::Tuple<Sp<tgt::decl::TyVar>>,
        Sp<tgt::ty::Clocked<tgt::ty::Base>>,
    );
    type Output = ();
    fn translate(
        self,
        _eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        _span: Span,
        (vars, ty): Self::Ctx<'_>,
    ) -> Option<()> {
        for id in self.ids {
            vars.push(id.map(|span, id| {
                tgt::decl::TyVar {
                    name: tgt::var::Local {
                        repr: id.to_string().with_span(span),
                    }
                    .with_span(span),
                    ty: ty.clone().map(|span, ty| {
                        tgt::ty::Stream {
                            ty: ty.with_span(span),
                            depth: tgt::past::Depth {
                                // Putting in a dummy value 0 for `dt`, don't forget to update
                                // it by depth propagation in the positivity check...
                                dt: 0,
                            }
                            .with_span(span),
                        }
                    }),
                }
            }));
        }
        Some(())
    }
}

impl Translate for src::ty::Type {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::ty::Clocked<tgt::ty::Base>;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        let inner = self.base.translate(eaccum, run_uid, ())?;
        let clk = self.clock.translate(eaccum, run_uid, ctx)?;
        Some(tgt::ty::Clocked { inner, clk })
    }
}

impl Translate for src::ty::Base {
    type Ctx<'i> = ();
    type Output = tgt::ty::Base;
    fn translate(
        self,
        _eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        _span: Span,
        (): (),
    ) -> Option<Self::Output> {
        Some(match self {
            Self::Int(_) => tgt::ty::Base::Int,
            Self::Float(_) => tgt::ty::Base::Float,
            Self::Bool(_) => tgt::ty::Base::Bool,
            Self::Other(t) => tgt::ty::Base::Other(t.as_ref().map(|_, c| format!("{c}"))),
        })
    }
}

impl Translate for src::ty::Clock {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::ty::Clock;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        match self {
            Self::When(w) => w.flat_translate(eaccum, run_uid, ctx),
            Self::Whenot(w) => w.flat_translate(eaccum, run_uid, ctx),
            Self::None => Some(tgt::ty::Clock::Implicit),
        }
    }
}

impl Translate for src::ty::When {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::ty::Clock;
    fn translate(
        self,
        _eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        ctx.push(
            tgt::var::Local {
                repr: self.clock.as_ref().map(|_, c| format!("{c}")),
            }
            .with_span(self.clock.span),
        );
        Some(tgt::ty::Clock::Explicit {
            activation: true,
            id: self.clock.map(|_, c| format!("{c}")),
        })
    }
}

impl Translate for src::ty::Whenot {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::ty::Clock;
    fn translate(
        self,
        _eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        ctx.push(
            tgt::var::Local {
                repr: self.clock.as_ref().map(|_, c| format!("{c}")),
            }
            .with_span(self.clock.span),
        );
        Some(tgt::ty::Clock::Explicit {
            activation: false,
            id: self.clock.map(|_, c| format!("{c}")),
        })
    }
}

impl Translate for src::OptionalVarsDecl {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::Tuple<Sp<tgt::decl::TyVar>>;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        match self {
            Self::Decls(d) => d.flat_translate(eaccum, run_uid, ctx),
            Self::None => Some(tgt::Tuple::default()),
        }
    }
}

impl Translate for src::VarsDecl {
    type Ctx<'i> = DepTyAccum<'i>;
    type Output = tgt::Tuple<Sp<tgt::decl::TyVar>>;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<Self::Output> {
        self.decls.flat_translate(eaccum, run_uid, ctx)
    }
}

impl Translate for src::Statement {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<()> {
        match self {
            Self::Assert(ass) => ass.flat_translate(eaccum, run_uid, ctx),
            Self::Def(def) => def.flat_translate(eaccum, run_uid, ctx),
        }
    }
}

impl Translate for src::Assertion {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<()> {
        let e = self.expr.translate(eaccum, run_uid, fork!(ctx))?;
        ctx.stmts
            .push(tgt::stmt::Statement::Assert(e).with_span(span));
        Some(())
    }
}

impl Translate for src::Def {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = ();
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<()> {
        let target = self.target.translate(eaccum, run_uid, ())?;
        let source = self.source.translate(eaccum, run_uid, fork!(ctx))?;
        ctx.stmts
            .push(tgt::stmt::Statement::Let { target, source }.with_span(span));
        Some(())
    }
}

impl Translate for src::TargetExpr {
    type Ctx<'i> = ();
    type Output = tgt::stmt::VarTuple;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        (): (),
    ) -> Option<Self::Output> {
        match self {
            Self::Var(i) => Some(tgt::stmt::VarTuple::Single(
                tgt::var::Local {
                    repr: i.map(|_, t| t.to_string()),
                }
                .with_span(span),
            )),
            Self::Tuple(ts) => ts.flat_translate(eaccum, run_uid, ()),
        }
    }
}

impl Translate for src::TargetExprTuple {
    type Ctx<'i> = ();
    type Output = tgt::stmt::VarTuple;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        (): (),
    ) -> Option<Self::Output> {
        let mut vs = tgt::Tuple::default();
        let trail = self.fields.trailing_punct();
        for t in self.fields {
            vs.push(t.translate(eaccum, run_uid, ())?);
        }
        if vs.len() == 1 && !trail {
            Some(
                vs.into_iter()
                    .next()
                    .unwrap_or_else(|| chandeliers_err::malformed!())
                    .t,
            )
        } else {
            Some(tgt::stmt::VarTuple::Multiple(vs.with_span(span)))
        }
    }
}

impl Translate for src::Expr {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        self.inner.flat_translate(eaccum, run_uid, ctx)
    }
}

impl Translate for src::expr::If {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let cond = self.cond.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        let yes = self.yes.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        let no = self.no.translate(eaccum, run_uid, ctx)?.boxed();
        Some(tgt::expr::Expr::Ifx { cond, yes, no })
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
    use super::{Sp, WithSpan};
    use chandeliers_err as err;
    use std::fmt::Display;
    use syn::punctuated::{Pair, Punctuated};

    /// Descriptor for a translation.
    pub struct Descr<'i, Label, Convert, Compose> {
        /// Parent context.
        pub ctx: super::ExprCtxView<'i>,
        /// Text to print if there is an error. Typically the name of the type
        /// being translated.
        pub label: Label,
        /// How to embed the type of elements in expressions.
        pub convert: Convert,
        /// How to combine two expressions into one.
        pub compose: Compose,
    }

    impl<'i, Label, Convert, Compose> Descr<'i, Label, Convert, Compose>
    where
        Label: Display,
    {
        /// Build the tree of left associative operations from a flat representation.
        ///
        /// Notice the type of `Compose`: the accumulator is to the left.
        pub fn left_associative<Elem, Punct, Accum, Item>(
            &mut self,
            elems: Punctuated<Sp<Elem>, Punct>,
        ) -> Option<Accum>
        where
            Convert: FnMut(Sp<Elem>, usize, super::ExprCtxView<'_>) -> Option<Sp<Item>>,
            Compose: FnMut(Sp<Accum>, Punct, usize, Sp<Item>, super::ExprCtxView<'_>) -> Accum,
            Accum: WithSpan<Output = Sp<Accum>>,
            Item: Into<Accum>,
        {
            // We always assume that there is a trailing _element_, not a trailing punctuation.
            chandeliers_err::consistency!(
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
            let Some((depth, fst)) = pairs.next() else {
                chandeliers_err::abort!("Should not be able to get an empty `Pairs` from a `Punctuated::parse_nonempty`");
            };
            match fst {
                Pair::Punctuated(elem, punct) => {
                    accum = (self.convert)(elem, depth, fork!(self.ctx))?.map(|_, i| i.into());
                    oper = punct;
                }
                Pair::End(elem) => {
                    return Some((self.convert)(elem, depth, fork!(self.ctx))?.t.into());
                }
            }
            // Looping case.
            for (depth, pair) in pairs {
                match pair {
                    // One more element: add it to the accumulator and record
                    // the punctuation to be used in the next iteration.
                    Pair::Punctuated(elem, punct) => {
                        let expr = (self.convert)(elem, depth, fork!(self.ctx))?;
                        let Some(span) = expr.span.join(accum.span) else {
                            chandeliers_err::abort!(
                                "Malformed span between {:?} and {:?}",
                                expr.span,
                                accum.span
                            );
                        };
                        accum = (self.compose)(accum, oper, depth, expr, fork!(self.ctx))
                            .with_span(span);
                        oper = punct;
                    }
                    // End: combine now then return the accumulator.
                    Pair::End(elem) => {
                        let expr = (self.convert)(elem, depth, fork!(self.ctx))?;
                        return Some((self.compose)(accum, oper, depth, expr, fork!(self.ctx)));
                    }
                }
            }
            err::malformed!()
        }

        /// Build the tree of right associative operations from a flat representation.
        ///
        /// Notice the type of `Compose`: the accumulator is to the right.
        pub fn right_associative<Elem, Punct, Accum, Item>(
            &mut self,
            elems: Punctuated<Sp<Elem>, Punct>,
        ) -> Option<Accum>
        where
            Accum: WithSpan<Output = Sp<Accum>>,
            Convert: FnMut(Sp<Elem>, usize, super::ExprCtxView<'_>) -> Option<Sp<Item>>,
            Compose: FnMut(Sp<Item>, Punct, usize, Sp<Accum>, super::ExprCtxView<'_>) -> Accum,
            Item: Into<Accum>,
        {
            // We always assume that there is a trailing _element_, not a trailing punctuation.
            chandeliers_err::consistency!(
                !elems.trailing_punct(),
                "Bug in the parser: {} should not accept trailing punctuation",
                self.label
            );

            let mut pairs = elems.into_pairs().enumerate().rev();
            let mut accum: Sp<Accum>;
            // Because we've reversed the iterator, the first element is always
            // and `End`.
            let Some((depth, last)) = pairs.next() else {
                chandeliers_err::abort!("Should not be able to get an empty `Pairs` from a `Punctuated::parse_nonempty`");
            };

            let Pair::End(elem) = last else {
                err::malformed!()
            };
            accum = (self.convert)(elem, depth, fork!(self.ctx))?.map(|_, i| i.into());
            // Looping case.
            // WARNING: contrary to the left associative case, `End` is not
            // the end! In fact `End` is unreachable because it has already
            // been seen in the handling of `last`.
            for (depth, pair) in pairs {
                let Pair::Punctuated(elem, punct) = pair else {
                    err::malformed!()
                };
                let expr = (self.convert)(elem, depth, fork!(self.ctx))?;
                let Some(span) = expr.span.join(accum.span) else {
                    chandeliers_err::abort!(
                        "Malformed span between {:?} and {:?}",
                        expr.span,
                        accum.span
                    );
                };
                accum = (self.compose)(expr, punct, depth, accum, fork!(self.ctx)).with_span(span);
            }
            Some(accum.t)
        }
    }
}

impl Translate for src::expr::Or {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        assoc::Descr {
            ctx,
            label: "OrExpr",
            convert: |elem: Sp<src::expr::And>, _depth, ctx: ExprCtxView| {
                elem.translate(eaccum, run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<tgt::expr::Expr>,
                      _op,
                      _depth,
                      rhs: Sp<tgt::expr::Expr>,
                      _ctx: ExprCtxView| {
                tgt::expr::Expr::Bin {
                    op: tgt::op::Bin::BitOr,
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for src::expr::And {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        assoc::Descr {
            ctx,
            label: "AndExpr",
            convert: |elem: Sp<src::expr::Cmp>, _depth, ctx: ExprCtxView| {
                elem.translate(eaccum, run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<tgt::expr::Expr>,
                      _op,
                      _depth,
                      rhs: Sp<tgt::expr::Expr>,
                      _ctx: ExprCtxView| {
                tgt::expr::Expr::Bin {
                    op: tgt::op::Bin::BitAnd,
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for src::expr::Not {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let inner = self.inner.translate(eaccum, run_uid, ctx)?.boxed();
        Some(tgt::expr::Expr::Un {
            op: tgt::op::Un::Not,
            inner,
        })
    }
}

impl Translate for src::expr::Cmp {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        use syn::punctuated::Pair;
        chandeliers_err::consistency!(
            !self.items.trailing_punct(),
            "Bug in the parser: Comparisons should not accept trailing punctuation",
        );
        let mut it = self.items.into_pairs();
        // We must have a first element
        let Some(first) = it.next() else {
            err::abort!("Bug in the parser: comparison should have at least one member")
        };
        let Some(second) = it.next() else {
            // If we don't have a second element then this is just dropping
            // to the level below.
            // The first one can't have punctuation
            let Pair::End(first) = first else {
                err::malformed!()
            };
            return first.flat_translate(eaccum, run_uid, ctx);
        };
        let Pair::Punctuated(lhs, op) = first else {
            err::malformed!()
        };
        let lhs = lhs.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        let op = op.translate(eaccum, run_uid, span, ())?;
        // We must not have a third element.
        if let Some(third) = it.next() {
            // We're going to throw this path anyway, we might as well
            // make destructive changes to get a better error message.
            let Pair::Punctuated(second, oper2) = second else {
                err::malformed!()
            };
            let second = second.translate(eaccum, run_uid, fork!(ctx))?;
            let oper2 = oper2.translate(eaccum, run_uid, span, ())?;
            let third = match third {
                Pair::Punctuated(third, _) | Pair::End(third) => third,
            };
            let third = third.translate(eaccum, run_uid, fork!(ctx))?;
            return eaccum.error(err::CmpNotAssociative {
                site: span,
                first: lhs,
                oper1: op,
                oper2,
                second,
                third,
            });
        }
        let Pair::End(rhs) = second else {
            err::malformed!()
        };
        let rhs = rhs.translate(eaccum, run_uid, ctx)?.boxed();
        Some(tgt::expr::Expr::Cmp { op, lhs, rhs })
    }
}

impl Translate for src::op::Cmp {
    type Ctx<'i> = ();
    type Output = tgt::op::Cmp;
    fn translate(
        self,
        _eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        _span: Span,
        (): (),
    ) -> Option<Self::Output> {
        Some(match self {
            Self::Le(_) => tgt::op::Cmp::Le,
            Self::Lt(_) => tgt::op::Cmp::Lt,
            Self::Ge(_) => tgt::op::Cmp::Ge,
            Self::Gt(_) => tgt::op::Cmp::Gt,
            Self::Eq(_) => tgt::op::Cmp::Eq,
            Self::Ne(_) => tgt::op::Cmp::Ne,
        })
    }
}

impl Translate for src::expr::Fby {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        if ctx.use_registers {
            assoc::Descr {
                ctx,
                label: "FbyExpr",
                convert: |elem: Sp<src::expr::Then>, _depth, ctx: ExprCtxView| {
                    elem.translate(eaccum, run_uid, fork!(ctx) /* DO NOT BUMP */)
                },
                compose: |before: Sp<tgt::expr::Expr>,
                          _op,
                          _depth,
                          after: Sp<tgt::expr::Expr>,
                          ctx: ExprCtxView| {
                    let id: Sp<tgt::var::Register> = tgt::var::Register {
                        id: ctx.registers.len().with_span(span),
                    }
                    .with_span(span);
                    ctx.registers
                        .push(tgt::decl::RegisterInstance { id, typ: None });
                    ctx.stmts.push(
                        tgt::stmt::Statement::UpdateRegister {
                            id,
                            val: after.clone(),
                        }
                        .with_span(span),
                    );
                    ctx.stmts.push(
                        tgt::stmt::Statement::InitRegister {
                            id,
                            val: Some(before.clone()),
                            clk: None,
                        }
                        .with_span(span),
                    );

                    tgt::expr::Expr::FetchRegister {
                        id,
                        dummy_init: Some(before.boxed()),
                        dummy_followed_by: after.boxed(),
                    }
                },
            }
            .right_associative(self.items)
        } else {
            assoc::Descr {
                ctx,
                label: "FbyExpr",
                convert: |elem: Sp<src::expr::Then>, depth, ctx: ExprCtxView| {
                    elem.translate(eaccum, run_uid, fork!(ctx).bump(depth) /* BUMP */)
                },
                compose: |before: Sp<tgt::expr::Expr>,
                          _op,
                          depth,
                          after: Sp<tgt::expr::Expr>,
                          ctx: ExprCtxView<'_>| {
                    tgt::expr::Expr::Later {
                        delay: tgt::past::Depth {
                            dt: ctx.depth + depth, /* INCREASE DEPTH */
                        }
                        .with_span(
                            before.span.join(after.span).unwrap_or_else(|| {
                                err::abort!("Malformed span between {before:?} and {after:?}")
                            }),
                        ),
                        before: before.boxed(),
                        after: after.boxed(),
                    }
                },
            }
            .right_associative(self.items)
        }
    }
}

impl Translate for src::expr::Pre {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        if ctx.use_registers {
            let id = tgt::var::Register {
                id: ctx.registers.len().with_span(span),
            }
            .with_span(span);
            ctx.registers
                .push(tgt::decl::RegisterInstance { id, typ: None });
            // We don't increment the depth here because nodes with
            // registers don't have variables in the past.
            let inner = self.inner.translate(eaccum, run_uid, fork!(ctx))?;
            ctx.stmts.push(
                tgt::stmt::Statement::UpdateRegister {
                    id,
                    val: inner.clone(),
                }
                .with_span(span),
            );
            ctx.stmts.push(
                tgt::stmt::Statement::InitRegister {
                    id,
                    val: None,
                    clk: None,
                }
                .with_span(span),
            );
            Some(tgt::expr::Expr::FetchRegister {
                id,
                dummy_init: None,
                dummy_followed_by: inner.boxed(),
            })
        } else {
            Some(tgt::expr::Expr::DummyPre(
                self.inner.translate(eaccum, run_uid, ctx.incr())?.boxed(),
            ))
        }
    }
}

impl Translate for src::expr::Then {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        if ctx.use_registers {
            assoc::Descr {
                ctx,
                label: "ThenExpr",
                convert: |elem: Sp<src::expr::Add>, _depth, ctx: ExprCtxView| {
                    elem.translate(eaccum, run_uid, fork!(ctx) /* DO NOT BUMP */)
                },
                compose: |before: Sp<tgt::expr::Expr>,
                          _op,
                          _depth,
                          after: Sp<tgt::expr::Expr>,
                          ctx: ExprCtxView| {
                    let id: Sp<tgt::var::Flip> = tgt::var::Flip {
                        id: ctx.flips.len().with_span(span),
                    }
                    .with_span(span);
                    ctx.flips.push(tgt::decl::FlipInstance { id });
                    tgt::expr::Expr::Flip {
                        id,
                        initial: before.boxed(),
                        continued: after.boxed(),
                    }
                },
            }
            .right_associative(self.items)
        } else {
            // This looks similar to `FbyExpr`, but notice how we aren't using `depth`
            // in the same way.
            assoc::Descr {
                ctx,
                label: "ThenExpr",
                convert: |elem: Sp<src::expr::Add>, _depth, ctx: ExprCtxView| {
                    elem.translate(eaccum, run_uid, fork!(ctx) /* DO NOT BUMP */)
                },
                compose: |before: Sp<tgt::expr::Expr>,
                          _op,
                          depth,
                          after: Sp<tgt::expr::Expr>,
                          ctx: ExprCtxView<'_>| {
                    tgt::expr::Expr::Later {
                        delay: tgt::past::Depth {
                            dt: ctx.depth + depth,
                        }
                        .with_span(
                            before.span.join(after.span).unwrap_or_else(|| {
                                chandeliers_err::abort!(
                                    "Malformed span between {before:?} and {after:?}"
                                )
                            }),
                        ),
                        before: before.boxed(),
                        after: after.boxed(),
                    }
                },
            }
            .right_associative(self.items)
        }
    }
}

impl Translate for src::expr::Add {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        // Back to normal left assaciative stuff that doesn't care about the depth,
        // but this time we have to handle the fact that there are multiple possible operators.
        assoc::Descr {
            ctx,
            label: "AddExpr",
            convert: |elem: Sp<src::expr::Mul>, _depth, ctx: ExprCtxView| {
                elem.translate(eaccum, run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<tgt::expr::Expr>,
                      op: src::op::Add,
                      _depth,
                      rhs: Sp<tgt::expr::Expr>,
                      _ctx: ExprCtxView| {
                tgt::expr::Expr::Bin {
                    op: op.translate(),
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl src::op::Add {
    /// Convert an additive operator from one AST to the other.
    fn translate(self) -> tgt::op::Bin {
        match self {
            Self::Add(_) => tgt::op::Bin::Add,
            Self::Sub(_) => tgt::op::Bin::Sub,
        }
    }
}

impl src::op::Mul {
    /// Convert a multiplicative operator from one AST to the other.
    fn translate(self) -> tgt::op::Bin {
        match self {
            Self::Mul(_) => tgt::op::Bin::Mul,
            Self::Div(_) => tgt::op::Bin::Div,
            Self::Rem(_) => tgt::op::Bin::Rem,
        }
    }
}

impl Translate for src::expr::Mul {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        assoc::Descr {
            ctx,
            label: "MulExpr",
            convert: |elem: Sp<src::expr::Clock>, _depth, ctx: ExprCtxView| {
                elem.translate(eaccum, run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<tgt::expr::Expr>,
                      op: src::op::Mul,
                      _depth,
                      rhs: Sp<tgt::expr::Expr>,
                      _ctx: ExprCtxView| {
                tgt::expr::Expr::Bin {
                    op: op.translate(),
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl src::op::Clock {
    /// Convert a clock operator from one AST to the other.
    fn translate(self) -> tgt::op::Clock {
        match self {
            Self::When(_) => tgt::op::Clock::When,
            Self::Whenot(_) => tgt::op::Clock::Whenot,
        }
    }
}

impl Translate for src::expr::Clock {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        assoc::Descr {
            ctx,
            label: "ClockExpr",
            convert: |elem: Sp<src::expr::Positive>, _depth, ctx: ExprCtxView| {
                elem.translate(eaccum, run_uid, fork!(ctx))
            },
            compose: |lhs: Sp<tgt::expr::Expr>,
                      op: src::op::Clock,
                      _depth,
                      rhs: Sp<tgt::expr::Expr>,
                      _ctx: ExprCtxView| {
                tgt::expr::Expr::Clock {
                    op: op.translate(),
                    inner: lhs.boxed(),
                    activate: rhs.boxed(),
                }
            },
        }
        .left_associative(self.items)
    }
}

impl Translate for src::expr::Neg {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let inner = self.inner.translate(eaccum, run_uid, ctx)?.boxed();
        Some(tgt::expr::Expr::Un {
            op: tgt::op::Un::Neg,
            inner,
        })
    }
}

impl Translate for src::expr::Paren {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let mut es = tgt::Tuple::default();
        let trail = self.inner.trailing_punct();
        for e in self.inner {
            es.push(e.translate(eaccum, run_uid, fork!(ctx))?);
        }
        if es.len() == 1 && !trail {
            Some(tgt::expr::Expr::DummyParen(
                es.into_iter()
                    .next()
                    .unwrap_or_else(|| err::malformed!())
                    .boxed(),
            ))
        } else {
            Some(tgt::expr::Expr::Tuple(es.with_span(span)))
        }
    }
}

impl Translate for src::expr::Call {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let args = self.args.translate(eaccum, run_uid, fork!(ctx))?;
        let repr = self.fun.map(|_, t| t.to_string());
        let id = tgt::var::Node {
            id: ctx.blocks.len().with_span(span),
            repr: repr.clone(),
        }
        .with_span(span);
        ctx.blocks.push(tgt::decl::NodeInstance {
            name: tgt::decl::NodeName { repr, run_uid }.with_span(span),
            generics: None,
        });
        Some(tgt::expr::Expr::Substep {
            delay: ctx.depth,
            id: id.clone(),
            args: args.boxed(),
        })
    }
}

impl Translate for src::expr::Atomic {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        match self {
            Self::Lit(l) => l.flat_translate(eaccum, run_uid, ctx),
            Self::Var(v) => v.flat_translate(eaccum, run_uid, ctx),
            Self::Paren(p) => p.flat_translate(eaccum, run_uid, ctx),
        }
    }
}

impl Translate for src::expr::Positive {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        match self {
            Self::If(i) => i.flat_translate(eaccum, run_uid, ctx),
            Self::Merge(m) => m.flat_translate(eaccum, run_uid, ctx),
            Self::Call(c) => c.flat_translate(eaccum, run_uid, ctx),
            Self::Pre(p) => p.flat_translate(eaccum, run_uid, ctx),
            Self::Neg(n) => n.flat_translate(eaccum, run_uid, ctx),
            Self::Not(n) => n.flat_translate(eaccum, run_uid, ctx),
            Self::Atomic(a) => a.flat_translate(eaccum, run_uid, ctx),
        }
    }
}

impl Translate for src::expr::Merge {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        _span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let switch = self.clk.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        let on = self.on.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        let off = self.off.translate(eaccum, run_uid, fork!(ctx))?.boxed();
        Some(tgt::expr::Expr::Merge { switch, on, off })
    }
}

impl Translate for src::expr::Var {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        _eaccum: &mut EAccum,
        run_uid: Transparent<usize>,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        let repr = self.name.map(|_, t| t.to_string());
        if ctx.shadow_glob.contains(&repr.t) {
            Some(tgt::expr::Expr::Reference(
                tgt::var::Reference::Var(
                    tgt::var::Past {
                        var: tgt::var::Local { repr }.with_span(span),
                        depth: tgt::past::Depth { dt: ctx.depth }.with_span(span),
                    }
                    .with_span(span),
                )
                .with_span(span),
            ))
        } else {
            Some(tgt::expr::Expr::Reference(
                tgt::var::Reference::Global(tgt::var::Global { repr, run_uid }.with_span(span))
                    .with_span(span),
            ))
        }
    }
}

impl Translate for src::expr::Lit {
    type Ctx<'i> = ExprCtxView<'i>;
    type Output = tgt::expr::Expr;
    fn translate(
        self,
        eaccum: &mut EAccum,
        _run_uid: Transparent<usize>,
        span: Span,
        _ctx: Self::Ctx<'_>,
    ) -> Option<tgt::expr::Expr> {
        use syn::Lit;
        let lit = match self.lit.t {
            Lit::Bool(b) => tgt::expr::Lit::Bool(b.value()),
            Lit::Int(i) => tgt::expr::Lit::Int(
                i.base10_parse()
                    .unwrap_or_else(|e| err::abort!("Unable to parse {i} as an int: {e}")),
            ),
            Lit::Float(f) => tgt::expr::Lit::Float(
                f.base10_parse()
                    .unwrap_or_else(|e| err::abort!("Unable to parse {f} as a float: {e}")),
            ),
            _ => eaccum.error(err::UnhandledLitType { site: span })?,
        };
        Some(tgt::expr::Expr::Lit(lit.with_span(span)))
    }
}
