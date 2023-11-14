//! Verify the consistency of clocks within and between nodes.
//!
//! The main challenge here is the handling of tuples and functions, because
//! we need to have relative clocks and clock substitutions.
//! A lot of the difficulty in that comes from the fact that we want this
//! to be allowed:
//! `node foo() returns (b; i when b)`
//! This means that the output of `foo` must be a tuple where the second
//! element is clocked by the first, and as soon as we assign it to
//! a variable binding `(x, y) = foo()` we need to instanciate the type
//! as `y when x`.
//!
//! To this end this module operates on the following core types
//! - `AbsoluteClk` is a named clock, which is what we have for variables
//!   and function arguments,
//! - `RelativeClk` allows specifying dependencies within one tuple,
//! - `MappingClk` can additionally specify relations between an input
//!   tuple and an output tuple.
//!
//! Here are some examples to develop the intuition of how these are encoded:
//! In `var x; y when x` we have
//! - `x`: `AbsoluteClk::Implicit`
//! - `y`: `AbsoluteClk::Explicit("x")`
//!
//! In `(x, y when x, z when y)` we have
//! - `x`: `RelativeClk::Implicit`
//! - `y`: `RelativeClk::Nth(0)` (0 is the index of `x`)
//! - `z`: `RelativeClk::Nth(1)` (1 is the index of `y`)
//!
//! In `node foo(x; u when x) returns (y; z when x; w when y)` we have
//! - `x`: `RelativeClk::Implicit`
//! - `u`: `RelativeClk::Nth(0)` (0 is the index of `x`)
//! - `y`: `MappingClk::Implicit`
//! - `z`: `MappingClk::NthIn(0)` (0 is the index of `x`)
//! - `w`: `MappingClk::NthOut(0)` (0 is the index of `y`)
//!
//! In all of the above there is one additional layer to the clock that states
//! whether the dependency is positive (`when`) or negative (`whenot`).
//!
//! Once these types are in place and because each of them sufficiently
//! restricts the representable constraitts, most of the rest is straightforward
//! bookkeeping to translate from one representation to another.

#![allow(dead_code)] // FIXME

use std::collections::HashMap;

use chandeliers_err::{self as err, EAccum, Transparent};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, op, stmt, ty, var};
use crate::sp::{Sp, Span, WithSpan};

macro_rules! at {
    ($arr:expr, $idx:expr) => {
        match $arr.get($idx) {
            Some(x) => x,
            None => err::abort!(
                "Created a relative index of invalid size: index {} on tuple of length {}",
                $idx,
                $arr.len()
            ),
        }
    };
}

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
/// A generic wrapper for objects that have two canonical `Span`s,
/// one at the definition site and one at the call site.
#[derive(Clone, PartialEq, Eq, Debug)]
#[expect(dead_code)] // FIXME
struct WithDefSite<T> {
    /// Payload.
    data: Sp<T>,
    /// Place where it was defined.
    def_site: Transparent<Option<Span>>,
}

impl<T> WithDefSite<T> {
    /// No relevant span to associate.
    fn without(data: Sp<T>) -> Self {
        Self {
            data,
            def_site: Transparent::from(None),
        }
    }

    /// Attach an additional span to an object.
    fn with_def_site(mut self, span: Span) -> Self {
        self.def_site = Transparent::from(Some(span));
        self
    }

    /// Transfer a def site from one object to another.
    fn with_def_site_of<Other>(mut self, other: &WithDefSite<Other>) -> Self {
        self.def_site = other.def_site;
        self
    }
}

crate::sp::derive_with_span!(Clocked<T> where <T>);
#[derive(Debug, Clone, PartialEq, Eq)]
struct Clocked<Clk> {
    clk: Sp<Clk>,
    sign: bool,
}

impl<Clk> Clocked<Clk> {
    fn on(clk: Sp<Clk>) -> Self {
        Self { clk, sign: true }
    }

    fn off(clk: Sp<Clk>) -> Self {
        Self { clk, sign: false }
    }

    fn map<Clk2, F>(self, f: F) -> Clocked<Clk2>
    where
        F: FnOnce(Sp<Clk>) -> Sp<Clk2>,
    {
        Clocked {
            clk: f(self.clk),
            sign: self.sign,
        }
    }

    fn as_ref(&self) -> Clocked<&Clk> {
        Clocked {
            clk: self.clk.as_ref(),
            sign: self.sign,
        }
    }
}

crate::sp::derive_with_span!(AbsoluteClk);
#[derive(Debug, Clone, PartialEq, Eq)]
enum AbsoluteClk {
    Implicit,
    Adaptative,
    Explicit(WithDefSite<String>),
}

crate::sp::derive_with_span!(RelativeClk);
#[derive(Debug, Clone, PartialEq, Eq)]
enum RelativeClk {
    Extern,
    Nth(usize),
}

crate::sp::derive_with_span!(MappingClk);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MappingClk {
    Extern,
    NthIn(usize),
    NthOut(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Named<T> {
    name: Option<Sp<String>>,
    data: Sp<T>,
}

crate::sp::derive_with_span!(FlatTup<T> where <T>);
#[derive(Debug, Clone, PartialEq, Eq)]
struct FlatTup<T> {
    global: AbsoluteClk,
    elems: Vec<Sp<T>>,
}

crate::sp::derive_with_span!(ClkTup);
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClkTup {
    Single(Named<Clocked<AbsoluteClk>>),
    Multiple(FlatTup<ClkTup>),
}

crate::sp::derive_with_span!(ClkMap);
/// Helper struct to map input clocks to output clocks.
///
/// Because of the fixed dependency order where variables can only have
/// a clock that comes strictly before in the order of
/// inputs then outputs then locals and in increasing index of each tuple,
/// the correct way to build a `ClkMap` is to obtain a new one
/// with `ClkMap::default()` and then for each input to
/// declare a `map.new_input(name, clock)` and finally once all
/// inputs have been added to use `map.new_output(name, clock)`.
///
/// Once such a map is built it can be used any number of times to transslate
/// from an input tuple to an output tuple.
#[derive(Debug, Default, Clone)]
struct ClkMap {
    /// Useful only during building: reverse access from names to provided clocks.
    named_inputs: HashMap<Sp<String>, Sp<RelativeClk>>,
    named_outputs: HashMap<Sp<String>, Sp<MappingClk>>,
    /// Clocks used by the inputs, in order.
    inputs: Vec<Sp<Clocked<RelativeClk>>>,
    /// Clocks used by the outputs, in order.
    outputs: Vec<Sp<Clocked<MappingClk>>>,
}

// We must implement conversions between all these types of clocks.
// AbsoluteClk, RelativeClk<AbsoluteClk>, RelativeClk<()>, MappingClk<()>
// ClkTup, ClkMap, Clocks

impl AbsoluteClk {
    fn into_relative(
        self,
        span: Span,
        named_elems: &HashMap<Sp<String>, Sp<RelativeClk>>,
    ) -> Option<Sp<RelativeClk>> {
        match &self {
            Self::Implicit | Self::Adaptative => Some(RelativeClk::Extern.with_span(span)),
            Self::Explicit(name) => match named_elems.get(&name.data) {
                Some(idx) => Some(idx.clone()),
                None => Some(RelativeClk::Extern.with_span(span)),
            },
        }
    }

    fn into_mapping(
        self,
        span: Span,
        named_inputs: &HashMap<Sp<String>, Sp<RelativeClk>>,
        named_outputs: &HashMap<Sp<String>, Sp<MappingClk>>,
    ) -> Option<Sp<MappingClk>> {
        match &self {
            Self::Implicit | Self::Adaptative => Some(MappingClk::Extern.with_span(span)),
            Self::Explicit(name) => named_inputs
                .get(&name.data)
                .map(|c| c.t.clone().into_mapping(c.span))
                .or_else(|| named_outputs.get(&name.data).cloned()),
        }
    }
}

impl RelativeClk {
    fn into_mapping(self, span: Span) -> Sp<MappingClk> {
        match self {
            Self::Nth(i) => MappingClk::NthIn(i),
            Self::Extern => MappingClk::Extern,
        }
        .with_span(span)
    }
}

impl ty::Clock {
    fn into_absolute(&self, span: Span) -> Clocked<AbsoluteClk> {
        let sign = match self {
            Self::Explicit { activation, .. } => *activation,
            _ => true,
        };
        let clk = match self {
            Self::Implicit => AbsoluteClk::Implicit.with_span(span),
            Self::Adaptative => AbsoluteClk::Adaptative.with_span(span),
            Self::Explicit { id, .. } => {
                AbsoluteClk::Explicit(WithDefSite::without(id.clone()).with_def_site(id.span))
                    .with_span(id.span)
            }
        }; // FIXME: better def site
        Clocked { clk, sign }
    }
}

impl<T> Clocked<Option<T>> {
    fn transpose(self) -> Option<Clocked<T>> {
        Some(Clocked {
            sign: self.sign,
            clk: self.clk.transpose()?,
        })
    }
}

impl Clocked<AbsoluteClk> {
    fn into_relative(
        self,
        span: Span,
        named_elems: &HashMap<Sp<String>, Sp<RelativeClk>>,
    ) -> Option<Sp<Clocked<RelativeClk>>> {
        let clk = self.clk.t.into_relative(self.clk.span, named_elems)?;
        Some(
            Clocked {
                clk,
                sign: self.sign,
            }
            .with_span(span),
        )
    }

    fn into_mapping(
        self,
        span: Span,
        named_inputs: &HashMap<Sp<String>, Sp<RelativeClk>>,
        named_outputs: &HashMap<Sp<String>, Sp<MappingClk>>,
    ) -> Option<Sp<Clocked<MappingClk>>> {
        let clk = self
            .clk
            .t
            .into_mapping(self.clk.span, named_inputs, named_outputs)?;
        Some(
            Clocked {
                clk,
                sign: self.sign,
            }
            .with_span(span),
        )
    }
}

impl ClkMap {
    /// Provide a new named argument in the input tuple.
    /// This handles both
    /// - determining what the argument is clocked by, and
    /// - registering the argument as one that may itself be a clock
    ///   for others.
    /// Note that this step of the compilation is *not* in charge
    /// of checking that the argument is indeed a boolean.
    fn new_input(&mut self, name: Sp<&String>, clk: Sp<&ty::Clock>) {
        let uid = self.inputs.len();
        let provides_clock = RelativeClk::Nth(uid).with_span(name.span);
        let clocked_by: Sp<Clocked<RelativeClk>> = clk
            .map(|span, c| {
                c.into_absolute(span)
                    .into_relative(span, &self.named_inputs)
            })
            .transpose()
            .unwrap()
            .t;
        self.named_inputs.insert(name.cloned(), provides_clock);
        self.inputs.push(clocked_by);
    }

    /// Provide a new named argument in the output tuple.
    /// This handles both
    /// - determining what the argument is clocked by, and
    /// - registering the argument as one that may itself be a clock
    ///   for others.
    /// Note that this step of the compilation is *not* in charge
    /// of checking that the argument is indeed a boolean.
    fn new_output(&mut self, name: Sp<&String>, clk: Sp<&ty::Clock>) {
        let uid = self.outputs.len();
        let provides_clock = MappingClk::NthOut(uid).with_span(name.span);
        let clocked_by: Sp<Clocked<MappingClk>> = clk
            .map(|span, c| {
                c.into_absolute(span)
                    .into_mapping(span, &self.named_inputs, &self.named_outputs)
            })
            .transpose()
            .unwrap()
            .t;
        self.named_outputs.insert(name.cloned(), provides_clock);
        self.outputs.push(clocked_by);
    }
}

/// Global clock context of clock mappings for each function.
#[derive(Default)]
struct ClkCtx {
    /// Maps a node to its clock transformer.
    signatures: HashMap<decl::NodeName, WithDefSite<ClkMap>>,
}

/// Local clock context, mapping local variables to their clocks.
struct ExprClkCtx<'i> {
    /// Known clocked variables.
    local: HashMap<var::Local, WithDefSite<Clock>>,
    global: &'i ClkCtx,
}

impl ExprClkCtx<'_> {
    /// Fetch the clock of a local variable (global constants have an implicit clock that we never
    /// check).
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Clock> {
        self.local
            .get(var)
            .unwrap_or_else(|| {
                err::abort!(
                "{var} is not in the context, which should have been detected during typechecking"
            )
            })
            .clone()
    }
}

/// Clock typing interface for expressions.
trait ClockCheckExpr {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'node, 'prog>
    where
        'prog: 'node;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_, '_>)
        -> Option<ClkTup>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'node, 'prog>
    where
        'prog: 'node;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_, '_>) -> Option<Sp<ClkTup>>;
}

/// Clock typing interface for statements.
trait ClockCheckStmt {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'node, 'prog>
    where
        'prog: 'node;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_, '_>) -> Option<()>;
}

/// Helper trait for `Sp` to implement `ClockCheckStmt`.
trait ClockCheckSpanStmt {
    /// Projection to the inner `Ctx`.
    type Ctx<'node, 'prog>
    where
        'prog: 'node;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_, '_>) -> Option<Sp<()>>;
}

/// Clock typing interface of declarations.
trait ClockCheckDecl {
    /// Clock context (typically obtained by the `signature` on other toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks and produce the signature.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: Self::Ctx<'_>) -> Option<ClkMap>;
}

/// Helper trait for `Sp` to implement `ClockCheckDecl`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<Sp<ClkMap>>;
}

/// Clock typing interface.
pub trait ClockCheck {
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, eaccum: &mut EAccum) -> Option<()>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'node, 'prog> = T::Ctx<'node, 'prog> where 'prog: 'node;
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: T::Ctx<'_, '_>) -> Option<Sp<ClkTup>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckExpr> ClockCheckExpr for Box<T> {
    type Ctx<'node, 'prog> = T::Ctx<'node, 'prog> where 'prog: 'node;
    fn clockcheck(&self, eaccum: &mut EAccum, span: Span, ctx: T::Ctx<'_, '_>) -> Option<ClkTup> {
        self.as_ref().clockcheck(eaccum, span, ctx)
    }
}

impl<T: ClockCheckStmt> ClockCheckSpanStmt for Sp<T> {
    type Ctx<'node, 'prog> = T::Ctx<'node, 'prog> where 'prog: 'node;
    fn clockcheck(&self, eaccum: &mut EAccum, ctx: T::Ctx<'_, '_>) -> Option<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckDecl> ClockCheckSpanDecl for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, eaccum: &mut EAccum, ctx: Self::Ctx<'_>) -> Option<Sp<ClkMap>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(eaccum, span, ctx))
            .transpose()
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        Some(ClkTup::Single(Named {
            name: None,
            data: Clocked {
                sign: true,
                clk: AbsoluteClk::Adaptative.with_span(span),
            }
            .with_span(span),
        }))
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        Some(ClkTup::Single(Named {
            name: None,
            data: Clocked {
                sign: true,
                clk: AbsoluteClk::Adaptative.with_span(span),
            }
            .with_span(span),
        }))
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        _eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        let implicit = ctx.clock_of(self);
        let abs = implicit.data.t.into_absolute(implicit.data.span);
        Some(ClkTup::Single(Named {
            name: Some(self.repr.clone()),
            data: abs.with_span(span),
        }))
    }
}

impl ClockCheckExpr for var::Past {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        Some(self.var.clockcheck(eaccum, ctx)?.t)
    }
}

impl ClockCheckExpr for var::Reference {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        _span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        match self {
            Self::Global(g) => Some(g.clockcheck(eaccum, ctx)?.t),
            Self::Var(v) => Some(v.clockcheck(eaccum, ctx)?.t),
        }
    }
}

impl Sp<AbsoluteClk> {
    fn assert_matches(&self, eaccum: &mut EAccum, other: &Self) -> Option<()> {
        match (&self.t, &other.t) {
            (AbsoluteClk::Adaptative, _)
            | (_, AbsoluteClk::Adaptative)
            | (AbsoluteClk::Implicit, AbsoluteClk::Implicit) => Some(()),
            (AbsoluteClk::Explicit(id1), AbsoluteClk::Explicit(id2)) => {
                if id1 == id2 {
                    Some(())
                } else {
                    eaccum.error(err::Basic {
                        msg: format!(
                            "Mismatch between clocks: {:?} on the left but {:?} on the right",
                            id1, id2
                        ),
                        span: self.span,
                    })
                }
            }
            _ => eaccum.error(err::Basic {
                msg: format!(
                    "Mismatch between clocks: {:?} on the left but {:?} on the right",
                    self, other,
                ),
                span: self.span,
            }),
        }
    }
}

impl Sp<Clocked<AbsoluteClk>> {
    fn assert_matches(&self, eaccum: &mut EAccum, other: &Self) -> Option<()> {
        if self.t.sign != other.t.sign {
            return eaccum.error(err::Basic {
                msg: "Clocks cannot match because they have different signs".to_owned(),
                span: self.span,
            });
        }
        self.t.clk.assert_matches(eaccum, &other.t.clk)
    }
}

impl Sp<ClkTup> {
    fn assert_matches(&self, eaccum: &mut EAccum, other: &Self) -> Option<()> {
        match (&self.t, &other.t) {
            (ClkTup::Single(clk1), ClkTup::Single(clk2)) => {
                clk1.data.assert_matches(eaccum, &clk2.data)
            }
            (ClkTup::Multiple(tup1), ClkTup::Multiple(tup2)) => {
                assert!(tup1.elems.len() == tup2.elems.len());
                for (el1, el2) in tup1.elems.iter().zip(tup2.elems.iter()) {
                    el1.assert_matches(eaccum, el2)?;
                }
                Some(())
            }
            _ => unreachable!(),
        }
    }
}

impl Sp<ClkTup> {
    fn as_single(self) -> Sp<Clocked<AbsoluteClk>> {
        match self.t {
            ClkTup::Single(c) => c.data.with_span(self.span),
            ClkTup::Multiple(_) => unreachable!(),
        }
    }
}

impl Sp<Clocked<AbsoluteClk>> {
    fn into_clock(self, op: op::Clock) -> Sp<Clocked<AbsoluteClk>> {
        match self.t {
            ClkTup::Single(c) => Clocked {
                sign: op == op::Clock::When,
                clk: AbsoluteClk::Explicit(WithDefSite::without(c.name.unwrap()))
                    .with_span(self.span),
            }
            .with_span(self.span),
            _ => unreachable!(),
        }
    }
}

impl Clocked<AbsoluteClk> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        new: &Sp<Clocked<AbsoluteClk>>,
        ctx: &mut ExprClkCtx<'_>,
    ) -> Option<Self> {
        unimplemented!()
    }
}

impl Named<Clocked<AbsoluteClk>> {
    fn reclock(
        self,
        eaccum: &mut EAccum,
        new: &Sp<Clocked<AbsoluteClk>>,
        ctx: &mut ExprClkCtx<'_>,
    ) -> Option<Self> {
        Some(Self {
            name: None,
            data: self.data.reclock(eaccum, new, ctx)?,
        })
    }
}

impl Sp<ClkTup> {
    fn as_flat(self, eaccum: &mut EAccum) -> Option<Sp<FlatTup<Clocked<RelativeClk>>>> {
        unimplemented!("ClkTup as_flat")
    }

    fn reclock(
        self,
        eaccum: &mut EAccum,
        new: &Sp<Clocked<AbsoluteClk>>,
        ctx: &mut ExprClkCtx<'_>,
    ) -> Option<Self> {
        match self.t {
            ClkTup::Single(n) => {
                Some(ClkTup::Single(n.reclock(eaccum, new, ctx)?).with_span(self.span))
            }
            ClkTup::Multiple(tup) => tup.reclock(eaccum, new, ctx),
        }
    }
}

impl ClockCheckExpr for expr::Expr {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        match self {
            Self::Lit(l) => Some(l.clockcheck(eaccum, ctx)?.t),
            Self::Reference(r) => Some(r.clockcheck(eaccum, ctx)?.t),
            Self::Bin { op, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let lhs = lhs?.as_single();
                let rhs = rhs?.as_single();
                lhs.assert_matches(eaccum, &rhs)?;
                Some(ClkTup::Single(Named {
                    name: None,
                    data: lhs,
                }))
            }
            Self::Cmp { op, lhs, rhs } => {
                let lhs = lhs.clockcheck(eaccum, ctx);
                let rhs = rhs.clockcheck(eaccum, ctx);
                let lhs = lhs?.as_single();
                let rhs = rhs?.as_single();
                lhs.assert_matches(eaccum, &rhs)?;
                Some(ClkTup::Single(Named {
                    name: None,
                    data: lhs,
                }))
            }
            Self::Un { op, inner } => {
                let inner = inner.clockcheck(eaccum, ctx);
                let inner = inner?.as_single();
                Some(ClkTup::Single(Named {
                    name: None,
                    data: inner,
                }))
            }
            Self::Later {
                clk: _,
                before,
                after,
            } => {
                let before = before.clockcheck(eaccum, ctx);
                let after = after.clockcheck(eaccum, ctx);
                let before = before?;
                let after = after?;
                before.assert_matches(eaccum, &after)?;
                Some(before.t)
            }
            Self::Clock {
                op,
                inner,
                activate,
            } => {
                let inner = inner.clockcheck(eaccum, ctx);
                let activate = activate.clockcheck(eaccum, ctx)?.as_single();
                let inner = inner?;
                let activate = activate.into_clock(*op);
                Some(inner.reclock(eaccum, &activate, ctx)?.t)
            }
            Self::Substep { clk: _, id, args } => {
                let args = args.clockcheck(eaccum, ctx)?.as_flat(eaccum)?;
                let fsig = ctx
                    .global
                    .signatures
                    .get(&decl::NodeName {
                        repr: id.t.repr.clone(),
                        run_uid: Transparent::forge(),
                    })
                    .unwrap();
                if &args.t.elems != &fsig.data.t.inputs {
                    panic!()
                }
                let out = fsig.data.t.outputs.clone();
                let out = FlatTup {
                    global: args.t.global.clone(),
                    elems: out,
                };
                Some(out.resolve(eaccum, args.t)?)
            }
            _ => unimplemented!("{:?}", self),
        }
    }
}

impl FlatTup<Clocked<MappingClk>> {
    fn resolve(self, eaccum: &mut EAccum, inputs: FlatTup<Clocked<RelativeClk>>) -> Option<ClkTup> {
        unimplemented!("FlatTup resolve")
    }
}

impl Sp<AbsoluteClk> {
    fn is_implicit(&self, eaccum: &mut EAccum) -> Option<()> {
        match &self.t {
            AbsoluteClk::Implicit | AbsoluteClk::Adaptative => Some(()),
            AbsoluteClk::Explicit(e) => eaccum.error(err::Basic {
                msg: format!("This clock is too slow (clocked by {})", e.data),
                span: self.span,
            }),
        }
    }
}

impl Sp<Clocked<AbsoluteClk>> {
    fn is_implicit(&self, eaccum: &mut EAccum) -> Option<()> {
        if !self.t.sign {
            eaccum.error(err::Basic {
                msg: "This clock is negative, expected the positive implicit clock".to_owned(),
                span: self.span,
            })
        } else {
            self.t.clk.is_implicit(eaccum)
        }
    }
}

impl ClockCheckExpr for stmt::VarTuple {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(
        &self,
        eaccum: &mut EAccum,
        span: Span,
        ctx: Self::Ctx<'_, '_>,
    ) -> Option<ClkTup> {
        match self {
            Self::Single(v) => Some(v.clockcheck(eaccum, ctx)?.t),
            Self::Multiple(tup) => {
                unimplemented!("VarTuple {tup:?}")
            }
        }
    }
}

impl ClockCheckStmt for stmt::Statement {
    type Ctx<'node, 'prog> = &'node mut ExprClkCtx<'prog> where 'prog: 'node;
    fn clockcheck(&self, eaccum: &mut EAccum, _: Span, ctx: Self::Ctx<'_, '_>) -> Option<()> {
        match self {
            Self::Assert(e) => {
                let clk = e.clockcheck(eaccum, ctx)?.as_single();
                clk.is_implicit(eaccum)?;
                Some(())
            }
            Self::Let { target, source } => {
                let target = target.clockcheck(eaccum, ctx)?;
                let source = source.clockcheck(eaccum, ctx)?;
                target.assert_matches(eaccum, &source)
            }
        }
    }
}

impl ClockCheckDecl for decl::Node {
    type Ctx<'i> = &'i mut ClkCtx;
    fn clockcheck(&self, eaccum: &mut EAccum, _span: Span, ctx: Self::Ctx<'_>) -> Option<ClkMap> {
        // First the interface
        let interface = {
            let mut map = ClkMap::default();
            for i in self.inputs.t.iter() {
                map.new_input(i.t.name.t.repr.as_ref(), i.t.ty.t.ty.t.clk.as_ref());
            }
            for o in self.outputs.t.iter() {
                map.new_output(o.t.name.t.repr.as_ref(), o.t.ty.t.ty.t.clk.as_ref());
            }
            map
        };
        // Now the body
        let mut ectx = ExprClkCtx {
            local: HashMap::new(),
            global: ctx,
        };
        for vs in [&self.inputs.t, &self.outputs.t, &self.locals.t] {
            for v in vs.iter() {
                ectx.local.insert(
                    v.t.name.t.clone(),
                    WithDefSite::without(v.t.ty.t.ty.t.clk.clone()).with_def_site(v.span),
                );
            }
        }
        for stmt in &self.stmts {
            stmt.clockcheck(eaccum, &mut ectx)?;
        }
        Some(interface)
    }
}

impl ClockCheckDecl for decl::ExtNode {
    type Ctx<'i> = ();
    fn clockcheck(&self, _eaccum: &mut EAccum, _: Span, (): ()) -> Option<ClkMap> {
        let mut map = ClkMap::default();
        for i in self.inputs.t.iter() {
            map.new_input(i.t.name.t.repr.as_ref(), i.t.ty.t.ty.t.clk.as_ref());
        }
        for o in self.outputs.t.iter() {
            map.new_output(o.t.name.t.repr.as_ref(), o.t.ty.t.ty.t.clk.as_ref());
        }
        Some(map)
    }
}

impl ClockCheck for Sp<decl::Prog> {
    fn clockcheck(&self, eaccum: &mut EAccum) -> Option<()> {
        let mut ctx = ClkCtx::default();
        for decl in &self.t.decls {
            match &decl.t {
                decl::Decl::Const(_) | decl::Decl::ExtConst(_) => {}
                decl::Decl::Node(n) => {
                    let sig = n.clockcheck(eaccum, &mut ctx)?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
                decl::Decl::ExtNode(n) => {
                    let sig = n.clockcheck(eaccum, ())?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
            }
        }
        Some(())
    }
}

#[cfg(test)]
mod translation_tests {
    use super::*;

    macro_rules! sp {
        ( $x:expr ) => {
            $x.with_span(Span::forge())
        };
    }
    macro_rules! defsite {
        ( $x:expr ) => {
            WithDefSite::without($x)
        };
    }

    macro_rules! forge {
        ( & $T:tt : $($x:tt)* ) => { forge!($T:$($x)*).as_ref() };
        ( str : $s:expr ) => { String::from($s).with_span(Span::forge()) };
        ( relclk : = . ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(()))), sign: true } };
        ( relclk : = ? ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(AbsoluteClk::Implicit))), sign: true } };
        ( relclk : = $c:expr ) => { Clocked { clk: sp!(RelativeClk::Implicit(sp!(AbsoluteClk::Explicit(defsite!(forge!(str:$c)))))), sign: true }.with_span(Span::forge()) };
        ( relclk : + $c:expr ) => { sp!(Clocked { clk: sp!(RelativeClk::Nth($c)), sign: true }) };
        ( relclk : - $c:expr ) => { sp!(Clocked { clk: sp!(RelativeClk::Nth($c)), sign: false }) };
        ( mapclk : = ) => { Clocked { clk: sp!(MappingClk::Implicit(sp!(()))), sign: true } };
        ( mapclk : <+ $c:expr ) => { Clocked { clk: sp!(MappingClk::NthIn($c)), sign: true } };
        ( mapclk : <- $c:expr ) => { Clocked { clk: sp!(MappingClk::NthIn($c)), sign: false } };
        ( mapclk : +> $c:expr ) => { Clocked { clk: sp!(MappingClk::NthOut($c)), sign: true } };
        ( mapclk : -> $c:expr ) => { Clocked { clk: sp!(MappingClk::NthOut($c)), sign: false } };
        ( tyclk : = ) => { sp!(ty::Clock::Implicit) };
        ( tyclk : + $s:expr ) => { sp!(ty::Clock::Explicit { id: forge!(str:$s), activation: true }) };
        ( tyclk : - $s:expr ) => { sp!(ty::Clock::Explicit { id: forge!(str:$s), activation: false }) };
        ( abs : + ? ) => { Clocked { clk: sp!(AbsoluteClk::Implicit), sign: true } };
        ( abs : + $name:expr ) => { Clocked { clk: sp!(AbsoluteClk::Explicit(defsite!(forge!(str:$name)))), sign: true } };
    }

    #[test]
    fn simple_io() {
        // Inserting `(t0, t1 when t0, t2 whenot t1, t3)`
        // And then we follow with the output tuple `(t4, t5 when t1, t6 when t4)`
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"t0"), forge!(&tyclk:=));
            map.new_input(forge!(&str:"t1"), forge!(&tyclk:+"t0"));
            map.new_input(forge!(&str:"t2"), forge!(&tyclk:-"t1"));
            map.new_input(forge!(&str:"t3"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"t4"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"t5"), forge!(&tyclk:+"t1"));
            map.new_output(forge!(&str:"t6"), forge!(&tyclk:+"t4"));
            map
        };
        assert_eq!(
            map.inputs,
            [
                sp!(forge!(relclk:=.)),
                sp!(forge!(relclk:+0)),
                sp!(forge!(relclk:-1)),
                sp!(forge!(relclk:=.))
            ]
        );
        assert_eq!(
            map.outputs,
            [
                sp!(forge!(mapclk:=)),
                sp!(forge!(mapclk:<+1)),
                sp!(forge!(mapclk:+>0))
            ]
        );
    }

    #[test]
    #[should_panic]
    fn undeclared_input() {
        // Trying to insert a clock that does not correspond to an already
        // declared variable.
        let _map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"t0"), forge!(&tyclk:+"t0"));
        };
    }

    #[test]
    fn empty_mapping() {
        // () -> ()
        let mut eaccum = EAccum::new();
        let map = {
            let map = ClkMap::default();
            map
        };
        // Preserves the implicit clock
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![],
            }
        );
        // Preserves an explicit clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![],
            }
        );
    }

    #[test]
    fn singleton_mapping() {
        let mut eaccum = EAccum::new();
        // (x) -> (y)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:=));
            map
        };
        // Recognizes a tuple element going at the right speed
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="c"))],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![(None, forge!(relclk:="c"))],
            }
        );
        // Preserves the implicit clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), sp!(forge!(relclk:=?)))],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![(None, sp!(forge!(relclk:=?)))],
            }
        );
        // Rejects a mismatch between the tuple's speed and the first element's speed
        let itup3 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="x"))],
        };
        assert!(map
            .translate(&mut eaccum, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn depends_on_input() {
        let mut eaccum = EAccum::new();
        // (x) -> (y when x)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:+"x"));
            map
        };
        // Transports whatever name `x` has.
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(Some(forge!(str:"x0")), sp!(forge!(relclk:=?)))],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+?)),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Including if `x` itself is clocked.
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"z")),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="z"))],
        };
        let otup2 = map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            Clocks {
                implicit: sp!(forge!(abs:+"z")),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Fails if the input is not named.
        let itup3 = Clocks {
            implicit: sp!(forge!(abs:+?)),
            clocks: vec![(None, sp!(forge!(relclk:=?)))],
        };
        assert!(map
            .translate(&mut eaccum, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn transports_autoref() {
        let mut eaccum = EAccum::new();
        // (x1, x2 when x1) -> (y1, y2 whenot y1)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x1"), forge!(&tyclk:=));
            map.new_input(forge!(&str:"x2"), forge!(&tyclk:+"x1"));
            map.new_output(forge!(&str:"y1"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y2"), forge!(&tyclk:-"y1"));
            map
        };
        // Normal application.
        let itup1 = Clocks {
            implicit: sp!(forge!(abs:+"c")),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="c")),
                (Some(forge!(str:"a2")), forge!(relclk:+0)),
            ],
        };
        let otup1 = map
            .translate(&mut eaccum, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            Clocks {
                implicit: sp!(forge!(abs:+"c")),
                clocks: vec![(None, forge!(relclk:="c")), (None, forge!(relclk:-0)),],
            }
        );
        // Second element has the wrong clock
        let itup2 = Clocks {
            implicit: sp!(forge!(abs:+"z")),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="z")),
                (Some(forge!(str:"a2")), forge!(relclk:="z")),
            ],
        };
        assert!(map
            .translate(&mut eaccum, itup2.with_span(Span::forge()))
            .is_none());
    }
}
