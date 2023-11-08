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

use chandeliers_err::{self as err, Acc};

use crate::ast::ty::Clock;
use crate::ast::{decl, expr, ty, var};
use crate::sp::{Sp, Span, WithSpan};

crate::sp::derive_with_span!(WithDefSite<T> where <T>);
/// A generic wrapper for objects that have two canonical `Span`s,
/// one at the definition site and one at the call site.
#[derive(Clone)]
#[expect(dead_code)] // FIXME
struct WithDefSite<T> {
    /// Payload.
    data: Sp<T>,
    /// Place where it was defined.
    def_site: Option<Span>,
}

impl<T> WithDefSite<T> {
    /// No relevant span to associate.
    fn without(data: Sp<T>) -> Self {
        Self {
            data,
            def_site: None.into(),
        }
    }

    /// Attach an additional span to an object.
    fn with_def_site(mut self, span: Span) -> Self {
        self.def_site = Some(span);
        self
    }
}

crate::sp::derive_with_span!(Clocked<T> where <T>);
#[derive(Debug, Clone, PartialEq, Eq)]
struct Clocked<Clk> {
    clk: Clk,
    sign: bool,
}

impl<Clk> Clocked<Clk> {
    fn on(clk: Clk) -> Self {
        Self { clk, sign: true }
    }

    fn off(clk: Clk) -> Self {
        Self { clk, sign: false }
    }

    fn map<Clk2, F>(self, f: F) -> Clocked<Clk2>
    where
        F: FnOnce(Clk) -> Clk2,
    {
        Clocked {
            clk: f(self.clk),
            sign: self.sign,
        }
    }

    fn as_ref(&self) -> Clocked<&Clk> {
        Clocked {
            clk: &self.clk,
            sign: self.sign,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AbsoluteClk {
    Implicit,
    Adaptative,
    Explicit(Sp<String>),
}

crate::sp::derive_with_span!(RelativeClk<Ref> where <Ref>);
#[derive(Debug, Clone, PartialEq, Eq)]
enum RelativeClk<Ref> {
    Implicit(Ref),
    Nth(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MappingClk<Ref> {
    Implicit(Ref),
    NthIn(usize),
    NthOut(usize),
}

impl<Ref> MappingClk<Ref> {
    fn as_relative(self) -> Option<RelativeClk<Ref>> {
        match self {
            Self::Implicit(r) => Some(RelativeClk::Implicit(r)),
            Self::NthIn(n) => Some(RelativeClk::Nth(n)),
            Self::NthOut(_) => None,
        }
    }
}

crate::sp::derive_with_span!(ClkTup);
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClkTup {
    implicit: AbsoluteClk,
    clocks: Vec<(Option<Sp<String>>, Sp<Clocked<RelativeClk<AbsoluteClk>>>)>,
}

crate::sp::derive_with_span!(ClkMap);
/// Helper struct to map input clocks to output clocks.
#[derive(Debug, Default, Clone)]
struct ClkMap {
    clock_of: HashMap<Sp<String>, MappingClk<()>>,
    inputs: Vec<Sp<Clocked<RelativeClk<()>>>>,
    outputs: Vec<Sp<Clocked<MappingClk<()>>>>,
}

impl AbsoluteClk {
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Adaptative, _) | (_, Self::Adaptative) | (Self::Implicit, Self::Implicit) => {
                true
            }
            (Self::Explicit(id1), Self::Explicit(id2)) => id1 == id2,
            _ => false,
        }
    }
}

impl RelativeClk<AbsoluteClk> {
    fn matches_absolute(&self, other: &AbsoluteClk) -> bool {
        match self {
            Self::Implicit(abs) => abs.matches(other),
            Self::Nth(_) => false,
        }
    }

    fn strictly_relative(&self) -> Option<usize> {
        match self {
            Self::Implicit(_) => None,
            Self::Nth(i) => Some(*i),
        }
    }
}

impl Clocked<RelativeClk<AbsoluteClk>> {
    fn matches_positive_absolute(&self, other: &AbsoluteClk) -> bool {
        self.sign && self.clk.matches_absolute(other)
    }
}

impl ClkMap {
    fn new_input(&mut self, name: Sp<&String>, clk: Sp<&ty::Clock>) {
        let uid = self.inputs.len();
        let provides_clock = MappingClk::NthIn(uid);
        let clocked_by = self
            .fetch_clock_for(clk)
            .map(|clk| clk.as_relative().unwrap())
            .with_span(clk.span);
        self.clock_of.insert(name.cloned(), provides_clock);
        self.inputs.push(clocked_by);
    }

    fn new_output(&mut self, name: Sp<&String>, clk: Sp<&ty::Clock>) {
        let uid = self.outputs.len();
        let provides_clock = MappingClk::NthOut(uid);
        let clocked_by = self.fetch_clock_for(clk).with_span(clk.span);
        self.clock_of.insert(name.cloned(), provides_clock);
        self.outputs.push(clocked_by);
    }

    fn fetch_clock_for(&self, clk: Sp<&ty::Clock>) -> Clocked<MappingClk<()>> {
        match &clk.t {
            ty::Clock::Implicit | ty::Clock::Adaptative => Clocked::on(MappingClk::Implicit(())),
            ty::Clock::Explicit { id, activation } => Clocked {
                clk: *self
                    .clock_of
                    .get(&id)
                    .unwrap_or_else(|| err::abort!("Malformed clock tuple: no such variable {id}")),
                sign: *activation,
            },
        }
    }

    fn translate(&self, acc: &mut Acc, tup_src: Sp<ClkTup>) -> Option<ClkTup> {
        let mut named: HashMap<Sp<String>, RelativeClk<()>> = HashMap::new();
        let implicit: AbsoluteClk = tup_src.t.implicit;
        err::consistency!(
            self.inputs.len() == tup_src.t.clocks.len(),
            "Wrong sized tuple for function arguments, should have been caught during typechecking"
        );
        for (idx, (found, expected)) in tup_src.t.clocks.iter().zip(&self.inputs).enumerate() {
            if found.1.t.matches_positive_absolute(&implicit) {
                // We expect the implicit clock
                if matches!(expected.t.clk, RelativeClk::Implicit(_)) {
                    err::consistency!(
                        expected.t.sign,
                        "Can only be positive w.r.t. the implicit clock"
                    );
                    if let Some(name) = &found.0 {
                        named.insert(name.clone(), RelativeClk::Nth(idx));
                    }
                } else {
                    acc.error(err::Basic {
                        msg: format!("Tuple element {} is going at the wrong speed", idx),
                        span: found.1.span,
                    })?;
                }
            } else {
                // Here we should find a clock that has been added previously
                // to the tuple.
                if let Some(src_clk) = found.1.t.clk.strictly_relative() {
                    assert!(found.1.t.sign == expected.t.sign);
                    match found.1.t.clk {
                        RelativeClk::Nth(found_clk) => assert_eq!(found_clk, src_clk),
                        _ => panic!(),
                    };
                } else {
                    acc.error(err::Basic {
                        msg: format!("Tuple element {} is going at the wrong speed", idx),
                        span: found.1.span,
                    })?;
                }
            }
        }
        let mut tup_tgt: Vec<(Option<Sp<String>>, Sp<Clocked<RelativeClk<AbsoluteClk>>>)> =
            Vec::new();
        for clk in &self.outputs {
            let replace = match &clk.t.clk {
                MappingClk::Implicit(()) => RelativeClk::Implicit(implicit.clone()),
                MappingClk::NthOut(i) => RelativeClk::Nth(*i),
                MappingClk::NthIn(i) => {
                    // If the input is named we can use it as clock,
                    // otherwise it fails
                    RelativeClk::Implicit(AbsoluteClk::Explicit(match &tup_src.t.clocks[*i].0 {
                        Some(name) => name.clone(),
                        None => acc.error(err::Basic {
                            msg: "Clock name is lost in the mapping, please bind it to a variable"
                                .to_owned(),
                            span: clk.span,
                        })?,
                    }))
                }
            };
            tup_tgt.push((
                None,
                Clocked {
                    clk: replace,
                    sign: clk.t.sign,
                }
                .with_span(clk.span),
            ));
        }
        Some(ClkTup {
            implicit,
            clocks: tup_tgt,
        })
    }
}

#[cfg(test)]
mod translation_tests {
    use super::*;

    macro_rules! forge {
        ( & $T:tt : $($x:tt)* ) => { forge!($T:$($x)*).as_ref() };
        ( str : $s:expr ) => { String::from($s).with_span(Span::forge()) };
        ( relclk : = . ) => { Clocked { clk: RelativeClk::Implicit(()), sign: true }.with_span(Span::forge()) };
        ( relclk : = ? ) => { Clocked { clk: RelativeClk::Implicit(AbsoluteClk::Implicit), sign: true }.with_span(Span::forge()) };
        ( relclk : = $c:expr ) => { Clocked { clk: RelativeClk::Implicit(AbsoluteClk::Explicit(forge!(str:$c))), sign: true }.with_span(Span::forge()) };
        ( relclk : + $c:expr ) => { Clocked { clk: RelativeClk::Nth($c), sign: true }.with_span(Span::forge()) };
        ( relclk : - $c:expr ) => { Clocked { clk: RelativeClk::Nth($c), sign: false }.with_span(Span::forge()) };
        ( mapclk : = ) => { Clocked { clk: MappingClk::Implicit(()), sign: true }.with_span(Span::forge()) };
        ( mapclk : <+ $c:expr ) => { Clocked { clk: MappingClk::NthIn($c), sign: true }.with_span(Span::forge()) };
        ( mapclk : <- $c:expr ) => { Clocked { clk: MappingClk::NthIn($c), sign: false }.with_span(Span::forge()) };
        ( mapclk : +> $c:expr ) => { Clocked { clk: MappingClk::NthOut($c), sign: true }.with_span(Span::forge()) };
        ( mapclk : -> $c:expr ) => { Clocked { clk: MappingClk::NthOut($c), sign: false }.with_span(Span::forge()) };
        ( tyclk : = ) => { ty::Clock::Implicit.with_span(Span::forge()) };
        ( tyclk : + $s:expr ) => { ty::Clock::Explicit { id: forge!(str:$s), activation: true }.with_span(Span::forge()) };
        ( tyclk : - $s:expr ) => { ty::Clock::Explicit { id: forge!(str:$s), activation: false }.with_span(Span::forge()) };
        ( abs : ? ) => { AbsoluteClk::Implicit };
        ( abs : $name:expr ) => { AbsoluteClk::Explicit(forge!(str:$name)) };
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
                forge!(relclk:=.),
                forge!(relclk:+0),
                forge!(relclk:-1),
                forge!(relclk:=.)
            ]
        );
        assert_eq!(
            map.outputs,
            [forge!(mapclk:=), forge!(mapclk:<+1), forge!(mapclk:+>0)]
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
        let mut acc = Acc::new();
        let map = {
            let map = ClkMap::default();
            map
        };
        // Preserves the implicit clock
        let itup1 = ClkTup {
            implicit: forge!(abs:?),
            clocks: vec![],
        };
        let otup1 = map
            .translate(&mut acc, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            ClkTup {
                implicit: forge!(abs:?),
                clocks: vec![],
            }
        );
        // Preserves an explicit clock
        let itup2 = ClkTup {
            implicit: forge!(abs:"c"),
            clocks: vec![],
        };
        let otup2 = map
            .translate(&mut acc, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            ClkTup {
                implicit: forge!(abs:"c"),
                clocks: vec![],
            }
        );
    }

    #[test]
    fn singleton_mapping() {
        let mut acc = Acc::new();
        // (x) -> (y)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:=));
            map
        };
        // Recognizes a tuple element going at the right speed
        let itup1 = ClkTup {
            implicit: forge!(abs:"c"),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="c"))],
        };
        let otup1 = map
            .translate(&mut acc, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            ClkTup {
                implicit: forge!(abs:"c"),
                clocks: vec![(None, forge!(relclk:="c"))],
            }
        );
        // Preserves the implicit clock
        let itup2 = ClkTup {
            implicit: forge!(abs:?),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:=?))],
        };
        let otup2 = map
            .translate(&mut acc, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            ClkTup {
                implicit: forge!(abs:?),
                clocks: vec![(None, forge!(relclk:=?))],
            }
        );
        // Rejects a mismatch between the tuple's speed and the first element's speed
        let itup3 = ClkTup {
            implicit: forge!(abs:?),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="x"))],
        };
        assert!(map
            .translate(&mut acc, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn depends_on_input() {
        let mut acc = Acc::new();
        // (x) -> (y when x)
        let map = {
            let mut map = ClkMap::default();
            map.new_input(forge!(&str:"x"), forge!(&tyclk:=));
            map.new_output(forge!(&str:"y"), forge!(&tyclk:+"x"));
            map
        };
        // Transports whatever name `x` has.
        let itup1 = ClkTup {
            implicit: forge!(abs:?),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:=?))],
        };
        let otup1 = map
            .translate(&mut acc, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            ClkTup {
                implicit: forge!(abs:?),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Including if `x` itself is clocked.
        let itup2 = ClkTup {
            implicit: forge!(abs:"z"),
            clocks: vec![(Some(forge!(str:"x0")), forge!(relclk:="z"))],
        };
        let otup2 = map
            .translate(&mut acc, itup2.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup2,
            ClkTup {
                implicit: forge!(abs:"z"),
                clocks: vec![(None, forge!(relclk:="x0"))],
            }
        );
        // Fails if the input is not named.
        let itup3 = ClkTup {
            implicit: forge!(abs:?),
            clocks: vec![(None, forge!(relclk:=?))],
        };
        assert!(map
            .translate(&mut acc, itup3.with_span(Span::forge()))
            .is_none());
    }

    #[test]
    fn transports_autoref() {
        let mut acc = Acc::new();
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
        let itup1 = ClkTup {
            implicit: forge!(abs:"c"),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="c")),
                (Some(forge!(str:"a2")), forge!(relclk:+0)),
            ],
        };
        let otup1 = map
            .translate(&mut acc, itup1.with_span(Span::forge()))
            .unwrap();
        assert_eq!(
            otup1,
            ClkTup {
                implicit: forge!(abs:"c"),
                clocks: vec![(None, forge!(relclk:="c")), (None, forge!(relclk:-0)),],
            }
        );
        // Second element has the wrong clock
        let itup2 = ClkTup {
            implicit: forge!(abs:"z"),
            clocks: vec![
                (Some(forge!(str:"a1")), forge!(relclk:="z")),
                (Some(forge!(str:"a2")), forge!(relclk:="z")),
            ],
        };
        assert!(map
            .translate(&mut acc, itup2.with_span(Span::forge()))
            .is_none());
    }
}

/// Global clock context of clock mappings for each function.
#[derive(Default)]
struct ClkCtx {
    /// Maps a node to its clock transformer.
    signatures: HashMap<decl::NodeName, WithDefSite<ClkMap>>,
}

/// Local clock context, mapping local variables to their clocks.
struct ExprClkCtx {
    /// Known clocked variables.
    interface: HashMap<var::Local, WithDefSite<Clock>>,
}

impl ExprClkCtx {
    /// Fetch the clock of a local variable (global constants have an implicit clock that we never
    /// check).
    fn clock_of(&self, var: &var::Local) -> WithDefSite<Clock> {
        self.interface
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
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(
        &self,
        acc: &mut Acc,
        span: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<WithDefSite<Clock>>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, acc: &mut Acc, ctx: Self::Ctx<'_>) -> Option<Sp<WithDefSite<Clock>>>;
}

/// Clock typing interface for statements.
trait ClockCheckStmt {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, acc: &mut Acc, span: Span, ctx: Self::Ctx<'_>) -> Option<()>;
}

/// Helper trait for `Sp` to implement `ClockCheckStmt`.
trait ClockCheckSpanStmt {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, acc: &mut Acc, ctx: Self::Ctx<'_>) -> Option<Sp<()>>;
}

/// Clock typing interface of declarations.
trait ClockCheckDecl {
    /// Clock context (typically obtained by the `signature` on other toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks and produce the signature.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, acc: &mut Acc, span: Span, ctx: Self::Ctx<'_>) -> Option<ClkMap>;
}

/// Helper trait for `Sp` to implement `ClockCheckDecl`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, acc: &mut Acc, ctx: Self::Ctx<'_>) -> Option<Sp<ClkMap>>;
}

/// Clock typing interface.
pub trait ClockCheck {
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, acc: &mut Acc) -> Option<()>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, acc: &mut Acc, ctx: T::Ctx<'_>) -> Option<Sp<WithDefSite<Clock>>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(acc, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckStmt> ClockCheckSpanStmt for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, acc: &mut Acc, ctx: T::Ctx<'_>) -> Option<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(acc, span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckDecl> ClockCheckSpanDecl for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, acc: &mut Acc, ctx: Self::Ctx<'_>) -> Option<Sp<ClkMap>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(acc, span, ctx))
            .transpose()
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'i> = ();
    fn clockcheck(&self, _acc: &mut Acc, span: Span, (): ()) -> Option<WithDefSite<Clock>> {
        Some(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'i> = ();
    fn clockcheck(&self, _acc: &mut Acc, span: Span, (): ()) -> Option<WithDefSite<Clock>> {
        Some(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'i> = &'i mut ExprClkCtx;
    fn clockcheck(
        &self,
        _acc: &mut Acc,
        _: Span,
        ctx: Self::Ctx<'_>,
    ) -> Option<WithDefSite<Clock>> {
        Some(ctx.clock_of(self))
    }
}

impl ClockCheckDecl for decl::Node {
    type Ctx<'i> = &'i mut ClkCtx;
    fn clockcheck(&self, _acc: &mut Acc, _span: Span, _ctx: Self::Ctx<'_>) -> Option<ClkMap> {
        Some(ClkMap::default()) // FIXME
    }
}

impl ClockCheckDecl for decl::ExtNode {
    type Ctx<'i> = ();
    fn clockcheck(&self, _acc: &mut Acc, _: Span, (): ()) -> Option<ClkMap> {
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
    fn clockcheck(&self, acc: &mut Acc) -> Option<()> {
        let mut ctx = ClkCtx::default();
        for decl in &self.t.decls {
            match &decl.t {
                decl::Decl::Const(_) | decl::Decl::ExtConst(_) => {}
                decl::Decl::Node(n) => {
                    let sig = n.clockcheck(acc, &mut ctx)?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
                decl::Decl::ExtNode(n) => {
                    let sig = n.clockcheck(acc, ())?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
            }
        }
        Some(())
    }
}
