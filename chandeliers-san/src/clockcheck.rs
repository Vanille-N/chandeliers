//! Verify the consistency of clocks within and between nodes.

#![allow(dead_code)] // FIXME

use std::collections::HashMap;

use chandeliers_err::{self as err, Result};

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum AbsoluteClk {
    Implicit,
    Adaptative,
    Explicit(Sp<String>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct ClkTup {
    implicit: AbsoluteClk,
    clocks: Vec<(Option<Sp<String>>, Clocked<RelativeClk<AbsoluteClk>>)>,
}

crate::sp::derive_with_span!(ClkMap);
/// Helper struct to map input clocks to output clocks.
#[derive(Debug, Default, Clone)]
struct ClkMap {
    clock_of: HashMap<Sp<String>, MappingClk<()>>,
    inputs: Vec<Clocked<RelativeClk<()>>>,
    outputs: Vec<Clocked<MappingClk<()>>>,
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
    fn new_input(&mut self, name: Option<Sp<&String>>, clk: Sp<&ty::Clock>) {
        let uid = self.inputs.len();
        let provides_clock = MappingClk::NthIn(uid);
        let clocked_by = self
            .fetch_clock_for(clk)
            .map(|clk| clk.as_relative().unwrap());
        if let Some(name) = name {
            self.clock_of.insert(name.cloned(), provides_clock);
        }
        self.inputs.push(clocked_by);
    }

    fn new_output(&mut self, name: Option<Sp<&String>>, clk: Sp<&ty::Clock>) {
        let uid = self.inputs.len();
        let provides_clock = MappingClk::NthOut(uid);
        let clocked_by = self.fetch_clock_for(clk);
        if let Some(name) = name {
            self.clock_of.insert(name.cloned(), provides_clock);
        }
        self.outputs.push(clocked_by);
    }

    fn fetch_clock_for(&self, clk: Sp<&ty::Clock>) -> Clocked<MappingClk<()>> {
        match &clk.t {
            ty::Clock::Implicit | ty::Clock::Adaptative => Clocked::on(MappingClk::Implicit(())),
            ty::Clock::Explicit { id, activation } => Clocked {
                clk: *self.clock_of.get(&id).unwrap(),
                sign: *activation,
            },
        }
    }

    fn translate(&self, tup_src: ClkTup) -> Result<ClkTup> {
        let mut named: HashMap<Sp<String>, RelativeClk<()>> = HashMap::new();
        let implicit: AbsoluteClk = tup_src.implicit;
        assert!(self.inputs.len() == tup_src.clocks.len(), "Wrong length");
        for (idx, (found, expected)) in tup_src.clocks.iter().zip(&self.inputs).enumerate() {
            if found.1.matches_positive_absolute(&implicit) {
                // We expect the implicit clock
                if matches!(expected.clk, RelativeClk::Implicit(_)) {
                    assert!(expected.sign, "Can only be positive on the implicit clock");
                    if let Some(name) = &found.0 {
                        named.insert(name.clone(), RelativeClk::Nth(idx));
                    }
                } else {
                    panic!("Expected the absolute clock here");
                }
            } else {
                // Here we should find a clock that has been added previously
                // to the tuple.
                if let Some(src_clk) = found.1.clk.strictly_relative() {
                    assert!(found.1.sign == expected.sign);
                    match found.1.clk {
                        RelativeClk::Nth(found_clk) => assert_eq!(found_clk, src_clk),
                        _ => panic!(),
                    };
                } else {
                    panic!("Wrong speed: {found:?}")
                }
            }
        }
        let tup_tgt: Vec<(Option<Sp<String>>, Clocked<RelativeClk<AbsoluteClk>>)> = self
            .outputs
            .iter()
            .map(|t| {
                (
                    None,
                    t.as_ref().map(|c| match c {
                        MappingClk::Implicit(()) => RelativeClk::Implicit(implicit.clone()),
                        MappingClk::NthOut(i) => RelativeClk::Nth(*i),
                        MappingClk::NthIn(i) => {
                            // If the input is named we can use it as clock,
                            // otherwise it fails
                            RelativeClk::Implicit(AbsoluteClk::Explicit(
                                tup_src.clocks[*i].0.clone().unwrap(),
                            ))
                        }
                    }),
                )
            })
            .collect();
        Ok(ClkTup {
            implicit,
            clocks: tup_tgt,
        })
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
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>>;
}

/// Helper trait for `Sp` to implement `ClockCheckExpr`.
trait ClockCheckSpanExpr {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<WithDefSite<Clock>>>;
}

/// Clock typing interface for statements.
trait ClockCheckStmt {
    /// Clock context (typically obtained by the `signature` on local variables and toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks.
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<()>;
}

/// Helper trait for `Sp` to implement `ClockCheckStmt`.
trait ClockCheckSpanStmt {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<()>>;
}

/// Clock typing interface of declarations.
trait ClockCheckDecl {
    /// Clock context (typically obtained by the `signature` on other toplevel declarations)
    type Ctx<'i>;
    /// Verify internal consistency of the clocks and produce the signature.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self, span: Span, ctx: Self::Ctx<'_>) -> Result<ClkMap>;
}

/// Helper trait for `Sp` to implement `ClockCheckDecl`.
trait ClockCheckSpanDecl {
    /// Projection to the inner `Ctx`.
    type Ctx<'i>;
    /// Projection to the inner `clockcheck`.
    fn clockcheck(&self, ctx: Self::Ctx<'_>) -> Result<Sp<ClkMap>>;
}

/// Clock typing interface.
pub trait ClockCheck {
    /// Verify internal consistency of the clocks.
    /// # Errors
    /// Cannot verify that all clocks match.
    fn clockcheck(&self) -> Result<()>;
}

impl<T: ClockCheckExpr> ClockCheckSpanExpr for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, ctx: T::Ctx<'_>) -> Result<Sp<WithDefSite<Clock>>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckStmt> ClockCheckSpanStmt for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck(&self, ctx: T::Ctx<'_>) -> Result<Sp<()>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}
impl<T: ClockCheckDecl> ClockCheckSpanDecl for Sp<T> {
    type Ctx<'i> = T::Ctx<'i>;
    fn clockcheck<'i>(&self, ctx: Self::Ctx<'_>) -> Result<Sp<ClkMap>> {
        self.as_ref()
            .map(|span, t| t.clockcheck(span, ctx))
            .transpose()
    }
}

impl ClockCheckExpr for expr::Lit {
    type Ctx<'i> = ();
    fn clockcheck(&self, span: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Global {
    type Ctx<'i> = ();
    fn clockcheck(&self, span: Span, (): ()) -> Result<WithDefSite<Clock>> {
        Ok(WithDefSite::without(Clock::Adaptative.with_span(span)))
    }
}

impl ClockCheckExpr for var::Local {
    type Ctx<'i> = &'i mut ExprClkCtx;
    fn clockcheck(&self, _: Span, ctx: Self::Ctx<'_>) -> Result<WithDefSite<Clock>> {
        Ok(ctx.clock_of(self))
    }
}

impl ClockCheckDecl for decl::Node {
    type Ctx<'i> = &'i mut ClkCtx;
    fn clockcheck(&self, _span: Span, _ctx: Self::Ctx<'_>) -> Result<ClkMap> {
        Ok(ClkMap::default()) // FIXME
    }
}

impl ClockCheckDecl for decl::ExtNode {
    type Ctx<'i> = ();
    fn clockcheck(&self, _: Span, (): ()) -> Result<ClkMap> {
        let mut map = ClkMap::default();
        for i in self.inputs.t.iter() {
            map.new_input(Some(i.t.name.t.repr.as_ref()), i.t.ty.t.ty.t.clk.as_ref());
        }
        for o in self.outputs.t.iter() {
            map.new_output(Some(o.t.name.t.repr.as_ref()), o.t.ty.t.ty.t.clk.as_ref());
        }
        Ok(map)
    }
}

impl ClockCheck for Sp<decl::Prog> {
    fn clockcheck(&self) -> Result<()> {
        let mut ctx = ClkCtx::default();
        for decl in &self.t.decls {
            match &decl.t {
                decl::Decl::Const(_) | decl::Decl::ExtConst(_) => {}
                decl::Decl::Node(n) => {
                    let sig = n.clockcheck(&mut ctx)?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
                decl::Decl::ExtNode(n) => {
                    let sig = n.clockcheck(())?;
                    let sp_sig =
                        WithDefSite::without(sig.t.with_span(n.span)).with_def_site(sig.span);
                    ctx.signatures.insert(n.t.name.t.clone(), sp_sig);
                }
            }
        }
        Ok(())
    }
}
