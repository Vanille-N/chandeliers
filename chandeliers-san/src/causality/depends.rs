//! Defines the dependencies between elements of the program.
//!
//! An important feature of Lustre is that it resolves dependency conflicts
//! not by the order in which the variables are declared, but by doing
//! an analysis of the dependency graph.
//! This file defines the notion of dependency so that `causality.rs` and
//! `graph.rs` may perform analysis on the dependency graph.
//!
//! The entry point to this file is through `causality.rs` by means of
//! an implementor of `Depends` that will output a list of constraints
//! to resolve.

#![allow(clippy::redundant_closure_call)]

use std::fmt;

use crate::ast::{decl, expr, stmt};
use crate::sp::Sp;
use chandeliers_err as err;
use proc_macro2::Span;

/// Construct the dependency constraints introduced by `Self`.
/// A typical implementation will consist of
/// - leaf elements (variables, function names) will insert themselves
///   in both methods so that they can serve as either depending on the context,
/// - higher-level constructs will recurse differently in both methods,
/// - wrappers will recurse into everything.
///
/// For example, the following definitions illustrate these behaviors
/// in a way that will be common across most languages' definitions of dependencies.
/// - `Var`, as it serves as the basic building block for both defining a variable
///   or fetching the value of one, will both provide and require itself.
/// - `Expr` is a pure expression: it never `provides` anything, and
///   recurses into all fields to determine what it `requires`.
/// - `Def` is a variable assignment `x = v`: it will recurse into
///   `x` for `provides`, and into `v` for `requires`.
///
/// The macros used here for easy implementation are tailore for these
/// kinds of behaviors.
pub trait Depends {
    /// The type of elementary dependencies (e.g. variable names)
    type Output;
    /// List all the basic building blocks that this element defines.
    fn provides(&self, v: &mut Vec<Self::Output>);
    /// List all the basic building blocks that must be defined before this element.
    fn requires(&self, v: &mut Vec<Self::Output>);
}

/// This is the type that serves as our dependency.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Reference {
    /// A node.
    FunName(Sp<String>),
    /// A variable.
    LocalVarName(Sp<String>),
    /// A global constant.
    GlobalVarName(Sp<String>),
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunName(fun) => write!(f, "Node `{fun}`"),
            Self::LocalVarName(var) => write!(f, "Variable `{var}`"),
            Self::GlobalVarName(var) => write!(f, "Global `{var}`"),
        }
    }
}

impl err::TrySpan for Reference {
    fn try_span(&self) -> Option<Span> {
        match self {
            Reference::FunName(i) | Reference::LocalVarName(i) | Reference::GlobalVarName(i) => {
                i.try_span()
            }
        }
    }
}

/// Implement `provide` by recursing into all given branches of the match.
macro_rules! provide_by_match {
    ( $( $pat:pat => $($y:ident),* ; )* ) => {
        fn provides(&self, v: &mut Vec<Self::Output>) {
            match self {
                $( $pat => { $( $y.t.provides(v); )* } )*
            }
        }
    }
}

/// Implement `require` by recursing into all given branches of the match.
macro_rules! require_by_match {
    ( $( $pat:pat => $($y:ident),* ; )* ) => {
        fn requires(&self, v: &mut Vec<Self::Output>) {
            match self {
                $( $pat => { $( $y.t.requires(v); )* } )*
            }
        }
    }
}

/// Implement `provide` by recursing into the fields of the struct.
macro_rules! provide_in_fields {
    ( $( $field:ident ),* ) => {
        fn provides(&self, v: &mut Vec<Self::Output>) {
            $( self.$field.provides(v); )*
        }
    }
}

/// Implement `require` by recursing into the fields of the struct.
macro_rules! require_in_fields {
    ( $( $field:ident ),* ) => {
        fn requires(&self, v: &mut Vec<Self::Output>) {
            $( self.$field.requires(v); )*
        }
    }
}

/// Trivial implementation of `provide`
macro_rules! provide_nothing {
    () => {
        fn provides(&self, _v: &mut Vec<Self::Output>) {}
    };
}

/// Trivial implementation of `require`
macro_rules! require_nothing {
    () => {
        fn requires(&self, _v: &mut Vec<Self::Output>) {}
    };
}

/// Leaf of `provide`: exactly one basic reference.
macro_rules! provide_this {
    ( $fun:expr ) => {
        fn provides(&self, v: &mut Vec<Self::Output>) {
            v.push(($fun)(self));
        }
    };
}

/// Leaf of `require`: exactly one basic reference.
macro_rules! require_this {
    ( $fun:expr ) => {
        fn requires(&self, v: &mut Vec<Self::Output>) {
            v.push(($fun)(self));
        }
    };
}

/// Sp is a wrapper and recurses into everything.
impl<T: Depends> Depends for Sp<T> {
    type Output = T::Output;
    provide_in_fields!(t);
    require_in_fields!(t);
}

/// Vec is a wrapper and recurses into everything.
impl<T: Depends> Depends for Vec<T> {
    type Output = T::Output;
    fn provides(&self, v: &mut Vec<Self::Output>) {
        for i in self {
            i.provides(v);
        }
    }
    fn requires(&self, v: &mut Vec<Self::Output>) {
        for i in self {
            i.requires(v);
        }
    }
}

/// Decl is a wrapper and recurses into everything.
impl Depends for decl::Decl {
    type Output = Reference;
    provide_by_match! {
        Self::Const(c) => c;
        Self::Node(n) => n;
        Self::ExtConst(c) => c;
        Self::ExtNode(n) => n;
    }
    require_by_match! {
        Self::Const(c) => c;
        Self::Node(n) => n;
        Self::ExtConst(c) => c;
        Self::ExtNode(n) => n;
    }
}

/// Const definition `const X = V` provides `X` and requires `V`.
impl Depends for decl::Const {
    type Output = Reference;
    provide_in_fields!(name);
    require_in_fields!(value);
}

/// Const definition `extern const X` provides `X`.
/// Warning: `extern` declarations across different parse invocations
/// are assumed and not verified to be causal. Rustc will complain otherwise.
impl Depends for decl::ExtConst {
    type Output = Reference;
    provide_in_fields!(name);
    require_nothing!();
}

/// Node definition `node foo(...) returns (...) let STMTS tel`
/// provides `foo` and requires `STMTS`.
///
/// Do not confuse `Node` as a *subject* of `Causality` on `Prog`
/// and as itself an implementor of `Prog`!
///
/// Here we are talking about what a `Node` requires and provides
/// of and to the outside world, not about what local variables it makes
/// available to its statements. This is why we don't consider
/// its `inputs` and `outputs` to be provided here.
/// See also: `impl Causality for Node` in `causality.rs` where
///           do define local variables to be provided for the purposes
///           of sorting the statements.
impl Depends for decl::Node {
    type Output = Reference;
    provide_in_fields!(name);
    require_in_fields!(blocks, stmts);
}

/// Node definition `extern node foo(...) returns (...)`
/// provides `foo`.
/// Warning: `extern` declarations across different parse invocations
/// are assumed and not verified to be causal. Rustc will complain otherwise.
impl Depends for decl::ExtNode {
    type Output = Reference;
    provide_in_fields!(name);
    require_nothing!();
}

/// A `NodeName` is a leaf element.
impl Depends for decl::NodeName {
    type Output = Reference;
    provide_this!(|this: &Self| Reference::FunName(this.repr.clone()));
    require_this!(|this: &Self| Reference::FunName(this.repr.clone()));
}

/// Statement recurses differently in both methods.
/// Node that `Statement` is only ever considered to provide anything
/// within the body of a `Node` since `impl Depends for Node` does not
/// recurse on `provide`.
impl Depends for stmt::Statement {
    type Output = Reference;
    provide_by_match! {
        // `target = ...` provides target
        Self::Let { target, .. } => target;
        // Pure, provides nothing.
        Self::Assert(_) => ;
    }
    require_by_match! {
        // `_ = source` requires the value to be assigned.
        Self::Let { source, .. } => source;
        // Assertion is a wrapper.
        Self::Assert(e) => e;
    }
}

/// `Expr` provides nothing (all expressions are pure),
/// and recurses into all non-punctuation fields for `require`.
impl Depends for expr::Expr {
    type Output = Reference;
    provide_nothing!();
    require_by_match! {
        Self::Lit(_) => ;
        Self::Reference(r) => r;
        Self::Tuple(t) => t;
        Self::BinOp { lhs, rhs, .. } => lhs, rhs;
        Self::CmpOp { lhs, rhs, .. } => lhs, rhs;
        Self::UnOp { inner, .. } => inner;
        Self::Later { before, after, .. } => before, after;
        Self::Ifx { cond, yes, no } => cond, yes, no;
        Self::Substep { args, .. } => args;
        Self::ClockOp { inner, activate, .. } => inner, activate;
        Self::Merge { switch, on, off } => switch, on, off;
    }
}

/// `LocalVar` is a leaf.
impl Depends for expr::LocalVar {
    type Output = Reference;
    provide_this!(|this: &Self| Reference::LocalVarName(this.repr.clone()));
    require_this!(|this: &Self| Reference::LocalVarName(this.repr.clone()));
}

/// `GlobalVar` is a leaf.
impl Depends for expr::GlobalVar {
    type Output = Reference;
    provide_this!(|this: &Self| Reference::GlobalVarName(this.repr.clone()));
    require_this!(|this: &Self| Reference::GlobalVarName(this.repr.clone()));
}

/// `Reference` is a wrapper.
impl Depends for expr::Reference {
    type Output = Reference;
    provide_nothing!();
    require_by_match! {
        Self::Var(v) => v;
        Self::Global(g) => g;
    }
}

/// `ClockVar` is only used for accesses, so it provides nothing.
/// Nonzero clocks are not subject to acyclicity checks and are instead
/// handled by the positivity check, so `ClockVar` introduces a dependency
/// only if it is at the root depth.
impl Depends for expr::PastVar {
    type Output = Reference;
    provide_nothing!();
    fn requires(&self, v: &mut Vec<Reference>) {
        if self.depth.t.dt == 0 {
            self.var.requires(v);
        }
    }
}

/// `VarTuple` is a wrapper.
impl Depends for stmt::VarTuple {
    type Output = Reference;
    provide_by_match! {
        Self::Single(s) => s;
        Self::Multiple(m) => m;
    }
    require_by_match! {
        Self::Single(s) => s;
        Self::Multiple(m) => m;
    }
}
