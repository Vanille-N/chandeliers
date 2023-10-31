//! Internal representation of a program in Candle,
//! before it is first translated to macros defined in `chandeliers-sem`
//! and then expanded to Rust.
//!
//! You should construct elements defined in this file by going through
//! the `translate` feature on the parent crate `chandeliers-syn`,
//! and you can use them after performing the proper verifications by
//! converting them into interpretable code using the methods from `codegen.rs`.

use crate::causality::depends::Depends;
use proc_macro2::Span;
use std::fmt;
use std::hash::Hash;

crate::sp::derive_spanned!(Tuple<T> where <T>);
/// Because `Vec` does not implement `Parse` as we want,
/// `Tuple` is used to for sequences.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple<T> {
    elems: Vec<T>,
}

impl<T> Default for Tuple<T> {
    fn default() -> Self {
        Self {
            elems: Default::default(),
        }
    }
}

impl<T> Tuple<T> {
    /// Map a function by reference to a tuple.
    pub fn map_ref<F, U>(&self, f: F) -> Tuple<U>
    where
        F: Fn(&T) -> U,
    {
        Tuple {
            elems: self.elems.iter().map(f).collect(),
        }
    }

    /// Map a faillible funciton by reference to a tuple.
    pub fn try_map<F, U, E>(&self, f: F) -> Result<Tuple<U>, E>
    where
        F: Fn(&T) -> Result<U, E>,
    {
        let mut elems = Vec::new();
        for e in &self.elems {
            elems.push(f(e)?);
        }
        Ok(Tuple { elems })
    }

    /// Iterate over elements of the tuple.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.elems.iter()
    }

    /// Iterate over elements of the tuple.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.elems.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Append to the tuple.
    pub fn push(&mut self, e: T) {
        self.elems.push(e);
    }
}

impl<T> IntoIterator for Tuple<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.elems.into_iter()
    }
}

/// `Tuple` is a transparent wrapper, `Depends` recurses into all fieds.
impl<T: Depends> Depends for Tuple<T> {
    type Output = T::Output;
    fn provides(&self, v: &mut Vec<Self::Output>) {
        self.elems.provides(v);
    }
    fn requires(&self, v: &mut Vec<Self::Output>) {
        self.elems.requires(v);
    }
}

/// Timing primitives.
pub mod past {
    use super::Span;
    use std::fmt;

    /// The depth of a variable (how many `pre`/`fby` are in front)
    #[derive(Debug, Clone, Copy)]
    pub struct Depth {
        pub span: Span,
        pub dt: usize,
    }

    impl fmt::Display for Depth {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.dt)
        }
    }
}

impl<T> fmt::Display for Tuple<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut es = self.elems.iter();
        if let Some(e) = es.next() {
            write!(f, "{e}")?;
        }
        for e in es {
            write!(f, ", {e}")?;
        }
        write!(f, ")")
    }
}

/// Types of expressions.
pub mod ty {
    use super::past;
    use super::Tuple;
    use crate::sp::Sp;
    use std::fmt;

    crate::sp::derive_spanned!(TyBase);
    /// A basic scalar type.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum TyBase {
        Int,
        Float,
        Bool,
    }

    #[derive(Debug, Clone)]
    pub enum Clock {
        Explicit { activation: bool, id: Sp<String> },
        Implicit,
        Adaptative,
    }

    impl fmt::Display for TyBase {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Int => write!(f, "int"),
                Self::Float => write!(f, "float"),
                Self::Bool => write!(f, "bool"),
            }
        }
    }

    crate::sp::derive_spanned!(Clocked<T> where <T>);
    #[derive(Debug, Clone)]
    pub struct Clocked<T> {
        pub inner: Sp<T>,
        pub clk: Sp<Clock>,
    }

    /// A scalar type with a clock becomes a fixed-size rotating stream
    /// of values.
    #[derive(Debug, Clone)]
    pub struct Stream {
        pub ty: Sp<Clocked<TyBase>>,
        pub depth: past::Depth,
    }

    crate::sp::derive_spanned!(TyTuple);
    /// A composite type of several values arbitrarily deeply nested.
    #[derive(Debug, Clone)]
    pub enum TyTuple {
        Single(Sp<TyBase>),
        Multiple(Sp<Tuple<Sp<TyTuple>>>),
    }

    impl fmt::Display for TyTuple {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Single(t) => write!(f, "{t}"),
                Self::Multiple(ts) => write!(f, "{ts}"),
            }
        }
    }

    impl fmt::Display for Clock {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Explicit { activation, id } => write!(
                    f,
                    " {when} {id}",
                    when = if *activation { "when" } else { "whenot" }
                ),
                _ => Ok(()),
            }
        }
    }
}

/// Definitions of expressions.
pub mod expr {
    use super::past;
    use super::Tuple;
    use crate::sp::Sp;
    use std::fmt;

    crate::sp::derive_spanned!(Lit);
    /// A literal.
    #[derive(Debug, Clone, Copy)]
    pub enum Lit {
        Int(i64),
        Float(f64),
        Bool(bool),
    }

    impl fmt::Display for Lit {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Int(i) => write!(f, "{i}"),
                Self::Float(x) => write!(f, "{x}"),
                Self::Bool(b) => write!(f, "{b}"),
            }
        }
    }

    crate::sp::derive_spanned!(LocalVar);
    /// A local variable.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct LocalVar {
        pub repr: Sp<String>,
        pub run_uid: usize,
    }

    crate::sp::derive_spanned!(GlobalVar);
    /// A global constant.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct GlobalVar {
        pub repr: Sp<String>,
        pub run_uid: usize,
    }

    impl fmt::Display for LocalVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.repr)
        }
    }

    impl fmt::Display for GlobalVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.repr)
        }
    }

    crate::sp::derive_spanned!(PastVar);
    /// A past value of a variable.
    #[derive(Debug, Clone)]
    pub struct PastVar {
        pub var: Sp<LocalVar>,
        pub depth: past::Depth,
    }

    crate::sp::derive_spanned!(NodeId);
    /// A subnode.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeId {
        pub id: Sp<usize>,
        pub repr: Sp<String>,
    }

    impl fmt::Display for PastVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if self.depth.dt > 0 {
                write!(f, "{}@{}", self.var, self.depth)
            } else {
                write!(f, "{}", self.var)
            }
        }
    }

    impl fmt::Display for NodeId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.repr)
        }
    }

    crate::sp::derive_spanned!(Reference);
    /// An extern value, i.e. not composed of primitive literals
    /// and operators.
    #[derive(Debug, Clone)]
    pub enum Reference {
        /// A global variable.
        Global(Sp<GlobalVar>),
        /// A local variable, possibly in the past.
        Var(Sp<PastVar>),
    }

    impl fmt::Display for Reference {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Var(v) => write!(f, "{v}"),
                Self::Global(g) => write!(f, "{g}"),
            }
        }
    }

    /// A binary operator.
    #[derive(Debug, Clone, Copy)]
    pub enum BinOp {
        Add,
        Mul,
        Sub,
        Div,
        Rem,
        BitAnd,
        BitOr,
        BitXor,
    }

    impl fmt::Display for BinOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Add => write!(f, "+"),
                Self::Mul => write!(f, "*"),
                Self::Sub => write!(f, "-"),
                Self::Div => write!(f, "/"),
                Self::Rem => write!(f, "%"),
                Self::BitAnd => write!(f, "and"),
                Self::BitOr => write!(f, "or"),
                Self::BitXor => write!(f, "^"),
            }
        }
    }

    /// A unary operator.
    #[derive(Debug, Clone, Copy)]
    pub enum UnOp {
        Neg,
        Not,
    }

    impl fmt::Display for UnOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Neg => write!(f, "-"),
                Self::Not => write!(f, "not"),
            }
        }
    }

    /// A comparison operator.
    #[derive(Debug, Clone, Copy)]
    pub enum CmpOp {
        Le,
        Ge,
        Lt,
        Gt,
        Eq,
        Ne,
    }

    impl fmt::Display for CmpOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Le => write!(f, "<="),
                Self::Ge => write!(f, ">="),
                Self::Lt => write!(f, "<"),
                Self::Gt => write!(f, ">"),
                Self::Eq => write!(f, "="),
                Self::Ne => write!(f, "<>"),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum ClockOp {
        When,
        Whenot,
    }

    impl fmt::Display for ClockOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::When => write!(f, "when"),
                Self::Whenot => write!(f, "whenot"),
            }
        }
    }

    crate::sp::derive_spanned!(Expr);
    /// An expression.
    #[derive(Debug, Clone)]
    pub enum Expr {
        /// Literals `1.0`, `42`, `true`, ...
        Lit(Sp<Lit>),
        /// External values `x`, `_0`, ...
        Reference(Sp<Reference>),
        /// Tuples `(1, 2.0, x)`.
        Tuple(Sp<Tuple<Sp<Expr>>>),
        /// Application of a binary operator `a + b`
        BinOp {
            op: BinOp,
            lhs: Sp<Box<Expr>>,
            rhs: Sp<Box<Expr>>,
        },
        /// Application of a unary operator `!b`
        UnOp { op: UnOp, inner: Sp<Box<Expr>> },
        /// Application of a comparison function `a != b`
        CmpOp {
            op: CmpOp,
            lhs: Sp<Box<Expr>>,
            rhs: Sp<Box<Expr>>,
        },
        /// A when or whenop expression
        ClockOp {
            op: ClockOp,
            inner: Sp<Box<Expr>>,
            activate: Sp<Box<Expr>>,
        },
        /// The special operator "later" in (Lustre `->`)
        /// to perform case analysis on the current value of the clock.
        Later {
            clk: past::Depth,
            before: Sp<Box<Expr>>,
            after: Sp<Box<Expr>>,
        },
        /// A conditional expression `if b then x else y`
        Ifx {
            cond: Sp<Box<Expr>>,
            yes: Sp<Box<Expr>>,
            no: Sp<Box<Expr>>,
        },
        /// Clock combinator
        Merge {
            switch: Sp<Box<Expr>>,
            on: Sp<Box<Expr>>,
            off: Sp<Box<Expr>>,
        },
        /// Advance a block.
        Substep {
            clk: usize,
            id: Sp<NodeId>,
            args: Sp<Box<Expr>>,
        },
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Lit(l) => write!(f, "{l}"),
                Self::Reference(r) => write!(f, "{r}"),
                Self::Tuple(t) => write!(f, "{t}"),
                Self::BinOp { op, lhs, rhs } => write!(f, "({lhs} {op} {rhs})"),
                Self::UnOp { op, inner } => write!(f, "({op} {inner})"),
                Self::CmpOp { op, lhs, rhs } => write!(f, "({lhs} {op} {rhs})"),
                Self::Later { clk, before, after } => write!(f, "({before} ->{clk} {after})"),
                Self::Ifx { cond, yes, no } => {
                    write!(f, "if {cond} {{ {yes} }} else {{ {no} }}")
                }
                Self::Substep { clk, id, args } => {
                    if *clk > 0 {
                        write!(f, "{id}@{clk}{args}")
                    } else {
                        write!(f, "{id}{args}")
                    }
                }
                Self::ClockOp {
                    op,
                    inner,
                    activate,
                } => write!(f, "({inner} {op} {activate})"),
                Self::Merge { switch, on, off } => write!(f, "(merge {switch} {on} {off})"),
            }
        }
    }
}

/// Statements and operations with side-effects.
pub mod stmt {
    use super::expr;
    use super::Tuple;
    use crate::sp::Sp;
    use std::fmt;

    /// The target of an assignment `(x, y, z) = ...`
    #[derive(Debug, Clone)]
    pub enum VarTuple {
        Single(Sp<expr::LocalVar>),
        Multiple(Sp<Tuple<Sp<VarTuple>>>),
    }

    impl fmt::Display for VarTuple {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Single(v) => write!(f, "{v}"),
                Self::Multiple(vs) => write!(f, "{vs}"),
            }
        }
    }

    crate::sp::derive_spanned!(Statement);
    /// A statement.
    #[derive(Debug, Clone)]
    pub enum Statement {
        /// Variable binding `let x = ...`
        Let {
            target: Sp<VarTuple>,
            source: Sp<expr::Expr>,
        },
        /// Print debug information.
        Trace {
            msg: String,
            fmt: Tuple<expr::LocalVar>,
        },
        /// Perform an assertion.
        Assert(Sp<expr::Expr>),
    }
}

/// Toplevel declarations.
pub mod decl {
    use super::expr;
    use super::stmt;
    use super::ty;
    use super::ty::TyBase;
    use super::Tuple;
    use crate::sp::Sp;
    use std::fmt;

    /// A typed variable.
    #[derive(Debug, Clone)]
    pub struct TyVar {
        pub name: Sp<expr::LocalVar>,
        pub ty: Sp<ty::Stream>,
    }

    crate::sp::derive_spanned!(NodeName);
    /// A node name (either for a declaration or for an invocation)
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeName {
        pub repr: Sp<String>,
        pub run_uid: usize,
    }

    impl fmt::Display for NodeName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.repr)
        }
    }

    #[derive(Debug, Clone)]
    pub struct NodeOptions {
        pub trace: bool,
        pub export: bool,
        pub main: Option<usize>,
        pub rustc_allow: Vec<syn::Ident>,
    }

    #[derive(Debug, Clone)]
    pub struct ExtNodeOptions {
        pub trace: bool,
        pub main: Option<usize>,
        pub rustc_allow: Vec<syn::Ident>,
    }

    #[derive(Debug, Clone)]
    pub struct ConstOptions {
        pub export: bool,
        pub rustc_allow: Vec<syn::Ident>,
    }

    #[derive(Debug, Clone)]
    pub struct ExtConstOptions {
        pub rustc_allow: Vec<syn::Ident>,
    }

    /// A node declaration.
    #[derive(Debug, Clone)]
    pub struct Node {
        pub name: Sp<NodeName>,
        pub options: NodeOptions,
        /// Input and output variables.
        pub inputs: Sp<Tuple<Sp<TyVar>>>,
        pub outputs: Sp<Tuple<Sp<TyVar>>>,
        pub locals: Sp<Tuple<Sp<TyVar>>>,
        /// Other nodes that are used by this one.
        pub blocks: Vec<Sp<NodeName>>,
        /// Body of the node declaration.
        pub stmts: Vec<Sp<stmt::Statement>>,
    }

    /// A global constant.
    #[derive(Debug, Clone)]
    pub struct Const {
        pub name: Sp<expr::GlobalVar>,
        pub options: ConstOptions,
        pub ty: Sp<TyBase>,
        pub value: Sp<expr::Expr>,
    }

    /// A trusted node declaration.
    /// It does not have a body, and the rest of the program will
    /// assume that it is well-defined.
    #[derive(Debug, Clone)]
    pub struct ExtNode {
        pub name: Sp<NodeName>,
        pub inputs: Sp<Tuple<Sp<TyVar>>>,
        pub outputs: Sp<Tuple<Sp<TyVar>>>,
        pub options: ExtNodeOptions,
    }

    /// A trusted constant declaration.
    /// It does not have a value, and the rest of the program will
    /// asusme that it is well-defined.
    #[derive(Debug, Clone)]
    pub struct ExtConst {
        pub name: Sp<expr::GlobalVar>,
        pub ty: Sp<TyBase>,
        pub options: ExtConstOptions,
    }

    /// A toplevel declaration.
    #[derive(Debug, Clone)]
    pub enum Decl {
        Const(Sp<Const>),
        Node(Sp<Node>),
        ExtConst(Sp<ExtConst>),
        ExtNode(Sp<ExtNode>),
    }

    /// A Lustre program is a sequence of declarations.
    #[derive(Debug, Clone)]
    pub struct Prog {
        pub decls: Vec<Sp<Decl>>,
    }
}
