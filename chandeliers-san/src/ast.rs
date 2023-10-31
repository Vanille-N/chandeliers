//! Internal representation of a program in Candle,
//! before it is first translated to macros defined in `chandeliers-sem`
//! and then expanded to Rust.
//!
//! You should construct elements defined in this file by going through
//! the `translate` feature on the parent crate `chandeliers-syn`,
//! and you can use them after performing the proper verifications by
//! converting them into interpretable code using the methods from `codegen.rs`.

use crate::causality::depends::Depends;
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

    /// Number of elements in the tuple.
    pub fn len(&self) -> usize {
        self.elems.len()
    }

    /// Whether this tuple is empty.
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
    use std::fmt;

    crate::sp::derive_spanned!(Depth);
    /// The depth of a variable (how many `pre`/`fby` are in front)
    #[derive(Debug, Clone, Copy)]
    pub struct Depth {
        /// How many instants in the past.
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
        /// Integer type (parsed from `int`, represented by `i64`)
        Int,
        /// Float type (parsed from `float`, represented by `f64`)
        Float,
        /// Bool type (parsed from `bool` represented by `bool`)
        Bool,
    }

    /// A clock type.
    #[derive(Debug, Clone)]
    pub enum Clock {
        /// A clock written in the source code (e.g. `when b`).
        Explicit {
            /// Whether this clock is positive (when) or negative (whenot).
            activation: bool,
            /// Name of the boolean variable representing the clock.
            id: Sp<String>,
        },
        /// The default clock of the node.
        Implicit,
        /// Any clock.
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
    /// A clock bound to an object (typically a `TyBase`).
    #[derive(Debug, Clone)]
    pub struct Clocked<T> {
        /// Contents (the type).
        pub inner: Sp<T>,
        /// Attached clock.
        pub clk: Sp<Clock>,
    }

    /// A scalar type with a clock becomes a fixed-size rotating stream
    /// of values.
    #[derive(Debug, Clone)]
    pub struct Stream {
        /// Inner type.
        pub ty: Sp<Clocked<TyBase>>,
        /// How many steps into the past this variable is used
        /// (computed by `MakePositive`).
        pub depth: Sp<past::Depth>,
    }

    crate::sp::derive_spanned!(TyTuple);
    /// A composite type of several values arbitrarily deeply nested.
    #[derive(Debug, Clone)]
    pub enum TyTuple {
        /// End of the nesting by a singleton element.
        /// Covers both `x` and `(x)`.
        Single(Sp<TyBase>),
        /// A tuple of any number of elements.
        /// E.g. `()`, `(x,)`, `(x, y, z)`, ...
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
        /// Integer literal (parsed).
        Int(i64),
        /// Float literal (parsed).
        Float(f64),
        /// Bool literal (parsed).
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
        /// Name.
        pub repr: Sp<String>,
        /// Number to generate unique identifiers.
        pub run_uid: usize,
    }

    crate::sp::derive_spanned!(GlobalVar);
    /// A global constant.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct GlobalVar {
        /// Name.
        pub repr: Sp<String>,
        /// Number to generate unique identifiers.
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
        /// Variable.
        pub var: Sp<LocalVar>,
        /// How many steps in the past.
        pub depth: Sp<past::Depth>,
    }

    crate::sp::derive_spanned!(NodeId);
    /// A subnode.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeId {
        /// Index in the `struct`'s `__node` field.
        pub id: Sp<usize>,
        /// Name of the call.
        pub repr: Sp<String>,
    }

    impl fmt::Display for PastVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if self.depth.t.dt > 0 {
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
        /// `+`
        Add,
        /// `*`
        Mul,
        /// `-` (binary)
        Sub,
        /// `/`
        Div,
        /// `%`
        Rem,
        /// `and`
        BitAnd,
        /// `or`
        BitOr,
        /// Not currently representable.
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
        /// `-` (unary)
        Neg,
        /// `not`
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
        /// `<=`
        Le,
        /// `>=`
        Ge,
        /// `<`
        Lt,
        /// `>`
        Gt,
        /// `=`
        Eq,
        /// `<>`
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

    /// A clock operator applied to an expression.
    #[derive(Debug, Clone, Copy)]
    pub enum ClockOp {
        /// `when`
        When,
        /// `whenot`
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
        /// Application of a binary operator `a + b`.
        BinOp {
            /// Binary operator (e.g. `+`).
            op: BinOp,
            /// Left-hand-side (e.g. `a`).
            lhs: Sp<Box<Expr>>,
            /// Right-hand-side (e.g. `b`).
            rhs: Sp<Box<Expr>>,
        },
        /// Application of a unary operator `not b`.
        UnOp {
            /// Unary operator (e.g. `not`).
            op: UnOp,
            /// Contents (e.g. `b`).
            inner: Sp<Box<Expr>>,
        },
        /// Application of a comparison function `a <> b`.
        CmpOp {
            /// Comparison operator (e.g. `<>`).
            op: CmpOp,
            /// Left-hand-side (e.g. `a`).
            lhs: Sp<Box<Expr>>,
            /// Right-hand-side (e.g. `b`).
            rhs: Sp<Box<Expr>>,
        },
        /// A when or whenot expression `e when b`.
        ClockOp {
            /// Clock operator (e.g. `when`).
            op: ClockOp,
            /// Expression being clocked (e.g. `e`).
            inner: Sp<Box<Expr>>,
            /// Clock variable (e.g. `b`).
            activate: Sp<Box<Expr>>,
        },
        /// The special operator "later" in (Lustre `->`)
        /// to perform case analysis on the current value of the clock.
        Later {
            /// Number of steps after which we read the `after` field.
            clk: Sp<past::Depth>,
            /// Value to evaluate before `clk`.
            before: Sp<Box<Expr>>,
            /// Value to evaluate after `clk`.
            after: Sp<Box<Expr>>,
        },
        /// A conditional expression `if b then x else y`
        Ifx {
            /// Boolean condition.
            cond: Sp<Box<Expr>>,
            /// Evaluate if the condition holds.
            yes: Sp<Box<Expr>>,
            /// Evaluate if the condition does not hold.
            no: Sp<Box<Expr>>,
        },
        /// Clock combinator
        Merge {
            /// Boolean condition.
            switch: Sp<Box<Expr>>,
            /// Evaluate if the condition holds.
            on: Sp<Box<Expr>>,
            /// Evaluate if the condition does not hold.
            off: Sp<Box<Expr>>,
        },
        /// Advance a block.
        Substep {
            /// Number of steps to wait before activating for the first time.
            clk: usize,
            /// Index in the `struct`'s `__nodes` field.
            id: Sp<NodeId>,
            /// Arguments of the function call (parenthesized but not a tuple).
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

    /// The target of an assignment `(x, y, z) = ...`.
    /// May be abritrarily nested.
    #[derive(Debug, Clone)]
    pub enum VarTuple {
        /// End of the recursion through a single variable.
        Single(Sp<expr::LocalVar>),
        /// Comma-separated tuple.
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
            /// Variable tuple for destructuring the assignment.
            target: Sp<VarTuple>,
            /// Expression to compute.
            source: Sp<expr::Expr>,
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
        /// Name of the variable.
        pub name: Sp<expr::LocalVar>,
        /// Type of the variable (including the temporal depth and clock).
        pub ty: Sp<ty::Stream>,
    }

    crate::sp::derive_spanned!(NodeName);
    /// A node name (either for a declaration or for an invocation)
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeName {
        /// Name of the node.
        pub repr: Sp<String>,
        /// Number to generate unique identifiers.
        pub run_uid: usize,
    }

    impl fmt::Display for NodeName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.repr)
        }
    }

    /// Options Available for a node.
    #[derive(Debug, Clone)]
    pub struct NodeOptions {
        /// `#[trace]`: display debug information on the inputs and outputs
        pub trace: bool,
        /// `#[export]`: this struct is public.
        pub export: bool,
        /// `#[main(n)]` generate a main function for this node that runs `n` times.
        pub main: Option<usize>,
        /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
        /// to silence Rustc warnings.
        pub rustc_allow: Vec<syn::Ident>,
    }

    /// Options Available for an extern node.
    #[derive(Debug, Clone)]
    pub struct ExtNodeOptions {
        /// `#[trace]`: display debug information on the inputs and outputs
        pub trace: bool,
        /// `#[main(n)]` generate a main function for this node that runs `n` times.
        pub main: Option<usize>,
        /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
        /// to silence Rustc warnings.
        pub rustc_allow: Vec<syn::Ident>,
    }

    /// Options available for a const.
    #[derive(Debug, Clone)]
    pub struct ConstOptions {
        /// `#[export]`: this const is public.
        pub export: bool,
        /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
        /// to silence Rustc warnings.
        pub rustc_allow: Vec<syn::Ident>,
    }

    /// Options available for an extern const.
    #[derive(Debug, Clone)]
    pub struct ExtConstOptions {
        /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
        /// to silence Rustc warnings.
        pub rustc_allow: Vec<syn::Ident>,
    }

    /// A node declaration `node foo(x) returns (y); var z; let <body> tel.
    #[derive(Debug, Clone)]
    pub struct Node {
        /// Public name of the node (`foo`).
        pub name: Sp<NodeName>,
        /// Compilation options attached (in the form `#[...]` as per the standard Rust notation)
        pub options: NodeOptions,
        /// Input variables and types (`x`).
        pub inputs: Sp<Tuple<Sp<TyVar>>>,
        /// Output variables and types (`y`).
        pub outputs: Sp<Tuple<Sp<TyVar>>>,
        /// Local variables and types (`z`).
        pub locals: Sp<Tuple<Sp<TyVar>>>,
        /// Other nodes that are used by this one (function calls in `<body>`).
        pub blocks: Vec<Sp<NodeName>>,
        /// Body of the node declaration (`<body>`).
        pub stmts: Vec<Sp<stmt::Statement>>,
    }

    /// A global constant `const X : int = 0`.
    #[derive(Debug, Clone)]
    pub struct Const {
        /// Public name of the constant (`X`).
        pub name: Sp<expr::GlobalVar>,
        /// Compilation options attached (in the form `#[...]` as per the standard Rust notation)
        pub options: ConstOptions,
        /// Type of the constant (`int`).
        pub ty: Sp<TyBase>,
        /// Const-computable value (`0`).
        pub value: Sp<expr::Expr>,
    }

    /// A trusted node declaration `extern node foo(x) returns (y);`.
    /// It does not have a body, and the rest of the program will
    /// assume that it is well-defined.
    #[derive(Debug, Clone)]
    pub struct ExtNode {
        /// Public name of the node (`foo`).
        pub name: Sp<NodeName>,
        /// Input variables and types (`x`).
        pub inputs: Sp<Tuple<Sp<TyVar>>>,
        /// Output variables and types (`y`).
        pub outputs: Sp<Tuple<Sp<TyVar>>>,
        /// Compilation options attached (in the form `#[...]` as per the standard Rust notation)
        pub options: ExtNodeOptions,
    }

    /// A trusted constant declaration `extern const X : int;`.
    /// It does not have a value, and the rest of the program will
    /// asusme that it is well-defined.
    #[derive(Debug, Clone)]
    pub struct ExtConst {
        /// Public name of the constant (`X`).
        pub name: Sp<expr::GlobalVar>,
        /// Type of the constant (`int`).
        pub ty: Sp<TyBase>,
        /// Compilation options attached (in the form `#[...]` as per the standard Rust notation)
        pub options: ExtConstOptions,
    }

    /// A toplevel declaration.
    #[derive(Debug, Clone)]
    pub enum Decl {
        /// `const X : int = 0`
        Const(Sp<Const>),
        /// `node foo() returns (); let tel`
        Node(Sp<Node>),
        /// `extern const X : int`.
        ExtConst(Sp<ExtConst>),
        /// `extern node foo() returns ();`
        ExtNode(Sp<ExtNode>),
    }

    /// A Lustre program is a sequence of declarations.
    #[derive(Debug, Clone)]
    pub struct Prog {
        /// Sequence of declarations.
        pub decls: Vec<Sp<Decl>>,
    }
}
