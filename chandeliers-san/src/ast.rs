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
use std::hash::{Hash, Hasher};

/// Span wrapper.
///
/// This type is ubiquitous across this entire crate and you can expect
/// an overwhelming majority of the fields of all structs to contain one or
/// several `Sp<_>`.
///
/// `Sp` is mostly used through `map`, `new`, and it also implements
/// many traits by projecting into its `.t` field.
#[derive(Debug, Clone, Copy)]
pub struct Sp<T> {
    /// A payload.
    pub t: T,
    /// The span associated with the payload.
    pub span: Span,
}

impl<T> Sp<T> {
    /// Create a new `Sp` from a payload and a span.
    pub fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    /// Convert a `&Sp<T>` into a `Sp<&T>`.
    /// Mostly useful because `map` consumes Self and
    /// because `Sp` derives `Copy`.
    pub fn as_ref(&self) -> Sp<&T> {
        Sp {
            t: &self.t,
            span: self.span,
        }
    }

    /// Convert an `&mut Sp<T>` into a `Sp<&mut T>`.
    /// Mostly useful because `map` consumes Self and
    /// because `Sp` derives `Copy`.
    pub fn as_ref_mut(&mut self) -> Sp<&mut T> {
        Sp {
            t: &mut self.t,
            span: self.span,
        }
    }

    /// Apply a transformation to the payload while preserving the same span.
    ///
    /// This lets us track the same portion of the source code from beginning
    /// to end through translations into different ASTs.
    pub fn map<U, F>(self, f: F) -> Sp<U>
    where
        F: FnOnce(Span, T) -> U,
    {
        Sp {
            t: f(self.span, self.t),
            span: self.span,
        }
    }
}

/// Monad combinator to map faillible functions on a `Sp`.
impl<T, E> Sp<Result<T, E>> {
    pub fn transpose(self) -> Result<Sp<T>, E> {
        match self.t {
            Ok(t) => Ok(Sp { t, span: self.span }),
            Err(e) => Err(e),
        }
    }
}

/// Equality ignores the span.
impl<T: PartialEq> PartialEq for Sp<T> {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t
    }
}
impl<T: Eq> Eq for Sp<T> {}

/// Hash ignores the span.
impl<T: Hash> Hash for Sp<T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.t.hash(h);
    }
}

/// `SpanEnd` is the way that `Sp` has of computing its own span:
/// upon parsing it will record the beginning of the span to use as its
/// own, but ideally it also wants to eventually know the end of the Span,
/// and for that it asks `T`.
///
/// `SpanEnd` should be implemented for all types `T` for which you want to
/// be able to parse a `Sp<T>`
pub trait SpanEnd {
    /// Where this object ends.
    ///
    /// This is intentionally defined very loosely:
    /// - all types may return `None` if they do not want to reveal their
    ///   size or if they don't know it,
    /// - in the case of `Some(s)`, `s` may or may not include the entire
    ///   the only thing that is relevant is the endpoint.
    fn span_end(&self) -> Option<Span>;
}

/// Straightforward projection.
impl<T: SpanEnd> SpanEnd for Box<T> {
    fn span_end(&self) -> Option<Span> {
        self.as_ref().span_end()
    }
}

/// `Punctuated` projects to its last element.
///
/// Historical note: `SpanEnd` was created for the most part to handle
/// the fact that `Punctuated` is sometimes empty and has no span.
/// Although `Sp` is the main implementor, `Punctuated` is the reason
/// that it has to exist at all.
/// `Punctuated` is one of very few implementations that can even return `None`,
/// since most parsed objects are nonempty.
impl<T: SpanEnd, P> SpanEnd for syn::punctuated::Punctuated<T, P> {
    fn span_end(&self) -> Option<Span> {
        self.last().and_then(|x| x.span_end())
    }
}

/// Parsing `Sp<T>` invoques `SpanEnd` to know where the parsing ended.
impl<T> syn::parse::Parse for Sp<T>
where
    T: syn::parse::Parse + SpanEnd,
{
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let begin = input.span();
        let t: T = input.parse()?;
        let end = t.span_end().unwrap_or(begin);
        Ok(Self {
            t,
            span: begin.join(end).unwrap(),
        })
    }
}

/// Don't let the trivial implementation of `SpanEnd` for `Sp<T>` distract
/// you from its importance, the fact that `Sp<T>` returns a `Some(_)` in
/// one step is what makes it computationally feasible at all to have
/// most implementors of `SpanEnd` recurse into one field without blowing up
/// the stack of Rustc.
impl<T> SpanEnd for Sp<T> {
    fn span_end(&self) -> Option<Span> {
        Some(self.span)
    }
}

/// `Sp` is transparently displayable.
impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

/// Implement `SpanEnd` from `Spanned`.
/// Only recommended for items whose `span` is trivial, others should
/// be wrapped in a `Sp<_>` to alleviate the computation.
macro_rules! span_end_from_spanned {
    ( $($ty:tt)* ) => {
        impl SpanEnd for $($ty)* {
            fn span_end(&self) -> Option<Span> {
                Some(self.span())
            }
        }
    }
}

// Some implementation for `syn` types.
span_end_from_spanned!(syn::Ident);
span_end_from_spanned!(syn::Lit);
impl SpanEnd for syn::token::Paren {
    fn span_end(&self) -> Option<Span> {
        Some(self.span.join())
    }
}

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
pub mod clock {
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
    use super::clock;
    use super::Sp;
    use super::Tuple;
    use std::fmt;

    /// A basic scalar type.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum TyBase {
        Int,
        Float,
        Bool,
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

    /// A scalar type with a clock becomes a fixed-size rotating stream
    /// of values.
    #[derive(Debug, Clone)]
    pub struct Stream {
        pub base: Sp<TyBase>,
        pub depth: clock::Depth,
    }

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
}

/// Definitions of expressions.
pub mod expr {
    use super::clock;
    use super::Sp;
    use super::Tuple;
    use std::fmt;

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

    /// A local variable.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct LocalVar {
        pub name: Sp<String>,
    }

    /// A global constant.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct GlobalVar {
        pub name: Sp<String>,
    }

    impl fmt::Display for LocalVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    impl fmt::Display for GlobalVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    /// A past value of a variable.
    #[derive(Debug, Clone)]
    pub struct ClockVar {
        pub var: Sp<LocalVar>,
        pub depth: clock::Depth,
    }

    /// A subnode.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NodeId {
        pub id: Sp<usize>,
    }

    impl fmt::Display for ClockVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.var, self.depth)
        }
    }

    impl fmt::Display for NodeId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#{}", self.id)
        }
    }

    /// An extern value, i.e. not composed of primitive literals
    /// and operators.
    #[derive(Debug, Clone)]
    pub enum Reference {
        /// A global variable.
        Global(Sp<GlobalVar>),
        /// A local variable, possibly in the past.
        Var(Sp<ClockVar>),
        /// A subnode, possibly in the past.
        Node(Sp<NodeId>),
    }

    impl fmt::Display for Reference {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Var(v) => write!(f, "{v}"),
                Self::Node(n) => write!(f, "{n}"),
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
                Self::BitAnd => write!(f, "&"),
                Self::BitOr => write!(f, "|"),
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
                Self::Not => write!(f, "!"),
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
                Self::Eq => write!(f, "=="),
                Self::Ne => write!(f, "!="),
            }
        }
    }

    /// A primitive builtin function.
    #[derive(Debug, Clone)]
    pub enum Builtin {
        Float(Box<Sp<Expr>>),
    }

    impl fmt::Display for Builtin {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Float(e) => write!(f, "(float)({e})"),
            }
        }
    }

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
            lhs: Box<Sp<Expr>>,
            rhs: Box<Sp<Expr>>,
        },
        /// Application of a unary operator `!b`
        UnOp { op: UnOp, inner: Box<Sp<Expr>> },
        /// Application of a comparison function `a != b`
        CmpOp {
            op: CmpOp,
            lhs: Box<Sp<Expr>>,
            rhs: Box<Sp<Expr>>,
        },
        /// The special operator "later" in (Lustre `->`)
        /// to perform case analysis on the current value of the clock.
        Later {
            clk: clock::Depth,
            before: Box<Sp<Expr>>,
            after: Box<Sp<Expr>>,
        },
        /// A builtin (`float`)
        Builtin(Sp<Builtin>),
        /// A conditional expression `if b then x else y`
        Ifx {
            cond: Box<Sp<Expr>>,
            yes: Box<Sp<Expr>>,
            no: Box<Sp<Expr>>,
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
                Self::Builtin(primitive) => write!(f, "{primitive}"),
                Self::Ifx { cond, yes, no } => write!(f, "if {cond} {{ {yes} }} else {{ {no} }}"),
            }
        }
    }
}

/// Statements and operations with side-effects.
pub mod stmt {
    use super::clock;
    use super::expr;
    use super::Sp;
    use super::Tuple;
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

    /// A statement.
    #[derive(Debug, Clone)]
    pub enum Statement {
        /// Variable binding `let x = ...`
        Let {
            target: Sp<VarTuple>,
            source: Sp<expr::Expr>,
        },
        /// Advance a block.
        Substep {
            clk: clock::Depth,
            id: Sp<expr::NodeId>,
            args: Sp<Tuple<Sp<expr::Expr>>>,
            nbret: Sp<Option<usize>>,
        },
        /// Print debug information.
        Trace { msg: String, fmt: Tuple<expr::LocalVar> },
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
    use super::Sp;
    use super::Tuple;
    use std::fmt;

    /// A typed variable.
    #[derive(Debug, Clone)]
    pub struct TyVar {
        pub name: Sp<expr::LocalVar>,
        pub ty: Sp<ty::Stream>,
    }

    /// A node name (either for a declaration or for an invocation)
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeName(pub Sp<String>);

    impl fmt::Display for NodeName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    /// A node declaration.
    #[derive(Debug, Clone)]
    pub struct Node {
        pub name: Sp<NodeName>,
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
    }

    /// A trusted constant declaration.
    /// It does not have a value, and the rest of the program will
    /// asusme that it is well-defined.
    #[derive(Debug, Clone)]
    pub struct ExtConst {
        pub name: Sp<expr::GlobalVar>,
        pub ty: Sp<TyBase>,
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
