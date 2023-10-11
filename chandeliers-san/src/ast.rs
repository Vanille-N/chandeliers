//! Translation from Lustre to Candle

use proc_macro2::Span;
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct Sp<T> {
    pub span: Span,
    pub t: T,
}

impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

impl<T> Sp<T> {
    pub fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    pub fn as_ref(&self) -> Sp<&T> {
        Sp {
            t: &self.t,
            span: self.span,
        }
    }

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

impl<T, E> Sp<Result<T, E>> {
    pub fn transpose(self) -> Result<Sp<T>, E> {
        match self.t {
            Ok(t) => Ok(Sp { t, span: self.span }),
            Err(e) => Err(e),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple<T> {
    pub elems: Vec<T>,
}

impl<T> Default for Tuple<T> {
    fn default() -> Self {
        Self {
            elems: Default::default(),
        }
    }
}

pub mod clock {
    use std::fmt;
    use super::Span;

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

pub mod ty {
    use super::clock;
    use super::Sp;
    use super::Tuple;
    use std::fmt;

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

    #[derive(Debug, Clone)]
    pub struct Stream {
        pub base: Sp<TyBase>,
        pub depth: clock::Depth,
    }

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

pub mod expr {
    use super::clock;
    use super::Sp;
    use super::Tuple;
    use std::fmt;

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

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Var {
        pub name: String,
    }

    impl fmt::Display for Var {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    #[derive(Debug, Clone)]
    pub struct ClockVar {
        pub var: Sp<Var>,
        pub depth: clock::Depth,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NodeId {
        pub id: usize,
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

    #[derive(Debug, Clone)]
    pub enum Reference {
        Global(Sp<Var>),
        Var(Sp<ClockVar>),
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

    #[derive(Debug, Clone)]
    pub enum Expr {
        Lit(Sp<Lit>),
        Reference(Sp<Reference>),
        Tuple(Sp<Tuple<Sp<Expr>>>),
        BinOp {
            op: BinOp,
            lhs: Box<Sp<Expr>>,
            rhs: Box<Sp<Expr>>,
        },
        UnOp {
            op: UnOp,
            inner: Box<Sp<Expr>>,
        },
        CmpOp {
            op: CmpOp,
            lhs: Box<Sp<Expr>>,
            rhs: Box<Sp<Expr>>,
        },
        Later {
            clk: clock::Depth,
            before: Box<Sp<Expr>>,
            after: Box<Sp<Expr>>,
        },
        Builtin(Sp<Builtin>),
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

pub mod stmt {
    use super::clock;
    use super::expr;
    use super::Sp;
    use super::Tuple;
    use std::fmt;

    #[derive(Debug, Clone)]
    pub enum VarTuple {
        Single(Sp<expr::Var>),
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

    #[derive(Debug, Clone)]
    pub enum Statement {
        Let {
            target: Sp<VarTuple>,
            source: Sp<expr::Expr>,
        },
        Substep {
            clk: clock::Depth,
            id: Sp<expr::NodeId>,
            args: Sp<Tuple<Sp<expr::Expr>>>,
        },
        Trace {
            msg: String,
            fmt: Tuple<expr::Var>,
        },
        Assert(Sp<expr::Expr>),
    }
}

pub mod decl {
    use super::expr;
    use super::stmt;
    use super::ty;
    use super::Sp;
    use super::Tuple;
    use super::ty::TyBase;
    use std::fmt;

    #[derive(Debug, Clone)]
    pub struct Var {
        pub name: Sp<expr::Var>,
        pub ty: Sp<ty::Stream>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeName(pub String);

    impl fmt::Display for NodeName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Node {
        pub name: Sp<NodeName>,
        pub inputs: Sp<Tuple<Sp<Var>>>,
        pub outputs: Sp<Tuple<Sp<Var>>>,
        pub locals: Sp<Tuple<Sp<Var>>>,
        pub blocks: Vec<Sp<NodeName>>,
        pub stmts: Vec<Sp<stmt::Statement>>,
    }

    #[derive(Debug, Clone)]
    pub struct Const {
        pub name: Sp<expr::Var>,
        pub ty: Sp<TyBase>,
        pub value: Sp<expr::Expr>,
    }

    #[derive(Debug, Clone)]
    pub struct ExtNode {
        pub name: Sp<NodeName>,
        pub inputs: Sp<Tuple<Sp<Var>>>,
        pub outputs: Sp<Tuple<Sp<Var>>>,
    }

    #[derive(Debug, Clone)]
    pub struct ExtConst {
        pub name: Sp<expr::Var>,
        pub ty: Sp<TyBase>,
    }

    #[derive(Debug, Clone)]
    pub enum Decl {
        Const(Sp<Const>),
        Node(Sp<Node>),
        ExtConst(Sp<ExtConst>),
        ExtNode(Sp<ExtNode>),
    }

    #[derive(Debug, Clone)]
    pub struct Prog {
        pub decls: Vec<Sp<Decl>>,
    }
}
