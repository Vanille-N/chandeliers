//! Translation from Lustre to Candle

use std::fmt;

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

pub(crate) mod clock {
    use std::fmt;

    #[derive(Debug, Clone, Copy)]
    pub struct Depth {
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

pub(crate) mod ty {
    use super::clock;
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
        pub base: TyBase,
        pub depth: clock::Depth,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum TyTuple {
        Single(TyBase),
        Multiple(Tuple<TyTuple>),
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

pub(crate) mod expr {
    use super::clock;
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
        pub var: Var,
        pub depth: clock::Depth,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        Var(ClockVar),
        Node(NodeId),
    }

    impl fmt::Display for Reference {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Var(v) => write!(f, "{v}"),
                Self::Node(n) => write!(f, "{n}"),
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
        Float(Box<Expr>),
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
        Lit(Lit),
        Reference(Reference),
        Tuple(Tuple<Expr>),
        BinOp {
            op: BinOp,
            lhs: Box<Expr>,
            rhs: Box<Expr>,
        },
        UnOp {
            op: UnOp,
            inner: Box<Expr>,
        },
        CmpOp {
            op: CmpOp,
            lhs: Box<Expr>,
            rhs: Box<Expr>,
        },
        Later {
            clk: clock::Depth,
            before: Box<Expr>,
            after: Box<Expr>,
        },
        Builtin(Builtin),
        Ifx {
            cond: Box<Expr>,
            yes: Box<Expr>,
            no: Box<Expr>,
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

pub(crate) mod stmt {
    use super::clock;
    use super::expr;
    use super::Tuple;
    use std::fmt;

    #[derive(Debug, Clone)]
    pub enum VarTuple {
        Single(expr::Var),
        Multiple(Tuple<VarTuple>),
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
        Tick,
        Update(expr::Var),
        Let {
            target: VarTuple,
            source: expr::Expr,
        },
        Substep {
            clk: clock::Depth,
            id: expr::NodeId,
            args: Tuple<expr::Expr>,
        },
        Trace {
            msg: String,
            fmt: Tuple<expr::Var>,
        },
        Assert(expr::Expr),
    }
}

pub(crate) mod decl {
    use std::fmt;
    use super::expr;
    use super::stmt;
    use super::ty;
    use super::Tuple;

    #[derive(Debug, Clone)]
    pub struct Var {
        pub name: expr::Var,
        pub ty: ty::Stream,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct NodeName(String);

    impl fmt::Display for NodeName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Node {
        pub name: NodeName,
        pub inputs: Tuple<Var>,
        pub outputs: Tuple<Var>,
        pub locals: Tuple<Var>,
        pub blocks: Vec<NodeName>,
        pub stmts: Vec<stmt::Statement>,
    }
}
