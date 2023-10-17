use std::fmt::Display;
use proc_macro2::{TokenStream, Span};

pub fn type_mismatch<L, R>(source: Span, left: L, right: R, msg: String) -> Error
where L: Into<Span> + std::fmt::Display,
      R: Into<Span> + std::fmt::Display,
{
    Error::TypeMismatch(TypeMismatch {
        source,
        left: (format!("{}", left), left.into()),
        right: (format!("{}", right), right.into()),
        msg,
    })
}

pub fn var_not_found<V0, V1, V1s, V2, V2s>(var: V0, vars1: V1s, vars2: V2s) -> Error
where V0: Into<Span> + std::fmt::Display,
      V1: std::fmt::Display,
      V1s: Iterator<Item = V1>,
      V2: std::fmt::Display,
      V2s: Iterator<Item = V2>,
{
    let suggest1 = vars1.map(|v| format!("{v}")).collect::<Vec<_>>();
    let suggest2 = vars2.map(|v| format!("{v}")).collect::<Vec<_>>();
    Error::VarNotFound(VarNotFound {
        var: (format!("{}", var), var.into()),
        suggest1: if suggest1.is_empty() { String::from("(none declared)") } else { suggest1.join(", ") },
        suggest2: if suggest2.is_empty() { String::from("(none declared)") } else { suggest2.join(", ") },
    })
}

pub fn not_const<S>(msg: S, span: Span) -> Error
where S: std::fmt::Display,
{
    Error::NotConst(NotConst {
        msg: format!("{}", msg),
        span,
    })
}

pub fn binop_mismatch<S1, S2, L, R>(op: S1, span: Span, problem: S2, left: L, right: R) -> Error
where S1: Display,
      S2: Display,
      L: Into<Span> + Display,
      R: Into<Span> + Display,
{
    Error::BinopMismatch(BinopMismatch {
        msg: format!("Binary operator {} expects arguments of {}", op, problem),
        span,
        left: (format!("{left}"), left.into()),
        right: (format!("{right}"), right.into()),
    })
}

pub fn unop_mismatch<S1, S2, I>(op: S1, span: Span, problem: S2, inner: I) -> Error
where S1: Display,
      S2: Display,
      I: Into<Span> + Display,
{
    Error::UnopMismatch(UnopMismatch {
        msg: format!("Binary operator {} expects arguments of {}", op, problem),
        span,
        inner: (format!("{inner}"), inner.into()),
    })
}

pub fn bool_required<S, I>(msg: S, span: Span, inner: I) -> Error
where S: Display,
      I: Into<Span> + Display,
{
    Error::BoolRequired(BoolRequired {
        msg: format!("{msg} should be of type bool"),
        span,
        inner: (format!("{inner}"), inner.into()),
    })
}

pub fn token_stream(toks: TokenStream) -> Error {
    Error::TokenStream(toks)
}


pub enum Error {
    TypeMismatch(TypeMismatch),
    TokenStream(TokenStream),
    VarNotFound(VarNotFound),
    NotConst(NotConst),
    BinopMismatch(BinopMismatch),
    UnopMismatch(UnopMismatch),
    BoolRequired(BoolRequired),
}

impl Error {
    pub fn elements(self) -> Result<Vec<(String, Option<Span>)>, TokenStream> {
        match self {
            Self::TypeMismatch(e) => Ok(e.elements()),
            Self::VarNotFound(e) => Ok(e.elements()),
            Self::NotConst(e) => Ok(e.elements()),
            Self::BinopMismatch(e) => Ok(e.elements()),
            Self::UnopMismatch(e) => Ok(e.elements()),
            Self::BoolRequired(e) => Ok(e.elements()),
            Self::TokenStream(ts) => Err(ts),
        }
    }
}

pub struct TypeMismatch {
    pub source: Span,
    pub left: (String, Span),
    pub right: (String, Span),
    pub msg: String,
}

impl TypeMismatch {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((format!("Type mismatch between the left and right sides: {}", self.msg), Some(self.source)));
        v.push((format!("This element has type {}", self.left.0), Some(self.left.1)));
        v.push((format!("While this element has type {}", self.right.0), Some(self.right.1)));
        v
    }
}

pub struct VarNotFound {
    pub var: (String, Span),
    pub suggest1: String,
    pub suggest2: String,
}

impl VarNotFound {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((format!("Variable {} not found in the context.", self.var.0), Some(self.var.1)));
        v.push((format!("Perhaps you meant one of the local variables: {}", self.suggest1), None));
        v.push((format!("or one of the global variables: {}", self.suggest2), None));
        v
    }
}

pub struct NotConst {
    pub msg: String,
    pub span: Span,
}

impl NotConst {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((format!("{} not valid in const contexts", self.msg), Some(self.span)));
        v.push((format!("You must put this definition inside a node"), None));
        v
    }
}

pub struct BinopMismatch {
    pub msg: String,
    pub span: Span,
    pub left: (String, Span),
    pub right: (String, Span),
}

impl BinopMismatch {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((self.msg, Some(self.span)));
        v.push((format!("The left-hand-side is found to be of type {}", self.left.0), Some(self.left.1)));
        v.push((format!("The right-hand-side is found to be of type {}", self.right.0), Some(self.right.1)));
        v
    }
}

pub struct UnopMismatch {
    pub msg: String,
    pub span: Span,
    pub inner: (String, Span),
}

impl UnopMismatch {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((self.msg, Some(self.span)));
        v.push((format!("The inner value is found to be of type {}", self.inner.0), Some(self.inner.1)));
        v
    }
}

pub struct BoolRequired {
    pub msg: String,
    pub span: Span,
    pub inner: (String, Span),
}

impl BoolRequired {
    pub fn elements(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((self.msg, Some(self.span)));
        v.push((format!("The argument is found to be of type {}", self.inner.0), Some(self.inner.1)));
        v
    }
}
