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
    Error::VarNotFound(VarNotFound {
        var: (format!("{}", var), var.into()),
        suggest1: vars1.map(|v| format!("{v}")).collect::<Vec<_>>().join(", "),
        suggest2: vars2.map(|v| format!("{v}")).collect::<Vec<_>>().join(", "),
    })
}

pub fn token_stream(toks: TokenStream) -> Error {
    Error::TokenStream(toks)
}


pub enum Error {
    TypeMismatch(TypeMismatch),
    TokenStream(TokenStream),
    VarNotFound(VarNotFound),
}

impl Error {
    pub fn elements(self) -> Result<Vec<(String, Option<Span>)>, TokenStream> {
        match self {
            Self::TypeMismatch(tm) => Ok(tm.elements()),
            Self::VarNotFound(v) => Ok(v.elements()),
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


