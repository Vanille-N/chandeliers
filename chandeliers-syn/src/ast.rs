use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{Ident, Lit, Token};

trait Hint {
    fn hint(s: ParseStream) -> bool;
}

pub mod kw {
    use syn::custom_keyword;

    // We might eventually accept an arbitrary Ident as type.
    custom_keyword!(int);
    // Warning: this locally overrides the builtin `bool`
    custom_keyword!(bool);
    custom_keyword!(float);

    custom_keyword!(assert);

    custom_keyword!(node);
    custom_keyword!(returns);
    custom_keyword!(var);
    custom_keyword!(tel);

    custom_keyword!(fby);
    custom_keyword!(and);
    custom_keyword!(pre);
    custom_keyword!(or);
    custom_keyword!(not);
    custom_keyword!(then);
}

pub mod punct {
    use syn::custom_punctuation;

    custom_punctuation!(Neq, <>);
}

/// A trait for keywords and punctuation that should be parsed
/// in one way but printed in another.
/// E.g. `Neq` must be parsed as `<>` but printed as `!=`,
///      `and`                   `and`               `&&`,
///      `or`                    `or`                `||`,
/// etc.
trait ParseTransmute: Parse {
    type Target;
    /// Required method
    /// How to transform `Self`.
    fn tok_transmute(self) -> Self::Target;
    /// Provided method
    /// How to parse and transform `Self`.
    fn parse_transmute(s: ParseStream) -> Result<Self::Target> {
        let me: Self = s.parse()?;
        let res = Self::tok_transmute(me);
        Ok(res)
    }
}

#[derive(syn_derive::Parse)]
pub enum BaseType {
    #[parse(peek = kw::int)]
    Int(kw::int),
    #[parse(peek = kw::bool)]
    Bool(kw::bool),
    #[parse(peek = kw::float)]
    Float(kw::float),
}

#[derive(syn_derive::Parse)]
pub struct Type {
    #[parse(BaseType::parse)]
    pub base: BaseType,
}

#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Ident, Token![,]>,
}

#[derive(syn_derive::Parse)]
pub struct ArgsTy {
    pub args: Decls,
    _colon: Token![:],
    pub ty: Type,
}

#[derive(Default)]
pub struct ArgsTys {
    pub items: Punctuated<ArgsTy, Token![;]>,
}
impl ArgsTys {
    fn parse_terminated(input: ParseStream) -> Result<Self> {
        Ok(Self {
            items: Punctuated::parse_terminated(input)?,
        })
    }
}
impl ArgsTys {
    fn parse_separated_trailing_until_let(input: ParseStream) -> Result<Self> {
        Ok(Self {
            items: punctuated_parse_separated_trailing_until::<ArgsTy, Token![;], Token![let]>(
                input,
            )?,
        })
    }
}

#[derive(syn_derive::Parse)]
pub enum TargetExpr {
    #[parse(peek = Paren)]
    Tuple(TargetExprTuple),
    Var(Ident),
}

#[derive(syn_derive::Parse)]
pub struct TargetExprTuple {
    #[syn(parenthesized)]
    _paren_token: Paren,
    #[syn(in = _paren_token)]
    #[parse(Punctuated::parse_terminated)]
    pub fields: Punctuated<TargetExpr, Token![,]>,
}

pub fn punctuated_parse_separated_nonempty_costly<T, P>(
    input: ParseStream,
) -> Result<Punctuated<T, P>>
where
    T: Parse,
    P: Parse,
{
    let mut punctuated = Punctuated::new();

    loop {
        let value: T = input.parse()?;
        punctuated.push_value(value);
        if P::parse(&input.fork()).is_err() {
            break;
        }
        let punct: P = input.parse()?;
        punctuated.push_punct(punct);
    }

    Ok(punctuated)
}

pub fn punctuated_parse_separated_trailing_until<T, P, E>(
    input: ParseStream,
) -> Result<Punctuated<T, P>>
where
    T: Parse,
    P: syn::token::Token + Parse,
    E: syn::token::Token + Parse,
{
    let mut punctuated = Punctuated::new();

    loop {
        if E::peek(input.cursor()) {
            break;
        }
        let value = T::parse(input)?;
        punctuated.push_value(value);
        let punct = input.parse()?;
        punctuated.push_punct(punct);
    }

    Ok(punctuated)
}
pub mod expr {
    pub use super::*;
    // Expressions by order of decreasing precedence
    //    FIXME: if
    //    [ _ or _ ] (<-)
    //    [ _ and _ ] (<-)
    //    [ not _ ]
    //    [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ] (==)
    //    [ _ fby _ ] (<-)
    //    [ _ -> _ ] (<-)
    //    [ pre _ ]
    //    [ _ + _ ], [ _ - _ ] (->)
    //    [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    //    [ - _ ]
    //    [ ( _, ... ) ]
    //    [ f( _, ... ) ]
    //    [ v ]

    #[derive(syn_derive::Parse)]
    pub enum ExprHierarchyParser<Here, Below>
    where
        Here: Parse + Hint,
        Below: Parse,
    {
        #[parse(peek_func = Here::hint)]
        Here(Here),
        Below(Below),
    }

    pub enum ExprHierarchy<Here, Below> {
        Here(Here),
        Below(Below),
    }

    impl<Here, Below> Parse for ExprHierarchy<Here, Below>
    where
        Here: Parse + Hint,
        Below: Parse,
    {
        fn parse(input: ParseStream) -> Result<Self> {
            let x: ExprHierarchyParser<Here, Below> = input.parse()?;
            Ok(match x {
                ExprHierarchyParser::Here(t) => ExprHierarchy::Here(t),
                ExprHierarchyParser::Below(t) => ExprHierarchy::Below(t),
            })
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct LitExpr {
        pub lit: Lit,
    }
    pub type LitLevelExpr = LitExpr;
    impl Hint for LitExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Lit)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct VarExpr {
        pub name: Ident,
    }
    pub type VarLevelExpr = ExprHierarchy<VarExpr, LitLevelExpr>;
    impl Hint for VarExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Ident)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct CallExpr {
        pub fun: Ident,
        #[syn(parenthesized)]
        pub _paren_token: Paren,
        #[syn(in = _paren_token)]
        #[parse(Punctuated::parse_terminated)]
        pub args: Punctuated<Box<Expr>, Token![,]>,
    }
    pub type CallLevelExpr = ExprHierarchy<CallExpr, VarLevelExpr>;
    impl Hint for CallExpr {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<Paren> {
                s.parse::<Ident>()?;
                let content;
                let p = syn::parenthesized!(content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()

        }
    }

    #[derive(syn_derive::Parse)]
    pub struct ParenExpr {
        #[syn(parenthesized)]
        pub _paren: Paren,
        #[syn(in = _paren)]
        #[parse(Punctuated::parse_terminated)]
        pub inner: Punctuated<Box<Expr>, Token![,]>,
    }
    pub type ParenLevelExpr = ExprHierarchy<ParenExpr, CallLevelExpr>;
    impl Hint for ParenExpr {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<Paren> {
                let content;
                let p = syn::parenthesized!(content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NegExpr {
        pub _neg: Token![-],
        pub inner: Box<NegLevelExpr>,
    }
    pub type NegLevelExpr = ExprHierarchy<NegExpr, ParenLevelExpr>;
    impl Hint for NegExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![-])
        }
    }

    #[derive(syn_derive::Parse)]
    pub enum MulOp {
        #[parse(peek = Token![*])]
        Mul(Token![*]),
        #[parse(peek = Token![/])]
        Div(Token![/]),
        #[parse(peek = Token![%])]
        Rem(Token![%]),
    }

    fn exactly_token_neg(s: ParseStream) -> bool {
        s.peek(Token![-]) && !s.peek2(Token![>])
    }
    #[derive(syn_derive::Parse)]
    pub enum AddOp {
        #[parse(peek = Token![+])]
        Add(Token![+]),
        #[parse(peek_func = exactly_token_neg)]
        Sub(Token![-]),
    }

    #[derive(syn_derive::Parse)]
    pub enum CmpOp {
        #[parse(peek = Token![<=])]
        Le(Token![<=]),
        #[parse(peek = Token![>=])]
        Ge(Token![>=]),
        #[parse(peek = Token![<])]
        Lt(Token![<]),
        #[parse(peek = Token![>])]
        Gt(Token![>]),
        #[parse(peek = Token![=])]
        Eq(Token![=]),
        #[parse(peek = punct::Neq)]
        Ne(punct::Neq),
    }

    #[derive(syn_derive::Parse)]
    pub struct MulExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<NegLevelExpr, MulOp>,
    }
    pub type MulLevelExpr = MulExpr;

    #[derive(syn_derive::Parse)]
    pub struct AddExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<MulLevelExpr, AddOp>,
    }
    pub type AddLevelExpr = AddExpr;

    #[derive(syn_derive::Parse)]
    pub struct PreExpr {
        pub _pre: kw::pre,
        pub inner: Box<PreLevelExpr>,
    }
    pub type PreLevelExpr = ExprHierarchy<PreExpr, AddLevelExpr>;
    impl Hint for PreExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::pre)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct ThenExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<PreLevelExpr, Token![->]>,
    }
    pub type ThenLevelExpr = ThenExpr;

    #[derive(syn_derive::Parse)]
    pub struct FbyExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<ThenLevelExpr, kw::fby>,
    }
    pub type FbyLevelExpr = FbyExpr;

    #[derive(syn_derive::Parse)]
    pub struct CmpExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<FbyLevelExpr, CmpOp>,
    }
    pub type CmpLevelExpr = CmpExpr;

    #[derive(syn_derive::Parse)]
    pub struct NotExpr {
        pub _not: kw::not,
        pub inner: Box<NotLevelExpr>,
    }
    pub type NotLevelExpr = ExprHierarchy<NotExpr, CmpLevelExpr>;
    impl Hint for NotExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::not)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct AndExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<NotLevelExpr, kw::and>,
    }
    pub type AndLevelExpr = AndExpr;

    #[derive(syn_derive::Parse)]
    pub struct OrExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<AndLevelExpr, kw::or>,
    }
    pub type OrLevelExpr = OrExpr;

    #[derive(syn_derive::Parse)]
    pub struct IfExpr {
        pub _if: Token![if],
        pub cond: OrLevelExpr,
        pub _then: kw::then,
        pub yes: OrLevelExpr,
        pub _else: Token![else],
        pub no: OrLevelExpr,
    }
    impl Hint for IfExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![if])
        }
    }
    pub type IfLevelExpr = ExprHierarchy<IfExpr, OrLevelExpr>;

    pub struct Expr {
        pub inner: IfLevelExpr,
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: IfLevelExpr = input.parse()?;
            Ok(Self { inner })
        }
    }
}

pub use expr::Expr;

#[derive(syn_derive::Parse)]
pub struct Def {
    pub target: TargetExpr,
    _equal: Token![=],
    pub source: expr::Expr,
}

#[derive(syn_derive::Parse)]
pub struct Assertion {
    _assert: kw::assert,
    pub expr: expr::Expr,
}

#[derive(syn_derive::Parse)]
pub enum Statement {
    #[parse(peek = kw::assert)]
    Assert(Assertion),
    Def(Def),
}

#[derive(syn_derive::Parse)]
pub struct VarsDecl {
    _var: kw::var,
    #[parse(ArgsTys::parse_separated_trailing_until_let)]
    pub decls: ArgsTys,
}

#[derive(syn_derive::Parse)]
pub enum OptionalVarsDecl {
    #[parse(peek = kw::var)]
    Decls(VarsDecl),
    None,
}

#[derive(syn_derive::Parse)]
pub struct Node {
    _node: kw::node,

    pub name: Ident,

    #[syn(parenthesized)]
    inputs_paren_token: Paren,
    #[syn(in = inputs_paren_token)]
    #[parse(ArgsTys::parse_terminated)]
    pub inputs: ArgsTys,

    _returns: kw::returns,

    #[syn(parenthesized)]
    outputs_paren_token: Paren,
    #[syn(in = outputs_paren_token)]
    #[parse(ArgsTys::parse_terminated)]
    pub outputs: ArgsTys,

    _decl_semi: Token![;],

    pub locals: OptionalVarsDecl,

    _kwlet: Token![let],

    #[parse(punctuated_parse_separated_trailing_until::<Statement, Token![;], kw::tel>)]
    pub defs: Punctuated<Statement, Token![;]>,

    _kwtel: kw::tel,
}

#[derive(syn_derive::Parse)]
pub enum AttrArg {
    #[parse(peek = Ident)]
    Ident(Ident),
    #[parse(peek = Lit)]
    Lit(Lit),
}

#[derive(syn_derive::Parse)]
pub struct AttrDef {
    action: Ident,
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
    #[parse(Punctuated::parse_separated_nonempty)]
    values: Punctuated<AttrArg, Token![,]>,
}

#[derive(syn_derive::Parse)]
pub struct Attribute {
    marker: Token![#],
    #[syn(bracketed)]
    _brack: Bracket,
    #[syn(in = _brack)]
    attr: AttrDef,
}

#[derive(syn_derive::Parse)]
pub enum AttrNode {
    #[parse(peek = Token![#])]
    Tagged(Attribute, Box<AttrNode>),
    Node(Node),
}

pub struct Prog(Vec<Node>);

use proc_macro2::Span;

pub trait InputSpan {
    fn input_span(&self) -> Span;
}

impl InputSpan for Token![#] { fn input_span(&self) -> Span { self.span() } }
impl InputSpan for Token![-] { fn input_span(&self) -> Span { self.span() } }
impl InputSpan for Ident { fn input_span(&self) -> Span { self.span() } }
impl InputSpan for kw::pre { fn input_span(&self) -> Span { self.span() } }
impl InputSpan for syn::Lit { fn input_span(&self) -> Span { self.span() } }
impl InputSpan for Paren { fn input_span(&self) -> Span { self.span.join() } }

fn joined<L: InputSpan, R: InputSpan>(l: &L, r: &R) -> Span {
    l.input_span().join(r.input_span()).unwrap()
}

impl<T, P> InputSpan for Punctuated<T, P>
where T: InputSpan {
    fn input_span(&self) -> Span {
        if self.len() == 1 {
            self.first().unwrap().input_span()
        } else {
            joined(self.first().unwrap(), self.last().unwrap())
        }
    }
}

impl<X, Y> InputSpan for expr::ExprHierarchy<X, Y>
where X: InputSpan,
      Y: InputSpan,
{
    fn input_span(&self) -> Span {
        match self {
            Self::Here(x) => x.input_span(),
            Self::Below(y) => y.input_span(),
        }
    }
}

impl InputSpan for AttrNode {
    fn input_span(&self) -> Span {
        match self {
            Self::Tagged(a, n) => joined(a, n.as_ref()),
            Self::Node(n) => n.input_span(),
        }
    }
}

impl InputSpan for Attribute {
    fn input_span(&self) -> Span {
        joined(&self.marker, &self.attr)
    }
}

impl InputSpan for expr::LitExpr {
    fn input_span(&self) -> Span {
        self.lit.input_span()
    }
}

impl InputSpan for expr::CmpExpr {
    fn input_span(&self) -> Span {
        self.items.input_span()
    }
}

impl InputSpan for AttrDef {
    fn input_span(&self) -> Span {
        joined(&self.action, &self._paren)
    }
}

impl InputSpan for Node {
    fn input_span(&self) -> Span {
        self._node.span().join(self._kwtel.span()).unwrap()
    }
}

impl InputSpan for expr::FbyExpr {
    fn input_span(&self) -> Span {
        self.items.input_span()
    }
}

impl InputSpan for expr::ThenExpr {
    fn input_span(&self) -> Span {
        self.items.input_span()
    }
}

impl InputSpan for expr::PreExpr {
    fn input_span(&self) -> Span {
        joined(&self._pre, self.inner.as_ref())
    }
}

impl InputSpan for expr::AddExpr {
    fn input_span(&self) -> Span {
        self.items.input_span()
    }
}

impl InputSpan for expr::MulExpr {
    fn input_span(&self) -> Span {
        self.items.input_span()
    }
}

impl InputSpan for expr::NegExpr {
    fn input_span(&self) -> Span {
        joined(&self._neg, self.inner.as_ref())
    }
}

impl InputSpan for expr::VarExpr {
    fn input_span(&self) -> Span {
        self.name.input_span()
    }
}

impl InputSpan for expr::CallExpr {
    fn input_span(&self) -> Span {
        joined(&self.fun, &self._paren_token)
    }
}

impl InputSpan for expr::ParenExpr {
    fn input_span(&self) -> Span {
        self._paren.span.join()
    }
}
