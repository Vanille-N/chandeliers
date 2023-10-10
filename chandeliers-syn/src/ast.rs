use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{Ident, Lit, Token};

trait Hint {
    fn hint(s: ParseStream) -> bool;
}

pub fn skip<T>(_s: ParseStream) -> Result<Option<T>> {
    Ok(None)
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
    #[parse(skip)]
    pub span: Option<Span>,
    #[parse(BaseType::parse)]
    pub base: BaseType,
}

#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(skip)]
    pub span: Option<Span>,
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Ident, Token![,]>,
}

#[derive(syn_derive::Parse)]
pub struct ArgsTy {
    #[parse(skip)]
    pub span: Option<Span>,
    pub args: Decls,
    _colon: Token![:],
    pub ty: Type,
}

#[derive(Default)]
pub struct ArgsTys {
    pub span: Option<Span>,
    pub items: Punctuated<ArgsTy, Token![;]>,
}
impl ArgsTys {
    fn parse_terminated(input: ParseStream) -> Result<Self> {
        Ok(Self {
            span: None,
            items: Punctuated::parse_terminated(input)?,
        })
    }
}
impl ArgsTys {
    fn parse_separated_trailing_until_let(input: ParseStream) -> Result<Self> {
        Ok(Self {
            span: None,
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
    #[parse(skip)]
    pub span: Option<Span>,
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
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

    #[allow(private_bounds)]
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
        #[parse(skip)]
        pub span: Option<Span>,
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
        #[parse(skip)]
        pub span: Option<Span>,
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
        #[parse(skip)]
        pub span: Option<Span>,
        pub fun: Ident,
        #[syn(parenthesized)]
        pub _paren: Paren,
        #[syn(in = _paren)]
        #[parse(Punctuated::parse_terminated)]
        pub args: Punctuated<Box<Expr>, Token![,]>,
    }
    pub type CallLevelExpr = ExprHierarchy<CallExpr, VarLevelExpr>;
    impl Hint for CallExpr {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<Paren> {
                s.parse::<Ident>()?;
                let _content;
                let p = syn::parenthesized!(_content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct ParenExpr {
        #[parse(skip)]
        pub span: Option<Span>,
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
                let _content;
                let p = syn::parenthesized!(_content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NegExpr {
        #[parse(skip)]
        pub span: Option<Span>,
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
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<NegLevelExpr, MulOp>,
    }
    pub type MulLevelExpr = MulExpr;

    #[derive(syn_derive::Parse)]
    pub struct AddExpr {
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<MulLevelExpr, AddOp>,
    }
    pub type AddLevelExpr = AddExpr;

    #[derive(syn_derive::Parse)]
    pub struct PreExpr {
        #[parse(skip)]
        pub span: Option<Span>,
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
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<PreLevelExpr, Token![->]>,
    }
    pub type ThenLevelExpr = ThenExpr;

    #[derive(syn_derive::Parse)]
    pub struct FbyExpr {
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<ThenLevelExpr, kw::fby>,
    }
    pub type FbyLevelExpr = FbyExpr;

    #[derive(syn_derive::Parse)]
    pub struct CmpExpr {
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<FbyLevelExpr, CmpOp>,
    }
    pub type CmpLevelExpr = CmpExpr;

    #[derive(syn_derive::Parse)]
    pub struct NotExpr {
        #[parse(skip)]
        pub span: Option<Span>,
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
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<NotLevelExpr, kw::and>,
    }
    pub type AndLevelExpr = AndExpr;

    #[derive(syn_derive::Parse)]
    pub struct OrExpr {
        #[parse(skip)]
        pub span: Option<Span>,
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<AndLevelExpr, kw::or>,
    }
    pub type OrLevelExpr = OrExpr;

    #[derive(syn_derive::Parse)]
    pub struct IfExpr {
        #[parse(skip)]
        pub span: Option<Span>,
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
        pub span: Option<Span>,
        pub inner: IfLevelExpr,
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: IfLevelExpr = input.parse()?;
            Ok(Self { span: None, inner })
        }
    }
}

pub use expr::Expr;

#[derive(syn_derive::Parse)]
pub struct Def {
    #[parse(skip)]
    pub span: Option<Span>,
    pub target: TargetExpr,
    _equal: Token![=],
    pub source: expr::Expr,
}

#[derive(syn_derive::Parse)]
pub struct Assertion {
    #[parse(skip)]
    pub span: Option<Span>,
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
    #[parse(skip)]
    pub span: Option<Span>,
    _var: kw::var,
    #[parse(ArgsTys::parse_separated_trailing_until_let)]
    pub decls: ArgsTys,
}

pub struct SpanMarker(Span);

impl Parse for SpanMarker {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(SpanMarker(input.span()))
    }
}

#[derive(syn_derive::Parse)]
pub enum OptionalVarsDecl {
    #[parse(peek = kw::var)]
    Decls(VarsDecl),
    None(SpanMarker),
}

#[derive(syn_derive::Parse)]
pub struct Node {
    #[parse(skip)]
    pub span: Option<Span>,

    _node: kw::node,

    pub name: Ident,

    #[syn(parenthesized)]
    _inputs_paren: Paren,
    #[syn(in = _inputs_paren)]
    #[parse(ArgsTys::parse_terminated)]
    pub inputs: ArgsTys,

    _returns: kw::returns,

    #[syn(parenthesized)]
    _outputs_paren: Paren,
    #[syn(in = _outputs_paren)]
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
    #[parse(skip)]
    pub span: Option<Span>,
    action: Ident,
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
    #[parse(Punctuated::parse_separated_nonempty)]
    values: Punctuated<AttrArg, Token![,]>,
}

#[derive(syn_derive::Parse)]
pub struct Attribute {
    #[parse(skip)]
    pub span: Option<Span>,
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

pub trait InputSpan: Sized {
    fn force_input_span(&self) -> Span;
    fn input_span(&self) -> Span;
    fn span_everything(&mut self);
    // Provided
    fn map_with_span<U, F>(self, f: F) -> chandeliers_san::candle::ast::Sp<U>
    where
        F: FnOnce(Span, Self) -> U,
    {
        let span = self.input_span();
        chandeliers_san::candle::ast::Sp::new(f(span, self), span)
    }
    fn map_ref_with_span<U, F>(&self, f: F) -> chandeliers_san::candle::ast::Sp<U>
    where
        F: FnOnce(Span, &Self) -> U,
    {
        let span = self.input_span();
        chandeliers_san::candle::ast::Sp::new(f(span, self), span)
    }

}

macro_rules! input_span_cached {
    ($ty:ident) => {
        fn input_span(&self) -> Span {
            self.span.expect(&format!("A Span was not properly registered on type {}. Don't forget to call `span_everything`.", stringify!($ty)))
        }
    }
}

macro_rules! span_everything_trivial {
    () => {
        fn span_everything(&mut self) {}
    };
}

macro_rules! input_span_trivial {
    () => {
        fn input_span(&self) -> Span {
            self.force_input_span()
        }
    };
}

macro_rules! impl_input_span_by_spanned {
    ($t:ty) => {
        impl InputSpan for $t {
            fn force_input_span(&self) -> Span {
                self.span()
            }
            input_span_trivial!();
            span_everything_trivial!();
        }
    };
}

macro_rules! force_input_span_by_match {
    ( $( $variant:ident ( $field:ident ), )* ) => {
        fn force_input_span(&self) -> Span {
            match self {
                $(
                    Self::$variant ( $field ) => {
                        $field.input_span()
                    }
                )*
            }
        }
    }
}

macro_rules! span_everything_by_match {
    ( $( $variant:ident ( $field:ident ), )* ) => {
        fn span_everything(&mut self) {
            match self {
                $(
                    Self::$variant ( $field ) => {
                        $field.span_everything();
                    }
                )*
            }
        }
    }
}

macro_rules! force_input_span_is_projection {
    ( $field:ident ) => {
        fn force_input_span(&self) -> Span {
            self.$field.input_span()
        }
    };
}
macro_rules! force_input_span_is_join {
    ( $left:ident .. $right:ident ) => {
        fn force_input_span(&self) -> Span {
            self.$left
                .input_span()
                .join(self.$right.input_span())
                .expect("Faulty span")
        }
    };
}

macro_rules! span_everything_for_fields {
    ( $( $field:ident ),* ) => {
        fn span_everything(&mut self) {
            $(
                self.$field.span_everything();
            )*
            self.span = Some(self.force_input_span());
        }
    }
}

impl_input_span_by_spanned!(Token![#]);
impl_input_span_by_spanned!(Token![-]);
impl_input_span_by_spanned!(Token![if]);
impl_input_span_by_spanned!(Ident);
impl_input_span_by_spanned!(kw::pre);
impl_input_span_by_spanned!(kw::node);
impl_input_span_by_spanned!(kw::tel);
impl_input_span_by_spanned!(kw::not);
impl_input_span_by_spanned!(kw::var);
impl_input_span_by_spanned!(kw::assert);
impl_input_span_by_spanned!(syn::Lit);
impl_input_span_by_spanned!(kw::int);
impl_input_span_by_spanned!(kw::float);
impl_input_span_by_spanned!(kw::bool);

impl InputSpan for Paren {
    fn force_input_span(&self) -> Span {
        self.span.join()
    }
    input_span_trivial!();
    span_everything_trivial!();
}

fn joined<L: InputSpan, R: InputSpan>(l: &L, r: &R) -> Span {
    l.input_span().join(r.input_span()).expect("Faulty span")
}

impl<T> InputSpan for Box<T>
where
    T: InputSpan,
{
    fn force_input_span(&self) -> Span {
        self.as_ref().force_input_span()
    }
    input_span_trivial!();
    fn span_everything(&mut self) {
        self.as_mut().span_everything();
    }
}

impl<T, P> InputSpan for Punctuated<T, P>
where
    T: InputSpan,
{
    fn force_input_span(&self) -> Span {
        assert!(!self.is_empty(), "Empty punctuated has no span");
        if self.len() == 1 {
            self.first().unwrap().input_span()
        } else {
            joined(self.first().unwrap(), self.last().unwrap())
        }
    }
    fn span_everything(&mut self) {
        for i in self.iter_mut() {
            i.span_everything();
        }
    }
    input_span_trivial!();
}

impl<X, Y> InputSpan for expr::ExprHierarchy<X, Y>
where
    X: InputSpan,
    Y: InputSpan,
{
    force_input_span_by_match! {
        Here(x),
        Below(y),
    }
    input_span_trivial!();
    span_everything_by_match! {
        Here(x),
        Below(y),
    }
}

impl InputSpan for AttrNode {
    fn force_input_span(&self) -> Span {
        match self {
            Self::Tagged(a, n) => joined(a, n.as_ref()),
            Self::Node(n) => n.input_span(),
        }
    }
    input_span_trivial!();
    fn span_everything(&mut self) {
        match self {
            Self::Tagged(a, n) => {
                a.span_everything();
                n.span_everything();
            }
            Self::Node(y) => y.span_everything(),
        }
    }
}

impl InputSpan for Attribute {
    force_input_span_is_join!(marker..attr);
    input_span_cached!(Attribute);
    span_everything_for_fields!(attr);
}

impl InputSpan for expr::LitExpr {
    force_input_span_is_projection!(lit);
    input_span_cached!(LitExpr);
    span_everything_for_fields!(lit);
}

impl InputSpan for expr::CmpExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(CmpExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for AttrDef {
    force_input_span_is_join!(action.._paren);
    input_span_cached!(AttrDef);
    span_everything_for_fields!(values);
}

impl InputSpan for Node {
    force_input_span_is_join!(_node.._kwtel);
    input_span_cached!(Node);
    span_everything_for_fields!(name, inputs, outputs, locals, defs);
}

impl InputSpan for ArgsTys {
    force_input_span_is_projection!(items);
    input_span_cached!(ArgsTys);
    span_everything_for_fields!(items);
}

impl InputSpan for ArgsTy {
    force_input_span_is_join!(args..ty);
    input_span_cached!(ArgsTy);
    span_everything_for_fields!(args, ty);
}

impl InputSpan for Type {
    force_input_span_is_projection!(base);
    input_span_cached!(Type);
    span_everything_for_fields!(base);
}

impl InputSpan for BaseType {
    force_input_span_by_match! {
        Int(i),
        Bool(b),
        Float(f),
    }
    input_span_trivial!();
    span_everything_trivial!();
}

impl InputSpan for Decls {
    force_input_span_is_projection!(ids);
    input_span_cached!(Decls);
    span_everything_for_fields!(ids);
}

impl InputSpan for expr::FbyExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(FbyExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::ThenExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(ThenExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::PreExpr {
    force_input_span_is_join!(_pre..inner);
    input_span_cached!(PreExpr);
    span_everything_for_fields!(inner);
}

impl InputSpan for expr::AddExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(AddExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::MulExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(MulExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::NegExpr {
    force_input_span_is_join!(_neg..inner);
    input_span_cached!(NegExpr);
    span_everything_for_fields!(inner);
}

impl InputSpan for expr::VarExpr {
    force_input_span_is_projection!(name);
    input_span_cached!(VarExpr);
    span_everything_for_fields!(name);
}

impl InputSpan for expr::CallExpr {
    force_input_span_is_join!(fun.._paren);
    input_span_cached!(CallExpr);
    span_everything_for_fields!(args);
}

impl InputSpan for expr::ParenExpr {
    force_input_span_is_projection!(_paren);
    input_span_cached!(ParenExpr);
    span_everything_for_fields!(inner);
}

impl InputSpan for Statement {
    force_input_span_by_match! {
        Assert(a),
        Def(d),
    }
    input_span_trivial!();
    span_everything_by_match! {
        Assert(a),
        Def(d),
    }
}
impl InputSpan for Def {
    force_input_span_is_join!(source..target);
    input_span_cached!(Def);
    span_everything_for_fields!(source, target);
}

impl InputSpan for TargetExpr {
    force_input_span_by_match! {
        Tuple(t),
        Var(v),
    }
    input_span_trivial!();
    span_everything_by_match! {
        Tuple(t),
        Var(v),
    }
}
impl InputSpan for TargetExprTuple {
    force_input_span_is_projection!(_paren);
    input_span_trivial!();
    span_everything_for_fields!(fields);
}

impl InputSpan for expr::Expr {
    force_input_span_is_projection!(inner);
    input_span_cached!(Expr);
    span_everything_for_fields!(inner);
}

impl InputSpan for Assertion {
    force_input_span_is_join!(_assert..expr);
    input_span_cached!(Assertion);
    span_everything_for_fields!(expr);
}

impl InputSpan for expr::IfExpr {
    force_input_span_is_join!(_if..no);
    input_span_cached!(IfExpr);
    span_everything_for_fields!(cond, yes, no);
}

impl InputSpan for expr::OrExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(OrExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::AndExpr {
    force_input_span_is_projection!(items);
    input_span_cached!(AndExpr);
    span_everything_for_fields!(items);
}

impl InputSpan for expr::NotExpr {
    force_input_span_is_join!(_not..inner);
    input_span_cached!(NotExpr);
    span_everything_for_fields!(inner);
}

impl InputSpan for AttrArg {
    force_input_span_by_match! {
        Ident(i),
        Lit(l),
    }
    input_span_trivial!();
    span_everything_trivial!();
}

impl InputSpan for OptionalVarsDecl {
    force_input_span_by_match! {
        Decls(d),
        None(m),
    }
    input_span_trivial!();
    span_everything_by_match! {
        Decls(d),
        None(m),
    }
}

impl InputSpan for VarsDecl {
    force_input_span_is_join!(_var..decls);
    input_span_cached!(VarsDecl);
    span_everything_for_fields!(decls);
}

impl InputSpan for SpanMarker {
    fn force_input_span(&self) -> Span {
        self.0
    }
    input_span_trivial!();
    span_everything_trivial!();
}
