use chandeliers_san::ast::{Sp, SpanEnd};
use proc_macro2::Span;
use std::fmt;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{Ident, Lit, Token};

macro_rules! span_end_on_field {
    ($ty:ident . $field:ident) => {
        impl SpanEnd for $ty {
            fn span_end(&self) -> Option<Span> {
                self.$field.span_end()
            }
        }
    };
}

macro_rules! span_end_by_match {
    ( $ty:ident . $( $variant:ident ( $($field:tt),* ) => $select:ident ; )* ) => {
        impl SpanEnd for $ty {
            #[allow(unreachable_patterns)]
            fn span_end(&self) -> Option<Span> {
                match self {
                    $( Self::$variant ( $($field),* ) => $select.span_end(), )*
                    _ => None,
                }
            }
        }
    }
}

macro_rules! span_end_from_spanned {
    ( $($ty:tt)* ) => {
        impl SpanEnd for $($ty)* {
            fn span_end(&self) -> Option<Span> {
                Some(self.span())
            }
        }
    }
}

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

span_end_from_spanned!(kw::int);
span_end_from_spanned!(kw::bool);
span_end_from_spanned!(kw::float);
span_end_from_spanned!(kw::tel);

pub mod punct {
    use syn::custom_punctuation;

    custom_punctuation!(Neq, <>);
}

pub struct LusIdent {
    pub inner: Ident,
}

impl Parse for LusIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.fork();
        match ahead.call(Ident::parse_any) {
            Ok(inner) => {
                match inner.to_string().as_str() {
                    "true" | "false" | "fby" | "if" | "then" | "else" | "or" | "and" | "not"
                    | "pre" | "node" | "const" | "extern" | "returns" | "var" | "let" | "tel"
                    | "assert" => Err(syn::Error::new(
                        inner.span(),
                        "expected identifier, found keyword reserved by Lustre",
                    )),
                    "crate" | "self" | "Self" | "super" | "move" | "static" => {
                        Err(syn::Error::new(
                            inner.span(),
                            "expected identifier, found keyword reserved by Rust",
                        ))
                    }
                    // NOTE: "int", "float" and "bool" are also keywords, but they are not
                    // reserved. Because the grammar is not ambiguous we can always tell if "float"
                    // refers to the type or the builtin.
                    _ => {
                        let _ = input.call(Ident::parse_any).unwrap();
                        Ok(Self { inner })
                    }
                }
            }
            Err(e) => Err(e),
        }
    }
}

impl LusIdent {
    fn peek(input: ParseStream) -> bool {
        let ahead = input.fork();
        Self::parse(&ahead).is_ok()
    }
}

impl fmt::Display for LusIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

span_end_on_field!(LusIdent.inner);

#[derive(syn_derive::Parse)]
pub enum BaseType {
    #[parse(peek = kw::int)]
    Int(kw::int),
    #[parse(peek = kw::bool)]
    Bool(kw::bool),
    #[parse(peek = kw::float)]
    Float(kw::float),
}
span_end_by_match! {
    BaseType.
        Int(i) => i;
        Bool(b) => b;
        Float(f) => f;
}

#[derive(syn_derive::Parse)]
pub struct Type {
    pub base: Sp<BaseType>,
}
span_end_on_field!(Type.base);

#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Sp<LusIdent>, Token![,]>,
}
span_end_on_field!(Decls.ids);

#[derive(syn_derive::Parse)]
pub struct ArgsTy {
    pub args: Sp<Decls>,
    _colon: Token![:],
    pub ty: Sp<Type>,
}
span_end_on_field!(ArgsTy.ty);

#[derive(Default)]
pub struct ArgsTys {
    pub items: Punctuated<Sp<ArgsTy>, Token![;]>,
}
span_end_on_field!(ArgsTys.items);
impl ArgsTys {
    fn parse_terminated(input: ParseStream) -> Result<Sp<Self>> {
        let mut span = input.span();
        let items = Punctuated::parse_terminated(input)?;
        span = span.join(input.span()).unwrap();
        Ok(Sp {
            t: Self { items },
            span,
        })
    }
}
impl ArgsTys {
    fn parse_separated_trailing_until_let(input: ParseStream) -> Result<Sp<Self>> {
        let mut span = input.span();
        let items =
            punctuated_parse_separated_trailing_until::<Sp<ArgsTy>, Token![;], Token![let]>(input)?;
        span = span.join(input.span()).unwrap();
        Ok(Sp {
            t: Self { items },
            span,
        })
    }
}

#[derive(syn_derive::Parse)]
pub enum TargetExpr {
    #[parse(peek = Paren)]
    Tuple(Sp<TargetExprTuple>),
    Var(Sp<LusIdent>),
}
span_end_by_match! {
    TargetExpr.
        Tuple(t) => t;
        Var(v) => v;
}

#[derive(syn_derive::Parse)]
pub struct TargetExprTuple {
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
    #[parse(Punctuated::parse_terminated)]
    pub fields: Punctuated<Sp<TargetExpr>, Token![,]>,
}
span_end_on_field!(TargetExprTuple.fields);

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
    //    [ _ or _ ] (<-)
    //    [ _ and _ ] (<-)
    //    [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ] (==)
    //    [ _ fby _ ] (<-)
    //    [ _ -> _ ] (<-)
    //    [ _ + _ ], [ _ - _ ] (->)
    //    [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    //
    // Atomics:
    //    [ pre _ ]
    //    [ - _ ]
    //    [ ( _, ... ) ]
    //    [ f( _, ... ) ]
    //    [ v ]
    //    [ not _ ]

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

    impl<X, Y> SpanEnd for ExprHierarchy<X, Y>
    where
        X: SpanEnd,
        Y: SpanEnd,
    {
        fn span_end(&self) -> Option<Span> {
            match self {
                Self::Here(x) => x.span_end(),
                Self::Below(y) => y.span_end(),
            }
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct LitExpr {
        pub lit: Sp<Lit>,
    }
    span_end_on_field!(LitExpr.lit);
    impl Hint for LitExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Lit)
        }
    }

    #[derive(syn_derive::Parse)]
    pub enum AtomicExpr {
        #[parse(peek_func = ParenExpr::hint)]
        Paren(ParenExpr),
        #[parse(peek_func = PreExpr::hint)]
        Pre(PreExpr),
        #[parse(peek_func = NegExpr::hint)]
        Neg(NegExpr),
        #[parse(peek_func = NotExpr::hint)]
        Not(NotExpr),
        #[parse(peek_func = CallExpr::hint)]
        Call(CallExpr),
        #[parse(peek_func = LitExpr::hint)]
        Lit(LitExpr),
        Var(VarExpr),
    }
    span_end_by_match! {
        AtomicExpr.
            Paren(p) => p;
            Pre(p) => p;
            Neg(n) => n;
            Not(n) => n;
            Call(c) => c;
            Lit(l) => l;
            Var(v) => v;

    }

    #[derive(syn_derive::Parse)]
    pub struct VarExpr {
        pub name: Sp<LusIdent>,
    }
    span_end_on_field!(VarExpr.name);
    impl Hint for VarExpr {
        fn hint(s: ParseStream) -> bool {
            LusIdent::peek(s)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct CallExpr {
        pub fun: Sp<LusIdent>,
        #[syn(parenthesized)]
        pub _paren: Paren,
        #[syn(in = _paren)]
        #[parse(Punctuated::parse_terminated)]
        pub args: Punctuated<Sp<Box<Expr>>, Token![,]>,
    }
    span_end_on_field!(CallExpr._paren);
    impl Hint for CallExpr {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<Paren> {
                s.parse::<LusIdent>()?;
                let _content;
                let p = syn::parenthesized!(_content in s);
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
        pub inner: Punctuated<Sp<Box<Expr>>, Token![,]>,
    }
    span_end_on_field!(ParenExpr._paren);
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
    pub struct PreExpr {
        pub _pre: kw::pre,
        pub inner: Sp<Box<AtomicExpr>>,
    }
    span_end_on_field!(PreExpr.inner);
    impl Hint for PreExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::pre)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NegExpr {
        pub _neg: Token![-],
        pub inner: Sp<Box<AtomicExpr>>,
    }
    span_end_on_field!(NegExpr.inner);
    impl Hint for NegExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![-])
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NotExpr {
        pub _not: kw::not,
        pub inner: Sp<Box<AtomicExpr>>,
    }
    span_end_on_field!(NotExpr.inner);
    impl Hint for NotExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::not)
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
        pub items: Punctuated<Sp<AtomicExpr>, MulOp>,
    }
    span_end_on_field!(MulExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct AddExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<MulExpr>, AddOp>,
    }
    span_end_on_field!(AddExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct ThenExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<AddExpr>, Token![->]>,
    }
    span_end_on_field!(ThenExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct FbyExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<ThenExpr>, kw::fby>,
    }
    span_end_on_field!(FbyExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct CmpExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<FbyExpr>, CmpOp>,
    }
    span_end_on_field!(CmpExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct AndExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<Sp<CmpExpr>, kw::and>,
    }
    span_end_on_field!(AndExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct OrExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<Sp<AndExpr>, kw::or>,
    }
    span_end_on_field!(OrExpr.items);

    #[derive(syn_derive::Parse)]
    pub struct IfExpr {
        pub _if: Token![if],
        pub cond: Sp<OrExpr>,
        pub _then: kw::then,
        pub yes: Sp<OrExpr>,
        pub _else: Token![else],
        pub no: Sp<OrExpr>,
    }
    impl Hint for IfExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![if])
        }
    }
    span_end_on_field!(IfExpr.no);
    pub type IfLevelExpr = ExprHierarchy<IfExpr, OrExpr>;

    pub struct Expr {
        pub inner: Sp<IfLevelExpr>,
    }
    span_end_on_field!(Expr.inner);

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: Sp<IfLevelExpr> = input.parse()?;
            Ok(Self { inner })
        }
    }
}

pub use expr::Expr;

#[derive(syn_derive::Parse)]
pub struct Def {
    pub target: Sp<TargetExpr>,
    _equal: Token![=],
    pub source: Sp<expr::Expr>,
}
span_end_on_field!(Def.source);

#[derive(syn_derive::Parse)]
pub struct Assertion {
    _assert: kw::assert,
    pub expr: Sp<expr::Expr>,
}
span_end_on_field!(Assertion.expr);

#[derive(syn_derive::Parse)]
pub enum Statement {
    #[parse(peek = kw::assert)]
    Assert(Sp<Assertion>),
    Def(Sp<Def>),
}
span_end_by_match! {
    Statement.
        Assert(a) => a;
        Def(d) => d;
}

#[derive(syn_derive::Parse)]
pub struct VarsDecl {
    _var: kw::var,
    #[parse(ArgsTys::parse_separated_trailing_until_let)]
    pub decls: Sp<ArgsTys>,
}
span_end_on_field!(VarsDecl.decls);

#[derive(syn_derive::Parse)]
pub enum OptionalVarsDecl {
    #[parse(peek = kw::var)]
    Decls(Sp<VarsDecl>),
    None,
}
span_end_by_match! {
    OptionalVarsDecl.
        Decls(d) => d;
}

#[derive(syn_derive::Parse)]
pub struct Node {
    _node: kw::node,

    pub name: Sp<LusIdent>,

    #[syn(parenthesized)]
    pub _inputs_paren: Paren,
    #[syn(in = _inputs_paren)]
    #[parse(ArgsTys::parse_terminated)]
    pub inputs: Sp<ArgsTys>,

    _returns: kw::returns,

    #[syn(parenthesized)]
    pub _outputs_paren: Paren,
    #[syn(in = _outputs_paren)]
    #[parse(ArgsTys::parse_terminated)]
    pub outputs: Sp<ArgsTys>,

    _decl_semi: Token![;],

    pub locals: Sp<OptionalVarsDecl>,

    _kwlet: Token![let],

    #[parse(punctuated_parse_separated_trailing_until::<Sp<Statement>, Token![;], kw::tel>)]
    pub defs: Punctuated<Sp<Statement>, Token![;]>,

    _kwtel: kw::tel,
}
span_end_on_field!(Node._kwtel);

#[derive(syn_derive::Parse)]
pub struct Const {
    _const: Token![const],
    pub name: Sp<LusIdent>,
    _colon: Token![:],
    pub ty: Sp<Type>,
    _equal: Token![=],
    pub value: Sp<Expr>,
}
span_end_on_field!(Const.value);

#[derive(syn_derive::Parse)]
pub struct ExtNode {
    _extern: Token![extern],
    _node: kw::node,
    pub name: Sp<LusIdent>,

    #[syn(parenthesized)]
    pub _inputs_paren: Paren,
    #[syn(in = _inputs_paren)]
    #[parse(ArgsTys::parse_terminated)]
    pub inputs: Sp<ArgsTys>,

    _returns: kw::returns,

    #[syn(parenthesized)]
    pub _outputs_paren: Paren,
    #[syn(in = _outputs_paren)]
    #[parse(ArgsTys::parse_terminated)]
    pub outputs: Sp<ArgsTys>,
}
span_end_on_field!(ExtNode.outputs);

#[derive(syn_derive::Parse)]
pub struct ExtConst {
    _extern: Token![extern],
    _const: Token![const],
    pub name: Sp<LusIdent>,
    _colon: Token![:],
    pub ty: Sp<Type>,
}
span_end_on_field!(ExtConst.ty);

#[derive(syn_derive::Parse)]
#[parse(prefix = <Token![extern]>::parse)]
pub enum Extern {
    #[parse(peek = Token![const])]
    Const(Sp<ExtConst>),
    Node(Sp<ExtNode>),
}
span_end_by_match! {
    Extern.
        Const(c) => c;
        Node(n) => n;
}

#[derive(syn_derive::Parse)]
pub enum AttrArg {
    #[parse(peek_func = LusIdent::peek)]
    LusIdent(Sp<LusIdent>),
    #[parse(peek = Lit)]
    Lit(Sp<Lit>),
}
span_end_by_match! {
    AttrArg.
        LusIdent(i) => i;
        Lit(l) => l;
}

#[derive(syn_derive::Parse)]
pub struct AttrDef {
    // FIXME
    _action: Sp<LusIdent>,
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
    #[parse(Punctuated::parse_separated_nonempty)]
    values: Punctuated<Sp<AttrArg>, Token![,]>,
}
span_end_on_field!(AttrDef.values);

#[derive(syn_derive::Parse)]
pub struct Attribute {
    _marker: Token![#],
    #[syn(bracketed)]
    _brack: Bracket,
    #[syn(in = _brack)]
    attr: Sp<AttrDef>,
}
span_end_on_field!(Attribute.attr);

#[derive(syn_derive::Parse)]
pub enum Decl {
    #[parse(peek = Token![extern])]
    Extern(Sp<Extern>),
    #[parse(peek = Token![const])]
    Const(Sp<Const>),
    Node(Sp<Node>),
}
span_end_by_match! {
    Decl.
        Extern(e) => e;
        Const(c) => c;
        Node(n) => n;
}

#[derive(syn_derive::Parse)]
pub enum AttrDecl {
    #[parse(peek = Token![#])]
    Tagged(Sp<Attribute>, Sp<Box<AttrDecl>>),
    Node(Sp<Decl>),
}
span_end_by_match! {
    AttrDecl.
        Tagged(_, d) => d;
        Node(n) => n;
}

#[derive(syn_derive::Parse)]
pub struct Prog {
    #[parse(Punctuated::parse_terminated)]
    pub decls: Punctuated<Sp<AttrDecl>, Token![;]>,
}
span_end_on_field!(Prog.decls);
