#![feature(associated_type_defaults)]
#![feature(proc_macro_diagnostic)]

use proc_macro2::TokenStream;
//use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Ident, Token, Lit};
use syn::token::{Paren, Bracket};

mod test;

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

/*
impl ToTokens for BaseType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use BaseType::*;
        tokens.extend(match self {
            Int(i) => quote!( i64 ),
            Bool(b) => quote!( bool ),
            Float(f) => quote!( f64 ),
        });
    }
}
*/

#[derive(syn_derive::Parse)]
pub struct Type {
    #[parse(BaseType::parse)]
    pub base: BaseType,
}

/*
impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.base.to_tokens(tokens)
    }
}
*/

#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Ident, Token![,]>,
}

#[derive(syn_derive::Parse)]
struct ArgsTy {
    args: Decls,
    _colon: Token![:],
    ty: Type,
}

/*
impl ToTokens for ArgsTy {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { ty, args, .. } = self;
        let Decls { ids, .. } = args;
        let ids = ids.into_iter().collect::<Vec<_>>();
        tokens.extend(quote! {

            #( #ids : #ty , )*

        });
    }
}
*/

#[derive(Default)]
pub struct ArgsTys {
    items: Punctuated<ArgsTy, Token![;]>,
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
            items: punctuated_parse_separated_trailing_until::<ArgsTy, Token![;], Token![let]>(input)?,
        })
    }
}

/*
impl ToTokens for ArgsTys {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { items } = self;
        for item in items.into_iter() {
            item.to_tokens(tokens);
        }
    }
}
*/

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
    fields: Punctuated<TargetExpr, Token![,]>,
}

/*
impl ToTokens for TargetExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use TargetExpr::*;
        match self {
            Var(v) => v.to_tokens(tokens),
            Tuple(ts) => ts.to_tokens(tokens),
        }
    }
}

impl ToTokens for TargetExprTuple {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { fields, .. } = self;
        let fields = fields.into_iter().collect::<Vec<_>>();
        tokens.extend(quote! {

            ( #( #fields ),* )

        });
    }
}
*/


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
mod expr {
    pub use super::*;
    // Expressions by order of decreasing precedence
    //    [ _ or _ ] (<-)
    //    [ _ and _ ] (<-)
    //    [ not _ ]
    //    [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ] (==)
    //    [ _ fby _ ] (<-)
    //    [ pre _ ]
    //    [ _ -> _ ] (<-)
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

    /*
    impl<Here, Below> ToTokens for ExprHierarchy<Here, Below>
    where
        Here: ToTokens,
        Below: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            use ExprHierarchy::*;
            match self {
                Here(t) => t.to_tokens(tokens),
                Below(t) => t.to_tokens(tokens),
            }
        }
    }
    */

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
        _paren_token: Paren,
        #[syn(in = _paren_token)]
        #[parse(Punctuated::parse_terminated)]
        pub args: Punctuated<Box<Expr>, Token![,]>,
    }
    pub type CallLevelExpr = ExprHierarchy<CallExpr, VarLevelExpr>;
    impl Hint for CallExpr {
        fn hint(s: ParseStream) -> bool {
            s.parse::<CallExpr>().is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct ParenExpr {
        #[syn(parenthesized)]
        _paren_token: Paren,
        #[syn(in = _paren_token)]
        #[parse(Punctuated::parse_terminated)]
        inner: Punctuated<Box<Expr>, Token![,]>,
    }
    pub type ParenLevelExpr = ExprHierarchy<ParenExpr, CallLevelExpr>;
    impl Hint for ParenExpr {
        fn hint(s: ParseStream) -> bool {
            s.parse::<ParenExpr>().is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NegExpr {
        _neg: Token![-],
        inner: Box<NegLevelExpr>,
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
        Mod(Token![%]),
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
    }

    #[derive(syn_derive::Parse)]
    pub struct MulExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        items: Punctuated<NegLevelExpr, MulOp>,
    }
    pub type MulLevelExpr = MulExpr;

    #[derive(syn_derive::Parse)]
    pub struct AddExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        items: Punctuated<MulLevelExpr, AddOp>,
    }
    pub type AddLevelExpr = AddExpr;

    #[derive(syn_derive::Parse)]
    pub struct ThenExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        items: Punctuated<AddLevelExpr, Token![->]>,
    }
    pub type ThenLevelExpr = ThenExpr;

    #[derive(syn_derive::Parse)]
    pub struct PreExpr {
        _pre: kw::pre,
        inner: Box<PreLevelExpr>,
    }
    pub type PreLevelExpr = ExprHierarchy<PreExpr, ThenLevelExpr>;
    impl Hint for PreExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::pre)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct FbyExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        items: Punctuated<PreLevelExpr, kw::fby>,
    }
    pub type FbyLevelExpr = FbyExpr;

    #[derive(syn_derive::Parse)]
    pub struct CmpExpr {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        items: Punctuated<FbyLevelExpr, CmpOp>,
    }
    pub type CmpLevelExpr = CmpExpr;

    #[derive(syn_derive::Parse)]
    pub struct NotExpr {
        _pre: kw::not,
        inner: Box<NotLevelExpr>,
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
        items: Punctuated<NotLevelExpr, kw::and>,
    }
    pub type AndLevelExpr = AndExpr;

    #[derive(syn_derive::Parse)]
    pub struct OrExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        items: Punctuated<AndLevelExpr, kw::or>,
    }
    pub type OrLevelExpr = OrExpr;


    pub struct Expr {
        inner: OrLevelExpr,
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: OrLevelExpr = input.parse()?;
            Ok(Self { inner })
        }
    }

    /*
    impl ToTokens for OrExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( ( todo #items ) ));
        }
    }

    impl ToTokens for AndExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( ( todo #items ) ));
        }
    }

    impl ToTokens for NotExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { inner, .. } = self;
            tokens.extend(quote!( ! ( #inner ) ));
        }
    }

    impl ToTokens for CmpExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( ( #items ) ));
        }
    }

    impl ToTokens for FbyExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( ( todo::fby ) ));
        }
    }

    impl ToTokens for PreExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { inner, .. } = self;
            tokens.extend(quote!( builtins::pre ( #inner ) ));
        }
    }

    impl ToTokens for ThenExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( ( todo::then ) ));
        }
    }

    impl ToTokens for MulExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            let items = items.into_iter().collect::<Vec<_>>();
            tokens.extend(quote!( { todo::mul } ));
        }
    }

    impl ToTokens for CmpOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            use CmpOp::*;
            tokens.extend(match self {
                Le(_) => quote!( <= ),
                Ge(_) => quote!( >= ),
                Lt(_) => quote!( < ),
                Gt(_) => quote!( > ),
                Eq(_) => quote!( == ),
            })
        }
    }

    impl ToTokens for MulOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            use MulOp::*;
            tokens.extend(match self {
                Mul(_) => quote!( * ),
                Div(_) => quote!( / ),
                Mod(_) => quote!( % ),
            })
        }
    }

    impl ToTokens for AddOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            use AddOp::*;
            tokens.extend(match self {
                Add(_) => quote!( + ),
                Sub(_) => quote!( - ),
            });
        }
    }

    impl ToTokens for AddExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items } = self;
            tokens.extend(quote!( { todo::add } ));
        }
    }

    impl ToTokens for NegExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { inner, .. } = self;
            tokens.extend(quote!( ( - #inner ) ));
        }
    }

    impl ToTokens for ParenExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { inner, .. } = self;
            tokens.extend(quote!( ( #inner ) ));
        }
    }

    impl ToTokens for VarExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { name } = self;
            tokens.extend(quote!( #name ));
        }
    }

    impl ToTokens for LitExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { lit } = self;
            tokens.extend(quote!( #lit ));
        }
    }

    impl ToTokens for CallExpr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { fun, args, .. } = self;
            let args = args.into_iter().collect::<Vec<_>>();
            tokens.extend(quote!( #fun ( #( #args ,)* ) ));
        }
    }

    impl ToTokens for Expr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { inner } = self;
            tokens.extend(quote!( #inner ));
        }
    }
    */
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
    assert: kw::assert,
    expr: expr::Expr,
}

#[derive(syn_derive::Parse)]
pub enum Statement {
    #[parse(peek = kw::assert)]
    Assert(Assertion),
    Def(Def),
}

/*
impl ToTokens for Def {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { target, source, .. } = self;
        tokens.extend(quote! {

            #target = #source ;

        });
    }
}
*/

#[derive(syn_derive::Parse)]
pub struct VarsDecl {
    var: kw::var,
    #[parse(ArgsTys::parse_separated_trailing_until_let)]
    decls: ArgsTys,
}

#[derive(syn_derive::Parse)]
pub enum OptionalVarsDecl {
    #[parse(peek = kw::var)]
    Decls(VarsDecl),
    None,
}

#[derive(syn_derive::Parse)]
pub struct Node {
    node: kw::node,

    pub name: Ident,

    #[syn(parenthesized)]
    inputs_paren_token: Paren,
    #[syn(in = inputs_paren_token)]
    #[parse(ArgsTys::parse_terminated)]
    pub inputs: ArgsTys,

    returns: kw::returns,

    #[syn(parenthesized)]
    outputs_paren_token: Paren,
    #[syn(in = outputs_paren_token)]
    #[parse(ArgsTys::parse_terminated)]
    pub outputs: ArgsTys,

    decl_semi: Token![;],

    pub locals: OptionalVarsDecl,

    kwlet: Token![let],

    #[parse(punctuated_parse_separated_trailing_until::<Statement, Token![;], kw::tel>)]
    defs: Punctuated<Statement, Token![;]>,

    kwtel: kw::tel,
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
    paren: Paren,
    #[syn(in = paren)]
    #[parse(Punctuated::parse_separated_nonempty)]
    values: Punctuated<AttrArg, Token![,]>,
}

#[derive(syn_derive::Parse)]
pub struct Attribute {
    marker: Token![#],
    #[syn(bracketed)]
    brack: Bracket,
    #[syn(in = brack)]
    attr: AttrDef,
}

#[derive(syn_derive::Parse)]
pub enum AttrNode {
    #[parse(peek = Token![#])]
    Tagged(Attribute, Box<AttrNode>),
    Node(Node),
}

/*
impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            defs,
            ..
        } = self;
        let defs = defs.into_iter().collect::<Vec<_>>();
        tokens.extend(quote! {

            #[allow(non_camel_case_types)]
            struct #name {
                __clock: usize,
                #inputs
                #outputs
                #locals
            }

            impl #name {
                fn test() {
                    #( #defs ; )*
                }
            }
        });
    }
}

impl ToTokens for OptionalVarsDecl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use OptionalVarsDecl::*;
        match self {
            Decls(vars) => vars.to_tokens(tokens),
            None => {},
        }
    }
}

impl ToTokens for VarsDecl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            decls,
            ..
        } = self;
        decls.to_tokens(tokens);
    }
}
*/

pub struct Prog(Vec<Node>);

