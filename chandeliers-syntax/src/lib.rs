#![feature(associated_type_defaults)]
#![feature(proc_macro_diagnostic)]

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Ident, Token, Lit};
use syn::token::Paren;

mod test;

trait MultiPeek {
    fn multi_peek(s: ParseStream) -> bool;
}

impl<T: syn::token::Token + Parse> MultiPeek for T {
    fn multi_peek(s: ParseStream) -> bool {
        loop {
            if let Ok(_) = T::parse(&s.fork()) {
                return true;
            } else if s.is_empty() {
                return false;
            } else {
                let _ = s.step(|cursor| {
                    let rest = cursor.token_stream();
                    Ok(((), *cursor))
                });
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

mod punct {
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

impl ToTokens for BaseType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use BaseType::*;
        tokens.extend(match self {
            Int(i) => quote_spanned!(i.span()=> i64),
            Bool(b) => quote_spanned!(b.span()=> bool),
            Float(f) => quote_spanned!(f.span()=> f64),
        });
    }
}

#[derive(syn_derive::Parse)]
pub struct Type {
    #[parse(BaseType::parse)]
    pub base: BaseType,
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.base.to_tokens(tokens)
    }
}

#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Ident, Token![,]>,
}

#[derive(syn_derive::Parse)]
struct ArgsTy {
    args: Decls,
    colon: Token![:],
    ty: Type,
}

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

#[derive(Default, syn_derive::Parse)]
pub struct ArgsTys {
    #[parse(Punctuated::parse_terminated)]
    items: Punctuated<ArgsTy, Token![;]>,
}

impl ToTokens for ArgsTys {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { items } = self;
        let items = items.into_iter().collect::<Vec<_>>();
        tokens.extend(quote! {

            #( #items )*

        });
    }
}

#[derive(syn_derive::Parse)]
pub enum TargetExpr {
    #[parse(peek = Ident)]
    Var(Ident),
    Tuple(TargetExprTuple),
}

#[derive(syn_derive::Parse)]
pub struct TargetExprTuple {
    #[syn(parenthesized)]
    paren_token: Paren,
    #[syn(in = paren_token)]
    #[parse(Punctuated::parse_terminated)]
    fields: Punctuated<TargetExpr, Token![,]>,
}

impl ToTokens for TargetExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use TargetExpr::*;
        let span = self.span();
        tokens.extend(match self {
            Var(v) => quote_spanned!(span=> #v),
            Tuple(ts) => quote_spanned!(span=> #ts),
        });
    }
}

impl ToTokens for TargetExprTuple {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { fields, .. } = self;
        let span = self.span();
        let fields = fields.into_iter().collect::<Vec<_>>();
        tokens.extend(quote_spanned! {span=>

            ( #( #fields ),* )

        });
    }
}

pub fn parse_separated_nonempty_costly<P, T>(
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

mod expr {
    pub use super::*;
    // Expressions by order of decreasing precedence
    //  x [ _ or _ ] (<-)
    //  x [ _ and _ ] (<-)
    //    [ not _ ]
    //  x [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ] (==)
    //  x [ _ fby _ ] (<-)
    //    [ pre _ ]
    //  x [ _ -> _ ] (<-)
    //  x [ _ + _ ], [ _ - _ ] (->)
    //  x [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    //    [ - _ ]
    //  x [ ( _, ... ) ]
    //  x [ f( _, ... ) ]
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

    impl<Here, Below> ToTokens for ExprHierarchy<Here, Below>
    where
        Here: ToTokens,
        Below: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            use ExprHierarchy::*;
            tokens.extend(match self {
                Here(t) => quote!( #t ),
                Below(t) => quote!( #t ),
            });
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct LitExpr {
        pub lit: Lit,
    }
    type LitLevelExpr = LitExpr;
    impl Hint for LitExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Lit)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct VarExpr {
        pub name: Ident,
    }
    type VarLevelExpr = ExprHierarchy<VarExpr, LitLevelExpr>;
    impl Hint for VarExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(Ident)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct CallExpr {
        pub fun: Ident,
        #[syn(parenthesized)]
        paren_token: Paren,
        #[syn(in = paren_token)]
        #[parse(Punctuated::parse_terminated)]
        pub args: Punctuated<Box<Expr>, Token![,]>,
    }
    type CallLevelExpr = ExprHierarchy<CallExpr, VarLevelExpr>;
    impl Hint for CallExpr {
        fn hint(s: ParseStream) -> bool {
            s.parse::<CallExpr>().is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct ParenExpr {
        #[syn(parenthesized)]
        paren_token: Paren,
        #[syn(in = paren_token)]
        #[parse(Punctuated::parse_terminated)]
        inner: Punctuated<Box<Expr>, Token![,]>,
    }
    type ParenLevelExpr = ExprHierarchy<ParenExpr, CallLevelExpr>;
    impl Hint for ParenExpr {
        fn hint(s: ParseStream) -> bool {
            s.parse::<ParenExpr>().is_ok()
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct NegExpr {
        neg: Token![-],
        inner: Box<NegLevelExpr>,
    }
    type NegLevelExpr = ExprHierarchy<NegExpr, ParenLevelExpr>;
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
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<NegLevelExpr, MulOp>,
    }
    type MulLevelExpr = MulExpr;

    #[derive(syn_derive::Parse)]
    pub struct AddExpr {
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<MulLevelExpr, AddOp>,
    }
    type AddLevelExpr = AddExpr;

    #[derive(syn_derive::Parse)]
    pub struct ThenExpr {
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<AddLevelExpr, Token![->]>,
    }
    type ThenLevelExpr = ThenExpr;

    #[derive(syn_derive::Parse)]
    pub struct PreExpr {
        pre: kw::pre,
        inner: Box<PreLevelExpr>,
    }
    type PreLevelExpr = ExprHierarchy<PreExpr, ThenLevelExpr>;
    impl Hint for PreExpr {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::pre)
        }
    }

    #[derive(syn_derive::Parse)]
    pub struct FbyExpr {
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<PreLevelExpr, kw::fby>,
    }
    type FbyLevelExpr = FbyExpr;

    #[derive(syn_derive::Parse)]
    pub struct CmpExpr {
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<FbyLevelExpr, CmpOp>,
    }
    type CmpLevelExpr = CmpExpr;

    #[derive(syn_derive::Parse)]
    pub struct NotExpr {
        pre: kw::not,
        inner: Box<NotLevelExpr>,
    }
    type NotLevelExpr = ExprHierarchy<NotExpr, CmpLevelExpr>;
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
    type AndLevelExpr = AndExpr;

    #[derive(syn_derive::Parse)]
    pub struct OrExpr {
        #[parse(Punctuated::parse_separated_nonempty)]
        items: Punctuated<AndLevelExpr, kw::or>,
    }
    type OrLevelExpr = OrExpr;


    pub struct Expr {
        inner: OrLevelExpr,
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: OrLevelExpr = input.parse()?;
            Ok(Self { inner })
        }
    }

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
}

pub use expr::Expr;

#[derive(syn_derive::Parse)]
pub struct Def {
    pub target: TargetExpr,
    equal: Token![=],
    pub source: expr::Expr,
}

impl ToTokens for Def {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { target, source, .. } = self;
        let span = self.span();
        tokens.extend(quote_spanned! {span=>

            #target = #source ;

        });
    }
}

pub struct Defs(Vec<Def>);

pub struct Node {
    pub name: Ident,
    pub inputs: ArgsTys,
    pub outputs: ArgsTys,
    pub locals: ArgsTys,
    pub defs: Defs,
}

/*
impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: kw::node = input.parse()?;
        let name: Ident = input.parse()?;

        let buf_inputs;
        parenthesized!(buf_inputs in input);
        let inputs = ArgsTys::parse_terminated(&buf_inputs)?;

        let _: kw::returns = input.parse()?;
        let buf_outputs;
        parenthesized!(buf_outputs in input);
        let outputs = ArgsTys::parse_terminated(&buf_outputs)?;
        let _: Option<Token![;]> = input.parse()?;

        let locals = if input.peek(kw::var) {
            let _: kw::var = input.parse()?;
            let locals = ArgsTys::parse_separated_until::<Token![let]>(input)?;
            let _: Option<Token![;]> = input.parse()?;
            locals
        } else {
            ArgsTys::default()
        };

        let _: Token![let] = input.parse()?;
        let defs = Defs::parse_separated_until::<kw::tel>(input)?;
        let _: kw::tel = input.parse()?;

        Ok(Self {
            name,
            inputs,
            outputs,
            locals,
            defs,
        })
    }
}
*/

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            defs,
        } = self;
        tokens.extend(quote! {

            #[allow(non_camel_case_types)]
            struct #name {
                #inputs
                #outputs
                #locals
            }

        });
    }
}

pub struct Prog(Vec<Node>);

