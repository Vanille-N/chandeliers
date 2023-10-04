#![feature(associated_type_defaults)]
#![feature(proc_macro_diagnostic)]

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Ident, Token};
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
    custom_punctuation!(FAdd, +.);
    custom_punctuation!(FMul, *.);
    custom_punctuation!(FDiv, /.);
    custom_punctuation!(FNeg, -.);
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
    // x  [ _ and _ ] (<-)
    // x  [ _ or _ ] (<-)
    // x  [ not _ ]
    // x  [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ]
    // x  [ _ fby _ ] (<-)
    //    [ pre _ ]
    //    [ _ -> _ ] (<-)
    //    [ _ + _ ], [ _ - _ ] (->)
    //    [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    //    [ - _ ]
    //    [ ( _ ) ]
    //    [ f(_,...) ]
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
    pub struct VarExpr {
        pub name: Ident,
    }
    type VarLevelExpr = VarExpr;
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
        inner: Box<Expr>,
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
        inner: Box<Expr>,
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
    pub struct MulExpr {
        #[parse(parse_separated_nonempty_costly)]
        items: Punctuated<ParenLevelExpr, MulOp>,
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

    pub struct Expr {
        inner: PreLevelExpr,
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: PreLevelExpr = input.parse()?;
            Ok(Self { inner })
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


    /*
    trait ResolveAssociativity: Sized {
        type Target;
        fn resolve_associativity(self) -> Self::Target;
    }
    impl ResolveAssociativity for NegExpr {
        type Target = Self;
        fn resolve_associativity(self) -> Self::Target {
            self
        }
    }
    impl ResolveAssociativity for ParenExpr {
        type Target = Self;
        fn resolve_associativity(self) -> Self::Target {
            self
        }
    }
    impl ResolveAssociativity for VarExpr {
        type Target = Self;
        fn resolve_associativity(self) -> Self::Target {
            self
        }
    }
    impl ResolveAssociativity for CallExpr {
        type Target = Self;
        fn resolve_associativity(self) -> Self::Target {
            self
        }
    }

    impl<Here, Below> ResolveAssociativity for ExprHierarchy<Here, Below>
    where
        Here: Parse + ResolveAssociativity,
        Below: Parse + ResolveAssociativity,
    {
        type Target = ExprHierarchy<Here::Target, Below::Target>;
        fn resolve_associativity(self) -> Self::Target {
            use ExprHierarchy::*;
            match self {
                Here(t) => Here(t.resolve_associativity()),
                Below(t) => Below(t.resolve_associativity()),
            }
        }
    }

    impl ResolveAssociativity for RevMulExpr {
        type Target = MulExpr;
        fn resolve_associativity(self) -> Self::Target {
            // We want to turn `x . (y . (z . w))` into `((x . y) . z) +.w`
            fn aux(lhs: MulLevelExpr, op: MulOp, rhs: RevMulLevelExpr) -> MulExpr {
                match rhs {
                    ExprHierarchy::Here(rhs) => {
                        // we want to insert lhs to the left of rhs.
                        let RevMulExpr { lhs: rlhs, op: rop, rhs: rrhs } = rhs;
                        aux(
                            ExprHierarchy::Here(MulExpr {
                                lhs: Box::new(lhs),
                                op: op,
                                rhs: rlhs,
                            }),
                            rop,
                            *rrhs,
                        )
                    }
                    ExprHierarchy::Below(rhs) => {
                        // We have reached the base case, insert lhs there
                        MulExpr {
                            lhs: Box::new(lhs),
                            op: op,
                            rhs: Box::new(rhs),
                        }
                    }
                }
            }
            let Self { lhs, op, rhs } = self;
            aux(ExprHierarchy::Below(*lhs), op, *rhs)
        }
    }

    impl ResolveAssociativity for RevAddExpr {
        type Target = AddExpr;
        fn resolve_associativity(self) -> Self::Target {
            // We want to turn `x . (y . (z . w))` into `((x . y) . z) . w`
            fn aux(lhs: AddLevelExpr, op: AddOp, rhs: RevAddLevelExpr) -> AddExpr {
                match rhs {
                    ExprHierarchy::Here(rhs) => {
                        // we want to insert lhs to the left of rhs.
                        let RevAddExpr { lhs: rlhs, op: rop, rhs: rrhs } = rhs;
                        aux(
                            ExprHierarchy::Here(AddExpr {
                                lhs: Box::new(lhs),
                                op: op,
                                rhs: Box::new(rlhs.resolve_associativity()),
                            }),
                            rop,
                            *rrhs,
                        )
                    }
                    ExprHierarchy::Below(rhs) => {
                        // We have reached the base case, insert lhs there
                        AddExpr {
                            lhs: Box::new(lhs),
                            op: op,
                            rhs: Box::new(rhs.resolve_associativity()),
                        }
                    }
                }
            }
            let Self { lhs, op, rhs } = self;
            aux(ExprHierarchy::Below(lhs.resolve_associativity()), op, *rhs)
        }
    }

    impl ResolveAssociativity for RevPreExpr {
        type Target = PreExpr;
        fn resolve_associativity(self) -> Self::Target {
            let RevPreExpr { pre, inner } = self;
            let inner = Box::new(inner.resolve_associativity());
            PreExpr { pre, inner }
        }
    }

    impl ResolveAssociativity for RevThenExpr {
        type Target = ThenExpr;
        fn resolve_associativity(self) -> Self::Target {
            let RevThenExpr { lhs, arrow, rhs } = self;
            let lhs = Box::new(lhs.resolve_associativity());
            let rhs = Box::new(rhs.resolve_associativity());
            ThenExpr { lhs, arrow, rhs }
        }
    }
    */
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

