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

mod expr {
    use super::*;
    // Expressions by order of decreasing precedence
    // [ _ or _ ] (<-)
    // [ _ and _ ] (<-)
    // [ not _ ]
    // [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ]
    // [ _ fby _ ] (<-)
    // [ pre _ ]
    // [ _ -> _ ] (<-)
    // [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    // [ _ + _ ], [ _ - _ ] (->)
    // [ - _ ]
    // [ ( _ ) ]
    // [ v ]
    #[derive(syn_derive::Parse)]
    pub struct Var {
        pub name: Ident,
    }

    impl ToTokens for Var {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { name } = self;
            let span = self.span();
            tokens.extend(quote_spanned! {span=>

                #name

            });
        }
    }
}

#[derive(syn_derive::Parse)]
pub struct Def {
    pub target: TargetExpr,
    equal: Token![=],
    pub source: expr::Var,
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

