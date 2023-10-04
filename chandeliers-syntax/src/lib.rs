#![feature(proc_macro_diagnostic)]

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Ident, Token};

mod kw {
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

/// A parser combinator that looks for zero or more occurences
/// of `P` separated by `T` and stops when it encounters `End`.
/// This may end even if there are tokens in the stream.
trait PunctUntil<T, P>: Sized {
    fn parse_separated_until<End>(input: ParseStream) -> Result<Self>
    where
        T: Parse,
        End: syn::token::Token + Parse,
    {
        Self::parse_separated_with_until::<End>(input, T::parse)
    }

    fn parse_separated_with_until<End>(
        input: ParseStream,
        parser: fn(ParseStream) -> Result<T>,
    ) -> Result<Self>
    where
        End: syn::token::Token + Parse;
}

impl<T, P> PunctUntil<T, P> for Punctuated<T, P>
where
    P: syn::token::Token + Parse,
{
    fn parse_separated_with_until<End>(
        input: ParseStream,
        parser: fn(ParseStream) -> Result<T>,
    ) -> Result<Self>
    where
        End: syn::token::Token + Parse,
    {
        let mut punctuated = Punctuated::new();

        loop {
            if End::peek(input.cursor()) {
                break;
            }
            let value = parser(input)?;
            punctuated.push_value(value);
            if !P::peek(input.cursor()) {
                break;
            }
            let punct = input.parse()?;
            punctuated.push_punct(punct);
        }

        Ok(punctuated)
    }
}

impl ArgsTys {
    fn parse_terminated(input: ParseStream) -> Result<Self> {
        let inner = Punctuated::<ArgsTy, Token![;]>::parse_terminated(input)?
            .into_iter()
            .collect::<Vec<_>>();
        Ok(Self(inner))
    }

    fn parse_separated_nonempty(input: ParseStream) -> Result<Self> {
        let inner = Punctuated::<ArgsTy, Token![;]>::parse_separated_nonempty(input)?
            .into_iter()
            .collect::<Vec<_>>();
        Ok(Self(inner))
    }

    fn parse_separated_until<End>(input: ParseStream) -> Result<Self>
    where
        End: syn::token::Token + Parse,
    {
        let inner = Punctuated::<ArgsTy, Token![;]>::parse_separated_until::<End>(input)?
            .into_iter()
            .collect::<Vec<_>>();
        Ok(Self(inner))
    }
}

#[derive(Default)]
pub struct ArgsTys(Vec<ArgsTy>);

impl ToTokens for ArgsTys {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self(decls) = self;
        tokens.extend(quote! {

            #( #decls )*

        });
    }
}

#[derive(syn_derive::Parse)]
pub enum TargetExprKind {
    Var(Ident),
    Tuple(Vec<TargetExpr>),
}

pub struct TargetExpr {
    span: Span,
    pub kind: TargetExprKind,
}

impl Parse for TargetExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.lookahead1();
        let span;
        let kind = if ahead.peek(Ident) {
            let id: Ident = input.parse()?;
            span = id.span();
            TargetExprKind::Var(id)
        } else {
            let tup;
            parenthesized!(tup in input);
            let sub = Punctuated::<TargetExpr, Token![,]>::parse_terminated(&tup)?;
            span = sub.span();
            TargetExprKind::Tuple(sub.into_iter().collect())
        };
        Ok(Self {
            kind,
            span,
        })
    }
}

impl ToTokens for TargetExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use TargetExprKind::*;
        let span = self.span;
        tokens.extend(match &self.kind {
            Var(v) => quote_spanned!(span=> #v),
            Tuple(ts) => {
                quote_spanned! {span=>

                    ( #( #ts ),* )

                }
            }
        });
    }
}

pub enum BinOp {
    Fby(kw::fby),
    Then(Token![->]),
    Eql(Token![=]),
    Le(Token![<=]),
    Lt(Token![<]),
    Ge(Token![>=]),
    Gt(Token![>]),
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
    Mod(Token![%]),
}

pub enum UnOp {
    Pre(kw::pre),
    Not(kw::not),
    Neg(Token![-]),
}

pub enum SourceExpr {
    Var(Ident),
    Tuple(Vec<SourceExpr>),
    BinOp(BinOp, Box<SourceExpr>, Box<SourceExpr>),
    UnOp(UnOp, Box<SourceExpr>),
    Call(Ident, Vec<SourceExpr>),
}

impl SourceExpr {
    fn inner_span(&self) -> Span {
        use SourceExpr::*;
        match self {
            Var(v) => v.span(),
            Tuple(ss) => ss.first().unwrap().inner_span().join(ss.last().unwrap().inner_span()).unwrap(),
            BinOp(op, lhs, rhs) => lhs.inner_span().join(rhs.inner_span()).unwrap().join(op.inner_span()).unwrap(),
            UnOp(op, sub) => sub.inner_span().join(op.inner_span()).unwrap(),
        }
    }
}

impl Parse for SourceExpr {
    fn parse(_input: ParseStream) -> Result<Self> {
        // First off, 
        unimplemented!("Parse for SourceExpr")
    }
}

impl ToTokens for SourceExpr {
    fn to_tokens(&self, _tokens: &mut TokenStream) {
        unimplemented!("ToTokens for SourceExpr")
    }
}

pub struct Def {
    span: Span,
    pub target: TargetExpr,
    pub source: SourceExpr,
}

impl Parse for Def {
    fn parse(input: ParseStream) -> Result<Self> {
        let target: TargetExpr = input.parse()?;
        let _: Token![=] = input.parse()?;
        let source: SourceExpr = input.parse()?;
        let span = target.span.join(source.inner_span()).unwrap();
        Ok(Self {
            span,
            target,
            source,
        })
    }
}

impl ToTokens for Def {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { span, target, source } = self;
        tokens.extend(quote_spanned! {*span=>

            #target = #source ;

        });
    }
}

pub struct Defs(Vec<Def>);

impl Defs {
    fn parse_separated_until<End>(input: ParseStream) -> Result<Self>
    where
        End: syn::token::Token + Parse,
    {
        let inner = Punctuated::<Def, Token![;]>::parse_separated_until::<End>(input)?
            .into_iter()
            .collect::<Vec<_>>();
        Ok(Self(inner))
    }
}

pub struct Node {
    pub name: Ident,
    pub inputs: ArgsTys,
    pub outputs: ArgsTys,
    pub locals: ArgsTys,
    pub defs: Defs,
}

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

