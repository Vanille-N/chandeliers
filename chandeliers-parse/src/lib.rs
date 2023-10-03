#![feature(proc_macro_diagnostic)]

use proc_macro2::{TokenStream, Span, Punct, Spacing};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use syn::parenthesized;
use syn::{parse_macro_input, Ident, Token};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(int);
    custom_keyword!(bool);
    custom_keyword!(float);

    custom_keyword!(node);
    custom_keyword!(returns);
    custom_keyword!(var);
    custom_keyword!(r#let);
}

enum BaseType {
    Int(kw::int),
    Bool(kw::bool),
    Float(kw::float),
}

impl BaseType {
    fn inner_span(&self) -> Span {
        use BaseType::*;
        match self {
            Int(i) => i.span(),
            Bool(b) => b.span(),
            Float(f) => f.span(),
        }
    }
}

impl Parse for BaseType {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::int) {
            Ok(Self::Int(input.parse()?))
        } else if lookahead.peek(kw::bool) {
            Ok(Self::Bool(input.parse()?))
        } else if lookahead.peek(kw::float) {
            Ok(Self::Float(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for BaseType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use BaseType::*;
        let span = self.inner_span();
        tokens.extend(match self {
            Int(i) => quote_spanned!(span=> i64),
            Bool(b) => quote_spanned!(span=> bool),
            Float(f) => quote_spanned!(span=> f64),
        });
    }
}

enum IKeyword {
    Node,
    Returns,
    Var,
    Let,
    Tel,
}


enum IBinOper {
    And,
    Or,
    Fby,
}

enum IUnOper {
    Pre,
    Not,
}

struct Type {
    span: Span,
    base: BaseType,
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.base.to_tokens(tokens)
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let base: BaseType = input.parse()?;
        Ok(Self { base, span: input.span() })
    }
}

struct Tuple(Vec<Ident>);

struct Decls {
    span: Span,
    ids: Vec<Ident>,
}

impl Parse for Decls {
    fn parse(input: ParseStream) -> Result<Self> {
        let ids = Punctuated::<Ident, Token![,]>::parse_separated_nonempty(input)?.into_iter().collect::<Vec<_>>();
        let span = ids.first().map(|i| i.span().join(ids.last().unwrap().span())).flatten().unwrap_or_else(|| input.span());
        Ok(Self { span, ids })
    }
}

struct ArgsTy {
    span: Span,
    ty: Type,
    args: Decls,
}

impl Parse for ArgsTy {
    fn parse(input: ParseStream) -> Result<Self> {
        let args: Decls = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;
        let span = args.span.join(ty.span).unwrap();
        Ok(ArgsTy {
            span,
            ty,
            args,
        })
    }
}

impl ToTokens for ArgsTy {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { span, ty, args } = self;
        let Decls { ids, .. } = args;
        tokens.extend(quote_spanned! {*span=>

            #( #ids : #ty , )*

        });
    }
}

#[derive(Default)]
struct ArgsTys(Vec<ArgsTy>);

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
    where End: syn::token::Token + Parse;
}

impl<T, P> PunctUntil<T, P> for Punctuated<T, P>
where
    P: syn::token::Token + Parse,
{
    fn parse_separated_with_until<End>(
        input: ParseStream,
        parser: fn(ParseStream) -> Result<T>,
    ) -> Result<Self>
    where End: syn::token::Token + Parse,
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
    where End: syn::token::Token + Parse {
        let inner = Punctuated::<ArgsTy, Token![;]>::parse_separated_until::<End>(input)?
            .into_iter()
            .collect::<Vec<_>>();
        Ok(Self(inner))
    }

}

impl ToTokens for ArgsTys {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self(decls) = self;
        tokens.extend(quote! {

            #( #decls )*

        });
    }
}

struct Def {
    target: Tuple,
    source: Tuple,
}

struct Defs(Vec<Def>);

struct Node {
    name: Ident,
    inputs: ArgsTys,
    outputs: ArgsTys,
    locals: ArgsTys,
    //defs: Defs,
}

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: kw::node = input.parse()?;
        let name: Ident = input.parse()?;

        let mut buf_inputs;
        parenthesized!(buf_inputs in input);
        let inputs = ArgsTys::parse_terminated(&buf_inputs)?;

        let _: kw::returns = input.parse()?;
        let mut buf_outputs;
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

        Ok(Self { name, inputs, outputs, locals })
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { name, inputs, outputs, locals } = self;
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

struct Prog(Vec<Node>);

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let prog = parse_macro_input!(i as Node);
    let mut toks = TokenStream::new();
    prog.to_tokens(&mut toks);
    dbg!(&toks);
    proc_macro::TokenStream::from(toks)
}
