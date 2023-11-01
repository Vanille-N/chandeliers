//! Parsing output of a Lustre program.
//!
//! This AST is targeted for ease of parsing and quality of error messages,
//! not for traversal.
//! See `translate.rs` for how we can transform this AST into one that is
//! more easy to use.
//!
//! A Lustre program is a sequence of declarations, that are either
//! constants or nodes.
//! A node has inputs, outputs, and a body constituted of definitions of
//! outputs from inputs.

#![allow(clippy::missing_docs_in_private_items)]

use std::fmt;

use proc_macro2::Span;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{Ident, Lit, Token};

use chandeliers_san::sp::{Sp, SpanEnd};

/// Impl `SpanEnd` for structs.
///
/// Example:
/// ```skip
/// #[derive(syn_derive::Parse)]
/// struct Addition {
///     lhs: Ident,
///     plus: Token![+],
///     rhs: Ident,
/// }
/// span_end_on_field!(Addition.rhs);
/// ```
macro_rules! span_end_on_field {
    ($ty:ident . $field:ident $( or $alt:ident )?) => {
        impl SpanEnd for $ty {
            fn span_end(&self) -> Option<Span> {
                self.$field.span_end()
                    $( .or_else(|| self.$alt.span_end()) )?
            }
        }
    };
}

/// Impl `SpanEnd` for enums.
///
/// Example:
/// ```skip
/// #[derive(syn_derive::Parse)]
/// enum Operator {
///     #[parse(peek = Token![+])]
///     Plus(Token![+]),
///     #[parse(peek = Token![-])]
///     Minus(Token![-]),
///     Times(Token![*]),
/// }
/// span_end_by_match! {
///     Operator.
///         Plus(o) => o;
///         Minus(o) => o;
///         Times(o) => o;
/// }
/// ```
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

/// Trivial implementation of `SpanEnd` for types that already have a
/// `span` method.
macro_rules! span_end_from_spanned {
    ( $($ty:tt)* ) => {
        impl SpanEnd for $($ty)* {
            fn span_end(&self) -> Option<Span> {
                Some(self.span())
            }
        }
    }
}

/// `peek`-like implementation for types that do not implement `syn`'s
/// `Token` but are nevertheless still trivially peekable.
trait Hint {
    fn hint(s: ParseStream) -> bool;
}

/// Reserved keywords defined by Lustre.
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

    custom_keyword!(when);
    custom_keyword!(whenot);
    custom_keyword!(merge);
}

span_end_from_spanned!(kw::int);
span_end_from_spanned!(kw::bool);
span_end_from_spanned!(kw::float);
span_end_from_spanned!(kw::tel);

/// Extra punctuation defined by Lustre.
pub mod punct {
    use syn::custom_punctuation;

    custom_punctuation!(Neq, <>);
}

/// A valid Lustre identifier.
///
/// The identifiers accepted here are not comparable with those accepted by
/// Rust, because some keywords are reserved by Lustre that do not exist in
/// Rust, and some Rust keywords can't even be used as raw identifiers.
///
/// See the `Parse` implementation for details.
pub struct LusIdent {
    pub inner: Ident,
}

impl Parse for LusIdent {
    /// Attempts to read an identifier from a stream.
    ///
    /// Valid identifiers *exclude*
    /// - Keywords that both Lustre and Rust agree are reserved
    ///     * literals: `true`, `false`
    ///     * control flow: `if`, `else`
    ///     * declarations: `let`, `const`, `extern`
    /// - Lustre-only reserved keywords
    ///     * temporal operators: `pre`, `fby`
    ///     * boolean operators: `or`, `and`, `not`
    ///     * control flow: `then`, `assert`
    ///     * declarations: `node`, `returns`, `var`, `tel`
    /// - Rust keywords that cannot be raw identifiers
    ///     * path keywords: `crate`, `self`, `Self`, `super`
    ///     * `move`
    ///     * `static`
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.fork();
        match ahead.call(Ident::parse_any) {
            Ok(inner) => {
                match inner.to_string().as_str() {
                    "true" | "false" | "fby" | "if" | "then" | "else" | "or" | "and" | "not"
                    | "pre" | "node" | "const" | "extern" | "returns" | "var" | "let" | "tel"
                    | "assert" | "when" | "whenot" | "merge" => Err(syn::Error::new(
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

pub mod ty {
    use super::{kw, LusIdent, Sp, Span, SpanEnd};

    /// A scalar type: `int`, `bool`, `float`.
    #[derive(syn_derive::Parse)]
    pub enum Base {
        #[parse(peek = kw::int)]
        Int(kw::int),
        #[parse(peek = kw::bool)]
        Bool(kw::bool),
        #[parse(peek = kw::float)]
        Float(kw::float),
    }
    span_end_by_match! {
        Base.
            Int(i) => i;
            Bool(b) => b;
            Float(f) => f;
    }

    #[derive(syn_derive::Parse)]
    pub struct When {
        _when: kw::when,
        pub clock: Sp<LusIdent>,
    }
    span_end_on_field!(When.clock);

    #[derive(syn_derive::Parse)]
    pub struct Whenot {
        _whenot: kw::whenot,
        pub clock: Sp<LusIdent>,
    }
    span_end_on_field!(Whenot.clock);

    #[derive(syn_derive::Parse)]
    pub enum Clock {
        #[parse(peek = kw::when)]
        When(Sp<When>),
        #[parse(peek = kw::whenot)]
        Whenot(Sp<Whenot>),
        None,
    }
    span_end_by_match! {
        Clock.
            When(c) => c;
            Whenot(c) => c;
    }

    #[derive(syn_derive::Parse)]
    pub struct Type {
        pub base: Sp<Base>,
        pub clock: Sp<Clock>,
    }
    span_end_on_field!(Type./*clock or*/ base);
}

/// A comma-separated list of idents.
///
/// ```lus
/// var x, y, z : int; a, b, c : float;
///     ^^^^^^^
///                    ^^^^^^^
/// ```
#[derive(syn_derive::Parse)]
pub struct Decls {
    #[parse(Punctuated::parse_separated_nonempty)]
    pub ids: Punctuated<Sp<LusIdent>, Token![,]>,
}
span_end_on_field!(Decls.ids);

/// Variables and one type that applies to them all.
///
/// ```lus
/// var x, y, z : int; a, b, c : float;
///     ^^^^^^^^^^^^^
///                    ^^^^^^^^^^^^^^^
/// ```
#[derive(syn_derive::Parse)]
pub struct ArgsTy {
    pub args: Sp<Decls>,
    _colon: Token![:],
    pub ty: Sp<ty::Type>,
}
span_end_on_field!(ArgsTy.ty);

/// Declarations of variables and their types.
///
/// ```lus
/// var x, y, z : int; a, b, c : float;
///     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
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

/// Assignment expression.
///
/// ```lus
/// x
/// ^Var
///
/// (x, y, z)
/// ^^^^^^^^^Tuple
///
/// (x, (y, z), w)
///  ^Var       ^Var
///     ^^^^^^Tuple
/// ^^^^^^^^^^^^^^Tuple
/// ```
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

/// Parenthesized tuple of variables.
///
/// ```lus
/// (x, (y, z), w)
///      ^^^^fields
///  ^^^^^^^^^^^^fields
/// ```
#[derive(syn_derive::Parse)]
pub struct TargetExprTuple {
    #[syn(parenthesized)]
    _paren: Paren,
    #[syn(in = _paren)]
    #[parse(Punctuated::parse_terminated)]
    pub fields: Punctuated<Sp<TargetExpr>, Token![,]>,
}
span_end_on_field!(TargetExprTuple.fields);

/// Implement a version of `Punctuated::parse_separated_nonempty` but
/// for punctuation that is not trivially peekable.
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

/// Implement a version of `Punctuated::parse_terminated` but that requires
/// trailing punctuation and stops upon seeing a terminator of type `E`.
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

pub mod op {
    use super::{kw, punct, ParseStream, Token};

    pub use kw::and as And;
    pub use kw::fby as Fby;
    pub use kw::or as Or;
    pub type Arrow = Token![->];

    /// Multiplicative operators: `*`, `/`, `%`, all with the same precedence.
    #[allow(clippy::enum_variant_names)]
    #[derive(syn_derive::Parse)]
    pub enum Mul {
        #[parse(peek = Token![*])]
        Mul(Token![*]),
        #[parse(peek = Token![/])]
        Div(Token![/]),
        #[parse(peek = Token![%])]
        Rem(Token![%]),
    }

    /// Do not confuse an actual `-` with the beginning of a `->`.
    fn exactly_token_neg(s: ParseStream) -> bool {
        s.peek(Token![-]) && !s.peek2(Token![>])
    }

    /// Additive operators: `+`, `-`, all with the same precedence.
    #[derive(syn_derive::Parse)]
    pub enum Add {
        #[parse(peek = Token![+])]
        Add(Token![+]),
        #[parse(peek_func = exactly_token_neg)]
        Sub(Token![-]),
    }

    /// Comparison operators: `<=`, `>=`, `<`, `>`, `=`, `<>`.
    ///
    /// NOTE: these operators are associative at the parsing level,
    /// but the typechecker will ensure that they are exactly binary.
    #[derive(syn_derive::Parse)]
    pub enum Cmp {
        #[parse(peek = punct::Neq)]
        Ne(punct::Neq),
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

    /// Clock operators: `when` and `whenot`.
    #[derive(syn_derive::Parse)]
    pub enum Clock {
        #[parse(peek = kw::when)]
        When(kw::when),
        #[parse(peek = kw::whenot)]
        Whenot(kw::whenot),
    }
}

/// Parsing expressions.
#[allow(clippy::module_name_repetitions)]
pub mod expr {
    //! Expressions by order of decreasing precedence
    //!    [ _ or _ ] (<-)
    //!    [ _ and _ ] (<-)
    //!    [ _ <= _ ], [ _ >= _ ], [ _ < _ ], [ _ > _ ] [ _ = _ ] (==)
    //!    [ _ fby _ ] (<-)
    //!    [ _ -> _ ] (<-)
    //!    [ _ + _ ], [ _ - _ ] (->)
    //!    [ _ * _ ], [ _ / _ ], [ _ % _ ] (->)
    //!
    //! Atomics:
    //!    [ pre _ ]
    //!    [ - _ ]
    //!    [ ( _, ... ) ]
    //!    [ f( _, ... ) ]
    //!    [ v ]
    //!    [ not _ ]

    #[allow(clippy::wildcard_imports)]
    use super::*;

    /// A literal.
    ///
    /// ```lus
    /// 1.0
    /// ^^^lit
    ///
    /// 2
    /// ^lit
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Lit {
        pub lit: Sp<syn::Lit>,
    }
    span_end_on_field!(Lit.lit);
    impl Hint for Lit {
        fn hint(s: ParseStream) -> bool {
            s.peek(Lit)
        }
    }

    /// An expression that is atomically parseable.
    ///
    /// It must not have any associativity, must always consume
    /// at least one token immediately, and its end must be unambiguous.
    ///
    /// ```lus
    /// (a + b, c)
    /// ^^^^^^^^^^Paren
    ///
    /// f(x, y)
    /// ^^^^^^^Call
    ///
    /// 1.0
    /// ^^^Lit
    ///
    /// y
    /// ^Var
    /// ```
    #[derive(syn_derive::Parse)]
    pub enum Atomic {
        #[parse(peek_func = Paren::hint)]
        Paren(Sp<Paren>),
        #[parse(peek_func = Lit::hint)]
        Lit(Sp<Lit>),
        #[parse(peek_func = Var::hint)]
        Var(Sp<Var>),
    }
    span_end_by_match! {
        Atomic.
            Paren(p) => p;
            Lit(l) => l;
            Var(v) => v;

    }

    /// An expression that consumes at least one token immediately.
    ///
    /// ```skip
    /// pre x
    /// ^^^^^Pre
    ///
    /// -x
    /// ^^Neg
    ///
    /// not x
    /// ^^^^^Not
    ///
    /// if b then y else n
    /// ^^^^^^^^^^^^^^^^^^If
    ///
    /// merge b on off
    /// ^^^^^^^^^^^^^^Merge
    /// ```
    #[derive(syn_derive::Parse)]
    pub enum Positive {
        #[parse(peek_func = Call::hint)]
        Call(Sp<Call>),
        #[parse(peek_func = If::hint)]
        If(Sp<If>),
        #[parse(peek_func = Merge::hint)]
        Merge(Sp<Merge>),
        #[parse(peek_func = Pre::hint)]
        Pre(Sp<Pre>),
        #[parse(peek_func = Neg::hint)]
        Neg(Sp<Neg>),
        #[parse(peek_func = Not::hint)]
        Not(Sp<Not>),
        Atomic(Sp<Atomic>),
    }
    span_end_by_match! {
        Positive.
            If(i) => i;
            Merge(m) => m;
            Call(c) => c;
            Pre(p) => p;
            Neg(n) => n;
            Not(n) => n;
            Atomic(a) => a;
    }

    /// A variable.
    ///
    /// It can have any name accepted under the rules defined by `LusIdent`
    /// (i.e. everything except Lustre keywords and some Rust keywords)
    #[derive(syn_derive::Parse)]
    pub struct Var {
        pub name: Sp<LusIdent>,
    }
    span_end_on_field!(Var.name);
    impl Hint for Var {
        fn hint(s: ParseStream) -> bool {
            LusIdent::peek(s)
        }
    }

    /// A function call
    ///
    /// ```lus
    /// foo(x, y, z)
    /// ^^^fun
    ///    ^^^^^^^^^args
    /// ```
    ///
    /// Notice how the arguments are a `Paren` and not a
    /// `Punctuated<Expr, Token![,]>`: this is to accomodate some identifications
    /// between `foo(())` and `foo()` at the translation level.
    #[derive(syn_derive::Parse)]
    pub struct Call {
        pub fun: Sp<LusIdent>,
        pub args: Sp<Paren>,
    }
    span_end_on_field!(Call.args);
    impl Hint for Call {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<syn::token::Paren> {
                s.parse::<LusIdent>()?;
                let _content;
                let p = syn::parenthesized!(_content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()
        }
    }

    /// An expression between parentheses.
    ///
    /// ```lus
    /// (x, y, z)
    /// ^_paren
    ///  ^^^^^^^inner
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Paren {
        #[syn(parenthesized)]
        pub paren: syn::token::Paren,
        #[syn(in = paren)]
        #[parse(Punctuated::parse_terminated)]
        pub inner: Punctuated<Sp<Box<Expr>>, Token![,]>,
    }
    span_end_on_field!(Paren.paren);
    impl Hint for Paren {
        fn hint(s: ParseStream) -> bool {
            fn is_parenthesized(s: ParseStream) -> Result<syn::token::Paren> {
                let _content;
                let p = syn::parenthesized!(_content in s);
                Ok(p)
            }
            is_parenthesized(s).is_ok()
        }
    }

    /// The temporal operator `pre`.
    ///
    /// ```lus
    /// pre x
    /// ^^^_pre
    ///     ^inner
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Pre {
        pub _pre: kw::pre,
        pub inner: Sp<Box<Positive>>,
    }
    span_end_on_field!(Pre.inner);
    impl Hint for Pre {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::pre)
        }
    }

    /// A unary negation.
    ///
    /// ```lus
    /// -x
    /// ^_neg
    ///  ^inner
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Neg {
        pub _neg: Token![-],
        pub inner: Sp<Box<Positive>>,
    }
    span_end_on_field!(Neg.inner);
    impl Hint for Neg {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![-])
        }
    }

    /// A unary boolean negation.
    ///
    /// ```lus
    /// not b
    /// ^^^_not
    ///     ^inner
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Not {
        pub _not: kw::not,
        pub inner: Sp<Box<Positive>>,
    }
    span_end_on_field!(Not.inner);
    impl Hint for Not {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::not)
        }
    }

    /// A clocked expression as a `Clock`-separated list of atomic expressions.
    #[derive(syn_derive::Parse)]
    pub struct Clock {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Positive>, op::Clock>,
    }
    span_end_on_field!(Clock.items);

    /// A multiplicative expression as a `Mul`-separated list of clocked expressions.
    #[derive(syn_derive::Parse)]
    pub struct Mul {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Clock>, op::Mul>,
    }
    span_end_on_field!(Mul.items);

    /// An additive expression as an `Add`-separated list of multiplicative expressions.
    #[derive(syn_derive::Parse)]
    pub struct Add {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Mul>, op::Add>,
    }
    span_end_on_field!(Add.items);

    /// A "Then" temporal expression as a `->`-separated list of additive expressions.
    #[derive(syn_derive::Parse)]
    pub struct Then {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Add>, op::Arrow>,
    }
    span_end_on_field!(Then.items);

    /// A "Fby" temporal expression as a `fby`-separated list of then expressions.
    #[derive(syn_derive::Parse)]
    pub struct Fby {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Then>, op::Fby>,
    }
    span_end_on_field!(Fby.items);

    /// A comparison expression as a `Cmp`-separated list of temporal expressions.
    #[derive(syn_derive::Parse)]
    pub struct Cmp {
        #[parse(punctuated_parse_separated_nonempty_costly)]
        pub items: Punctuated<Sp<Fby>, op::Cmp>,
    }
    span_end_on_field!(Cmp.items);

    /// A conjunction as an `and`-separated list of comparisons.
    #[derive(syn_derive::Parse)]
    pub struct And {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<Sp<Cmp>, op::And>,
    }
    span_end_on_field!(And.items);

    /// A disjunction as an `or`-separated list of conjunctions.
    #[derive(syn_derive::Parse)]
    pub struct Or {
        #[parse(Punctuated::parse_separated_nonempty)]
        pub items: Punctuated<Sp<And>, op::Or>,
    }
    span_end_on_field!(Or.items);

    /// A conditional expression.
    ///
    /// ```lus
    /// if c then y else n
    /// ^^_if
    ///    ^cond
    ///      ^^^^_then
    ///           ^yes
    ///             ^^^^_else
    ///                  ^no
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct If {
        pub _if: Token![if],
        pub cond: Sp<Expr>,
        pub _then: kw::then,
        pub yes: Sp<Expr>,
        pub _else: Token![else],
        pub no: Sp<Expr>,
    }
    impl Hint for If {
        fn hint(s: ParseStream) -> bool {
            s.peek(Token![if])
        }
    }
    span_end_on_field!(If.no);

    /// A merge of two clocks.
    ///
    /// ```lus
    /// merge b on off
    /// ^^^^^_merge
    ///       ^clk
    ///         ^^on
    ///            ^^^off
    /// ```
    #[derive(syn_derive::Parse)]
    pub struct Merge {
        pub _merge: kw::merge,
        pub clk: Sp<Box<Atomic>>,
        pub on: Sp<Box<Atomic>>,
        pub off: Sp<Box<Atomic>>,
    }
    impl Hint for Merge {
        fn hint(s: ParseStream) -> bool {
            s.peek(kw::merge)
        }
    }
    span_end_on_field!(Merge.off);

    /// Any expression.
    pub struct Expr {
        /// `or` has the lowest precedence.
        pub inner: Sp<Or>,
    }
    span_end_on_field!(Expr.inner);

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            let inner: Sp<Or> = input.parse()?;
            Ok(Self { inner })
        }
    }
}

pub use expr::Expr;

/// An assignment statement.
///
/// ```lus
/// (x, y, z) = foo(a, b);
/// ^^^^^^^^^target
///           ^_equal
///             ^^^^^^^^^source
/// ```
#[derive(syn_derive::Parse)]
pub struct Def {
    pub target: Sp<TargetExpr>,
    _equal: Token![=],
    pub source: Sp<expr::Expr>,
}
span_end_on_field!(Def.source);

/// An assertion.
///
/// ```lus
/// assert b;
/// ^^^^^^_assert
///        ^expr
/// ```
#[derive(syn_derive::Parse)]
pub struct Assertion {
    _assert: kw::assert,
    pub expr: Sp<expr::Expr>,
}
span_end_on_field!(Assertion.expr);

/// A statement in the body of a node.
///
/// ```lus
/// assert b;
/// ^^^^^^^^Assert
///
/// x = 1;
/// ^^^^^Def
/// ```
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

/// Declaration of local variables
///
/// ```lus
/// var x, y : int;
/// ^^^_var
///     ^^^^^^^^^^^decls
/// ```
#[derive(syn_derive::Parse)]
pub struct VarsDecl {
    _var: kw::var,
    #[parse(ArgsTys::parse_separated_trailing_until_let)]
    pub decls: Sp<ArgsTys>,
}
span_end_on_field!(VarsDecl.decls);

/// Maybe local variables, or maybe empty.
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

/// A Lustre node.
///
/// ```lus
/// node foo(i : int) returns (o : int);
/// ^^^^_node
///      ^^^name
///         ^^^^^^^^^inputs
///                   ^^^^^^^_returns
///                           ^^^^^^^^^outputs
///                                    ^_decl_semi
/// var n : int;
/// ^^^^^^^^^^^^locals
///
/// let
/// ^^^_kwlet
///     o = i;
///     n = i;
///     ^^^^^^defs
/// tel
/// ^^^_kwtel
/// ```
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

    kwtel: kw::tel,
}
span_end_on_field!(Node.kwtel);

/// Definition of a global constant.
///
/// ```lus
/// const x : int = 5;
/// ^^^^^_const
///       ^name
///         ^_colon
///           ^^^ty
///               ^_equal
///                 ^value
/// ```
#[derive(syn_derive::Parse)]
pub struct Const {
    _const: Token![const],
    pub name: Sp<LusIdent>,
    _colon: Token![:],
    pub ty: Sp<ty::Type>,
    _equal: Token![=],
    pub value: Sp<Expr>,
}
span_end_on_field!(Const.value);

/// A Lustre node that the compiler should trust is defined elsewhere.
///
/// ```lus
/// extern node foo(i : int) returns (o : int);
/// ^^^^^^_extern
///        ^^^^_node
///             ^^^name
///                ^^^^^^^^^inputs
///                          ^^^^^^^_returns
///                                  ^^^^^^^^^outputs
///                                           ^_decl_semi
/// ```
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

/// A global constant that the compiler should trust already exists.
///
/// ```lus
/// extern const x : int;
/// ^^^^^^_extern
///        ^^^^^_const
///              ^name
///                ^_colon
///                  ^^^ty
/// ```
#[derive(syn_derive::Parse)]
pub struct ExtConst {
    _extern: Token![extern],
    _const: Token![const],
    pub name: Sp<LusIdent>,
    _colon: Token![:],
    pub ty: Sp<ty::Type>,
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
pub struct AttrTargets {
    #[syn(parenthesized)]
    paren: Paren,
    #[syn(in = paren)]
    #[parse(Punctuated::parse_terminated)]
    targets: Punctuated<syn::Lit, Token![,]>,
}
span_end_on_field!(AttrTargets.paren);
impl Hint for AttrTargets {
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
pub enum OptionAttrTargets {
    #[parse(peek_func = AttrTargets::hint)]
    Targets(AttrTargets),
    None,
}
span_end_by_match! {
    OptionAttrTargets.
        Targets(p) => p;
}

#[derive(syn_derive::Parse)]
pub struct AttrParams {
    #[syn(bracketed)]
    brack: Bracket,
    #[syn(in = brack)]
    #[parse(Punctuated::parse_terminated)]
    params: Punctuated<LusIdent, Token![,]>,
}
span_end_on_field!(AttrParams.brack);
impl Hint for AttrParams {
    fn hint(s: ParseStream) -> bool {
        fn is_bracketed(s: ParseStream) -> Result<Bracket> {
            let _content;
            let p = syn::bracketed!(_content in s);
            Ok(p)
        }
        is_bracketed(s).is_ok()
    }
}

#[derive(syn_derive::Parse)]
pub enum OptionAttrParams {
    #[parse(peek_func = AttrParams::hint)]
    Params(AttrParams),
    None,
}
span_end_by_match! {
    OptionAttrParams.
        Params(p) => p;
}

impl OptionAttrParams {
    pub fn flatten(self) -> Vec<String> {
        match self {
            Self::None => vec![],
            Self::Params(ps) => ps.params.into_iter().map(|i| i.inner.to_string()).collect(),
        }
    }
}
impl OptionAttrTargets {
    pub fn flatten(self) -> Vec<syn::Lit> {
        match self {
            Self::None => vec![],
            Self::Targets(ts) => ts.targets.into_iter().collect(),
        }
    }
}

#[derive(syn_derive::Parse)]
pub struct AttrDef {
    pub action: Sp<LusIdent>,
    pub params: Sp<OptionAttrParams>,
    pub targets: Sp<OptionAttrTargets>,
}
span_end_on_field!(AttrDef.action);

#[derive(syn_derive::Parse)]
pub struct Attribute {
    _marker: Token![#],
    #[syn(bracketed)]
    _brack: Bracket,
    #[syn(in = _brack)]
    pub attr: Sp<AttrDef>,
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

/// A Lustre program.
#[derive(syn_derive::Parse)]
pub struct Prog {
    #[parse(Punctuated::parse_terminated)]
    pub(crate) decls: Punctuated<Sp<AttrDecl>, Token![;]>,
}
span_end_on_field!(Prog.decls);
