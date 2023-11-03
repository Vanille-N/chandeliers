//! Error message generation.

#![feature(lint_reasons)]
#![warn(
    missing_docs,
    unused_crate_dependencies,
    unused_macro_rules,
    variant_size_differences,
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::expect_used,
    clippy::indexing_slicing,
    clippy::missing_docs_in_private_items,
    clippy::multiple_inherent_impl,
    clippy::panic,
    clippy::pedantic,
    clippy::str_to_string,
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::use_debug
)]

use std::fmt::Display;

use proc_macro2::Span;

/// Repository of this project, to be displayed in error messages.
#[macro_export]
macro_rules! repo {
    () => {
        "https://github.com/Vanille-N/chandeliers"
    };
}

/// Generate an error message better than just "proc macro panicked".
#[macro_export]
macro_rules! abort {
    ($($err:tt)*) => {{
        std::panic!("
Chandeliers panicked: \x1b[1;31m{}.\x1b[0m
This error occured in \x1b[1;35m{}:{}:{}\x1b[0m

If you are not a developper of Chandeliers and you see this message then this is a bug.
I'd be grateful if you could report this error at \x1b[33m{}\x1b[0m
with the code that produced it and the version of Chandeliers you are using.
",
            format!($($err)*),
            file!(),
            line!(),
            column!(),
            ::chandeliers_err::repo!(),
        );
    }};
}

/// Special instance of `panic` for code that should be trivially unreachable.
#[macro_export]
macro_rules! malformed {
    () => {{
        ::chandeliers_err::abort!("Entered unreachable code");
    }};
}

/// Special instance of `panic` for assertions.
#[macro_export]
macro_rules! consistency {
    ($cond:expr, $($msg:tt)*) => {{
        if !$cond {
            ::chandeliers_err::abort!($($msg)*);
        }
    }};
}

/// Anything that went wrong: a sequence of [Span] and associated message.
pub type Error = Vec<(String, Option<Span>)>;

/// Generate an [Error].
pub trait IntoError {
    /// Produce the sequence of spans and help messages.
    fn into_err(self) -> Error;
}

/// Result type with errors that can be emitted by Rustc.
pub type Result<T> = std::result::Result<T, Error>;

/// Objects that can be converted to spans.
pub trait TrySpan {
    /// Try to get a span from the object (by default we don't get any,
    /// but a wrapper might provide one)
    fn try_span(&self) -> Option<Span> {
        None
    }
}

/// Always [Some].
impl TrySpan for Span {
    fn try_span(&self) -> Option<Span> {
        Some(*self)
    }
}

/// Trivial projection.
impl<T: TrySpan> TrySpan for &T {
    fn try_span(&self) -> Option<Span> {
        (*self).try_span()
    }
}

/// Trivial projection.
impl<T: TrySpan> TrySpan for Option<T> {
    fn try_span(&self) -> Option<Span> {
        self.as_ref().and_then(TrySpan::try_span)
    }
}

/// An explicit error message with its span.
pub struct Basic {
    /// Error kind.
    pub msg: String,
    /// Error location.
    pub span: Span,
}

impl IntoError for Basic {
    fn into_err(self) -> Error {
        vec![(self.msg, Some(self.span))]
    }
}

/// Generate an error for incompatible types between a "left" and a "right" values.
pub struct TypeMismatch<Source, Left, Right, Msg> {
    /// Span of the entire error.
    pub source: Source,
    /// Left expression and span.
    pub left: Left,
    /// Right expression and span.
    pub right: Right,
    /// Extra message.
    pub msg: Msg,
}

impl<Source, Left, Right, Msg> IntoError for TypeMismatch<Source, Left, Right, Msg>
where
    Source: TrySpan,
    Msg: Display,
    Left: Display + TrySpan,
    Right: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!(
                    "Type mismatch between the left and right sides: {}",
                    self.msg
                ),
                self.source.try_span(),
            ),
            (
                format!("This element has type {}", self.left),
                self.left.try_span(),
            ),
            (
                format!("While this element has type {}", self.right),
                self.right.try_span(),
            ),
        ]
    }
}

/// Generate an error for a variable that was not declared yet.
pub struct VarNotFound<Var, Suggest1, Suggest2> {
    /// What is missing.
    pub var: Var,
    /// Local variable suggestions.
    pub suggest1: Suggest1,
    /// Global variable suggestions.
    pub suggest2: Suggest2,
}

impl<Var, Suggest1, S1, Suggest2, S2> IntoError for VarNotFound<Var, Suggest1, Suggest2>
where
    Var: Display + TrySpan,
    Suggest1: IntoIterator<Item = S1>,
    Suggest2: IntoIterator<Item = S2>,
    S1: Display,
    S2: Display,
{
    fn into_err(self) -> Vec<(String, Option<Span>)> {
        let mut suggest1 = self
            .suggest1
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        let mut suggest2 = self
            .suggest2
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        suggest1.sort();
        suggest2.sort();
        let suggest1 = if suggest1.is_empty() {
            String::from("(none declared)")
        } else {
            suggest1.join(", ")
        };
        let suggest2 = if suggest2.is_empty() {
            String::from("(none declared)")
        } else {
            suggest2.join(", ")
        };

        vec![
            (
                format!("Variable {} not found in the context.", self.var),
                self.var.try_span(),
            ),
            (
                format!("Perhaps you meant one of the local variables: {suggest1}"),
                None,
            ),
            (format!("or one of the global variables: {suggest2}"), None),
        ]
    }
}

/// Generate an error for a type variable that was not declared yet,
/// which has consequences on what we should say is and isn't available.
pub struct TyVarNotFound<Var, Suggest> {
    /// What is missing.
    pub var: Var,
    /// Local variable suggestions.
    pub suggest: Suggest,
}

impl<Var, Suggest, S> IntoError for TyVarNotFound<Var, Suggest>
where
    Var: Display + TrySpan,
    Suggest: IntoIterator<Item = S>,
    S: Display,
{
    fn into_err(self) -> Vec<(String, Option<Span>)> {
        let mut suggest = self
            .suggest
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        suggest.sort();
        let suggest = if suggest.is_empty() {
            String::from("(none declared)")
        } else {
            suggest.join(", ")
        };

        vec![
            (
                format!(
                    "Variable {} is not available yet at this point of the typechecking.",
                    self.var
                ),
                self.var.try_span(),
            ),
            (
                "During this incremental typechecking, you cannot access global
variables and you may only use local variables that have been
declared strictly beforehand, in the order of inputs then outputs
then locals."
                    .to_owned(),
                None,
            ),
            (
                format!("These are the variables that are already useable: {suggest}"),
                None,
            ),
        ]
    }
}

/// Generate an erorr for an expression that is noot valid in a `const` declaration.
pub struct NotConst<Item, Site> {
    /// Description of the invalid expression constructor.
    pub what: Item,
    /// Location of the error.
    pub site: Site,
}

impl<Item, Site> IntoError for NotConst<Item, Site>
where
    Item: Display,
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!("{} not valid in const contexts", self.what),
                self.site.try_span(),
            ),
            (
                String::from("You must put this definition inside a node"),
                None,
            ),
        ]
    }
}

/// Generate an error for a binary operator that expected arguments of a specific type.
pub struct BinopMismatch<Oper, Site, Expect, Left, Right> {
    /// Description of the operator.
    pub oper: Oper,
    /// Location of the error.
    pub site: Site,
    /// What we expected in place of the arguments.
    pub expect: Expect,
    /// Left hand side and span.
    pub left: Left,
    /// Right hand side and span.
    pub right: Right,
}

impl<Oper, Site, Expect, Left, Right> IntoError for BinopMismatch<Oper, Site, Expect, Left, Right>
where
    Oper: Display,
    Site: TrySpan,
    Expect: Display,
    Left: Display + TrySpan,
    Right: Display + TrySpan,
{
    fn into_err(self) -> Vec<(String, Option<Span>)> {
        vec![
            (
                format!(
                    "Binary operator `{}` expects arguments of {}",
                    self.oper, self.expect
                ),
                self.site.try_span(),
            ),
            (
                format!("The left-hand-side is found to be of type {}", self.left),
                self.left.try_span(),
            ),
            (
                format!("The right-hand-side is found to be of type {}", self.right),
                self.right.try_span(),
            ),
        ]
    }
}

/// Generate an error for a unary operator that expected an argument of a specific type.
pub struct UnopMismatch<Oper, Expect, Site, Inner> {
    /// Description of the operator.
    pub oper: Oper,
    /// What the operator expects.
    pub expect: Expect,
    /// Location of the error.
    pub site: Site,
    /// Invalid expression and span.
    pub inner: Inner,
}

impl<Oper, Expect, Site, Inner> IntoError for UnopMismatch<Oper, Expect, Site, Inner>
where
    Oper: Display,
    Expect: Display,
    Site: TrySpan,
    Inner: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!(
                    "Unary operator `{}` expects an argument of {}",
                    self.oper, self.expect
                ),
                self.site.try_span(),
            ),
            (
                format!("The inner value is found to be of type {}", self.inner),
                self.inner.try_span(),
            ),
        ]
    }
}

/// Generate an error for something that should have been a bool but isn't,
/// e.g. `if 1 then 0 else 1`.
pub struct BoolRequired<Type, Site, Inner> {
    /// Type that was found (should have been bool).
    pub actual: Type,
    /// Location of the error.
    pub site: Site,
    /// Location of the inner contents.
    pub inner: Inner,
}

impl<Type, Site, Inner> IntoError for BoolRequired<Type, Site, Inner>
where
    Type: Display,
    Site: TrySpan,
    Inner: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!("{} should be of type bool", self.actual),
                self.site.try_span(),
            ),
            (
                format!("The argument is found to be of type {}", self.inner),
                self.inner.try_span(),
            ),
        ]
    }
}

/// Generate an error for a cyclic definition.
pub struct Cycle<Items> {
    /// A (not necessarily ordered) cycle.
    pub items: Items,
}

impl<Items, Item> IntoError for Cycle<Items>
where
    Items: IntoIterator<Item = (Item, Option<Span>)>,
    Item: Display,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        for (i, (it, sp)) in self.items.into_iter().enumerate() {
            v.push((
                if i == 0 {
                    format!("{it} was found to be part of a dependency cycle")
                } else {
                    format!("The cycle also goes through {it}")
                },
                sp,
            ));
        }
        v
    }
}

/// Error message for an object that was defined twice when only one
/// declaration should exist.
pub struct GraphUnitDeclTwice<Unit, NewSite, Prior, PriorSite> {
    /// Display of the redefined object.
    pub unit: Unit,
    /// Location of the superfluous definition.
    pub new_site: NewSite,
    /// Item that defined the object previously.
    pub prior: Prior,
    /// Location of the first definition.
    pub prior_site: PriorSite,
}

impl<Unit, NewSite, Prior, PriorSite> IntoError
    for GraphUnitDeclTwice<Unit, NewSite, Prior, PriorSite>
where
    Unit: Display,
    Prior: Display,
    NewSite: TrySpan,
    PriorSite: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!(
                    "Attempt to redefine {}, when {} already defines it",
                    self.unit, self.prior
                ),
                self.new_site.try_span(),
            ),
            (
                String::from("Already defined here"),
                self.prior_site.try_span(),
            ),
        ]
    }
}

/// Error for an object that should have been declared but was not.
pub struct GraphUnitUndeclared<Unit> {
    /// Missing object and site where usage was attempted.
    pub unit: Unit,
}

impl<Unit> IntoError for GraphUnitUndeclared<Unit>
where
    Unit: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![(
            format!("No definition provided for {} which is required", self.unit),
            self.unit.try_span(),
        )]
    }
}

/// Special case of [Cycle]: custom message for an object that depends
/// specifically on itself directly.
pub struct GraphUnitDependsOnItself<Unit, Site1, Site2> {
    /// Object that loops.
    pub unit: Unit,
    /// Where it is defined.
    pub def_site: Site1,
    /// Where it is used (usually a subspan of `def_site`).
    pub usage: Site2,
}

impl<Unit, Site1, Site2> IntoError for GraphUnitDependsOnItself<Unit, Site1, Site2>
where
    Unit: Display,
    Site1: TrySpan,
    Site2: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!("{} depends on itself", self.unit),
                self.def_site.try_span(),
            ),
            (
                String::from("used here within its own definition"),
                self.usage.try_span(),
            ),
        ]
    }
}

/// Error for when one tried to access too far into the past.
pub struct NotPositive<Var, Site> {
    /// Variable that is not deep enough.
    pub var: Var,
    /// Location of the error.
    pub site: Site,
    /// How deep we could have gone.
    pub available_depth: usize,
    /// How deep we actually tried to go.
    pub attempted_depth: usize,
}
impl<Var, Site> IntoError for NotPositive<Var, Site>
where
    Var: Display,
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!("Variable {} is not positive at this depth", self.var),
                self.site.try_span(),
            ),
            (
                format!(
                    "tried to reach {} steps into the past, with only {} available",
                    self.attempted_depth, self.available_depth
                ),
                None,
            ),
            (
                String::from("Maybe add a `->` in front of the expression to increase the depth ?"),
                None,
            ),
        ]
    }
}

/// Error for a literal that is not supported.
///
/// Lustre only has `float`, `int`, and `bool` literals, so e.g. a `&str` will trigger this error.
pub struct UnhandledLitType<Site> {
    /// Location of the literal.
    pub site: Site,
}
impl<Site> IntoError for UnhandledLitType<Site>
where
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        vec![(
            String::from("Lustre only accepts literals of type int, float, or bool"),
            self.site.try_span(),
        )]
    }
}

/// Error for when a comparison operator is used with associativity.
///
/// Since `a = b = c` is ambiguous (does it mean `(a = b) = c` or `a = (b = c)`
/// or `(a = b) and (b = c)`, we choose to reject all interpretations and
/// ask for explicit parentheses around comparison operators.
pub struct CmpNotAssociative<First, Oper1, Second, Oper2, Third, Site> {
    /// The `<` of `a < b > c`
    pub oper1: Oper1,
    /// The `a` of `a < b > c`
    pub first: First,
    /// The whole location of `a < b > c`
    pub site: Site,
    /// The `b` of `a < b > c`
    pub second: Second,
    /// The `c` of `a < b > c`
    pub third: Third,
    /// The `>` of `a < b > c`
    pub oper2: Oper2,
}
impl<First, Oper1, Second, Oper2, Third, Site> IntoError
    for CmpNotAssociative<First, Oper1, Second, Oper2, Third, Site>
where
    Oper1: Display,
    Oper2: Display,
    First: Display,
    Second: Display,
    Third: Display,
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        let Self {
            first,
            oper1,
            second,
            oper2,
            third,
            site,
        } = &self;
        vec![(
            format!("Comparison operator {oper1} is not associative"),
            site.try_span(),
        ),(
            format!("Maybe replace `{first} {oper1} {second} {oper2} {third}` with `{first} {oper1} {second} and {second} {oper2} {third}` ?"),
            None,
        )]
    }
}
