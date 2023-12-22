//! Error message generation.
//!
//! Here we provide the facilities to instanciate prebuilt error messages.
//! The general structure is that each kind of error message will be implemented
//! by a `struct` that implements [IntoError], where the blanks are filled
//! in by the `struct` fields' [Display] and [TrySpan] `impl`s.

use std::fmt::Display;

use crate::Span;

/// Anything that went wrong: a sequence of [Span] and associated message.
pub type Error = Vec<(String, Option<Span>)>;

/// Generate an [Error].
#[expect(
    clippy::module_name_repetitions,
    reason = "Of course the trait contains the word 'Error'"
)]
pub trait IntoError {
    /// Produce the sequence of spans and help messages.
    fn into_err(self) -> Error;
}

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

/// Trivial projection.
impl<T: TrySpan, E> TrySpan for Result<T, E> {
    fn try_span(&self) -> Option<Span> {
        self.as_ref().ok().and_then(TrySpan::try_span)
    }
}

/// Objects that have a different way that they can be seen as a span
pub trait TryDefSite {
    /// Try to get a span from the object (by default we don't get any,
    /// but a wrapper might provide one)
    fn try_def_site(&self) -> Option<Span> {
        None
    }
}

/// Always [None]: `Span` provides a usage site not a def site.
/// Put it in a wrapper if you want one.
impl TryDefSite for Span {}

/// Trivial projection.
impl<T: TryDefSite> TryDefSite for &T {
    fn try_def_site(&self) -> Option<Span> {
        (*self).try_def_site()
    }
}

/// Trivial projection.
impl<T: TryDefSite> TryDefSite for Option<T> {
    fn try_def_site(&self) -> Option<Span> {
        self.as_ref().and_then(TryDefSite::try_def_site)
    }
}

/// Trivial projection.
impl<T: TryDefSite, E> TryDefSite for Result<T, E> {
    fn try_def_site(&self) -> Option<Span> {
        self.as_ref().ok().and_then(TryDefSite::try_def_site)
    }
}

/// Generic wraper that implements `Display` and `TrySpan` to wrap together
/// items that don't implement both.
pub struct DisplayTrySpan<T> {
    /// Displayable part.
    pub display: T,
    /// Spannable part.
    pub try_span: Option<Span>,
}

impl<T: Display> Display for DisplayTrySpan<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display.fmt(f)
    }
}

impl<T> TrySpan for DisplayTrySpan<T> {
    fn try_span(&self) -> Option<Span> {
        self.try_span
    }
}

/// An explicit error message with its span.
/// This error construct is meant to be phased out in favor of a prebuilt error
/// message.
//#[deprecated = "This is a nonspecific error constructor that should eventually become a prebuilt message."]
pub struct Basic {
    /// Error kind.
    pub msg: String,
    /// Error location.
    pub span: Span,
}

#[allow(deprecated)]
impl IntoError for Basic {
    fn into_err(self) -> Error {
        vec![(self.msg, Some(self.span))]
    }
}

/// An explicit sequence of errors and spans
/// This error construct is meant to be phased out in favor of a prebuilt error
/// message.
//#[deprecated = "This is a nonspecific error constructor that should eventually become a prebuilt message."]
pub struct Raw {
    /// Messages and their spans.
    pub es: Vec<(String, Option<Span>)>,
}

#[allow(deprecated)]
impl IntoError for Raw {
    fn into_err(self) -> Error {
        self.es
    }
}

/// More macro black magic.
/// This one is supposed to reduce how repetitive it is to add new error messages.
/// Describe the error message in a succint format and the macro will generate
/// the `impl IntoError` automaticaly.
///
/// See examples for syntax.
macro_rules! error_message {
    (
        $( [ $predoc:expr ] )* // documentation of the struct
        struct $name:tt $( <$($explicit_generics:ident),*> )? where {
            $( // fields and trait bounds (typically `Display` and/or `TrySpan`)
                [ $doc:expr ]
                $field:ident : $( [$item:ident .. $($iterbounds:tt)* ] )? $( { $($bounds:tt)+ } )?,
            )*
        } impl { // ;-separated list of messages, handled by the auxiliary arms
            $( $message:tt )*
        }
    ) => {
        #[allow(non_camel_case_types)] // We are using a generic of the same name for each field
        $( #[doc = $predoc] )*
        pub struct $name <$($field),*> {
            $(
                #[doc = $doc]
                pub $field : $field ,
            )*
        }

        #[allow(non_camel_case_types)]
        impl <$($($explicit_generics),*,)? $($field),*> IntoError for $name<$($field),*>
        where $(
            $field: $( IntoIterator<Item = $item>, $item: $($iterbounds)* )?
                $( $($bounds)* , )? )*
        {
            fn into_err(self) -> Error {
                let Self { $($field),* } = self;
                let mut constructed = Vec::new();
                error_message!([constructed]
                    $($message)* // more black magic to turn these into statements
                );
                constructed
            }
        }
    };
    // Auxiliary arms to build just the message constructor.
    // These recursively consume the stream of tokens that describe the message
    // and produce the appropriate `push` operations.
    ( [$constructed:ident] ) => {}; // done
    ( [$constructed:ident] $fmt:tt @ $site:ident ; $($rest:tt)* ) => {
        // Base case looks like ["foo" @ site]: "foo" is treated as a format
        // string and `site` gives the `Span`.
        $constructed.push((format!($fmt), $site.try_span()));
        error_message!([$constructed] $($rest)*);
    };
    ( [$constructed:ident] $fmt:tt @* if let $site:ident ; $($rest:tt)* ) => {
        // Base case looks like ["foo" @ site]: "foo" is treated as a format
        // string and `site` gives the `Span`.
        if let Some(site) = $site.try_def_site() {
            $constructed.push((format!($fmt), Some(site)));
        }
        error_message!([$constructed] $($rest)*);
    };

    ( [$constructed:ident] for $iterator:ident => $fmt:tt @ $site:expr ; $($rest:tt)* ) => {
        // Loop over the base case [for items => "foo" @ site]
        for $iterator in $iterator {
            $constructed.push((format!($fmt), $site.try_span()));
        }
        error_message!([$constructed] $($rest)*);
    };
    ( [$constructed:ident] $fmt:tt @ if let $site:expr ; $($rest:tt)* ) => {
        if let Some(site) = $site.try_span() {
            $constructed.push((format!($fmt), Some(site)));
        }
        error_message!([$constructed] $($rest)*);
    };

    ( [$constructed:ident] for $iterator:ident => $fmt:tt ; $($rest:tt)* ) => {
        // Loop over the base case without a span [for items => "foo"]
        for $iterator in $iterator {
            $constructed.push((format!($fmt), None));
        }
        error_message!([$constructed] $($rest)*);
    };
    ( [$constructed:ident] $fmt:tt ; $($rest:tt)* ) => {
        // Base case without `Span`.
        $constructed.push((format!($fmt), None));
        error_message!([$constructed] $($rest)*);
    };

}

error_message! {
    ["Generate an error for incompatible types between a \"left\" and a \"right\" values."]
    struct TypeMismatch where {
        ["Span of the entire error"] source: {TrySpan},
        ["Left expression"] left: {Display + TrySpan},
        ["Right expression"] right: {Display + TrySpan},
        ["Further details on mismatch"] msg: {Display},
    } impl {
        "Type mismatch between the left and right sides: {msg}" @ source;
        "This element has type {left}" @ left;
        "While this element has type {right}" @ right;
    }
}

/// Wrapper to display suggestions.
pub struct Suggest<Its> {
    /// Iterator of suggested items
    pub available: Its,
}

impl<Its, It> Display for Suggest<Its>
where
    Its: IntoIterator<Item = It> + Clone,
    It: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut suggest = self
            .available
            .clone()
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        suggest.sort();
        if suggest.is_empty() {
            write!(f, "(none declared)")
        } else {
            write!(f, "{}", suggest.join(", "))
        }
    }
}

error_message! {
    ["Generate an error for a variable that was not declared yet."]
    struct VarNotFound where {
        ["What is missing"] var: {Display + TrySpan},
        ["Local variable suggestions."] suggest1: {Display},
        ["Global variable suggestions."] suggest2: {Display},
    } impl {
        "Variable {var} not found in the context." @ var;
        "Perhaps you meant one of the local variables: {suggest1}";
        "or one of the global variables: {suggest2}";
    }
}

error_message! {
    ["Generate an error for a type variable that was not declared yet,"]
    ["which has consequences on what we should say is and isn't available."]
    struct TyVarNotFound where {
        ["What is missing."] var: {Display + TrySpan},
        ["Local variable suggestions."] suggest: {Display},
    } impl {
        "Variable {var} is not available yet at this point of the typechecking." @ var;
        "During this incremental typechecking, you cannot access global
variables and you may only use local variables that have been
declared strictly beforehand, in the order of inputs then outputs
then locals.";
        "These are the variables that are already useable: {suggest}";
    }
}

error_message! {
    ["Generate an erorr for an expression that is noot valid in a `const` declaration."]
    struct NotConst where {
        ["Description of the invalid expression constructor."] what: {Display},
        ["Location of the error."] site: {TrySpan},
    } impl {
        "{what} not valid in const contexts" @ site;
        "You must put this definition inside a node";
    }
}

error_message! {
    ["Generate an error for a binary operator that expected arguments of a specific type."]
    struct BinopMismatch where {
        ["Description of the operator."] oper: {Display},
        ["Location of the error."] site: {TrySpan},
        ["What we expected in place of the arguments."] expect: {Display},
        ["Left hand side and span."] left: {Display + TrySpan},
        ["Right hand side and span."] right: {Display + TrySpan},
    } impl {
        "Binary operator `{oper}` expects arguments of {expect}" @ site;
        "The left-hand-side is found to be of type {left}" @ left;
        "The right-hand-side is found to be of type {right}" @ right;
    }
}

error_message! {
    ["Generate an error for a unary operator that expected an argument of a specific type."]
    struct UnopMismatch where {
        ["Description of the operator."] oper: {Display},
        ["What the operator expects."] expect: {Display},
        ["Location of the error."] site: {TrySpan},
        ["Invalid expression and span."] inner: {Display + TrySpan},
    } impl {
        "Unary operator `{oper}` expects an argument of {expect}" @ site;
        "The inner value is found to be of type {inner}" @ inner;
    }
}

error_message! {
    ["Generate an error for something that should have been a bool but isn't,"]
    ["e.g. `if 1 then 0 else 1`."]
    struct BoolRequired where {
        ["Explanation of what this item is (e.g. \"the condition of if\")"] what: {Display},
        ["Location of the error."] site: {TrySpan},
        ["Location of the inner contents."] inner: {Display + TrySpan},
    } impl {
        "{what} should be of type bool" @ site;
        "The argument is found to be of type {inner}" @ inner;
    }
}

error_message! {
    ["Generate an error for a cyclic definition."]
    struct Cycle<Its> where {
        ["Beginning of the cycle"] head: {Display + TrySpan},
        ["Rest of the cycle (not necessarily ordered)."] items: [Its .. TrySpan + Display],
    } impl {
        "{head} was found to be part of a dependency cycle" @ head;
        for items => "The cycle also goes through {items}" @ items;
    }
}

error_message! {
    ["Error message for an object that was defined twice when only one"]
    ["declaration should exist."]
    struct GraphUnitDeclTwice where {
        ["Display of the redefined object."] unit: {Display},
        ["Location of the superfluous definition."] new_site: {TrySpan},
        ["Item that defined the object previously."] prior: {Display},
        ["Location of the first definition."] prior_site: {TrySpan},
    } impl {
        "Attempt to redefine {unit}, when {prior} already defines it" @ new_site;
        "Already defined here" @ prior_site;
    }
}

error_message! {
    ["Error for an object that should have been declared but was not."]
    struct GraphUnitUndeclared where {
        ["Missing object and site where usage was attempted."] unit: {Display + TrySpan},
    } impl {
        "No definition provided for {unit} which is required" @ unit;
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

error_message! {
    ["Generate an error when due to `implicit` that has the implicit clock,"]
    ["`slow` was expected to also have the implicit clock but doesn't."]
    struct ClkTooSlowExpectImplicit<It> where {
        ["Clocked by something else, should have been `'self`."] slow: {Display + TrySpan + TryDefSite},
        ["Clocked by `'self`"] implicit: {TrySpan},
        ["Extra help messages, optionally."] extra: [It .. Display],
    } impl {
        "This expression is too slow: expected the implicit clock 'self, found {slow}" @ slow;
        "Found {slow} here" @* if let slow;
        "Expected because this expression moves at the implicit pace" @ if let implicit;
        //if let Some(def) = self.slow.try_def_site() {
        //    es.push((format!("Found {} here", self.slow), Some(def)));
        //}
        //if let Some(reason) = self.implicit.try_span() {
        //    es.push((
        //        "Expected because this expression moves at the implicit pace".to_owned(),
        //        Some(reason),
        //    ));
        //}
        for extra => "{extra}";
    }
}

/// When an expression is not a valid clock (anything but a local variable).
pub struct NotAClock<Expr> {
    /// The faulty expression.
    pub expr: Expr,
}

impl<Expr> IntoError for NotAClock<Expr>
where
    Expr: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![(
            format!("The expression `{}` cannot be interpreted as a clock because it is not a local boolean variable", self.expr),
            self.expr.try_span(),
        )]
    }
}

/// To help track clock errors we show the usage site and the definition site if
/// it exists.
pub fn clock_and_def_site<Clk>(es: &mut Error, clk: Clk)
where
    Clk: Display + TrySpan + TryDefSite,
{
    es.push((format!("This is clocked by {clk}"), clk.try_span()));
    if let Some(def) = clk.try_def_site() {
        es.push(("defined here".to_owned(), Some(def)));
    }
}

/// When two clocks are both non-implicit but different.
pub struct ClkNotComparable<First, Second, Whole> {
    /// First clock.
    pub first: First,
    /// Second clock.
    pub second: Second,
    /// Span of the whole expression that contains both.
    pub whole: Whole,
}

impl<First, Second, Whole> IntoError for ClkNotComparable<First, Second, Whole>
where
    First: Display + TrySpan + TryDefSite,
    Second: Display + TrySpan + TryDefSite,
    Whole: TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![(
            format!(
                "Two subexpressions have incomparable clocks: {} and {} are incompatible",
                self.first, self.second,
            ),
            self.whole.try_span(),
        )];
        clock_and_def_site(&mut v, self.first);
        clock_and_def_site(&mut v, self.second);
        v
    }
}

/// When a generic type variable is unused and thus not inferrable
pub struct UnusedGeneric<Unused, Inputs> {
    /// Type variable that is absent from the inputs declaration.
    pub unused: Unused,
    /// Declaration of said inputs.
    pub inputs: Inputs,
}

impl<Unused, Inputs> IntoError for UnusedGeneric<Unused, Inputs>
where
    Unused: Display + TrySpan,
    Inputs: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!(
                    "Type variable {} cannot be inferred from the inputs of this node",
                    self.unused
                ),
                self.unused.try_span(),
            ),
            (
                format!("None of these arguments are of type {}", self.unused),
                self.inputs.try_span(),
            ),
        ]
    }
}

/// Impossible to satisfy generic constraints introduced by bounds.
pub struct UnsatGenericConstraint<Variable, Previous, New, Context> {
    /// Type variable.
    pub variable: Variable,
    /// Already equal to.
    pub previous: Previous,
    /// Now additionally required to be equal to.
    pub new: New,
    /// Bound introduced by this node declaration.
    pub context: Context,
}

impl<Variable, Previous, New, Context> IntoError
    for UnsatGenericConstraint<Variable, Previous, New, Context>
where
    Variable: Display,
    Previous: Display + TrySpan,
    New: Display + TrySpan,
    Context: TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!(
                    "Cannot satisfy constraint {} = {} introduced here...",
                    self.variable, self.new
                ),
                self.new.try_span(),
            ),
            (
                format!(
                    "...because a previous bound already enforces {} = {}",
                    self.variable, self.previous
                ),
                self.previous.try_span(),
            ),
            (
                format!("Unable to satisfy the generic bounds on {}", self.variable,),
                self.context.try_span(),
            ),
        ]
    }
}

/// When a generic type variable is unused and thus not inferrable
pub struct UndeclaredGeneric<Undeclared> {
    /// Type variable that is absent from the generics declaration.
    pub undeclared: Undeclared,
}

impl<Undeclared> IntoError for UndeclaredGeneric<Undeclared>
where
    Undeclared: Display + TrySpan,
{
    fn into_err(self) -> Error {
        vec![
            (
                format!("Type variable {} was not declared", self.undeclared),
                self.undeclared.try_span(),
            ),
            (
                format!(
                    "Maybe add a `#[generic[{}]]` annotation to the node ?",
                    self.undeclared
                ),
                None,
            ),
        ]
    }
}

/// Node is declared as executable, but has nonempty inputs or outputs.
pub struct ExecutableNodeSig<Reason, Inputs, Outputs, Site> {
    /// Attribute that marks it as executable.
    pub reason: Reason,
    /// Whether there are any inputs.
    pub inputs_nonempty: bool,
    /// Where are the inputs.
    pub inputs: Inputs,
    /// Whether there are any outputs.
    pub outputs_nonempty: bool,
    /// Where are the outputs.
    pub outputs: Outputs,
    /// Entire call site.
    pub site: Site,
}

impl<Reason, Inputs, Outputs, Site> IntoError for ExecutableNodeSig<Reason, Inputs, Outputs, Site>
where
    Reason: Display,
    Inputs: TrySpan,
    Outputs: TrySpan,
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![
            (format!("Node has an incompatible signature to be marked as executable (required due to {})", self.reason), self.site.try_span())
        ];
        if self.inputs_nonempty {
            v.push((format!("Inputs should be ()"), self.inputs.try_span()));
        }
        if self.outputs_nonempty {
            v.push((format!("Outputs should be ()"), self.outputs.try_span()));
        }
        v
    }
}
