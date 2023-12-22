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

trait Condition {
    fn truth(&self) -> bool;
}

impl Condition for bool {
    fn truth(&self) -> bool {
        *self
    }
}

/// More macro black magic.
/// This one is supposed to reduce how repetitive it is to add new error messages.
/// Describe the error message in a succint format and the macro will generate
/// the `impl IntoError` automaticaly.
///
/// This defines a metalanguage to describe error messages.
/// Each error consists of a declaration and an implementation.
///
/// The declaration looks like this:
/// ```skip
/// ["Documentation for SomeError"]
/// struct SomeError where {
///     ["Documentation for foo"] foo: {Display},
///     ["Documentation for bar"] bar: {TrySpan},
///     ["Documentation for quux"] quux: {Display + TrySpan},
/// }
/// ```
/// As implied, this results in a struct having the fields `foo` and `bar`,
/// and these fields can be of any type that implements the trait bounds.
///
/// The second part is the implementation
/// ```skip
/// impl {
///     "Uh oh this is bad: {foo} occured here" @ bar;
///     "Fix this construct {quux}" @ quux;
/// }
/// ```
/// This will produce an error message that show is that order:
/// - the first text line, formatted with `self.foo`
/// - the span of `self.bar`
/// - the second text line, formated with `self.quux`
/// - the span of `self.quux`
///
/// The following further constructs are available to manipulate lines of messages
/// in a more fine-grained manner:
/// - `"msg"` plain message without span
/// - `"{bar}" format string (requires `bar: {Display}`)
/// - `"msg" @ foo` use the span of `foo` (requires `foo: {TrySpan}`)
/// - `"msg" @ if foo` use the span of `foo` but only if it is not `None` (requires `foo: {TrySpan}`)
/// - `"msg" @* foo` use the def site of `foo` (requires `foo: {TryDefSite}`)
/// - `"msg" @* if foo` use the def site of `foo` but only if it is not `None` (requires `foo: {TryDefSite}`)
/// - `if cond => "msg" @ foo` only insert if `cond` holds (implements `Condition` that returns `true`)
/// - `for items => "msg"` and `for items => "msg" @ foo`
///   iterate over `items`, can be reused as a binding in both the format string and the span.
///   Requires `items: [It .. Display + TrySpan]` (`Display` and `TrySpan` are optional depending
///   on what you do with `items`) i.e. requires `items` to be an iterator of `Display + TrySpan`
///   items.
///
/// See concrete syntax examples below.
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
    ( [$constructed:ident] $fmt:tt @* if $site:ident ; $($rest:tt)* ) => {
        // Only insert if `try_def_site` is defined.
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
    ( [$constructed:ident] if $cond:ident => $fmt:tt @ $site:expr ; $($rest:tt)* ) => {
        // Only insert if `cond` holds [if cond => "foo" @ site]
        if $cond.truth() {
            $constructed.push((format!($fmt), $site.try_span()));
        }
        error_message!([$constructed] $($rest)*);
    };
    ( [$constructed:ident] $fmt:tt @ if $site:expr ; $($rest:tt)* ) => {
        // Only insert if `try_span` is defined.
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

error_message! {
    ["Special case of [Cycle]: custom message for an object that depends"]
    ["specifically on itself directly."]
    struct GraphUnitDependsOnItself where {
        ["Object that loops."] unit: {Display},
        ["Where it is defined."] def_site: {TrySpan},
        ["Where it is used (usually a subspan of `def_site`)."] usage: {TrySpan},
    } impl {
        "{unit} depends on itself" @ def_site;
        "used here within its own definition" @ usage;
    }
}

error_message! {
    ["Error for when one tried to access too far into the past."]
    struct NotPositive where {
        ["Variable that is not deep enough."] var: {Display},
        ["Location of the error"] site: {TrySpan},
        ["How deep we could have gone"] available_depth: {Display},
        ["How deep we actually tried to go"] attempted_depth: {Display},
    } impl {
        "Variable {var} is not positive at this depth" @ site;
        "tried to reach {attempted_depth} steps into the past, with only {available_depth} available";
        "Maybe add a `->` in front of the expression to increase the depth ?";
    }
}

error_message! {
    ["Error for a literal that is not supported."]
    ["Lustre only has `float`, `int`, and `bool` literals, so e.g. a `&str` will trigger this error."]
    struct UnhandledLitType where {
        ["Location of the literal."] site: {TrySpan},
    } impl {
        "Lustre only accepts literals of type int, float, or bool" @ site;
    }
}

error_message! {
    ["Error for when a comparison operator is used with associativity."]
    ["Since `a = b = c` is ambiguous (does it mean `(a = b) = c` or `a = (b = c)`"]
    ["or `(a = b) and (b = c)`, we choose to reject all interpretations and"]
    ["ask for explicit parentheses around comparison operators."]
    struct CmpNotAssociative where {
        ["The `<` of `a < b > c`"] oper1: {Display},
        ["The `a` of `a < b > c`"] first: {Display},
        ["The whole location of `a < b > c`"] site: {TrySpan},
        ["The `b` of `a < b > c`"] second: {Display},
        ["The `c` of `a < b > c`"] third: {Display},
        ["The `>` of `a < b > c`"] oper2: {Display},
    } impl {
        "Comparison operator {oper1} is not associative" @ site;
        "Maybe replace `{first} {oper1} {second} {oper2} {third}` with `{first} {oper1} {second} and {second} {oper2} {third}` ?";
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
        "Found {slow} here" @* if slow;
        "Expected because this expression moves at the implicit pace" @ if implicit;
        for extra => "{extra}";
    }
}

error_message! {
    ["When an expression is not a valid clock (anything but a local variable)."]
    struct NotAClock where {
        ["The faulty expression."] expr: {Display + TrySpan},
    } impl {
        "The expression `{expr}` cannot be interpreted as a clock because it is not a local boolean variable" @ expr;
    }
}

error_message! {
    ["When two clocks are both non-implicit but different."]
    struct ClkNotComparable where {
        ["First clock."] first: {Display + TrySpan + TryDefSite},
        ["Second clock."] second: {Display + TrySpan + TryDefSite},
        ["Span of the whole expression that contains both."] whole: {TrySpan},
    } impl {
        "Two subexpressions have incomparable clocks: {first} and {second} are incompatible" @ whole;
        "This is clocked by {first}" @ first;
        "defined here" @* if first;
        "This is clocked by {second}" @ second;
        "defined here" @* if second;
    }
}

error_message! {
    ["When a generic type variable is unused and thus not inferrable"]
    struct UnusedGeneric where {
        ["Type variable that is absent from the inputs declaration."] unused: {Display + TrySpan},
        ["Declaration of said inputs."] inputs: {TrySpan},
    } impl {
        "Type variable {unused} cannot be inferred from the inputs of this node" @ unused;
        "None of these arguments are of type {unused}" @ inputs;
    }
}

error_message! {
    ["Impossible to satisfy generic constraints introduced by bounds."]
    struct UnsatGenericConstraint where {
        ["Type variable."] variable: {Display},
        ["Already equal to."] previous: {Display + TrySpan},
        ["Now additionally required to be equal to."] new: {Display + TrySpan},
        ["Bound introduced by this node declaration."] context: {TrySpan},
    } impl {
        "Cannot satisfy constraint {variable} = {new} introduced here..." @ new;
        "...because a previous bound already enforces {variable} = {previous}" @ previous;
        "Unable to satisfy the generic bounds on {variable}" @ context;
    }
}

error_message! {
    ["When a generic type variable is unused and thus not inferrable."]
    struct UndeclaredGeneric where {
        ["Type variable that is absent from the generics declaration."] undeclared: {Display + TrySpan},
    } impl {
        "Type variable {undeclared} was not declared" @ undeclared;
        "Maybe add a `#[generic[{undeclared}]]` annotation to the node ?";
    }
}

error_message! {
    ["Node is declared as executable, but has nonempty inputs or outputs."]
    struct ExecutableNodeSig where {
        ["Attribute that marks it as executable."] reason: {Display},
        ["Whether there are any inputs."] inputs_nonempty: {Condition},
        ["Where are the inputs."] inputs: {TrySpan},
        ["Whether there are any outputs."] outputs_nonempty: {Condition},
        ["Where are the outputs."] outputs: {TrySpan},
        ["Entire call site."] site: {TrySpan},
    } impl {
        "Node has an incompatible signature to be marked as executable (required due to {reason})" @ site;
        if inputs_nonempty => "Inputs should be ()" @ inputs;
        if outputs_nonempty => "Outputs should be ()" @ outputs;
    }
}

error_message! {
    ["Two types cannot be equal because one is a tuple and the other a scalar"]
    struct ScalarNotTuple where {
        ["Tuple type found"] typ: {Display + TrySpan},
    } impl {
        "Expected a scalar type, found a tuple {typ}" @ typ;
    }
}
