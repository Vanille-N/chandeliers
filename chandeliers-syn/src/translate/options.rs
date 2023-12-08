//! Project attributes to only those applicable to the object.
//!
//! Because the syntax of attributes looks like
//! ```skip
//! #[attr]
//! #[another("attr")]
//! #[yet_another[and, more]]
//! <declaration>
//! ```
//! We have no idea when we start translating attributes whether we are
//! looking at a (extern) node or const declaration.
//! Not all attributes can apply to each, for example the `#[main]` attribute
//! cannot be used on a `const`.
//!
//! This module defines the projection from all attributes available to
//! only those applicable to each declaration, and emits appropriate and
//! homogeneous error messages.

use chandeliers_err::{self as err, EAccum};
use chandeliers_san::ast::options::{
    Allow, Const, ExtConst, ExtNode, Node, TraceFile, TraceFormat,
};
use chandeliers_san::sp::{Sp, Span, WithSpan};

/// The main helper to set options.
///
/// The interface of `SetOpt` is as follows:
/// - create one with `create`, pass it a message in case there is an error
///   and a default value. `SetOpt` distinguishes between an uninitialized
///   option and one initialized to its default value,
/// - update with one of the provided methods (`set`, `some`, `push`, ...)
///   when the option needs to be set,
/// - when you are done using it, you can do one of two things:
///   (1) this option is relevant, you can `take` it which will give you
///       the set value for the option (and initialize to the default if
///       it was not initialized)
///   (2) this option is not relevant, you should use `skip` which will
///       assert that the option was never set to a value.
///
/// If you either `take` or `skip` all `SetOpt` that you created, you will
/// have initialized values for exactly the options needed, and all the
/// other options will be guaranteed to be unset.
#[must_use]
#[derive(Debug, Clone)]
struct SetOpt<T> {
    /// Current value of the option. Is `Some` only if it was explicitly set.
    value: Option<T>,
    /// Definition site of the value.
    span: Option<Span>,
    /// Value to pick if none was explicitly set.
    default: T,
    /// Extra data to help fild the source if an error occurs.
    message: &'static str,
}

impl<T: Default> SetOpt<T> {
    /// Build a new option with an explanation of what it refers to
    /// and a default value.
    fn create(message: &'static str, default: T) -> Self {
        Self {
            value: None,
            span: None,
            default,
            message,
        }
    }
}

impl SetOpt<bool> {
    /// Assign a value.
    fn set(&mut self, b: bool, span: Span) {
        self.value = Some(b);
        self.span = Some(span);
    }
}

impl<T> SetOpt<Option<T>> {
    /// Assign a `Some` value.
    fn some(&mut self, t: T, span: Span) {
        self.value = Some(Some(t));
        self.span = Some(span);
    }
}

impl<T> SetOpt<Vec<T>> {
    /// Append a value.
    fn push(&mut self, t: T, span: Span) {
        if let Some(values) = &mut self.value {
            values.push(t);
        } else {
            self.value = Some(vec![t]);
            self.span = Some(span);
        }
    }

    /// Append a vector.
    fn extend(&mut self, ts: Vec<T>, span: Span) {
        if let Some(values) = &mut self.value {
            values.extend(ts);
        } else {
            self.value = Some(ts);
            self.span = Some(span);
        }
    }
}

impl<T> SetOpt<T> {
    /// Get the value that was constructed, initialized to the default
    /// if not provided.
    fn take(mut self) -> T {
        self.value.take().unwrap_or(self.default)
    }

    /// Assert that this option was not set.
    fn skip(self, eaccum: &mut EAccum, current: &'static str) -> Option<()> {
        if self.value.is_some() {
            eaccum.error(err::TmpBasic {
                msg: format!(
                    "The attribute {} is not valid here (does not apply to {current})",
                    self.message
                ),
                span: self.span.unwrap_or_else(|| {
                    err::abort!("Malformed `SetOpt`: it has a `value` but no `span`")
                }),
            })
        } else {
            Some(())
        }
    }
}

/// Current options being applied to the node.
#[derive(Clone, Debug)]
pub struct Decl {
    /// `#[trace]`: print debug information.
    trace: SetOpt<Option<(TraceFile, (TraceFormat, TraceFormat))>>,
    /// `#[export]`: make the declaration visible.
    export: SetOpt<bool>,
    /// `#[pub]`: make the declaration public. (implies `#[export]`)
    public: SetOpt<bool>,
    /// `#[main(10)]`: generate a `fn main` that runs for `n` (integer) steps.
    main: SetOpt<Option<usize>>,
    /// `#[test(10)]`: generate a Rust unit test that runs for `n` (integer) steps.
    test: SetOpt<Option<usize>>,
    /// `#[rustc_allow[dead_code]]`: forward the attribute as a `#[allow(_)]`.
    rustc_allow: SetOpt<Vec<Allow>>,
    /// `#[doc("Message")]`: insert documentation for this node.
    doc: SetOpt<Vec<Sp<String>>>,
    /// `#[trait]`: implement `Step` for this node rather than just an inherent impl.
    impl_trait: SetOpt<bool>,
    /// `#[generic[T, U, V]]`: type variables.
    generics: SetOpt<Vec<Sp<String>>>,
}

impl Default for Decl {
    fn default() -> Self {
        Self {
            trace: SetOpt::create("#[trace]", None),
            export: SetOpt::create("#[export]", false),
            public: SetOpt::create("#[pub]", false),
            main: SetOpt::create("#[main(nb_iter)]", None),
            test: SetOpt::create("#[test(nb_iter)]", None),
            rustc_allow: SetOpt::create("#[rustc_allow[attr]]", vec![]),
            doc: SetOpt::create("#[doc(\"Message\")]", vec![]),
            impl_trait: SetOpt::create("`#[trait]`", false),
            generics: SetOpt::create("#[generics[T, U, V]]", vec![]),
        }
    }
}

/// Project to only the relevant fields.
///
/// This assumes that the fields of the target type are a subset of those
/// of the source type.
/// All fields will have either `take` or `skip` applied to them, ensuring
/// that all options are handled.
macro_rules! project {
    ( ?$eaccum:expr => $this:ident : $From:ident => $To:ident {
        take { $( $take:ident, )* }
        skip { $( $skip:ident, )* }
      }
    ) => {
        let $From {
            $( $take, )*
            $( $skip, )*
        } = $this;
        $( $skip.skip($eaccum, stringify!($To))?; )*
        Some($To {
            $( $take: chandeliers_san::ast::options::UseOpt::new($take.take()), )*
        })
    }
}

impl Decl {
    /// Project to the options that are valid on a `const`.
    pub fn for_const(self, eaccum: &mut EAccum) -> Option<Const> {
        project! {
            ?eaccum => self: Decl => Const {
                take { export, rustc_allow, doc, public, }
                skip { trace, main, impl_trait, test, generics, }
            }
        }
    }

    /// Project to the options that are valid on a `node`.
    pub fn for_node(self, _eaccum: &mut EAccum) -> Option<Node> {
        project! {
            ?_eaccum => self: Decl => Node {
                take { trace, export, main, rustc_allow, doc, public, impl_trait, test, generics, }
                skip {}
            }
        }
    }

    /// Project to the options that are valid on an `extern const`.
    pub fn for_ext_const(self, eaccum: &mut EAccum) -> Option<ExtConst> {
        project! {
            ?eaccum => self: Decl => ExtConst {
                take { rustc_allow, }
                skip { trace, export, main, doc, public, impl_trait, test, generics, }
            }
        }
    }

    /// Project to the options that are valid on an `extern node`.
    pub fn for_ext_node(self, eaccum: &mut EAccum) -> Option<ExtNode> {
        project! {
            ?eaccum => self: Decl => ExtNode {
                take { trace, main, rustc_allow, generics, }
                skip { export, doc, public, impl_trait, test, }
            }
        }
    }

    /// Update the current options with a new attribute.
    pub fn with(mut self, eaccum: &mut EAccum, attr: Sp<crate::ast::Attribute>) -> Option<Self> {
        use syn::Lit;
        let action = attr.t.attr.t.action.t.inner.to_string();

        /// Automatically generate a helpful error message if the attribute
        /// cannot be parsed.
        macro_rules! malformed {
            (
                msg:( $($msg:tt)* )
                $( syn:( $($syntax:tt)* ) )?
                $( note:( $($suggestion:tt)* ) )?
            ) => {
                eaccum.error(err::TmpRaw { es: vec![
                    (format!("Malformed attribute {}: {}", action, format!($($msg)*)), Some(attr.span.into())),
                    $( (format!("Maybe try this syntax: {}", format!($($syntax)*)), None), )?
                    $( (format!($($suggestion)*), None), )?
                ] })?
            };
        }

        /// Warning (not fatal) if we overwrite a previous attribute.
        macro_rules! duplicate {
            ($opt:ident) => {
                if self.$opt.value.is_some() {
                    eaccum.warning(err::TmpRaw {
                        es: vec![
                            (
                                format!(
                                    "{} is already set by a previous attribute",
                                    self.$opt.message
                                ),
                                Some(attr.span.into()),
                            ),
                            (
                                format!("Previously declared here"),
                                Some(self.$opt.span.unwrap()),
                            ),
                        ],
                    })
                }
            };
        }

        /// Warning if we try to set an attribute that is incompatible with another.
        /// This is not fatal because we resolve it to have the earlier one take precedence.
        macro_rules! conflicting {
            ($this:ident, $other:ident) => {
                if self.$other.value.is_some() {
                    eaccum.warning(err::TmpRaw {
                        es: vec![
                            (
                                format!(
                                    "{} is incompatible with {}",
                                    self.$this.message, self.$other.message,
                                ),
                                Some(attr.span.into()),
                            ),
                            (
                                format!("A conflicting attribute was set here"),
                                Some(self.$other.span.unwrap()),
                            ),
                        ],
                    })
                }
            };
        }

        /// Set the option with the given value. Typical invocation will look like
        /// `register!(main <- some 100)` or `register!(export <- set true)`.
        macro_rules! register {
            ( $field:ident <- $fn:ident $val:expr ) => {
                self.$field.$fn($val, attr.span.into())
            };
        }

        let params = &attr.t.attr.t.params.map(|_, t| t.flatten());
        let targets = attr.t.attr.t.targets.map(|_, t| t.flatten());
        let args = (&params.t[..], &targets.t[..]);
        match action.as_str() {
            "trace" => {
                let destination = match &params.t[..] {
                    [] => TraceFile::StdOut,
                    [ident] if ident.t.as_str() == "stdout" => TraceFile::StdOut,
                    [ident] if ident.t.as_str() == "stderr" => TraceFile::StdErr,
                    _ => malformed!(
                        msg:("expects either no arguments or one of `stderr`/`stdout`")
                        syn:("`#[trace]`")
                    ),
                };
                let format = match &targets.t[..] {
                    [] => (TraceFormat::Default, TraceFormat::Default),
                    [Lit::Str(o)] => (TraceFormat::Empty, TraceFormat::Str(o.value())),
                    [Lit::Str(i), Lit::Str(o)] => {
                        (TraceFormat::Str(i.value()), TraceFormat::Str(o.value()))
                    }
                    _ => malformed!(
                        msg:("expects at most two arguments, both strings (input and output formats)")
                        syn:("`#[trace(\"in\", \"out\")]`")
                    ),
                };
                duplicate!(trace);
                register!(trace <- some (destination, format));
            }
            "export" => match args {
                ([], []) => {
                    conflicting!(export, test);
                    duplicate!(export);
                    register!(export <- set true);
                }
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[export]`")
                ),
            },
            "pub" => match args {
                ([], []) => {
                    conflicting!(public, test);
                    duplicate!(public);
                    duplicate!(export);
                    register!(public <- set true);
                    register!(export <- set true);
                }
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[pub]`")
                ),
            },
            "trait" => match args {
                ([], []) => {
                    duplicate!(impl_trait);
                    register!(impl_trait <- set true);
                }
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[trait]`")
                ),
            },
            "main" => match args {
                ([], []) => {
                    conflicting!(main, test);
                    duplicate!(main);
                    register!(main <- some 100);
                }
                ([], [Lit::Int(i)]) => {
                    conflicting!(main, test);
                    duplicate!(main);
                    let i = i
                        .base10_parse()
                        .unwrap_or_else(|e| err::abort!("{i} cannot be parsed in base 10: {e}"));
                    register!(main <- some i);
                }
                _ => malformed!(
                    msg:("expects at most one integer argument")
                    syn:("`#[main]` or `#[main(42)]`")
                ),
            },
            "test" => match args {
                ([], []) => {
                    conflicting!(test, main);
                    conflicting!(test, public);
                    conflicting!(test, export);
                    duplicate!(test);
                    register!(test <- some 100);
                }
                ([], [Lit::Int(i)]) => {
                    conflicting!(test, main);
                    duplicate!(test);
                    let i = i
                        .base10_parse()
                        .unwrap_or_else(|e| err::abort!("{i} cannot be parsed in base 10: {e}"));
                    register!(test <- some i);
                }
                _ => malformed!(
                    msg:("expects at most one integer argument")
                    syn:("`#[test]` or `#[test(42)]`")
                ),
            },
            "rustc_allow" => match args {
                ([inner], []) => {
                    let id = Allow::Rustc(syn::Ident::new(&inner.t, inner.span.unwrap()));
                    register!(rustc_allow <- push id);
                }
                _ => malformed!(
                    msg:("expects exactly one identifier")
                    syn:("`#[rustc_allow[dead_code]]`")
                ),
            },
            "clippy_allow" => match args {
                ([inner], []) => {
                    let id = Allow::Clippy(syn::Ident::new(&inner.t, inner.span.unwrap()));
                    register!(rustc_allow <- push id);
                }
                _ => malformed!(
                    msg:("expects exactly one identifier")
                    syn:("`#[clippy_allow[len_zero]]`")
                ),
            },
            "doc" => match args {
                ([], [Lit::Str(doc)]) => {
                    let msg = doc.value().with_span(params.span);
                    register!(doc <- push msg);
                }
                _ => malformed!(
                    msg:("expects exactly one string")
                    syn:("`#[doc(\"Message\")]`")
                ),
            },
            "generics" => match args {
                (types, []) => {
                    let types = types.to_owned();
                    register!(generics <- extend types);
                }
                _ => malformed!(
                    msg:("expects only identifiers")
                    syn:("`#[generics[T, U, V]]`")
                ),
            },
            _ => malformed!(
                msg:("no such attribute")
                note:("See the available options and their definition at {}#compilation-options", err::repo!()) //FIXME
            ),
        }
        Some(self)
    }
}
