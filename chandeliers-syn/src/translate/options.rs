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

use chandeliers_err::{self as err, IntoError, Result};
use chandeliers_san::ast::options::{Const, ExtConst, ExtNode, Node};
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
}

impl<T> SetOpt<T> {
    /// Get the value that was constructed, initialized to the default
    /// if not provided.
    fn take(mut self) -> T {
        self.value.take().unwrap_or(self.default)
    }

    /// Assert that this option was not set.
    fn skip(self, current: &'static str) -> err::Result<()> {
        if self.value.is_some() {
            Err(err::Basic {
                msg: format!(
                    "The attribute {} is not valid here (does not apply to {current})",
                    self.message
                ),
                span: self.span.unwrap_or_else(|| {
                    err::abort!("Malformed `SetOpt`: it has a `value` but no `span`")
                }),
            }
            .into_err())
        } else {
            Ok(())
        }
    }
}

/// Current options being applied to the node.
#[derive(Clone, Debug)]
pub struct Decl {
    /// `#[trace]`: print debug information.
    trace: SetOpt<bool>,
    /// `#[export]`: make the declaration visible.
    export: SetOpt<bool>,
    /// `#[pub]`: make the declaration public. (implies `#[export]`)
    public: SetOpt<bool>,
    /// `#[main(10)]`: generate a `fn main` that runs for `n` (integer) steps.
    main: SetOpt<Option<usize>>,
    /// `#[rustc_allow[dead_code]]`: forward the attribute as a `#[allow(_)]`.
    rustc_allow: SetOpt<Vec<syn::Ident>>,
    /// `#[doc("Message")]`: insert documentation for this node.
    doc: SetOpt<Vec<Sp<String>>>,
}

impl Default for Decl {
    fn default() -> Self {
        Self {
            trace: SetOpt::create("#[trace]", false),
            export: SetOpt::create("#[export]", false),
            public: SetOpt::create("#[pub]", false),
            main: SetOpt::create("#[main(nb_iter)]", None),
            rustc_allow: SetOpt::create("#[rustc_allow(attr)]", vec![]),
            doc: SetOpt::create("#[doc(\"Message\")]", vec![]),
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
    ( $this:ident : $From:ident => $To:ident {
        take { $( $take:ident, )* }
        skip { $( $skip:ident, )* }
      }
    ) => {
        let $From {
            $( $take, )*
            $( $skip, )*
        } = $this;

        $( $skip.skip(stringify!($To))?; )*
        Ok($To {
            $( $take: chandeliers_san::ast::options::UseOpt::new($take.take()), )*
        })
    }
}

impl Decl {
    /// Project to the options that are valid on a `const`.
    pub fn for_const(self) -> Result<Const> {
        project! {
            self: Decl => Const {
                take { export, rustc_allow, doc, public, }
                skip { trace, main, }
            }
        }
    }

    /// Project to the options that are valid on a `node`.
    pub fn for_node(self) -> Result<Node> {
        project! {
            self: Decl => Node {
                take { trace, export, main, rustc_allow, doc, public, }
                skip {}
            }
        }
    }

    /// Project to the options that are valid on an `extern const`.
    pub fn for_ext_const(self) -> Result<ExtConst> {
        project! {
            self: Decl => ExtConst {
                take { rustc_allow, }
                skip { trace, export, main, doc, public, }
            }
        }
    }

    /// Project to the options that are valid on an `extern node`.
    pub fn for_ext_node(self) -> Result<ExtNode> {
        project! {
            self: Decl => ExtNode {
                take { trace, main, rustc_allow, }
                skip { export, doc, public, }
            }
        }
    }

    /// Update the current options with a new attribute.
    pub fn with(mut self, attr: Sp<crate::ast::Attribute>) -> Result<Self> {
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
                return Err(vec![
                    (format!("Malformed attribute {}: {}", action, format!($($msg)*)), Some(attr.span.into())),
                    $( (format!("Maybe try this syntax: {}", format!($($syntax)*)), None), )?
                    $( (format!($($suggestion)*), None), )?
                ])
            };
        }

        let params = &attr.t.attr.t.params.map(|_, t| t.flatten());
        let targets = attr.t.attr.t.targets.map(|_, t| t.flatten());
        let args = (&params.t[..], &targets.t[..]);
        match action.as_str() {
            "trace" => match args {
                ([], []) => self.trace.set(true, attr.span.into()),
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[trace]`")
                ),
            },
            "export" => match args {
                ([], []) => self.export.set(true, attr.span.into()),
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[export]`")
                ),
            },
            "pub" => match args {
                ([], []) => {
                    self.public.set(true, attr.span.into());
                    self.export.set(true, attr.span.into());
                }
                _ => malformed!(
                    msg:("expects no arguments")
                    syn:("`#[pub]`")
                ),
            },
            "main" => match args {
                ([], []) => self.main.some(100, attr.span.into()),
                ([], [Lit::Int(i)]) => self.main.some(
                    i.base10_parse()
                        .unwrap_or_else(|e| err::abort!("{i} cannot be parsed in base 10: {e}")),
                    attr.span.into(),
                ),
                _ => malformed!(
                    msg:("expects at most one integer argument")
                    syn:("`#[main]` or `#[main(42)]`")
                ),
            },
            "rustc_allow" => match args {
                ([inner], []) => {
                    self.rustc_allow
                        .push(syn::Ident::new(inner, params.span.into()), attr.span.into());
                }
                _ => malformed!(
                    msg:("expects exactly one identifier")
                    syn:("`#[rustc_allow(dead_code)]`")
                ),
            },
            "doc" => match args {
                ([], [Lit::Str(doc)]) => {
                    self.doc
                        .push(doc.value().with_span(params.span), attr.span.into());
                }
                _ => malformed!(
                    msg:("expects exactly one string")
                    syn:("`#[doc(\"Message\")]`")
                ),
            },
            _ => malformed!(
                msg:("no such attribute")
                note:("See the available options and their definition at {}", err::repo!()) //FIXME
            ),
        }
        Ok(self)
    }
}
