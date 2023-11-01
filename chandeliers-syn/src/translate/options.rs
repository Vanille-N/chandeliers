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

use crate::ast as lus;
use chandeliers_err::{self as err, IntoError, Result};
use chandeliers_san::ast::options::{Const, ExtConst, ExtNode, Node};
use chandeliers_san::sp::{Sp, Span};

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
    value: Option<T>,
    span: Option<Span>,
    default: T,
    message: &'static str,
}

impl<T: Default> SetOpt<T> {
    fn create(message: &'static str, default: T) -> Self {
        Self {
            value: None,
            span: None,
            default,
            message,
        }
    }
}

/// Assign a value.
impl SetOpt<bool> {
    fn set(&mut self, b: bool, span: Span) {
        self.value = Some(b);
        self.span = Some(span);
    }
}

/// Assign a `Some` value.
impl<T> SetOpt<Option<T>> {
    fn some(&mut self, t: T, span: Span) {
        self.value = Some(Some(t));
        self.span = Some(span);
    }
}

/// Append a value.
impl<T> SetOpt<Vec<T>> {
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
                span: self.span.unwrap(),
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
    /// `#[export]`: make the declaration public.
    export: SetOpt<bool>,
    /// `#[main(10)]`: generate a `fn main` that runs for `n` (integer) steps.
    main: SetOpt<Option<usize>>,
    /// `#[rustc_allow[dead_code]]`: forward the attribute as a `#[allow(_)]`.
    rustc_allow: SetOpt<Vec<syn::Ident>>,
}

impl Default for Decl {
    fn default() -> Self {
        Self {
            trace: SetOpt::create("#[trace]", false),
            export: SetOpt::create("#[export]", false),
            main: SetOpt::create("#[main(nb_iter)]", None),
            rustc_allow: SetOpt::create("#[rustc_allow(attr)]", vec![]),
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
            $( $take: $take.take(), )*
        })
    }
}

impl Decl {
    /// Project to the options that are valid on a `const`.
    pub fn for_const(self) -> Result<Const> {
        project! {
            self: Decl => Const {
                take { export, rustc_allow, }
                skip { trace, main, }
            }
        }
    }

    /// Project to the options that are valid on a `node`.
    pub fn for_node(self) -> Result<Node> {
        project! {
            self: Decl => Node {
                take { trace, export, main, rustc_allow, }
                skip {}
            }
        }
    }

    /// Project to the options that are valid on an `extern const`.
    pub fn for_ext_const(self) -> Result<ExtConst> {
        project! {
            self: Decl => ExtConst {
                take { rustc_allow, }
                skip { trace, export, main, }
            }
        }
    }

    /// Project to the options that are valid on an `extern node`.
    pub fn for_ext_node(self) -> Result<ExtNode> {
        project! {
            self: Decl => ExtNode {
                take { trace, main, rustc_allow, }
                skip { export, }
            }
        }
    }

    /// Update the current options with a new attribute.
    pub fn with(mut self, attr: Sp<lus::Attribute>) -> Result<Self> {
        use syn::Lit;
        let params = attr.t.attr.t.params.map(|_, t| t.flatten());
        let targets = attr.t.attr.t.targets.map(|_, t| t.flatten());
        match (
            attr.t.attr.t.action.t.inner.to_string().as_str(),
            &params.t[..],
            &targets.t[..],
        ) {
            ("trace", [], []) => self.trace.set(true, attr.span),
            ("export", [], []) => self.export.set(true, attr.span),
            ("main", [], []) => self.main.some(100, attr.span),
            ("main", [], [Lit::Int(i)]) => self.main.some(i.base10_parse().unwrap(), attr.span),
            ("rustc_allow", [inner], []) => {
                self.rustc_allow
                    .push(syn::Ident::new(inner, params.span), attr.span);
            }
            (other, _, _) => {
                return Err(err::Basic {
                    msg: format!("Unknown or malformed attribute {other:?}"),
                    span: attr.span,
                }
                .into_err())
            }
        }
        Ok(self)
    }
}
