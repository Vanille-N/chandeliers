//! A mostly transparent wrapper around any type to indicate that it carries
//! a `Span` with it.
//!
//! The recommended pattern is that whenever you have some type `T` that
//! needs a method `foo(&self, span: Span)`, you should implement
//! `foo(&self)` for `Sp<T>` that gives its associated span to the inner call.
//! Macros are provided to facilitate this usage.
//!
//! Note: yous should generally prefer putting the `Sp` on the outside
//! in public function arguments, and on the inside on return values.
//! That is,
//! - use `Sp<&T>`, `Sp<Box<T>>`, `Sp<Result<T, E>>` in inputs and struct fields,
//! - use `&Sp<T>`, `Box<Sp<T>>`, `Result<Sp<T>, E>>` in outputs.
//! The reason is that `Sp` implements the following conversions:
//! - `&Sp<T> -> Sp<&T>` through `as_ref`,
//! - `&mut Sp<T> -> Sp<&mut T>` through `as_ref_mut`,
//! - `Box<Sp<T>> -> Sp<Box<T>>` through `boxed`,
//! - `Sp<Result<T, E>> -> Result<Sp<T>, E>` through `transpose`.
//! Thus a function that takes as input a `Sp<&T>` is more general than
//! a function that takes a `&Sp<T>`.
//!
//! The above does not necessarily apply to functions that are strictly for
//! internal use, since if all callers are known the benefit of the generality
//! is lessened, and passing `&Sp<T>` may reduce copies of `Span` that come with
//! each invocation of `as_ref`.

use std::fmt;

pub use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::{quote_spanned, ToTokens};

use chandeliers_err as err;

use crate::transparent::Transparent;

/// Span wrapper.
///
/// This type is ubiquitous across this entire crate and you can expect
/// an overwhelming majority of the fields of all structs to contain one or
/// several `Sp<_>`.
///
/// `Sp` is mostly used through `map`, `new`, and it also implements
/// many traits by projecting into its `.t` field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sp<T> {
    /// A payload.
    pub t: T,
    /// The span associated with the payload.
    pub span: Transparent<Span>,
}

impl<T> err::TrySpan for Sp<T> {
    /// `Sp` always has a span, so `TrySpan` is guaranteed to succeed.
    fn try_span(&self) -> Option<Span> {
        Some(self.span.inner)
    }
}

impl<T> Sp<T> {
    /// Convert a `&Sp<T>` into a `Sp<&T>`.
    /// Mostly useful because `map` consumes Self and
    /// because `Sp` derives `Copy`.
    pub fn as_ref(&self) -> Sp<&T> {
        Sp {
            t: &self.t,
            span: self.span,
        }
    }

    /// Convert an `&mut Sp<T>` into a `Sp<&mut T>`.
    /// Mostly useful because `map` consumes Self and
    /// because `Sp` derives `Copy`.
    pub fn as_ref_mut(&mut self) -> Sp<&mut T> {
        Sp {
            t: &mut self.t,
            span: self.span,
        }
    }

    /// Apply a transformation to the payload while preserving the same span.
    ///
    /// This lets us track the same portion of the source code from beginning
    /// to end through translations into different ASTs.
    pub fn map<U, F>(self, f: F) -> Sp<U>
    where
        F: FnOnce(Span, T) -> U,
    {
        Sp {
            t: f(self.span.inner, self.t),
            span: self.span,
        }
    }

    /// Wrap the inner type in a Box.
    /// Useful when there are recursive structures that need indirection.
    pub fn boxed(self) -> Sp<Box<T>> {
        self.map(|_, t| Box::new(t))
    }

    /// Get the same contents but with a new span.
    #[must_use]
    pub fn with_span<S: Into<Transparent<Span>>>(mut self, span: S) -> Self {
        self.span = span.into();
        self
    }
}

impl<T, E> Sp<Result<T, E>> {
    /// Monad combinator to map faillible functions on a `Sp`.
    /// # Errors
    /// If the internal data already contains an error.
    pub fn transpose(self) -> Result<Sp<T>, E> {
        match self.t {
            Ok(t) => Ok(Sp { t, span: self.span }),
            Err(e) => Err(e),
        }
    }
}

/// `SpanEnd` is the way that `Sp` has of computing its own span:
/// upon parsing it will record the beginning of the span to use as its
/// own, but ideally it also wants to eventually know the end of the Span,
/// and for that it asks `T`.
///
/// `SpanEnd` should be implemented for all types `T` for which you want to
/// be able to parse a `Sp<T>`
pub trait SpanEnd {
    /// Where this object ends.
    ///
    /// This is intentionally defined very loosely:
    /// - all types may return `None` if they do not want to reveal their
    ///   size or if they don't know it,
    /// - in the case of `Some(s)`, `s` may or may not include the entire
    ///   the only thing that is relevant is the endpoint.
    fn span_end(&self) -> Option<Span>;
}

/// Straightforward projection.
impl<T: SpanEnd> SpanEnd for Box<T> {
    fn span_end(&self) -> Option<Span> {
        self.as_ref().span_end()
    }
}

/// `Punctuated` projects to its last element.
///
/// Historical note: `SpanEnd` was created for the most part to handle
/// the fact that `Punctuated` is sometimes empty and has no span.
/// Although `Sp` is the main implementor, `Punctuated` is the reason
/// that it has to exist at all.
/// `Punctuated` is one of very few implementations that can even return `None`,
/// since most parsed objects are nonempty.
impl<T: SpanEnd, P> SpanEnd for syn::punctuated::Punctuated<T, P> {
    fn span_end(&self) -> Option<Span> {
        self.last().and_then(SpanEnd::span_end)
    }
}

/// Parsing `Sp<T>` invoques `SpanEnd` to know where the parsing ended.
impl<T> syn::parse::Parse for Sp<T>
where
    T: syn::parse::Parse + SpanEnd,
{
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let begin = input.span();
        let t: T = input.parse()?;
        let end = t.span_end().unwrap_or(begin);
        Ok(Self {
            t,
            span: Transparent {
                inner: begin
                    .join(end)
                    .unwrap_or_else(|| err::abort!("Malformed span between {begin:?} and {end:?}")),
            },
        })
    }
}

/// Don't let the trivial implementation of `SpanEnd` for `Sp<T>` distract
/// you from its importance, the fact that `Sp<T>` returns a `Some(_)` in
/// one step is what makes it computationally feasible at all to have
/// most implementors of `SpanEnd` recurse into one field without blowing up
/// the stack of Rustc.
impl<T> SpanEnd for Sp<T>
where
    T: SpanEnd,
{
    fn span_end(&self) -> Option<Span> {
        Some(self.span.inner)
    }
}

/// `Sp` is transparently displayable.
impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

/// Implement `SpanEnd` from `syn::Spanned`.
/// Only recommended for items whose `span` is trivial, others should
/// be wrapped in a `Sp<_>` to alleviate the computation.
///
/// Usage: if your type `T` has an infaillible `span()` function,
/// then you can use `span_end_from_spanned!(T);`.
macro_rules! span_end_from_spanned {
    ( $($ty:tt)* ) => {
        impl SpanEnd for $($ty)* {
            fn span_end(&self) -> Option<Span> {
                Some(self.span())
            }
        }
    }
}

// Some implementation for `syn` types.
span_end_from_spanned!(syn::Ident);
span_end_from_spanned!(syn::Lit);
impl SpanEnd for syn::token::Paren {
    fn span_end(&self) -> Option<Span> {
        Some(self.span.join())
    }
}
impl SpanEnd for syn::token::Bracket {
    fn span_end(&self) -> Option<Span> {
        Some(self.span.join())
    }
}

/// Helper trait for projections through `Sp`.
/// This defines how types should look when they have an attached span.
/// For most types `T` this would be `Sp<T>`, however they may be some variants,
/// most notably `TokenStream` with a `Span` is still a `TokenStream`.
pub trait WithSpan: Sized {
    /// Self after wrapping, typically but not necessarily `Sp<Self>`.
    type Output;
    /// Add a span.
    fn with_span<S: Into<Transparent<Span>>>(self, span: S) -> Self::Output;
}

/// Implement `Spanned`
macro_rules! derive_with_span {
    ( $T:ty ) => {
        #[doc = "Default implementation of `Spanned` for"]
        #[doc = concat!("`", stringify!($T), "`")]
        #[doc = "simply wraps it in `Sp`"]
        impl $crate::sp::WithSpan for $T {
            type Output = $crate::sp::Sp<Self>;
            fn with_span<S: Into<$crate::transparent::Transparent<$crate::sp::Span>>>(self, span: S) -> $crate::sp::Sp<Self> {
                $crate::sp::Sp { t: self, span: span.into() }
            }
        }
    };
    ( $T:ty where $($t:tt)* ) => {
        #[doc = "Default implementation of `Spanned` for"]
        #[doc = concat!("`", stringify!($T), "`")]
        #[doc = "simply wraps it in `Sp`"]
        impl$($t)* $crate::sp::WithSpan for $T {
            type Output = $crate::sp::Sp<Self>;
            fn with_span<S: Into<$crate::transparent::Transparent<$crate::sp::Span>>>(self, span: S) -> $crate::sp::Sp<Self> {
                $crate::sp::Sp { t: self, span: span.into() }
            }
        }
    };

}
pub(crate) use derive_with_span;

impl WithSpan for TokenStream {
    type Output = TokenStream;
    /// This is not a noop, it invoques `quote_spanned` to give the `TokenStream`
    /// the right location.
    fn with_span<S: Into<Transparent<Span>>>(self, span: S) -> TokenStream {
        quote_spanned! {span.into().into()=>
            #self
        }
    }
}
derive_with_span!(usize);
derive_with_span!(String);
derive_with_span!(());

/// Helper to implement projections through `Sp`.
///
/// Modes:
/// - `fn foo return U where T`:<
///    assumes that there is a type `T` available, and that it has a method
///    `foo(&self, span: Span) -> U`.
///    This macro will implement `foo(&self) -> SpU` for `Sp<T>`,
///    where `SpU` is chosen appropriately depending on `U`:
///    generally `SpU = Sp<U>`, but for the specific case of `U = TokenStream`,
///    `SpU = TokenStream`.
///
macro_rules! transparent_impl {
    ( fn $fn:ident return $T:ty where $ty:ty ) => {
        impl Sp<$ty> {
            fn $fn(&self) -> <$T as $crate::sp::WithSpan>::Output {
                use $crate::sp::WithSpan;
                let inner = self.t.$fn(self.span.inner);
                inner.with_span(self.span)
            }
        }
    };
}
pub(crate) use transparent_impl;

/// `Sp` is transparently printable, but gives its own span to the output.
impl<T: ToTokens> ToTokens for Sp<T> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { span, t } = &self;
        toks.extend(quote_spanned! {span.inner=>
            #t
        });
    }
}
