//! Error accumulator and panic helpers

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

pub mod transparent;
pub use transparent::Transparent;

pub mod error;
pub use error::*; // reexporting all error message constructors

/// Reexport of `Span`, but this time forgeable in tests and
/// transparent to `Eq` and `Hash`.
pub type Span = Transparent<proc_macro2::Span>;

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
            $crate::repo!(),
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

/// Error accumulator to be able to
/// - emit several fatal errors at once
/// - emit warnings
pub struct Acc {
    err: Vec<Error>,
    warn: Vec<Error>,
}

/// Error scope to fail if one of several computations failed.
///
/// Usage example:
/// ```skip
/// for e in &es {
///     foo(acc, e)?;
/// }
/// ```
/// will only try to execute `foo` until it reaches the first error.
/// Subsequent elements will not be handled.
///
/// If instead we wish to execute them all we can do
///
///
pub struct AccScope<'a> {
    acc: &'a mut Acc,
    fatal: bool,
}

impl Acc {
    /// Create an empty accumulator with no errors and no warnings.
    pub fn new() -> Self {
        Self {
            err: vec![],
            warn: vec![],
        }
    }

    /// Push a fatal error to the accumulator.
    /// # Errors
    /// Infaillible. Returns a `Result` so that you can do
    /// `acc.error(...)?;`
    pub fn error<T, E: IntoError>(&mut self, e: E) -> Option<T> {
        self.err.push(e.into_err());
        None
    }

    /// Push a warning to the accumulator.
    pub fn warning<E: IntoError>(&mut self, e: E) {
        self.warn.push(e.into_err());
    }

    /// Determine if there were any errors inserted.
    pub fn is_fatal(&self) -> bool {
        !self.err.is_empty()
    }

    /// Extract the errors and warnings to be emitted.
    pub fn fetch(self) -> (Vec<Error>, Vec<Error>) {
        (self.err, self.warn)
    }

    /// Create a new scope to record the success of intermediate computation.
    pub fn scope(&mut self) -> AccScope {
        AccScope {
            acc: self,
            fatal: false,
        }
    }
}

impl<'a> AccScope<'a> {
    /// Record the success of a new computation.
    pub fn compute<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Acc) -> Option<()>,
    {
        let e = f(&mut *self.acc);
        if e.is_none() {
            self.fatal = true;
        }
    }

    /// Consume the scope and emit an error if and only if one of the
    /// recorded computations failed.
    pub fn close(self) -> Option<()> {
        if self.fatal {
            None
        } else {
            Some(())
        }
    }

    /// Apply an error to the inner accumulator.
    pub fn error<E: IntoError>(&mut self, e: E) {
        self.compute(|acc| acc.error(e))
    }

    /// Apply a warning to the inner accumulator.
    pub fn warning<E: IntoError>(&mut self, e: E) {
        self.compute(|acc| Some(acc.warning(e)))
    }
}
