//! Codegen options for toplevel declarations.
//!
//! This module has a lot of black magic going on, because we want very precise
//! handling of options. The behavior that we're going for is that options
//! should declare upfront where in the compilation process they will be useful,
//! and then we're going to check that
//! - they are not used elsewhere, and
//! - they are actually used in all places announced.
//!
//!
//! # Method
//!
//! How we achieve that is where the black magic comes in.
//!
//! Module `usage` defines structs that describe places where options are relevant.
//! We call one of these structs a "usage site".
//! The two types `Over` and `Sites<Head, Tail>` serve as constructors
//! for a list of usage sites.
//! We interpret a `Sites<Codegen, Over>` as meaning that the option should
//! be used exactly during code generation. A `Sites<Codegen, Sites<Typecheck, Over>>`
//! should be used in both code generation and typechecking.
//!
//! The trait `Usage` then comes in, through:
//! - `impl !Usage<T> for Over`
//! - `impl Usage<Head> for Sites<Head, Tail>`
//! - `impl Usage<Other> for Sites<Head, Tail> where Tail: Usage<Other>`
//!
//! These combine in a way that makes `Usage<T>` implement the test
//! of whether the list of types in `Sites<T1, Sites<T2, ...>>` contains
//! some `Ti = T`.
//!
//! Finally `AssertUsed` will verify that all types in the sequence
//! have had a matching `Usage` invocation.
//!
//!
//! # Usage
//!
//! Everywhere in the compiler if you have a `UseOpt` you can try
//! to invoque `opt.fetch::<This>()` where `This` is a usage site that should
//! match the step of the compilation process that you are doing.
//! Once you think you're done using the option, you can use
//! `opt.assert_used()` to verify it.
//! A good place to use `opt.assert_used()` is at the very end of whatever
//! codegen function you use.
//!
//!
//! # Symptoms
//!
//! If you use options as explained above, a misuse can manifest itself in one of two ways:
//!
//! ### 1.
//! A declared option that is not properly used will result in
//! a runtime error `Not all options of ... were properly used: missing '...'`.
//! This means that nowhere in the program have you ever `fetch`ed the option's value
//! and it is thus considered unused.
//!
//! Good ways to fix the error:
//! - use a `fetch` somewhere relevant to inform that you have used the option, or
//! - remove the extra usage site from the list so that no usage at that location
//!   is expected.
//!
//! Bad ways to fix the error:
//! - DO NOT insert a `let _ = opt.fetch::<This>()` in an unrelated location
//!   just to make the error go away.
//!
//! ### 2.
//! An option that should not have been used will produce a compilation error
//! `the trait Usage<...> is not implemented for 'Over'`
//! This means that you may not use this option during this step of the compilation.
//!
//! Good ways to fix the error:
//! - declare a new usage site in the list, or
//! - keep working as generically as possible so that code down the line
//!   can choose depending on the option.
//!
//! Bad ways to fix the error:
//! - DO NOT pretend to be someone else by using e.g.
//!   `opt.fetch::<Typecheck>()` during codegen.
//!
//!
//! # Robustness
//!
//! The entire option handling pipeline of the Chandeliers suite should be quite robust
//! as long as you start from the right place:
//! 0. Create a file with the right option.
//!    This will produce a *runtime error* that the option is malformed or missing.
//! 1. Parse a new option in `chandeliers_syn::translate::options`'s `fn with` for `Decl`.
//!    This will fix 0. and produce a new *compilation error* in the invocations
//!    of `project!` that there are fields missing.
//! 2. Add the field to either `take` or `skip` depending on what is relevant.
//!    This will fix 1. and produce a new *compilation error* that structs
//!    from this file do not have the right fields.
//! 3. In this file, modify `selection_aux_decl` to include the new definition
//!    and specify its usage site(s).
//! 4. In this file, update the invocations of `selection!` to project the right
//!    field. This will fix 2. only if you have added the fields to exactly the
//!    right option groups, and will produce a new *runtime error* that some
//!    options are unused.
//! 5. Add the relevant `fetch` where the option should be used. If you
//!    make a mistake during this step it will produce a *compilation error*
//!    that some trait is not implemented.
//!
//! As you can see most of the above steps are compilation errors and there
//! is little room for forgetting to handle an option somewhere.
//! Nevertheless several steps are runtime errors and thus may not be caught.
//! To minimize this risk it is recommended to
//! - keep the parsing in `Decl::with` as simple as possible,
//! - invoque `fetch` and `assert_used` on the main path and not behind conditionals.
//!

use crate::sp::Sp;

/// Places where options can be relevant.
pub mod usage {
    /// A type whose name can be printed (since we are using this in error
    /// messages for internal panics, it will help track the source).
    pub(super) trait ShowTy {
        /// How to print the name of this type.
        const NAME: &'static str;
    }

    /// Generate a usage site.
    macro_rules! site {
        ( $doc:tt : $name:ident ) => {
            #[doc = "Indicates that the option is useful during"]
            #[doc = $doc]
            #[derive(Debug, Clone, Copy, Default)]
            pub struct $name;

            impl ShowTy for $name {
                const NAME: &'static str = stringify!($name);
            }
        };
    }
    site!("code generation": Codegen);
    site!("typechecking": Typecheck);
}
use usage::{Codegen, Typecheck};

/// An option that records whether it was used somewhere.
#[derive(Debug, Clone)]
pub struct UseOpt<T, Sites> {
    /// Payload of the option.
    data: T,
    /// Record of all the places where the value was used, for
    /// the purpose of checking the exhaustivity of the application
    /// of the option.
    usage: std::cell::RefCell<Sites>,
}

/// Use the option at a place.
/// Generally `T` should be a usage site.
trait Usage<T> {
    /// Mark the option as used.
    fn record(&mut self);
}

/// Verify that the option was used everywhere.
trait AssertUsed {
    /// Panic if one of the declared use sites is missing.
    fn assert_used(&self, msg: &'static str);
}

impl<T, Sites: Default> UseOpt<T, Sites> {
    /// Get the value of this option and record it as used for the corresponding site.
    #[expect(private_bounds, reason = "Sealed trait pattern")]
    pub fn fetch<Use>(&self) -> &T
    where
        Sites: Usage<Use>,
    {
        Usage::<Use>::record(&mut *self.usage.borrow_mut());
        &self.data
    }

    /// Construct a new (for now unused) option value.
    pub fn new(data: T) -> Self {
        Self {
            data,
            usage: std::cell::RefCell::new(Sites::default()),
        }
    }

    /// Finish the handling of the option by asserting that it was used somewhere.
    #[expect(private_bounds, reason = "Sealed trait pattern")]
    pub fn assert_used(&self, msg: &'static str)
    where
        Sites: AssertUsed,
    {
        self.usage.borrow().assert_used(msg);
    }
}

/// Constructor for usage sites.
/// `AssertUsed` will succeed if `used` is set.
/// `Usage` will either set `used` if `Head` matches the caller's type,
/// or defer to `Tail` to try to find a matching site.
#[derive(Debug, Clone, Default)]
pub struct Sites<Head, Tail> {
    /// Records whether the value was used. Initially `false`, set to `true` by
    /// a call to `Usage`.
    used: bool,
    /// Type is only used by the trait implementations for matching against
    /// the type provided by the caller.
    head: std::marker::PhantomData<Head>,
    /// Other records, usually another `Sites<_, _>`.
    tail: Tail,
}

/// Allow attributes.
#[derive(Clone, Debug)]
pub enum Allow {
    /// `#[allow(x)]`
    Rustc(syn::Ident),
    /// `#[allow(clippy::x)]`
    Clippy(syn::Ident),
}

/// Empty usage site.
/// This is trivially `AssertUsed` but never `Usage`, which
/// ensures that we find a matching usage declaration for every call site.
#[derive(Debug, Clone, Default)]
pub struct Over {}

impl AssertUsed for Over {
    fn assert_used(&self, _: &'static str) {}
}

impl<Head: usage::ShowTy, Tail: AssertUsed> AssertUsed for Sites<Head, Tail> {
    fn assert_used(&self, msg: &'static str) {
        chandeliers_err::consistency!(self.used, "{} during {}", msg, Head::NAME);
        self.tail.assert_used(msg);
    }
}

/// `matching_head_aux!(Head; T1, T2, T3, T4, T5,)` implements
/// `Usage<Head>` for `Sites<Head, _>` trivially,
/// and `Usage<Ti>` for `Sites<Head, _>` by projection to the tail.
///
/// It necessary that `Head` is distinct from all the `Ti`,
/// and recommended that the `Ti` form an exhaustive enumeration of all
/// the other types you want to put in the lists.
macro_rules! matching_head_aux {
    ( $t:ty ; $($other:ty,)* ) => {
        #[doc = "Head matches the uage site, this is a termination point of `Usage`"]
        impl<Tail> Usage<$t> for Sites<$t, Tail> {
            fn record(&mut self) {
                self.used = true;
            }
        }

        $(
            #[doc = "Head does not match the usage site, recurse into the `Tail`."]
            impl<Tail> Usage<$other> for Sites<$t, Tail>
                where Tail: Usage<$other>,
            {
                fn record(&mut self) {
                    self.tail.record()
                }
            }
        )*
    };
}

/// Recursive helper of `matching_head`, given a list
/// `T1, T2, T3, ..., Tn` it will distribute `matching_head_aux`
/// to every `Ti` and `T1, ..., ^Ti, ..., Tn`.
macro_rules! matching_head_rec {
    ( ( $($handled:ty,)* ) -> () ) => {};
    ( ( $($handled:ty,)* ) -> ( $new:ty, $($remaining:ty,)* ) ) => {
        matching_head_aux!($new ; $($handled,)* $($remaining,)*);
        matching_head_rec!( ($($handled,)* $new,) -> ( $($remaining,)* ));
    };
}

/// Initialization stage of the automatic implementation of `Usage`
/// for all the types in the list.
macro_rules! matching_head {
    ( $($remaining:ty,)* ) => {
        matching_head_rec!(() -> ( $($remaining,)* ));
    };
}

matching_head! {
    Codegen,
    Typecheck,
}

/// Where to write debug trace to.
#[derive(Clone, Copy, Debug)]
pub enum TraceFile {
    /// Print to standard output.
    StdOut,
    /// Print to standard error.
    StdErr,
}

/// How to print the debug trace.
#[derive(Clone, Debug)]
pub enum TraceFormat {
    /// Use the default trace format that looks like
    /// "node_name <- (i1=0, i2=42, i3=true)" for the input, and
    /// "node_name -> (o1=false, o2=0.5)" for the output.
    Default,
    /// Do not display any trace
    Empty,
    /// Custom format string provided by the user.
    Str(String),
}

/// Black magic to generate option sets.
/// Will wrap types in `UseOpt` and attach to them the documentation
/// for the defined item.
macro_rules! selection_aux_decl {
    // Recursive cases: handle one extra argument
    //
    // How this works:
    // We use the structure `( <handled> ) ++ ( <unhandled> )` so that the macro
    // can parse its arguments unambiguously, and each recursive step of the macro
    // will grab one item from `<unhandled>` and add it to the end of `<handled>`
    // after doing some processing on it.
    //
    // Thus in one step of macro expansion,
    // `selection_aux_decl(foo ( <handled> ) ++ ( item, <unhandled ));`
    // turns into
    // `selection_aux_decl(foo ( <handled> <handle item> ) ++ ( <unhandled> ));`
    //
    // The processing in question includes inserting a `pub` qualifier to
    // all fields, adding documentation, and registering their type.
    ( $struct:ident ( $($done:tt)* ) ++ ( trace , $($rest:tt)* ) )
        // #[trace] is of type `Option<TraceFile>` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[trace]`: print debug information."]
                pub trace: UseOpt<Option<(TraceFile, (TraceFormat, TraceFormat))>, Sites<Codegen, Over>>,
            ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( export , $($rest:tt)* ) )
        // #[export] is of type `bool` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[export]`: make the struct visible."]
                pub export: UseOpt<bool, Sites<Codegen, Over>>,
        ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( public , $($rest:tt)* ) )
        // #[export] is of type `bool` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[pub]`: make the struct public."]
                pub public: UseOpt<bool, Sites<Codegen, Over>>,
        ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( impl_trait , $($rest:tt)* ) )
        // #[export] is of type `bool` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[trait]`: make the struct public."]
                pub impl_trait: UseOpt<bool, Sites<Codegen, Over>>,
        ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( main , $($rest:tt)* ) )
        // `#[main]` is of type `Option<usize>` and is useful only during both
        // typechecking (verify that the inputs and outputs are `()`) and codegen
        // (write the actual function).
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[main(42)]`: generate a main function that executes this node a fixed number of times."]
                pub main: UseOpt<Option<usize>, Sites<Typecheck, Sites<Codegen, Over>>>,
            ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( rustc_allow , $($rest:tt)* ) )
        // #[rustc_allow] is of type `Vec<syn::Ident>` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[rustc_allow[dead_code]]`: forward the attribute to Rustc as an `#[allow(dead_code)]`"]
                pub rustc_allow: UseOpt<Vec<Allow>, Sites<Codegen, Over>>,
            ) ++ ( $($rest)* ) ); };
    ( $struct:ident ( $($done:tt)* ) ++ ( doc , $($rest:tt)* ) )
        // #[doc] is of type `Vec<Sp<String>>` and is useful only during codegen.
        => { selection_aux_decl!($struct ( $($done)*
                #[doc = "`#[doc(\"Message\")]`: forward the attribute to Rustc for documentation purposes`"]
                pub doc: UseOpt<Vec<Sp<String>>, Sites<Codegen, Over>>,
            ) ++ ( $($rest)* ) ); };
    // Base case: generate the struct definition from all the accumulated tokens in `<handled>`
    // (by now `<unhandled>` is empty).
    ( $struct:ident ( $($done:tt)* ) ++ () )
        => {
            #[doc = "Codegen options available for "]
            #[doc = stringify!($struct)]
            #[derive(Debug, Clone)]
            pub struct $struct { $($done)* }
        };
}

/// Implement `asssert_used` for a group of options by applying
/// it to each field.
macro_rules! selection_aux_impl {
    ($struct:ident; $($field:ident),*) => {
        impl $struct {
            #[doc = "Verify that all fields of "]
            #[doc = stringify!($struct)]
            #[doc = " were used"]
            pub fn assert_used(&self) {
                $( self.$field.assert_used(
                        concat!(
                            "Not all options of ",
                            stringify!($struct),
                            " were properly used: missing `",
                            stringify!($field),
                            "`"
                        )
                    );
                )*
            }
        }
    }
}

/// Generate option groups from a subset of predefined options available.
macro_rules! selection {
    ( pub struct $struct:ident { $( $opt:ident ),* } )
        => {
            selection_aux_decl!($struct () ++ ( $($opt,)* ) );
            selection_aux_impl!($struct; $($opt),* );
    };
}

selection! { pub struct Node { trace, export, public, main, rustc_allow, doc, impl_trait } }
selection! { pub struct ExtNode { trace, main, rustc_allow } }
selection! { pub struct Const { export, public, rustc_allow, doc } }
selection! { pub struct ExtConst { rustc_allow } }
