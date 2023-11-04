//! Generate code depending on options passed through declaration attributes.

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};

use crate::ast;
use crate::ast::options::usage::Codegen as This;
use crate::ast::options::{Const, ExtNode, Node, TraceFile};
use crate::sp::Sp;

/// Define the behavior of an option.
///
/// We expect most options to be specific enough that their functionality
/// can be implemented homogeneously between all declarations that it can
/// apply to, to we define this macro as a way to provide the implementation
/// of an option for all declarations at once.
///
/// Typical usage will look like this:
/// ```skip
/// generic_option! {
///     #[doc = "Explanation of the option"]
///     // This defines the name of the trait that implements this option,
///     // and the implementors of the trait in question among `Node`, `ExtNode`,
///     // `Const`, and `ExtConst`.
///     trait Stuff for { Node, Const }
///     impl {
///         // All option groups should have a `stuff` field of type
///         // `UseOpt<bool, context[Codegen]>`
///         // See [`chandeliers_san::ast::options::UseOpt`]
///         // for more information.
///         from stuff return bool;
///         // Finally the implementation.
///         fn stuff(&self) -> TokenStream {
///             unimplemented!()
///         }
///     }
/// }
/// ```
macro_rules! generic_option {
    (
      $( #[$($doc:tt)*] )*
      trait $trait:ident for { $($implementor:ty),* }
      impl {
          from $field:ident return $fetch:ty ;
          $($func:tt)*
      }
    ) => {
        $( #[$($doc)*] )*
        pub trait $trait {
            #[doc = "How to get the value for this option"]
            fn fetch(&self) -> $fetch;

            #[doc = "How to construct a TokenStream that implements the feature from this option"]
            $($func)*
        }

        $(
            impl $trait for $implementor {
                fn fetch(&self) -> $fetch {
                    self.$field.fetch::<This>()
                }
            }
        )*
    };
}

generic_option! {
    #[doc = "`#[trace]`: print debug information."]
    trait Traces for { Node, ExtNode }
    impl {
        from trace return &Option<TraceFile>;
        fn traces(
            &self,
            prefix: &str,
            name: &Sp<ast::decl::NodeName>,
            inputs: Sp<&ast::Tuple<Sp<ast::decl::TyVar>>>,
            outputs: Sp<&ast::Tuple<Sp<ast::decl::TyVar>>>,
        ) -> (TokenStream, TokenStream) {
            let name = format!("{name}");
            let input_var_fmt = inputs.fmt_strings();
            let output_var_fmt = outputs.fmt_strings();
            let inputs = inputs.flattened_trailing_comma();
            let outputs = outputs.flattened_trailing_comma();
            let input_fmt = format!("{{}}{{}} <- {input_var_fmt}");
            let output_fmt = format!("{{}}{{}} -> {output_var_fmt}",);
            if let Some(file) = self.fetch() {
                let printer = match file {
                    TraceFile::StdOut => quote!(println),
                    TraceFile::StdErr => quote!(eprintln),
                };
                (
                    quote! {
                        #printer!(#input_fmt, #prefix, #name, #inputs);
                    },
                    quote! {
                        #printer!(#output_fmt, #prefix, #name, #outputs);
                    },
                )
            } else {
                (quote!(), quote!())
            }
        }
    }
}

generic_option! {
    #[doc = "`#[main(42)]`: build a `main` function that executes this node a set number of times."]
    trait FnMain for { Node, ExtNode }
    impl {
        from main return &Option<usize>;
        fn fn_main(&self, name: &Sp<ast::decl::NodeName>, rustc_allow: &Vec<syn::Ident>) -> TokenStream {
            if let Some(nb_iter) = self.fetch() {
                let ext_name = name.as_sanitized_ident();

                let doc = format!(
                    "Main function automatically generated from {name} (runs for {nb_iter} steps)"
                );
                quote_spanned! {name.span.into()=>
                    #[doc = #doc]
                    #( #[allow( #rustc_allow )] )*
                    pub fn main() {
                        use ::chandeliers_sem::traits::*;
                        let mut sys = #ext_name::default();
                        if #nb_iter == 0 {
                            loop {
                                sys.step(().embed()).trusted();
                            }
                        } else {
                            for _ in 1..=#nb_iter {
                                sys.step(().embed()).trusted();
                            }
                        }
                    }
                }
            } else {
                quote!()
            }
        }
    }
}

generic_option! {
    #[doc = "`#[pub]`: make this declaration public. Implies `#[export]`."]
    trait PubQualifier for { Const, Node }
    impl {
        from public return &bool;
        fn pub_qualifier(&self) -> TokenStream {
            if *self.fetch() {
                quote!(pub)
            } else {
                quote!()
            }
        }
    }
}

generic_option! {
    #[doc = "`[doc(\"Message\")]`: insert documentation in the generated code."]
    trait Docs for { Const, Node }
    impl {
        from doc return &Vec<Sp<String>>;
        fn docs(&self) -> TokenStream {
            let docs = self.fetch();
            if docs.is_empty() {
                quote! {
                    #[doc = "(No user documentation provided)"]
                }
            } else {
                quote! {
                    #( #[doc = #docs] )*
                }
            }
        }
    }
}
