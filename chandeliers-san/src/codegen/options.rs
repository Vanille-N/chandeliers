//! Generate code depending on options passed through declaration attributes.

use crate::sp::Sp;
use proc_macro2::TokenStream;
use quote::quote;

use crate::ast;
use crate::ast::options::usage::Codegen as This;
use ast::options::{Const, ExtNode, Node};

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
                    *self.$field.fetch::<This>()
                }
            }
        )*
    };
}

generic_option! {
    #[doc = "`#[trace]`: print debug information."]
    trait Traces for { Node, ExtNode }
    impl {
        from trace return bool;
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
            if self.fetch() {
                (
                    quote! {
                        println!(#input_fmt, #prefix, #name, #inputs);
                    },
                    quote! {
                        println!(#output_fmt, #prefix, #name, #outputs);
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
        from main return Option<usize>;
        fn fn_main(&self, name: &Sp<ast::decl::NodeName>) -> TokenStream {
            if let Some(nb_iter) = self.fetch() {
                let ext_name = name.as_raw_ident();
                let doc = format!(
                    "Main function automatically generated from {name} (runs for {nb_iter} steps)"
                );
                quote! {
                    #[doc = #doc]
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
    #[doc = "`#[export]`: make this declaration public."]
    trait PubQualifier for { Const, Node }
    impl {
        from export return bool;
        fn pub_qualifier(&self) -> TokenStream {
            if self.fetch() {
                quote!(pub)
            } else {
                quote!()
            }
        }
    }
}