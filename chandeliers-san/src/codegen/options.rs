//! Generate code depending on options passed through declaration attributes.

use crate::sp::Sp;
use proc_macro2::TokenStream;
use quote::quote;

use crate::ast;
use crate::ast::options::usage;
use ast::options::{Const, ExtNode, Node};

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
                    *self.$field.fetch::<usage::Codegen>()
                }
            }
        )*
    };
}

generic_option! {
    #[doc = "Print debug information."]
    #[doc = "Format: `#[trace]`."]
    #[doc = "Default: disabled."]
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
    #[doc = "Build a `main` function that executes this node."]
    #[doc = "Format: `#[main(42)]`."]
    #[doc = "Default: disabled."]
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
    #[doc = "Make this declaration public."]
    #[doc = "Format: `#[export]`"]
    #[doc = "Default: disabled."]
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
