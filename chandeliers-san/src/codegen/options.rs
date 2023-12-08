//! Generate code depending on options passed through declaration attributes.

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};

use crate::ast;
use crate::ast::options::usage::Codegen as This;
use crate::ast::options::{Allow, Const, ExtNode, Node, TraceFile, TraceFormat};
use crate::sp::Sp;

impl ToTokens for Allow {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Rustc(id) => quote!( #[allow(#id)] ),
            Self::Clippy(id) => quote!( #[allow(clippy::#id)] ),
        });
    }
}

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
        from trace return &Option<(TraceFile, (TraceFormat, TraceFormat))>;
        fn traces(
            &self,
            prefix: &str,
            name: &Sp<ast::decl::NodeName>,
            inputs: Sp<&ast::Tuple<Sp<ast::decl::TyVar>>>,
            locals: Sp<&ast::Tuple<Sp<ast::decl::TyVar>>>,
            outputs: Sp<&ast::Tuple<Sp<ast::decl::TyVar>>>,
        ) -> (TokenStream, TokenStream) {
            if let Some((file, (ifmt, ofmt))) = self.fetch() {
                let name = format!("{name}");
                let input_var_fmt = inputs.fmt_strings();
                let output_var_fmt = outputs.fmt_strings();
                let inputs_self_assigned = inputs.self_assigned();
                let locals_self_assigned = locals.self_assigned();
                let outputs_self_assigned = outputs.self_assigned();
                let input_fmt = match ifmt {
                    TraceFormat::Default => format!("{{_ext}}{{_this}} <- {input_var_fmt}\n"),
                    TraceFormat::Empty => String::new(),
                    TraceFormat::Str(s) => s.clone(),
                };
                let output_fmt = match ofmt {
                    TraceFormat::Default => format!("{{_ext}}{{_this}} -> {output_var_fmt}\n"),
                    TraceFormat::Empty => String::new(),
                    TraceFormat::Str(s) => s.clone(),
                };
                let printer = match file {
                    TraceFile::StdOut => quote!(print),
                    TraceFile::StdErr => quote!(eprint),
                };
                (
                    quote! {
                        #[allow(unused_variables)]
                        {
                            let _ext = #prefix;
                            let _this = #name;
                            let _clk = self.__clock;
                            #inputs_self_assigned
                            #printer!(#input_fmt);
                        }
                    },
                    quote! {
                        #[allow(unused_variables)]
                        {
                            let _ext = #prefix;
                            let _this = #name;
                            let _clk = self.__clock;
                            #inputs_self_assigned
                            #locals_self_assigned
                            #outputs_self_assigned
                            #printer!(#output_fmt);
                        }
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
        fn fn_main(&self, name: &Sp<ast::decl::NodeName>, rustc_allow: &Vec<Allow>) -> TokenStream {
            if let Some(nb_iter) = self.fetch() {
                let ext_name = name.as_sanitized_ident();

                let doc = format!(
                    "Main function automatically generated from {name} (runs for {nb_iter} steps)"
                );
                quote_spanned! {name.span.unwrap()=>
                    #[doc = #doc]
                    #[allow(unused_imports)] // Step, Embed, Trusted imported just in case.
                    #( #rustc_allow )*
                    pub fn main() {
                        use ::chandeliers_sem::traits::{Step, Embed, Trusted};
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
    #[doc = "`#[test(42)]`: build a test function that executes this node a set number of times."]
    trait FnTest for { Node }
    impl {
        from test return &Option<usize>;
        fn fn_test(&self, name: &Sp<ast::decl::NodeName>, rustc_allow: &Vec<Allow>) -> TokenStream {
            if let Some(nb_iter) = self.fetch() {
                let priv_name = name.as_sanitized_ident();
                let pub_name = name.as_raw_ident();

                let doc = format!(
                    "Test function automatically generated from {name} (runs for {nb_iter} steps)"
                );
                quote_spanned! {name.span.unwrap()=>
                    #[doc = #doc]
                    #[allow(unused_imports)] // Step, Embed, Trusted imported just in case.
                    #( #rustc_allow )*
                    #[test]
                    pub fn #pub_name() {
                        use ::chandeliers_sem::traits::{Step, Embed, Trusted};
                        let mut sys = #priv_name::default();
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
        fn pub_qualifier(&self, implementing_trait: bool) -> TokenStream {
            if *self.fetch() && !implementing_trait{
                quote!(pub)
            } else {
                quote!()
            }
        }
    }
}

generic_option! {
    #[doc = "`#[doc(\"Message\")]`: insert documentation in the generated code."]
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

generic_option! {
    #[doc = "`#[generics[T, U, V]]`: declare type variables."]
    trait GenericParams for { Node, ExtNode }
    impl {
        from generics return &Vec<Sp<String>>;
        fn generic_params(&self) -> TokenStream {
            let generics = self.fetch().iter().map(|t| syn::Ident::new_raw(&t.t, t.span.unwrap())).collect::<Vec<_>>();
            if generics.is_empty() {
                quote! {}
            } else {
                quote! {
                    < #( #generics ),* >
                }
            }
        }
    }
}
