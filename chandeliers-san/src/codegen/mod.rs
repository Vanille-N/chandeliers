//! Generate actual Candle statements from an AST.

use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use crate::ast::options::usage::Codegen as This;
use crate::ast::{decl, expr, op, past, stmt, ty, var, Tuple};
use crate::sp::{Sp, Span, WithSpan};
use chandeliers_err as err;

mod constexpr;
pub mod options;

use constexpr::ConstExprSpanTokens;
use options::{Docs, FnMain, FnTest, GenericParams, PubQualifier, Traces};

/// Generate a program simply by generating all declarations.
impl ToTokens for decl::Prog {
    fn to_tokens(&self, toks: &mut TokenStream) {
        for decl in &self.decls {
            decl.to_tokens(toks);
        }
    }
}

/// Declaration of a toplevel object is one of
/// - extern const: skipped, must be provided elsewhere,
/// - extern node: skipped, must be provided elsewhere,
/// - const: generate name and value,
/// - node: generate declaration and implementation of `update_mut`.
impl ToTokens for decl::Decl {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::ExtConst(c) => toks.extend(quote!( #c )),
            Self::ExtNode(n) => toks.extend(quote!( #n )),
            Self::Const(c) => toks.extend(quote!( #c )),
            Self::Node(n) => toks.extend(quote!( #n )),
        }
    }
}

/// Unit marker for `AsTy`.
//struct LustreTy;
/// Unit marker for `AsTy`.
struct DefinedTy;
/// Unit marker for `AsTy`.
//struct EmbeddedTy;
trait AsTy<T> {
    fn as_ty(&self, marker: T, span: Option<Span>) -> TokenStream;
}

impl<T, U: AsTy<T>> AsTy<T> for Sp<U> {
    fn as_ty(&self, marker: T, _span: Option<Span>) -> TokenStream {
        let ty = self.t.as_ty(marker, Some(self.span));
        quote_spanned! {self.span.unwrap()=> #ty }
    }
}

impl<T, U: AsTy<T>> AsTy<T> for &U {
    fn as_ty(&self, marker: T, span: Option<Span>) -> TokenStream {
        (*self).as_ty(marker, span)
    }
}

impl AsTy<DefinedTy> for ty::Base {
    fn as_ty(&self, _marker: DefinedTy, span: Option<Span>) -> TokenStream {
        match self {
            Self::Int => quote!(i64),
            Self::Float => quote!(f64),
            Self::Bool => quote!(bool),
            Self::Other(t) => {
                let id = Ident::new(
                    &format!("{t}"),
                    span.unwrap(/* Transparent<Span> */).unwrap(/* Span */),
                );
                quote!( #id )
            }
        }
    }
}

/// Unit marker for `AsIdent`.
struct SanitizedIdent;
/// Unit marker for `AsIdent`.
struct RawIdent;

trait IdentGeneration {
    fn new_ident(name: &str, span: Span) -> Ident;
}

impl IdentGeneration for SanitizedIdent {
    fn new_ident(name: &str, span: Span) -> Ident {
        Ident::new(name, span.unwrap())
    }
}

impl IdentGeneration for RawIdent {
    fn new_ident(name: &str, span: Span) -> Ident {
        Ident::new_raw(name, span.unwrap())
    }
}

trait AsIdent<T>
where
    T: IdentGeneration,
{
    fn name(&self) -> String;
    fn respan(&self, span: Option<Span>) -> Span {
        span.unwrap()
    }
    fn as_ident(&self, _marker: T, span: Option<Span>) -> TokenStream {
        let span = self.respan(span);
        let ident = T::new_ident(&self.name(), span);
        quote_spanned!(span.unwrap()=> #ident)
    }
}

impl<T: IdentGeneration, U: AsIdent<T>> AsIdent<T> for Sp<U> {
    fn name(&self) -> String {
        self.t.name()
    }
    fn respan(&self, _span: Option<Span>) -> Span {
        self.span
    }
}

impl<T: IdentGeneration, U: AsIdent<T>> AsIdent<T> for &U {
    fn name(&self) -> String {
        (*self).name()
    }
    fn respan(&self, span: Option<Span>) -> Span {
        (*self).respan(span)
    }
}

impl AsIdent<SanitizedIdent> for decl::NodeName {
    fn name(&self) -> String {
        format!("{}__lus_{}_node", &self.repr.t, self.run_uid)
    }
}

impl AsIdent<RawIdent> for decl::NodeName {
    fn name(&self) -> String {
        self.repr.t.to_string()
    }
}

impl AsIdent<SanitizedIdent> for var::Global {
    fn name(&self) -> String {
        format!("{}__lus_{}_global", &self.repr.t, self.run_uid)
    }
}

impl AsIdent<RawIdent> for var::Global {
    fn name(&self) -> String {
        self.repr.t.to_string()
    }
}

impl AsIdent<SanitizedIdent> for var::Local {
    fn name(&self) -> String {
        format!("{}__lus_local", &self.repr.t)
    }
}

impl AsIdent<RawIdent> for var::Local {
    fn name(&self) -> String {
        self.repr.t.to_string()
    }
}

/// Global constant.
///
/// This is straightforward generation of the components, apart from the
/// need to eliminate all non-const expression constructors (functions, nodes
/// later operator).
/// Because the internals use sanitized names, we also create a publicly
/// accessible alias.
impl ToTokens for decl::Const {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self {
            name,
            options,
            ty,
            value,
        } = self;
        let span = name.span;
        let ext_name = name.as_ident(RawIdent, None);
        let glob = name.as_ident(SanitizedIdent, None);
        let value = value.const_expr_tokens();
        let rs_ty = ty.as_ref().as_ty(DefinedTy, None);
        let pub_qualifier = options.pub_qualifier(/*trait*/ false);
        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();
        let export = *options.export.fetch::<This>();

        let pretty_ty = format!("`{ty}`");
        let docs = options.docs();
        let declaration = quote_spanned! {span.unwrap()=>
            #pub_qualifier const #ext_name : #rs_ty = #glob ;
        };

        toks.extend(quote_spanned! {span.unwrap()=>
            #[doc(hidden)] // We don't want the inner decl to be visible at all
            #[allow(non_upper_case_globals)] // Lustre naming conventions
            // Completely nonsensical suggestion by Clippy.
            #[allow(clippy::unreadable_literal, clippy::zero_prefixed_literal)]
            #( #rustc_allow_2 )* // User-provided
            const #glob : #rs_ty = #value ;
        });

        if export {
            toks.extend(quote! {
                #docs
                #[doc = "\n"]
                #[doc = "Constant of type"]
                #[doc = #pretty_ty]
                #[allow(non_upper_case_globals)] // Lustre naming conventions
                #( #rustc_allow_1 )* // User-provided
                #declaration
            });
        }

        options.assert_used();
    }
}

/// Extern global constant: assert existence and typing.
///
/// There is not really a value to generate, we simply create a dummy const
/// to assert that the one declared was found. While we're at it,
/// we create an alias of `G` into `lus__global_G` to avoid further collisions.
///
/// The trick here to get a decent error message that
/// - points to the right place, and also
/// - doesn't suggest nonsensical modifications,
/// is to
/// - do a variable assignment first to limit the compiler's visibility
///   on who is in the right and who is in the wrong.
/// - give some custom spans to the name and type so that the
///   right parts "expected _, found _" and "expected due to this" are underlined,
/// - wrap everything in an opaque wrapper `Type` so that the compiler
///   can't suggest "use a conversion function, e.g. `as f64`".
///
/// We are effectively obfuscating the source of the error for the compiler
/// so that it prints a less smart but more correct error message.
impl ToTokens for decl::ExtConst {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty, options } = self;
        let ext_name = name.as_ident(RawIdent, None);
        let expected = ty.as_ref().as_ty(DefinedTy, None);
        let glob = name.as_ident(SanitizedIdent, None);
        let real = quote_spanned!(name.span.unwrap()=> real );
        let expected_wrapped = quote_spanned!(ty.span.unwrap()=> Type<#expected> );
        let rustc_allow = options.rustc_allow.fetch::<This>().iter();
        toks.extend(quote_spanned! {name.span.unwrap()=>
            #[doc(hidden)] // Internal declaration only.
            #[allow(non_upper_case_globals)] // Lustre naming conventions.
            #( #rustc_allow )* // User-provided
            const #glob: #expected = {
                struct Type<T>(T);
                let #real = Type(#ext_name);
                let expected: #expected_wrapped = #real ;
                expected.0
            };
        });
        options.assert_used();
    }
}

impl ToTokens for decl::Node {
    /// Generate a node declaration and implementation as
    /// specified by the interface of Candle.
    ///
    /// ```skip
    /// struct MyNode {
    ///     __clock: usize,
    ///     // Inputs
    ///     i1: ty!(int+),
    ///     i2: ty!(int++),
    ///     // Outputs
    ///     o1: ty!(float+),
    ///     o1: ty!(float++++),
    ///     // Locals
    ///     l1: ty!(int+),
    ///     __nodes: (block1, block2, ...),
    /// }
    ///
    /// impl Step for MyNode {
    ///     fn step(&mut self, inputs: (ty!(int), ty!(int))) -> (ty!(float), ty!(float)) {
    ///         let ... = ...;
    ///         // other statements go here
    ///         // including stepsof subnodes
    ///         let _1 = substep!(self <~ 1; 0 => { ... }|*);
    ///         // then tick the clock,
    ///         tick!(self);
    ///         // update all internal values for the next step,
    ///         update!(self, i1);
    ///         update!(self, o1);
    ///         update!(self, l1);
    ///         // and return
    ///         (o1, o2)
    ///     }
    /// }
    /// ```
    ///
    /// See the actual definitions of the language in crate `chandeliers-sem`
    /// if you find the above example confusing.
    fn to_tokens(&self, toks: &mut TokenStream) {
        let int = self.internal_decl();
        let ext = self.external_decl();

        toks.extend(int);
        toks.extend(ext);
        toks.extend(
            self.options
                .fn_main(&self.name, self.options.rustc_allow.fetch::<This>()),
        );
        toks.extend(
            self.options
                .fn_test(&self.name, self.options.rustc_allow.fetch::<This>()),
        );
        self.options.assert_used();
    }
}

impl ToTokens for decl::NodeInstance {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, generics } = self;
        let name = name.as_ident(SanitizedIdent, None);
        let generics = generics
            .as_ref()
            .unwrap_or_else(|| err::abort!("Generics are not sufficiently instanciated"));
        let generics = if generics.is_empty() {
            quote!()
        } else {
            let generics = generics.iter().map(|t| t.as_ty(DefinedTy, None));
            quote! {
                < #( #generics ),* >
            }
        };
        toks.extend(quote! {
            #name #generics
        });
    }
}

impl decl::Node {
    /// Generate the node declaration and implementation
    /// with the sanitized name for internal use only.
    /// This is not expected to be used by either other Lustre blocks
    /// or the interfacing Rust code.
    #[expect(
        clippy::redundant_closure_for_method_calls,
        reason = "Required for type inference"
    )] // FIXME: make it a trait to fix it ?
    fn internal_decl(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
            deptys,
            options,
            registers,
            flips,
        } = self;
        let inputs = inputs.as_ref();
        let outputs = outputs.as_ref();
        let locals = locals.as_ref();

        let deptys = deptys.iter().map(|v| v.as_ident(SanitizedIdent, None));

        let uid_name = name.as_ident(SanitizedIdent, None);

        let pos_inputs_decl = inputs.strictly_positive();
        let pos_outputs_decl = outputs.strictly_positive();
        let pos_locals_decl = locals.strictly_positive();

        let pos_inputs_use = inputs.strictly_positive_sanitized_names();
        let pos_outputs_use = outputs.strictly_positive_sanitized_names();
        let pos_locals_use = locals.strictly_positive_sanitized_names();
        let pos_inputs_default = inputs.strictly_positive_sanitized_names();
        let pos_outputs_default = outputs.strictly_positive_sanitized_names();
        let pos_locals_default = locals.strictly_positive_sanitized_names();

        //let inputs_ty_3 = inputs.as_defined_tys();
        let expected_input_tys_impl = inputs.as_embedded_tys();
        let expected_output_tys_impl = outputs.as_embedded_tys();

        let inputs_vs_asst = inputs.as_assignment_target();

        //let outputs_ty_3 = outputs.as_defined_tys();
        let outputs_vs_2 = outputs.as_values();

        let pub_qualifier = options.pub_qualifier(/*trait*/ false);

        let (trace_pre, trace_post) = options.traces("      ", name, inputs, locals, outputs);
        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();
        let (generics, phantom, bounds) = options.generic_params();

        let cfg_test = if self.options.test.fetch::<This>().is_some() {
            quote!( #[cfg(test)] )
        } else {
            quote!()
        };

        let doc_name = format!(" `{name}` ");
        let declaration = quote_spanned! {name.span.unwrap()=>
            #[doc(hidden)] // Inner declaration only.
            #[allow(non_camel_case_types)] // Lustre naming conventions.
            #[allow(non_snake_case)] // Lustre naming conventions.
            #[allow(dead_code)] // Trigger only for impl step.
            #( #rustc_allow_1 )* // User-provided.
            #pub_qualifier struct #uid_name #generics {
                __clock: usize,
                __phantom: #phantom,
                #[doc = " Strictly positive variables of"]
                #[doc = #doc_name]
                #( #pos_inputs_decl , )*
                #( #pos_outputs_decl , )*
                #( #pos_locals_decl , )*
                #[doc = " Subnodes of"]
                #[doc = #doc_name]
                __nodes: ( #( #blocks , )* ),
                __flips: ( #( #flips , )* ),
                __regs: ( #( #registers , )* ),
            }
        };

        let implementation = quote_spanned! {name.span.unwrap()=>
            #[allow(clippy::derivable_impls)] // This is, in fact, not always derivable.
            impl #generics Default for #uid_name #generics #bounds {
                fn default() -> Self {
                    Self {
                        __clock: Default::default(),
                        __phantom: Default::default(),
                        #( #pos_inputs_default: Default::default() , )*
                        #( #pos_outputs_default: Default::default() , )*
                        #( #pos_locals_default: Default::default() , )*
                        __nodes: Default::default(),
                        __flips: Default::default(),
                        __regs: Default::default(),
                    }
                }
            }

            #[allow(non_snake_case)] // Lustre naming conventions.
            #[allow(unused_imports)] // Using sem::traits::* just in case.
            #[allow(clippy::no_effect)] // We are inserting "comments" as strings.
            // Completely nonsensical suggestions by Clippy.
            #[allow(clippy::unreadable_literal, clippy::zero_prefixed_literal)]
            #( #rustc_allow_2 )* // User-provided.
            #cfg_test
            impl #generics #uid_name #generics #bounds {
                fn step(
                    &mut self,
                    inputs: #expected_input_tys_impl,
                ) ->
                    #expected_output_tys_impl
                {
                    use ::chandeliers_sem::traits::{Embed, Step};
                    ::chandeliers_sem::implicit_clock!(inputs);
                    "Implicit clock is running";
                    let #inputs_vs_asst = inputs;
                    #trace_pre
                    "Begin body of the node";
                    #( #stmts )*
                    "Print the trace after all computations but before tick";
                    #trace_post
                    "End body of the node";
                    ::chandeliers_sem::tick!(self);
                    #( ::chandeliers_sem::update!(self, #pos_inputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_outputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_locals_use ); )*
                    "Ghost reads to tell the Rustc dead code analysis about dependent types";
                    #( let _ = #deptys; )*
                    "Finish by returning the outputs";
                    #outputs_vs_2.embed()
                }
            }
        };

        quote! {
            #declaration
            #implementation
        }
    }

    /// Generate the publicly visible wraper and its implementation.
    fn external_decl(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            options,
            ..
        } = self;
        let inputs = inputs.as_ref();
        let outputs = outputs.as_ref();

        let ext_name = name.as_ident(RawIdent, None);
        let uid_name = name.as_ident(SanitizedIdent, None);

        let expected_input_tys_decl = inputs.as_embedded_tys();
        let expected_output_tys_decl = outputs.as_embedded_tys();

        let docs = options.docs();

        let must_impl_trait = *options.impl_trait.fetch::<This>();
        let (impl_trait, trait_input, trait_output) = if must_impl_trait {
            let inputs = inputs.as_defined_tys();
            let outputs = outputs.as_defined_tys();
            (
                quote!( ::chandeliers_sem::stepping::Step for ),
                quote!( type Input = #inputs ; ),
                quote!( type Output = #outputs ; ),
            )
        } else {
            (quote!(), quote!(), quote!())
        };

        let pub_qualifier = options.pub_qualifier(must_impl_trait);
        if !options.export.fetch::<This>() {
            return quote!();
        }

        let pretty_io = format!("`{inputs} -> {outputs}`");

        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();
        let (generics, _phantom, bounds) = options.generic_params();

        let doc_name = format!(" `{name}` ");
        let ext_declaration = quote_spanned! {name.span.unwrap()=>
            #pub_qualifier struct #ext_name #generics { inner: #uid_name #generics }
        };

        let ext_annotated_declaration = quote_spanned! {name.span.unwrap()=>
            #docs
            #[doc = "\n"]
            #[allow(non_camel_case_types)] // Lustre naming conventions.
            #[allow(dead_code)] // Only trigger on impl.
            #( #rustc_allow_1 )* // User-provided.
            #ext_declaration
        };

        let ext_step_impl = quote_spanned! {name.span.unwrap()=>
            #[allow(clippy::derivable_impls)] // Not derivable when there are generics
            impl #generics Default for #ext_name #generics #bounds {
                fn default() -> Self {
                    Self {
                        inner: Default::default(),
                    }
                }
            }

            #( #rustc_allow_2 )* // User-provided.
            impl #generics #impl_trait #ext_name #generics #bounds {
                #trait_input
                #trait_output
                #[doc = #doc_name]
                #[doc = "is a Lustre node with signature"]
                #[doc = #pretty_io]
                #[allow(unused_imports)]
                #pub_qualifier fn step(
                    &mut self,
                    inputs: #expected_input_tys_decl,
                ) ->
                    #expected_output_tys_decl
                {
                    ::chandeliers_sem::implicit_clock!(inputs);
                    self.inner.step(inputs)
                }
            }
        };

        quote! {
            #ext_annotated_declaration
            #ext_step_impl
        }
    }
}

impl ToTokens for decl::FlipInstance {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(quote! {
            ::chandeliers_sem::registers::Flip
        });
    }
}

impl ToTokens for decl::RegisterInstance {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { id: _, typ } = self;
        let typ = typ.as_ref().unwrap().as_ref().as_defined_ty();
        toks.extend(quote! {
            ::chandeliers_sem::registers::Register<
                <#typ as ::chandeliers_sem::traits::Embed>::Target
            >
        })
    }
}

impl Sp<&ty::Tuple> {
    fn as_defined_ty(self) -> TokenStream {
        match self.t {
            ty::Tuple::Single(t) => {
                let ty = t.as_ref().as_ty(DefinedTy, None);
                quote_spanned! {self.span.unwrap()=> #ty }
            }
            ty::Tuple::Multiple(tup) => {
                let tys = tup.t.iter().map(|it| it.as_ref().as_defined_ty());
                quote_spanned! {self.span.unwrap()=>
                    ( #( #tys ,)* )
                }
            }
        }
    }
}

impl Sp<&Tuple<Sp<decl::TyVar>>> {
    /// Get the type tuple pre-embedding (no `Nillable`s).
    fn as_defined_tys(&self) -> TokenStream {
        let mut tup = self
            .t
            .iter()
            .map(|sv| sv.base_type_of().as_ref().as_ty(DefinedTy, None));
        if self.t.len() == 1 {
            tup.next().unwrap_or_else(|| chandeliers_err::malformed!())
        } else {
            quote_spanned! {self.span.unwrap()=>
                ( #( #tup ),* )
            }
        }
    }

    /// Get the type tuple post-embedding (with `Nillable` everywhere).
    fn as_embedded_tys(&self) -> TokenStream {
        let tys = self.as_defined_tys();
        quote_spanned! {self.span.unwrap()=>
            <#tys as ::chandeliers_sem::traits::Embed>::Target
        }
    }

    /// Produce the corresponding destructuring tuple.
    fn as_values(&self) -> TokenStream {
        let mut tup = self
            .t
            .iter()
            .map(|sv| sv.name_of().as_ident(SanitizedIdent, None));
        if self.t.len() == 1 {
            let first = tup.next().unwrap_or_else(|| chandeliers_err::malformed!());
            quote_spanned! {self.span.unwrap()=>
                #first
            }
        } else {
            quote_spanned! {self.span.unwrap()=>
                ( #( #tup ),* )
            }
        }
    }

    /// Simply a comma-separated flat tuple.
    fn self_assigned(&self) -> TokenStream {
        let tup = self.t.iter().map(|sv| {
            let san = sv.name_of().as_ident(SanitizedIdent, None);
            let raw = sv.name_of().as_ident(RawIdent, None);
            quote!( let #raw = #san; )
        });
        quote_spanned! {self.span.unwrap()=>
            #( #tup )*
        }
    }

    /// Generate a formatting string for this tuple, in the form
    /// "(a={}, b={}, c={})"
    fn fmt_strings(&self) -> String {
        let mut accum = Vec::new();
        for v in self.t.iter() {
            accum.push(format!("{n}={{{n}}}", n = v.name_of()));
        }
        format!("({})", accum.join(", "))
    }

    /// For an assignment, we turn `()` into `_`
    /// and the rest to a normal destructuring tuple.
    fn as_assignment_target(&self) -> TokenStream {
        if self.t.is_empty() {
            quote_spanned!(self.span.unwrap()=> _)
        } else {
            self.as_values()
        }
    }

    /// Iterate over all strictly positive variables of the tuple.
    /// This gives the list of all the variables that need to be stored
    /// in the struct.
    fn strictly_positive(&self) -> impl Iterator<Item = &Sp<decl::TyVar>> {
        self.t.iter().filter(|v| v.t.strictly_positive())
    }

    /// Get the sanitized identifiers for a tuple of typed variables.
    fn strictly_positive_sanitized_names(&self) -> impl Iterator<Item = TokenStream> + '_ {
        self.strictly_positive()
            .map(|v| v.name_of().as_ident(SanitizedIdent, None))
    }
}

/// Extern node declaration: assert that it implements the right trait.
///
/// We use similar techniques to the ones in `ExtConst` to block the compiler's
/// ability to make nonsensical suggestions and restrict how far ago it tries
/// to find the fault.
///
/// The code we emit here is very ugly, but it's also completely dead code.
/// It doesn't need to be good, it just needs to trigger precisely the right error
/// messages.
impl ToTokens for decl::ExtNode {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self {
            name,
            inputs,
            outputs,
            options,
        } = self;
        let inputs = inputs.as_ref();
        let outputs = outputs.as_ref();
        let locals = Tuple::default().with_span(inputs.span);

        let ext_name = name.as_ident(RawIdent, None);
        let uid_name = name.as_ident(SanitizedIdent, None);

        let expected_output_tys = outputs.as_embedded_tys();
        let expected_input_tys = inputs.as_embedded_tys();
        let actual_inputs = quote_spanned! {inputs.span.unwrap()=> inputs };
        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();

        let input_asst = inputs.as_assignment_target();
        let output_asst = outputs.as_assignment_target();
        let output_values = outputs.with_span(name.span).as_values();

        let (trace_pre, trace_post) =
            options.traces("[ext] ", name, inputs, locals.as_ref(), outputs);

        let (generics, _phantom, bounds) = options.generic_params();

        toks.extend(quote_spanned! {name.span.unwrap()=>
            #[doc(hidden)] // Internal only.
            #[allow(non_camel_case_types)] // Lustre naming conventions.
            #[allow(dead_code)] // Only trigger on impl step.
            #( #rustc_allow_1 )* // User-provided.
            struct #uid_name #generics #bounds {
                inner: #ext_name #generics,
                __clock: usize,
            }

            #[allow(clippy::derivable_impls)] // Not derivable when there are generics
            impl #generics Default for #uid_name #generics #bounds {
                fn default() -> Self {
                    Self {
                        inner: Default::default(),
                        __clock: Default::default(),
                    }
                }
            }

            #[allow(clippy::let_and_return)] // In case there is no trace.
            #( #rustc_allow_2 )* // User-provided.
            impl #generics #uid_name #generics #bounds {
                #[allow(unused_imports)] // Using sem::traits::* just in case.
                #[allow(non_snake_case)] // Lustre naming conventions.
                #[allow(unused_variables)] // Unpacked but not used.
                pub fn step(
                    &mut self,
                    #actual_inputs: #expected_input_tys,
                ) -> #expected_output_tys
                {
                    use ::chandeliers_sem::traits::{Embed, Step};
                    ::chandeliers_sem::implicit_clock!(inputs);

                    let #input_asst = inputs;
                    #trace_pre
                    let #output_asst = self.inner.step(#actual_inputs);
                    #trace_post

                    ::chandeliers_sem::tick!(self);
                    #output_values.embed()
                }
            }
        });

        toks.extend(options.fn_main(&self.name, self.options.rustc_allow.fetch::<This>()));
        options.assert_used();
    }
}

impl ToTokens for op::Bin {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Add => quote!( + ),
            Self::Mul => quote!( * ),
            Self::Sub => quote!( - ),
            Self::Div => quote!( / ),
            Self::Rem => quote!( % ),
            Self::BitAnd => quote!( & ),
            Self::BitOr => quote!( | ),
            Self::BitXor => quote!( ^ ),
        });
    }
}

impl ToTokens for op::Un {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Not => quote!(!),
            Self::Neg => quote!( - ),
        });
    }
}

impl ToTokens for op::Cmp {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Eq => quote!( == ),
            Self::Ne => quote!( != ),
            Self::Ge => quote!( >= ),
            Self::Gt => quote!( > ),
            Self::Le => quote!( <= ),
            Self::Lt => quote!( < ),
        });
    }
}

/// Part by convention and part by necessity,
/// we display local variables as
///    `_local_v`
/// and globals as
///    `lit!(X)`
///
/// This minimizes name collisions and provides the correct typing.
impl ToTokens for var::Reference {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Var(v) => {
                toks.extend(quote!( #v ));
            }
            Self::Global(v) => {
                let g = v.as_ident(SanitizedIdent, None);
                toks.extend(quote! {
                    ::chandeliers_sem::lit!(#g)
                });
            }
        }
    }
}

/// Candle specifies that variables in the past can be invoqued through the
/// notation `var!(self <~ dt; v)`.
impl ToTokens for var::Past {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { var, depth } = self;
        let var = var.as_ident(SanitizedIdent, None);
        let depth = depth.as_lit();
        toks.extend(quote! {
            ::chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

impl ToTokens for decl::TyVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        let name = name.as_ident(SanitizedIdent, None);
        toks.extend(quote! {
            #name : ::chandeliers_sem::ty!(#ty)
        });
    }
}

crate::sp::transparent_impl!(fn base_type_of return ty::Base where decl::TyVar);
crate::sp::transparent_impl!(fn name_of return var::Local where decl::TyVar);
impl decl::TyVar {
    /// A `TyVar` is a pair of a name and a type. This extracts the type.
    fn base_type_of(&self, _: Span) -> ty::Base {
        self.ty.t.ty.t.inner.t.clone()
    }

    /// A `TyVar` is a pair of a name and a type. This extracts the name.
    fn name_of(&self, _: Span) -> var::Local {
        self.name.t.clone()
    }

    /// The depth of any variable is guaranteed to be at least zero, but is
    /// it strictly more ? This determines for which variables we actually
    /// need to store data.
    fn strictly_positive(&self) -> bool {
        self.ty.t.depth.t.dt > 0
    }
}

/// Print the type of a stream without the temporal information.
/// This is the type that it has in function arguments and return values.
impl ToTokens for ty::Stream {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let ty = self.ty.t.inner.as_ref().as_lustre_ty();
        let mut pluses = Vec::new();
        for _ in 0..self.depth.t.dt {
            pluses.push(quote!( + ));
        }
        toks.extend(quote! {
            #ty #(#pluses)*
        });
    }
}

crate::sp::transparent_impl!(fn as_lustre_ty return TokenStream where &ty::Base);
impl ty::Base {
    /// Pass a type through `ty_mapping` to get `i64`/`f64`/`bool`.

    /// Print a type as the `ty_mapping` macro would expect it: `int`/`float`/`bool`.
    fn as_lustre_ty(&self, span: Span) -> TokenStream {
        let id = Ident::new(&format!("{self}"), span.unwrap());
        quote!( #id )
    }
}

impl ToTokens for stmt::Statement {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Let { source, target } => {
                let pretty = format!("Variable assignment: {target} := {source};");
                let target = target.as_assignment_target();
                toks.extend(quote! {
                    #pretty;
                    let #target = #source;
                });
            }
            Self::Assert(e) => {
                let pretty = format!("Assertion: {e}");
                let s = format!("{e}");
                toks.extend(quote! {
                    #pretty;
                    ::chandeliers_sem::truth!(#e, #s);
                });
            }
            Self::UpdateRegister { id, val } => toks.extend(quote! {
                self.__regs.#id.try_set(#val);
            }),
            Self::InitRegister { id, val: Some(val) } => toks.extend(quote! {
                self.__regs.#id.try_initialize(#val);
            }),
            Self::InitRegister { id: _, val: None } => {}
        }
    }
}

crate::sp::transparent_impl!(fn as_assignment_target return TokenStream where stmt::VarTuple);
#[expect(clippy::multiple_inherent_impl, reason = "This impl is local")]
impl stmt::VarTuple {
    /// An assignment tuple.
    ///
    /// We need a case analysis on the size of the tuple, where
    /// `_` is needed to bind an empty return `()`, and otherwise we need
    /// exactly one or several variable names to bind one scalar per variable.
    #[expect(
        clippy::redundant_closure_for_method_calls,
        reason = "Required for type inference"
    )] // FIXME: make it a trait
    fn as_assignment_target(&self, _span: Span) -> TokenStream {
        match self {
            Self::Single(s) => s.as_ident(SanitizedIdent, None),
            Self::Multiple(m) if m.t.is_empty() => {
                quote!(_)
            }
            Self::Multiple(m) => {
                let m = m.t.iter().map(|x| x.as_assignment_target());
                quote! {
                    ( #( #m ),* )
                }
            }
        }
    }
}

/// An expr in its normal (non-const) context is mostly straightforward,
/// apart from Candle's quirky syntaxes for
/// - later `later!(self <~ dt; a, b)`,
/// - if `ifx!((b) then { y } else { n })`, and
/// - function calls `substep!(self; id => {args})`.
impl ToTokens for expr::Expr {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Lit(l) => {
                let l = l.const_expr_tokens();
                quote!(::chandeliers_sem::lit!(#l))
            }
            Self::Reference(refer) => {
                quote!( #refer )
            }
            Self::DummyPre(e) => quote!( #e ),
            Self::DummyParen(e) => quote!( #e ),
            Self::Tuple(t) => {
                let elems = t.t.iter();
                quote!( ( #( #elems ),* ).embed() )
            }
            Self::Bin { op, lhs, rhs } => {
                quote!(::chandeliers_sem::binop!(#op; #lhs, #rhs))
            }
            Self::Un { op, inner } => {
                quote!(::chandeliers_sem::unop!(#op; #inner))
            }
            Self::Cmp { op, lhs, rhs } => {
                quote!(::chandeliers_sem::cmp!(#op; #lhs, #rhs))
            }
            Self::Later {
                delay,
                before,
                after,
            } => {
                let clk = delay.as_lit();
                quote!(::chandeliers_sem::later!(self <~ #clk; #before, #after))
            }
            Self::Ifx { cond, yes, no } => {
                quote!(::chandeliers_sem::ifx!((#cond) then { #yes } else { #no }))
            }
            Self::Substep { delay, id, args } => {
                let id_lit = syn::LitInt::new(&format!("{}", id.t.id), id.span.unwrap());
                quote! {
                    ::chandeliers_sem::substep!(
                        self <~ #delay;
                        #id_lit => {
                            #args
                        }
                    )
                }
            }
            Self::Merge { switch, on, off } => {
                quote!(::chandeliers_sem::merge!(#switch; #on, #off))
            }
            Self::Clock {
                op,
                inner,
                activate,
            } => {
                // `#op` expands to `when` or `whenot` which are Candle macros.
                quote!(::chandeliers_sem::#op!(#activate; #inner))
            }
            Self::FetchRegister { id, .. } => {
                quote! { self.__regs.#id.get() }
            }
            Self::Flip {
                id,
                initial,
                continued,
            } => {
                quote! {
                    {
                        let initial = #initial;
                        let continued = #continued;
                        use ::chandeliers_sem::nillable::FirstIsNil;
                        if initial.first_is_nil() {
                            // off-clock
                            ::chandeliers_sem::traits::AllNil::auto_size()
                        } else {
                            if self.__flips.#id.tas() {
                                // first on-clock iteration
                                initial
                            } else {
                                // following on-clock iterations
                                continued
                            }
                        }
                    }
                }
            }
        });
    }
}

impl ToTokens for var::Flip {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { id } = self;
        let id = syn::Index::from(id.t);
        toks.extend(quote! { #id })
    }
}

impl ToTokens for var::Register {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { id } = self;
        let id = syn::Index::from(id.t);
        toks.extend(quote! { #id })
    }
}

impl ToTokens for op::Clock {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::When => quote!(when),
            Self::Whenot => quote!(whenot),
        });
    }
}

crate::sp::transparent_impl!(fn as_lit return TokenStream where past::Depth);
impl past::Depth {
    /// Generate a literal token representing the depth.
    fn as_lit(self, span: Span) -> TokenStream {
        let lit = syn::Lit::Int(syn::LitInt::new(&format!("{}", self.dt), span.unwrap()));
        quote!( #lit )
    }
}
