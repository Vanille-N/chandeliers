//! Generate actual Candle statements from an AST.

use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use crate::ast::options::usage::Codegen as This;
use crate::ast::{decl, expr, op, past, stmt, ty, var, Tuple};
use crate::sp::{Sp, Span};

mod constexpr;
pub mod options;

use constexpr::ConstExprSpanTokens;
use options::{Docs, FnMain, PubQualifier, Traces};

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
        let ext_name = name.as_raw_ident();
        let glob = name.as_sanitized_ident();
        let value = value.const_expr_tokens();
        let ty = ty.as_defined_ty();
        let pub_qualifier = options.pub_qualifier();
        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();

        let docs = options.docs();
        let declaration = quote_spanned! {span.into()=>
            #pub_qualifier const #ext_name : #ty = #glob ;
        };

        let doc = format!("Toplevel constant `{name}`");
        toks.extend(quote! {
            #[doc = #doc]
            #[doc = "(Declared with UID for internal usage)"]
            #[allow(non_upper_case_globals)]
            #( #[allow( #rustc_allow_2 )] )*
            const #glob : #ty = #value ;

            #docs
            #[doc = "\n"]
            #[doc = #doc]
            #[doc = "(Visible by the outside)"]
            #[allow(non_upper_case_globals)]
            #( #[allow( #rustc_allow_1 )] )*
            #declaration
        });

        options.assert_used();
    }
}

/// Extern global constant: assert existence and typing.
///
/// There is not really a value to generate, we simply create a dummy const
/// to assert that the one declared was found. While we're at it,
/// we create an alias of `G` into `__global_G` to avoid further collisions.
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
        let ext_name = name.as_raw_ident();
        let expected = ty.as_defined_ty();
        let glob = name.as_sanitized_ident();
        let real = quote_spanned!(name.span.into()=> real );
        let expected_wrapped = quote_spanned!(ty.span.into()=> Type<#expected> );
        let rustc_allow = options.rustc_allow.fetch::<This>().iter();
        let doc = format!("Reimport of a toplevel constant; assumes that `{name}` is provided");
        toks.extend(quote! {
            #[doc = #doc]
            #[allow(non_upper_case_globals)]
            #( #[allow( #rustc_allow )] )*
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
        toks.extend(self.options.fn_main(&self.name));
        self.options.assert_used();
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
    )] // FIXME: make it a trait
    fn internal_decl(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
            options,
        } = self;
        let inputs = inputs.as_ref();
        let outputs = outputs.as_ref();
        let locals = locals.as_ref();

        let uid_name = name.as_sanitized_ident();

        let pos_inputs_decl = inputs.strictly_positive();
        let pos_outputs_decl = outputs.strictly_positive();
        let pos_locals_decl = locals.strictly_positive();
        let blocks = blocks.iter().map(|n| n.as_sanitized_ident());

        let pos_inputs_use = inputs
            .strictly_positive()
            .map(|v| v.name_of().as_sanitized_ident());
        let pos_outputs_use = outputs
            .strictly_positive()
            .map(|v| v.name_of().as_sanitized_ident());
        let pos_locals_use = locals
            .strictly_positive()
            .map(|v| v.name_of().as_sanitized_ident());
        let inputs_ty_3 = inputs.as_defined_tys();
        let expected_input_tys_impl = inputs.as_embedded_tys();
        let expected_output_tys_impl = outputs.as_embedded_tys();

        let inputs_vs_asst = inputs.as_assignment_target();

        let outputs_ty_3 = outputs.as_defined_tys();
        let outputs_vs_2 = outputs.as_values();

        let pub_qualifier = options.pub_qualifier();

        let (trace_pre, trace_post) = options.traces("      ", name, inputs, outputs);
        let rustc_allow_1 = options.rustc_allow.fetch::<This>().iter();
        let rustc_allow_2 = options.rustc_allow.fetch::<This>().iter();

        let doc_name = format!(" `{name}` ");
        let declaration = quote! {
            #[doc = " Node definition"]
            #[doc = #doc_name]
            #[doc = "(for internal use)"]
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #( #[allow( #rustc_allow_1 )] )*
            #pub_qualifier struct #uid_name {
                __clock: usize,
                #[doc = " Strictly positive variables of"]
                #[doc = #doc_name]
                #( #pos_inputs_decl , )*
                #( #pos_outputs_decl , )*
                #( #pos_locals_decl , )*
                #[doc = " Subnodes of"]
                #[doc = #doc_name]
                __nodes: ( #( #blocks , )* ),
            }
        };

        let implementation = quote! {
            #[doc = " Implementation of Step for"]
            #[doc = #doc_name]
            #[allow(non_snake_case)]
            #( #[allow( #rustc_allow_2 )] )*
            #[allow(clippy::unused_unit)]
            impl ::chandeliers_sem::traits::Step for #uid_name {
                type Input = #inputs_ty_3;
                type Output = #outputs_ty_3;
                fn step(
                    &mut self,
                    __inputs: #expected_input_tys_impl,
                ) ->
                    #expected_output_tys_impl
                {
                    use ::chandeliers_sem::traits::{Embed, Step};
                    ::chandeliers_sem::implicit_clock!(__inputs);
                    "Implicit clock is running";
                    let #inputs_vs_asst = __inputs;
                    #trace_pre
                    "Begin body of the node";
                    #( #stmts ; )*
                    "End body of the node";
                    ::chandeliers_sem::tick!(self);
                    #( ::chandeliers_sem::update!(self, #pos_inputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_outputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_locals_use ); )*
                    "Finish by returning the outputs";
                    #trace_post
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

        let ext_name = name.as_raw_ident();
        let uid_name = name.as_sanitized_ident();

        let inputs_ty_4 = inputs.as_defined_tys();
        let expected_input_tys_decl = inputs.as_embedded_tys();
        let expected_output_tys_decl = outputs.as_embedded_tys();

        let outputs_ty_4 = outputs.as_defined_tys();

        let pub_qualifier = options.pub_qualifier();

        let rustc_allow = options.rustc_allow.fetch::<This>().iter();

        let docs = options.docs();
        let doc_name = format!(" `{name}` ");
        let ext_declaration = quote_spanned! {name.span.into()=>
            #pub_qualifier struct #ext_name { inner: #uid_name }
        };

        let ext_annotated_declaration = quote_spanned! {name.span.into()=>
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(dead_code)]
            #( #[allow( #rustc_allow )] )*
            #ext_declaration
        };

        let ext_step_impl = quote_spanned! {name.span.into()=>
            #[allow(clippy::unused_unit)]
            impl ::chandeliers_sem::traits::Step for #ext_name {
                type Input = #inputs_ty_4;
                type Output = #outputs_ty_4;
                #[allow(unused_imports)]
                fn step(
                    &mut self,
                    __inputs: #expected_input_tys_decl,
                ) ->
                    #expected_output_tys_decl
                {
                    ::chandeliers_sem::implicit_clock!(__inputs);
                    self.inner.step(__inputs)
                }
            }
        };

        quote! {
            #docs
            #[doc = "\n"]
            #[doc = " Wrapper declaration of"]
            #[doc = #doc_name]
            #[doc = " (part of the public interface)"]
            #ext_annotated_declaration

            #[doc = " Implementation of Step for"]
            #[doc = #doc_name]
            #[doc = " (immediately defers to Step for inner)"]
            #ext_step_impl
        }
    }
}

impl Sp<&Tuple<Sp<decl::TyVar>>> {
    /// Get the type tuple pre-embedding (no `Nillable`s).
    fn as_defined_tys(&self) -> TokenStream {
        let mut tup = self.t.iter().map(|sv| sv.base_type_of().as_defined_ty());
        if self.t.len() == 1 {
            tup.next()
                .unwrap_or_else(|| chandeliers_err::provably_unreachable!())
        } else {
            quote_spanned! {self.span.into()=>
                ( #( #tup ),* )
            }
        }
    }

    /// Get the type tuple post-embedding (with `Nillable` everywhere).
    fn as_embedded_tys(&self) -> TokenStream {
        let tys = self.as_defined_tys();
        quote_spanned! {self.span.into()=>
            <#tys as ::chandeliers_sem::traits::Embed>::Target
        }
    }

    /// Produce the corresponding destructuring tuple.
    fn as_values(&self) -> TokenStream {
        let mut tup = self.t.iter().map(|sv| sv.name_of().as_sanitized_ident());
        if self.t.len() == 1 {
            let first = tup
                .next()
                .unwrap_or_else(|| chandeliers_err::provably_unreachable!());
            quote_spanned! {self.span.into()=>
                #first
            }
        } else {
            quote_spanned! {self.span.into()=>
                ( #( #tup ),* )
            }
        }
    }

    /// Simply a comma-separated flat tuple.
    fn flattened_trailing_comma(&self) -> TokenStream {
        let tup = self.t.iter().map(|sv| sv.name_of().as_sanitized_ident());
        quote_spanned! {self.span.into()=>
            #( #tup, )*
        }
    }

    /// Generate a formatting string for this tuple, in the form
    /// "(a={}, b={}, c={})"
    fn fmt_strings(&self) -> String {
        let mut accum = Vec::new();
        for v in self.t.iter() {
            accum.push(format!("{}={{}}", v.name_of()));
        }
        format!("({})", accum.join(", "))
    }

    /// For an assignment, we turn `()` into `_`
    /// and the rest to a normal destructuring tuple.
    fn as_assignment_target(&self) -> TokenStream {
        if self.t.is_empty() {
            quote_spanned!(self.span.into()=> _)
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

        let ext_name = name.as_raw_ident();
        let uid_name = name.as_sanitized_ident();

        let expected_output_tys = outputs.as_embedded_tys();
        let expected_input_tys = inputs.as_embedded_tys();
        let actual_inputs = quote_spanned! {inputs.span.into()=> __inputs };
        let rustc_allow = options.rustc_allow.fetch::<This>().iter();

        let input_asst = inputs.as_assignment_target();
        let output_asst = outputs.as_assignment_target();
        let output_values = outputs.with_span(name.span).as_values();

        let (trace_pre, trace_post) = options.traces("[ext] ", name, inputs, outputs);

        toks.extend(quote_spanned! {name.span.into()=>
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #( #[allow( #rustc_allow )] )*
            struct #uid_name { inner: #ext_name }

            #[allow(dead_code)]
            #[allow(clippy::let_and_return)]
            // FIXME: impl Step
            impl #uid_name {
                #[allow(unused_imports)]
                pub fn step(
                    &mut self,
                    #actual_inputs: #expected_input_tys,
                ) -> #expected_output_tys
                {
                    use ::chandeliers_sem::traits::{Embed, Step};
                    ::chandeliers_sem::implicit_clock!(__inputs);

                    let #input_asst = __inputs;
                    #trace_pre

                    let #output_asst = self.inner.step(#actual_inputs);

                    #trace_post
                    #output_values.embed()
                }
            }
        });

        toks.extend(options.fn_main(&self.name));
        options.assert_used();
    }
}

crate::sp::transparent_impl!(fn as_sanitized_ident return TokenStream where decl::NodeName);
crate::sp::transparent_impl!(fn as_raw_ident return TokenStream where decl::NodeName);
impl decl::NodeName {
    /// Format as a name that cannot have collisions with other global
    /// variables, including those of the same name in other invocations of
    /// the macro.
    fn as_sanitized_ident(&self, _span: Span) -> TokenStream {
        let id = Ident::new(
            &format!("__{}__node_{}", self.run_uid, &self.repr.t),
            self.repr.span.into(),
        );
        quote!( #id )
    }

    /// Format the name without sanitization. Still needs to be raw
    /// because it could be a Rust keyword.
    fn as_raw_ident(&self, _span: Span) -> TokenStream {
        let id = Ident::new_raw(&self.repr.t.to_string(), self.repr.span.into());
        quote!( #id )
    }
}

crate::sp::transparent_impl!(fn as_sanitized_ident return TokenStream where var::Global);
crate::sp::transparent_impl!(fn as_raw_ident return TokenStream where var::Global);
impl var::Global {
    /// Format as a name that cannot have collisions with other global
    /// variables, including those of the same name in other invocations of
    /// the macro.
    fn as_sanitized_ident(&self, _span: Span) -> TokenStream {
        let id = Ident::new(
            &format!("__{}__global_{}", self.run_uid, &self.repr.t),
            self.repr.span.into(),
        );
        quote!( #id )
    }

    /// Format the name without sanitization. Still needs to be raw
    /// because it could be a Rust keyword.
    fn as_raw_ident(&self, _span: Span) -> TokenStream {
        let id = Ident::new_raw(&self.repr.t.to_string(), self.repr.span.into());
        quote!( #id )
    }
}

crate::sp::transparent_impl!(fn as_sanitized_ident return TokenStream where var::Local);
impl var::Local {
    /// Format as a name that cannot have collisions with existing global variables.
    fn as_sanitized_ident(&self, _span: Span) -> TokenStream {
        let id = Ident::new(&format!("__local_{}", &self.repr.t), self.repr.span.into());
        quote!( #id )
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
                let g = v.as_sanitized_ident();
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
        let var = var.as_sanitized_ident();
        let depth = depth.as_lit();
        toks.extend(quote! {
            ::chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

impl ToTokens for decl::TyVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        let name = name.as_sanitized_ident();
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
        self.ty.t.ty.t.inner.t
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
        let ty = self.ty.t.inner.as_lustre_ty();
        let mut pluses = Vec::new();
        for _ in 0..self.depth.t.dt {
            pluses.push(quote!( + ));
        }
        toks.extend(quote! {
            #ty #(#pluses)*
        });
    }
}

crate::sp::transparent_impl!(fn as_defined_ty return TokenStream where ty::Base);
crate::sp::transparent_impl!(fn as_lustre_ty return TokenStream where ty::Base);
impl ty::Base {
    /// Pass a type through `ty_mapping` to get `i64`/`f64`/`bool`.
    fn as_defined_ty(self, span: Span) -> TokenStream {
        let ty = self.as_lustre_ty(span);
        quote! {
            ::chandeliers_sem::ty_mapping!(#ty)
        }
    }

    /// Print a type as the `ty_mapping` macro would expect it: `int`/`float`/`bool`.
    fn as_lustre_ty(self, span: Span) -> TokenStream {
        let id = Ident::new(&format!("{self}"), span);
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
                    let #target = #source
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
        }
    }
}

crate::sp::transparent_impl!(fn as_assignment_target return TokenStream where stmt::VarTuple);
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
            Self::Single(s) => s.as_sanitized_ident(),
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
            Self::Later { clk, before, after } => {
                let clk = clk.as_lit();
                quote!(::chandeliers_sem::later!(self <~ #clk; #before, #after))
            }
            Self::Ifx { cond, yes, no } => {
                quote!(::chandeliers_sem::ifx!((#cond) then { #yes } else { #no }))
            }
            Self::Substep { clk, id, args } => {
                let id_lit = syn::LitInt::new(&format!("{}", id.t.id), id.span.into());
                quote! {
                    ::chandeliers_sem::substep!(
                        self <~ #clk;
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
        });
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
        let lit = syn::Lit::Int(syn::LitInt::new(&format!("{}", self.dt), span));
        quote!( #lit )
    }
}
