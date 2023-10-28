//! Generate actual Candle statements from an AST.

use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use super::ast::*;

trait ConstExprTokens {
    fn const_expr_tokens(&self) -> TokenStream;
}

impl<T: ConstExprTokens> ConstExprTokens for Sp<T> {
    fn const_expr_tokens(&self) -> TokenStream {
        let inner = self.t.const_expr_tokens();
        quote_spanned! {self.span=>
            #inner
        }
    }
}
impl<T: ConstExprTokens> ConstExprTokens for Box<T> {
    fn const_expr_tokens(&self) -> TokenStream {
        let inner = self.as_ref().const_expr_tokens();
        quote!( #inner )
    }
}

/// `Sp` is transparently printable, but gives its own span to the output.
impl<T: ToTokens> ToTokens for Sp<T> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { span, t } = &self;
        toks.extend(quote_spanned! {*span=>
            #t
        })
    }
}

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
        let ext_name = Ident::new_raw(&format!("{}", name), name.span);
        let value = value.const_expr_tokens();
        let pub_qualifier = if options.export {
            quote!(pub)
        } else {
            quote!()
        };

        let declaration = quote_spanned! {span=>
            #pub_qualifier const #ext_name : ::chandeliers_sem::ty_mapping!(#ty) = #name ;
        };
        let rustc_allow = options.rustc_allow.iter();

        toks.extend(quote! {
            #[allow(non_upper_case_globals)]
            const #name : ::chandeliers_sem::ty_mapping!(#ty) = #value ;

            #[allow(non_upper_case_globals)]
            #( #[allow( #rustc_allow )] )*
            #declaration
        });
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
        let ext_name = Ident::new_raw(&format!("{}", name), name.span);
        let real = quote_spanned!(name.span=> real );
        let expected = quote_spanned!(ty.span=> ::chandeliers_sem::ty_mapping!(#ty) );
        let expected_wrapped = quote_spanned!(ty.span=> Type<#expected> );
        let rustc_allow = options.rustc_allow.iter();
        toks.extend(quote! {
            #[allow(non_upper_case_globals)]
            #( #[allow( #rustc_allow )] )*
            const #name: #expected = {
                struct Type<T>(T);
                let #real = Type(#ext_name);
                let expected: #expected_wrapped = #real ;
                expected.0
            };
        });
    }
}

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
impl ToTokens for decl::Node {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
            options,
        } = self;

        let name_span = name.span;
        let ext_name = Ident::new_raw(&format!("{}", name), name.span);
        let name = name.as_ident();

        let pos_inputs_decl = inputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs_decl = outputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_locals_decl = locals.t.iter().filter(|v| v.t.strictly_positive());
        let blocks = blocks.iter().map(|n| n.as_ident()).collect::<Vec<_>>();

        let pos_inputs_use = inputs
            .t
            .iter()
            .filter(|v| v.t.strictly_positive())
            .map(|v| v.as_ref().map(|_, v| v.name_of()));
        let pos_outputs_use = outputs
            .t
            .iter()
            .filter(|v| v.t.strictly_positive())
            .map(|v| v.as_ref().map(|_, v| v.name_of()));
        let pos_locals_use = locals
            .t
            .iter()
            .filter(|v| v.t.strictly_positive())
            .map(|v| v.as_ref().map(|_, v| v.name_of()));
        let inputs_ty = || {
            inputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()))
        };
        let inputs_ty_1 = inputs_ty();
        let inputs_ty_2 = inputs_ty();
        let inputs_ty_3 = inputs_ty();
        let inputs_ty_4 = inputs_ty();

        let inputs_vs = || {
            inputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.name_of()))
        };
        let inputs_vs_1 = inputs_vs();
        let inputs_vs_2 = if inputs.t.is_empty() {
            quote!(_)
        } else {
            let vs = inputs_vs();
            quote!( ( #( #vs ),* ) )
        };

        let outputs_ty = || {
            outputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()))
        };
        let outputs_ty_1 = outputs_ty();
        let outputs_ty_2 = outputs_ty();
        let outputs_ty_3 = outputs_ty();
        let outputs_ty_4 = outputs_ty();
        let outputs_vs = || {
            outputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.name_of()))
        };
        let outputs_vs_1 = outputs_vs();
        let outputs_vs_2 = outputs_vs();

        let pub_qualifier = if options.export {
            quote!(pub)
        } else {
            quote!()
        };

        let trace_pre = if options.trace {
            quote! {
                println!("{:?} -> {}", (#(#inputs_vs_1),*), stringify!(#ext_name));
            }
        } else {
            quote!()
        };

        let trace_post = if options.trace {
            quote! {
                println!("{} -> {:?}", stringify!(#ext_name), (#(#outputs_vs_1),*));
            }
        } else {
            quote!()
        };
        let ext_declaration = quote_spanned! {name_span=>
            #pub_qualifier struct #ext_name { inner: #name }
        };
        let rustc_allow = options.rustc_allow.iter();

        let ext_annotated_declaration = quote_spanned! {name_span=>
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(dead_code)]
            #( #[allow( #rustc_allow )] )*
            #ext_declaration
        };

        let ext_step_impl = quote_spanned! {name_span=>
            #[allow(clippy::unused_unit)]
            impl ::chandeliers_sem::traits::Step for #ext_name {
                type Input = ( #( ::chandeliers_sem::ty_mapping!(#inputs_ty_4) ),* );
                type Output = ( #( ::chandeliers_sem::ty_mapping!(#outputs_ty_4) ),* );
                #[allow(unused_imports)]
                fn step(
                    &mut self,
                    __inputs: <( #( ::chandeliers_sem::ty_mapping!(#inputs_ty_2) ),* ) as ::chandeliers_sem::traits::Embed>::Target,
                ) ->
                    <( #( ::chandeliers_sem::ty_mapping!(#outputs_ty_2) ),* ) as ::chandeliers_sem::traits::Embed>::Target
                {
                    ::chandeliers_sem::implicit_clock!(__inputs);
                    self.inner.step(__inputs)
                }
            }
        };

        toks.extend(quote! {
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #pub_qualifier struct #name {
                __clock: usize,
                #( #pos_inputs_decl , )*
                #( #pos_outputs_decl , )*
                #( #pos_locals_decl , )*
                __nodes: ( #( #blocks , )* ),
            }

            #[allow(non_snake_case)]
            #[allow(clippy::unused_unit)]
            impl ::chandeliers_sem::traits::Step for #name {
                type Input = ( #( ::chandeliers_sem::ty_mapping!(#inputs_ty_3) ),* );
                type Output = ( #( ::chandeliers_sem::ty_mapping!(#outputs_ty_3) ),* );
                fn step(
                    &mut self,
                    __inputs: <( #( ::chandeliers_sem::ty_mapping!(#inputs_ty_1) ),* ) as ::chandeliers_sem::traits::Embed>::Target,
                ) ->
                    <( #( ::chandeliers_sem::ty_mapping!(#outputs_ty_1) ),* ) as ::chandeliers_sem::traits::Embed>::Target
                {
                    use ::chandeliers_sem::traits::*;
                    ::chandeliers_sem::implicit_clock!(__inputs);
                    let #inputs_vs_2 = __inputs;
                    #trace_pre
                    // Actual body
                    #( #stmts ; )*
                    // Finish by incrementing the clock and updating the streams.
                    ::chandeliers_sem::tick!(self);
                    #( ::chandeliers_sem::update!(self, #pos_inputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_outputs_use ); )*
                    #( ::chandeliers_sem::update!(self, #pos_locals_use ); )*
                    #trace_post
                    ( #( #outputs_vs_2 ),* ).embed()
                }
            }

            #ext_annotated_declaration
            #ext_step_impl
        });

        if let Some(nb_iter) = options.main {
            toks.extend(quote! {
                fn main() {
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
            });
        }
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

        let ext_name = Ident::new_raw(&format!("{}", name), name.span);

        let inputs_ty = inputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()));
        let outputs_ty = outputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()));

        let expected_outputs_ty = quote_spanned! {outputs.span=>
            <( #( ::chandeliers_sem::ty_mapping!(#outputs_ty) ),* ) as ::chandeliers_sem::traits::Embed>::Target
        };
        let expected_inputs_ty = quote_spanned! {inputs.span=>
            <( #( ::chandeliers_sem::ty_mapping!(#inputs_ty) ),* ) as ::chandeliers_sem::traits::Embed>::Target
        };
        let actual_inputs = quote_spanned! {inputs.span=> __inputs };
        let rustc_allow = options.rustc_allow.iter();

        let name = name.as_ident();
        let inputs_vs = || {
            inputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.name_of()))
        };
        let inputs_vs_1 = inputs_vs();
        let inputs_vs_2 = if inputs.t.is_empty() {
            quote!(_)
        } else {
            let vs = inputs_vs();
            quote!( ( #( #vs ),* ) )
        };
        let outputs_vs = || {
            outputs
                .t
                .iter()
                .map(|sv| sv.as_ref().map(|_, v| v.name_of()))
        };
        let outputs_vs_1 = outputs_vs();
        let outputs_vs_2 = if outputs.t.is_empty() {
            quote!(_)
        } else {
            let vs = outputs_vs();
            quote!( ( #( #vs ),* ) )
        };
        let outputs_vs_3 = outputs_vs();

        let trace_pre = if options.trace {
            quote! {
                println!("[ext] {:?} -> {}", (#(#inputs_vs_1),*), stringify!(#ext_name));
            }
        } else {
            quote!()
        };

        let trace_post = if options.trace {
            quote! {
                println!("[ext] {} -> {:?}", stringify!(#ext_name), (#(#outputs_vs_1),*));
            }
        } else {
            quote!()
        };

        toks.extend(quote_spanned! {name.span()=>
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #( #[allow( #rustc_allow )] )*
            struct #name { inner: #ext_name }

            #[allow(dead_code)]
            #[allow(unused_parens)]
            #[allow(clippy::let_and_return)]
            // FIXME: impl Step
            impl #name {
                #[allow(unused_imports)]
                pub fn step(
                    &mut self,
                    #actual_inputs: #expected_inputs_ty,
                ) -> #expected_outputs_ty
                {
                    ::chandeliers_sem::implicit_clock!(__inputs);
                    let #inputs_vs_2 = __inputs;
                    #trace_pre

                    use ::chandeliers_sem::traits::*;
                    let #outputs_vs_2 = self.inner.step(#actual_inputs);

                    #trace_post
                    ( #( #outputs_vs_3 ),* ).embed()
                }
            }
        });

        if let Some(nb_iter) = options.main {
            toks.extend(quote! {
                fn main() {
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
            });
        }
    }
}

impl Sp<decl::NodeName> {
    fn as_ident(&self) -> Ident {
        Ident::new(
            &format!("__{}__node_{}", self.t.run_uid, &self.t.repr.t),
            self.t.repr.span,
        )
    }
}

impl decl::TyVar {
    fn strictly_positive(&self) -> bool {
        self.ty.t.depth.dt > 0
    }
}

impl ToTokens for Sp<expr::GlobalVar> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(
            &format!("__{}__global_{}", self.t.run_uid, &self.t.repr.t),
            self.t.repr.span,
        );
        toks.extend(quote!( #id ));
    }
}

impl ToTokens for Sp<expr::LocalVar> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("__local_{}", &self.t.repr.t), self.t.repr.span);
        toks.extend(quote!( #id ));
    }
}

/// Expr is one of the few nontrivial implementations in this file,
/// but at its core it's mostly just projecting to fields.
///
/// We do however need separate implementations for const and non-const
/// contexts because the basic types are different (`i64` vs `Nillable<i64>`).
///
/// In this method we are heavily taking advantage of the fact that rust has
/// rich const definitions and all binari/unary/comparisons/conditionals
/// that we are going to use here are valid in Rust const contexts.
impl ConstExprTokens for expr::Expr {
    fn const_expr_tokens(&self) -> TokenStream {
        match self {
            Self::Lit(l) => {
                let l = l.const_lit();
                quote!( #l )
            }
            Self::Reference(refer) => {
                let refer = refer.const_expr_tokens();
                quote!( #refer )
            }
            Self::BinOp { op, lhs, rhs } => {
                let lhs = lhs.const_expr_tokens();
                let rhs = rhs.const_expr_tokens();
                quote!( (#lhs #op #rhs) )
            }
            Self::UnOp { op, inner } => {
                let inner = inner.const_expr_tokens();
                quote!( (#op #inner) )
            }
            Self::CmpOp { op, lhs, rhs } => {
                let lhs = lhs.const_expr_tokens();
                let rhs = rhs.const_expr_tokens();
                quote!( (#lhs #op #rhs) )
            }
            Self::Tuple(t) => {
                let ts =
                    t.t.iter()
                        .map(|e| e.const_expr_tokens())
                        .collect::<Vec<_>>();
                quote!( ( #( #ts ),* ) )
            }
            Self::Later { .. } => unreachable!("Later is not valid in const contexts"),
            Self::Substep { .. } => unreachable!("Substep is not valid in const contexts"),
            Self::Ifx { cond, yes, no } => {
                let cond = cond.const_expr_tokens();
                let yes = yes.const_expr_tokens();
                let no = no.const_expr_tokens();
                quote! {
                    if #cond { #yes } else { #no }
                }
            }
            Self::Merge { .. } => unreachable!("Merge is not valid in const contexts"),
            Self::ClockOp { .. } => unreachable!("ClockOp is not valid in const contexts"),
        }
    }
}

impl ToTokens for expr::BinOp {
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

impl ToTokens for expr::UnOp {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Not => quote!(!),
            Self::Neg => quote!( - ),
        });
    }
}

impl ToTokens for expr::CmpOp {
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

/// It feels wrong to go back-and-forth between `i64`/`f64`/`bool` and
/// the same as `LitInt`/`LitFloat`/`LitBool`.
/// Maybe future implementations will change the internals of `expr::Lit`
/// to not perform parsing to base10.
impl Sp<expr::Lit> {
    fn const_lit(&self) -> syn::Lit {
        let mut lit = match self.t {
            expr::Lit::Int(i) => syn::Lit::Int(syn::LitInt::new(&format!("{}i64", i), self.span)),
            expr::Lit::Float(f) => {
                syn::Lit::Float(syn::LitFloat::new(&format!("{}f64", f), self.span))
            }
            expr::Lit::Bool(b) => syn::Lit::Bool(syn::LitBool::new(b, self.span)),
        };
        lit.set_span(self.span);
        lit
    }
}

/// Part by convention and part by necessity,
/// we display local variables as
///     _local_v
/// and globals as
///    lit!(X)
///
/// This minimizes name collisions and provides the correct typing.
impl ToTokens for expr::Reference {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Var(v) => {
                toks.extend(quote!( #v ));
            }
            Self::Global(v) => {
                toks.extend(quote! {
                    ::chandeliers_sem::lit!(#v)
                });
            }
        }
    }
}

impl ConstExprTokens for expr::Reference {
    fn const_expr_tokens(&self) -> TokenStream {
        match self {
            Self::Var(_) => unreachable!("Var is invalid in const contexts"),
            Self::Global(v) => quote!( #v ),
        }
    }
}

/// Candle specifies that variables in the past can be invoqued through the
/// notation `var!(self <~ dt; v)`.
impl ToTokens for expr::PastVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { var, depth } = self;
        toks.extend(quote! {
            ::chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

impl ToTokens for decl::TyVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        toks.extend(quote! {
            #name : ::chandeliers_sem::ty!(#ty)
        });
    }
}

impl decl::TyVar {
    fn base_type_of(&self) -> ty::TyBase {
        self.ty.t.base.t
    }

    fn name_of(&self) -> expr::LocalVar {
        self.name.t.clone()
    }
}

/// Print the type of a stream without the temporal information.
/// This is the type that it has in function arguments and return values.
impl ToTokens for Sp<ty::Stream> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let ty = self.t.base;
        let mut pluses = Vec::new();
        for _ in 0..self.t.depth.dt {
            pluses.push(quote!( + ));
        }
        toks.extend(quote_spanned!(self.span=> #ty #(#pluses)* ));
    }
}

impl ToTokens for Sp<ty::TyBase> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let ty = Ident::new(&format!("{}", self.t), self.span);
        toks.extend(quote_spanned!(self.span=> #ty ));
    }
}

impl ToTokens for stmt::Statement {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Let { source, target } => {
                toks.extend(quote! {
                    let #target = #source
                });
            }
            Self::Trace { .. } => {
                unimplemented!("Trace");
            }
            Self::Assert(e) => {
                let s = format!("{}", &e);
                toks.extend(quote! {
                    ::chandeliers_sem::truth!(#e, #s);
                })
            }
        }
    }
}

/// An assignment tuple.
///
/// We need a case analysis on the size of the tuple, where
/// `_` is needed to bind an empty return `()`, and otherwise we need
/// exactly one or several variable names to bind one scalar per variable.
impl ToTokens for stmt::VarTuple {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Single(s) => {
                toks.extend(quote!( #s ));
            }
            Self::Multiple(m) if m.t.is_empty() => {
                toks.extend(quote!(_));
            }
            Self::Multiple(m) => {
                let m = m.t.iter();
                toks.extend(quote! {
                    ( #( #m ),* )
                });
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
                let l = l.const_lit();
                quote!(::chandeliers_sem::lit!(#l))
            }
            Self::Reference(refer) => {
                quote!( #refer )
            }
            Self::Tuple(t) => {
                let elems = t.t.iter();
                quote!( ( #( #elems ),* ).embed() )
            }
            Self::BinOp { op, lhs, rhs } => {
                quote!(::chandeliers_sem::binop!(#op; #lhs, #rhs))
            }
            Self::UnOp { op, inner } => {
                quote!(::chandeliers_sem::unop!(#op; #inner))
            }
            Self::CmpOp { op, lhs, rhs } => {
                quote!(::chandeliers_sem::cmp!(#op; #lhs, #rhs))
            }
            Self::Later { clk, before, after } => {
                quote!(::chandeliers_sem::later!(self <~ #clk; #before, #after))
            }
            Self::Ifx { cond, yes, no } => {
                quote!(::chandeliers_sem::ifx!((#cond) then { #yes } else { #no }))
            }
            Self::Substep { id, args } => {
                let id_lit = syn::LitInt::new(&format!("{}", id.t.id), id.span);
                quote! {
                    ::chandeliers_sem::substep!(
                        self;
                        #id_lit => {
                            #args.embed()
                        }
                    )
                }
            }
            Self::Merge { switch, on, off } => {
                quote!(::chandeliers_sem::merge!(#switch; #on, #off))
            }
            Self::ClockOp {
                op,
                inner,
                activate,
            } => {
                quote!(::chandeliers_sem::#op!(#activate; #inner))
            }
        })
    }
}

impl ToTokens for expr::ClockOp {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::When => quote!(when),
            Self::Whenot => quote!(whenot),
        })
    }
}

impl ToTokens for past::Depth {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let lit = syn::Lit::Int(syn::LitInt::new(&format!("{}", self.dt), self.span));
        toks.extend(quote!( #lit ));
    }
}

impl ToTokens for Sp<expr::NodeId> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("_{}", self.t.id), self.span);
        toks.extend(quote!( #id ));
    }
}
