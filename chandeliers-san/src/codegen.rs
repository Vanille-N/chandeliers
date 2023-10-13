//! Generate actual Candle statements from an AST.

use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use super::ast::*;

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
            Self::ExtConst(_) => {}
            Self::ExtNode(_) => {}
            Self::Const(c) => {
                toks.extend(quote! {
                    const #c ;
                });
            }
            Self::Node(n) => {
                let declare = n.declare_node();
                let implement = n.implement_node();
                toks.extend(quote! {
                    #declare

                    #implement
                })
            }
        }
    }
}

/// Global constant.
///
/// This is straightforward generation of the components, apart from the
/// need to eliminate all non-const expression constructors (functions, nodes
/// later operator).
impl ToTokens for decl::Const {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty, value } = self;
        let name = name.as_ident();
        let value = value.const_expr();
        let ty = ty.as_base_ty();
        toks.extend(quote! {
            #name : chandeliers_sem::ty_mapping!(#ty) = #value
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
/// impl MyNode {
///     fn update_mut(&mut self, i1: ty!(int), i2: ty!(int)) -> (ty!(float), ty!(float)) {
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
impl decl::Node {
    fn declare_node(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts: _, // statements only relevant for impl
        } = self;
        let name = name.as_ident();
        let pos_inputs = inputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs = outputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_locals = locals.t.iter().filter(|v| v.t.strictly_positive());
        let blocks = blocks.iter().map(|n| n.as_ident()).collect::<Vec<_>>();
        quote! {
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            struct #name {
                __clock: usize,
                #( #pos_inputs , )*
                #( #pos_outputs , )*
                #( #pos_locals , )*
                __nodes: ( #( #blocks , )* ),
            }
        }
    }

    fn implement_node(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks: _, // blocks only relevant for decl
            stmts,
        } = self;
        let name = name.as_ident();
        let inputs_sep = inputs.comma_separated_decls();
        let outputs_ty = outputs.type_tuple();
        let outputs_vs = outputs.local_name_tuple();
        let pos_inputs = inputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs = outputs.t.iter().filter(|v| v.t.strictly_positive());
        let pos_locals = locals.t.iter().filter(|v| v.t.strictly_positive());
        quote! {
            #[allow(non_snake_case)]
            impl #name {
                fn update_mut(&mut self, #inputs_sep) -> #outputs_ty {
                    #( #stmts ; )*
                    chandeliers_sem::tick!(self);
                    #( update(self, #pos_inputs ); )*
                    #( update(self, #pos_outputs ); )*
                    #( update(self, #pos_locals ); )*
                    #outputs_vs
                }
            }
        }
    }
}

impl Sp<decl::NodeName> {
    fn as_ident(&self) -> Ident {
        Ident::new(&self.t.0.t, self.t.0.span)
    }
}

impl decl::Var {
    fn strictly_positive(&self) -> bool {
        self.ty.t.depth.dt > 0
    }
}

impl Sp<expr::Var> {
    /// Format the variable name verbatim.
    fn as_ident(&self) -> Ident {
        Ident::new(&self.t.name.t, self.t.name.span)
    }

    /// Format the variable name for a local variable.
    ///
    /// Rust does not allow const shadowing, so we need to make local
    /// identifiers unique.
    fn as_local_ident(&self) -> Ident {
        Ident::new(&format!("_local_{}", &self.t.name.t), self.t.name.span)
    }
}

impl Sp<decl::Node> {
    fn declare_node(&self) -> TokenStream {
        let toks = self.t.declare_node();
        quote_spanned! {self.span=> #toks }
    }
    fn implement_node(&self) -> TokenStream {
        let toks = self.t.implement_node();
        quote_spanned! {self.span=> #toks }
    }
}

impl Sp<expr::Expr> {
    fn const_expr(&self) -> TokenStream {
        let toks = self.t.const_expr();
        quote_spanned! {self.span=> #toks }
    }
}

impl Sp<expr::Reference> {
    fn const_expr(&self) -> TokenStream {
        let toks = self.t.const_expr();
        quote_spanned! {self.span=> #toks }
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
impl expr::Expr {
    fn const_expr(&self) -> TokenStream {
        match self {
            Self::Lit(l) => {
                let l = l.const_lit();
                quote!( #l )
            }
            Self::Reference(refer) => {
                let refer = refer.const_expr();
                quote!( #refer )
            }
            Self::BinOp { op, lhs, rhs } => {
                let lhs = lhs.const_expr();
                let rhs = rhs.const_expr();
                quote!( (#lhs #op #rhs) )
            }
            Self::UnOp { op, inner } => {
                let inner = inner.const_expr();
                quote!( (#op #inner) )
            }
            Self::CmpOp { op, lhs, rhs } => {
                let lhs = lhs.const_expr();
                let rhs = rhs.const_expr();
                quote!( (#lhs #op #rhs) )
            }
            Self::Tuple(t) => {
                let ts = t.t.iter().map(|e| e.const_expr()).collect::<Vec<_>>();
                quote!( #( #ts ),* )
            }
            Self::Later { .. } => unreachable!("Later is not valid in const contexts"),
            Self::Builtin(_) => {
                unreachable!("Builtins cannot be called in const contexts")
            }
            Self::Ifx { cond, yes, no } => {
                let cond = cond.const_expr();
                let yes = yes.const_expr();
                let no = no.const_expr();
                quote!( if #cond { #yes } else { #no } )
            }
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
/// subnodes as
///    _1
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
            Self::Node(n) => {
                let ident = Ident::new(&format!("_{}", n.t.id), n.span);
                toks.extend(quote!( #ident ));
            }
            Self::Global(v) => {
                let v = v.as_ident();
                toks.extend(quote! {
                    chandeliers_sem::lit!(#v)
                });
            }
        }
    }
}

impl expr::Reference {
    fn const_expr(&self) -> TokenStream {
        match self {
            Self::Var(_) => unreachable!("Var is invalid in const contexts"),
            Self::Node(_) => unreachable!("Node is invalid in const contexts"),
            Self::Global(v) => {
                let v = v.as_ident();
                quote!( #v )
            }
        }
    }
}

/// Candle specifies that variables in the past can be invoqued through the
/// notation `var!(self <~ dt; v)`.
impl ToTokens for expr::ClockVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { var, depth } = self;
        let var = var.as_local_ident();
        toks.extend(quote! {
            chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

/*
impl ToTokens for Sp<expr::Var> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("_local_{}", &self.t.name.t), self.t.name.span);
        toks.extend(quote!( #id ));
    }
}
*/

impl ToTokens for decl::Var {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        let name = name.as_local_ident();
        let ty = ty.as_base_ty();
        toks.extend(quote! {
            #name : chandeliers_sem::ty!(#ty)
        });
    }
}

impl Sp<Tuple<Sp<decl::Var>>> {
    /// A `Var` contains both names and types.
    /// This method prints the comma-separated types.
    fn type_tuple(&self) -> TokenStream {
        let toks = self.t.type_tuple();
        quote_spanned! {self.span=> #toks }
    }

    /// A `Var` contains both names and types.
    /// This method prints the comma-separated names.
    fn local_name_tuple(&self) -> TokenStream {
        let toks = self.t.local_name_tuple();
        quote_spanned! {self.span=> #toks }
    }

    /// A `Var` contains both names and types.
    /// This method prints the comma-separated `name: type`s
    fn comma_separated_decls(&self) -> TokenStream {
        let toks = self.t.comma_separated_decls();
        quote_spanned! {self.span=> #toks }
    }
}

impl Tuple<Sp<decl::Var>> {
    fn type_tuple(&self) -> TokenStream {
        if self.len() == 1 {
            let ty = self.iter().next().unwrap().t.ty.as_base_ty();
            quote! { chandeliers_sem::ty!(#ty) }
        } else {
            let tup = self.comma_separated_types();
            quote! {
                ( #tup )
            }
        }
    }

    fn local_name_tuple(&self) -> TokenStream {
        if self.len() == 1 {
            let name = self.iter().next().unwrap().t.name.as_local_ident();
            quote! { #name }
        } else {
            let tup = self.comma_separated_local_names();
            quote! {
                ( #tup )
            }
        }
    }

    fn comma_separated_types(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in self.iter() {
            let ty = v.t.ty.as_base_ty();
            toks.extend(quote! { chandeliers_sem::ty!(#ty) , });
        }
        toks
    }

    fn comma_separated_local_names(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in self.iter() {
            let name = v.t.name.as_local_ident();
            toks.extend(quote! { #name , });
        }
        toks
    }

    fn comma_separated_decls(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in self.iter() {
            toks.extend(quote! { #v , });
        }
        toks
    }
}

/// Print the type of a stream without the temporal information.
/// This is the type that it has in function arguments and return values.
impl Sp<ty::Stream> {
    fn as_base_ty(&self) -> Ident {
        self.t.base.as_base_ty()
    }
}

impl Sp<ty::TyBase> {
    fn as_base_ty(&self) -> Ident {
        Ident::new(&format!("{}", self.t), self.span)
    }
}

/// Statement has a nontrivial implementation mostly due to the
/// `Substep` variant that needs a lot of things.
///
/// As a reminder, the structure of a substep is
/// ```skip
/// substep!(self <~ dt; 0 => { arg1, arg2, ...}|***);
/// ```
/// where "***" tells candle the number of arguments that we
/// expect in the return value.
impl ToTokens for stmt::Statement {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Let { source, target } => {
                toks.extend(quote! {
                    let #target = #source
                });
            }
            Self::Substep {
                clk,
                id,
                args,
                nbret,
            } => {
                let id_lit = syn::LitInt::new(&format!("{}", id.t.id), id.span);
                let args = args.t.iter();
                let mut nbret_stars = Vec::new();
                for _ in 0..nbret.t.expect("Needs to know how many values are returned") {
                    nbret_stars.push(quote!(*));
                }
                toks.extend(quote! {
                    let #id = chandeliers_sem::substep!(
                        self <~ #clk;
                        #id_lit => {
                            #( #args , )*
                        }| #( #nbret_stars )*
                    )
                });
            }
            Self::Trace { .. } => {
                unimplemented!("Trace");
            }
            Self::Assert(e) => {
                let s = format!("Assertion failed: {}", &e);
                toks.extend(quote! {
                    chandeliers_sem::truth!(#e, #s);
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
                let s = s.as_local_ident();
                toks.extend(quote!( #s ));
            }
            Self::Multiple(m) if m.t.is_empty() => {
                toks.extend(quote!( _ ));
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
/// apart from Candle's quirky syntaxes for later `later!(self <~ dt; a, b)`
/// and if `ifx!((b) then { y } else { n })`.
impl ToTokens for expr::Expr {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Lit(l) => {
                let l = l.const_lit();
                quote!(chandeliers_sem::lit!(#l))
            }
            Self::Reference(refer) => {
                quote!( #refer )
            }
            Self::Tuple(t) => {
                let elems = t.t.iter();
                quote!( ( #( #elems ),* ) )
            }
            Self::BinOp { op, lhs, rhs } => {
                quote!(chandeliers_sem::binop!(#op; #lhs, #rhs))
            }
            Self::UnOp { op, inner } => {
                quote!(chandeliers_sem::unop!(#op; #inner))
            }
            Self::CmpOp { op, lhs, rhs } => {
                quote!(chandeliers_sem::cmp!(#op; #lhs, #rhs))
            }
            Self::Later { clk, before, after } => {
                quote!(chandeliers_sem::later!(self <~ #clk; #before, #after))
            }
            Self::Builtin(b) => {
                quote!( #b )
            }
            Self::Ifx { cond, yes, no } => {
                quote!(chandeliers_sem::ifx!((#cond) then { #yes } else { #no }))
            }
        })
    }
}

impl ToTokens for clock::Depth {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let lit = syn::Lit::Int(syn::LitInt::new(&format!("{}", self.dt), self.span));
        toks.extend(quote!( #lit ));
    }
}

/// Builtins should all have their own definition in Candle, there is no reason
/// to have this function do anything more complicated than a single macro
/// invocation.
impl ToTokens for expr::Builtin {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Float(arg) => {
                toks.extend(quote!(
                    chandeliers_sem::float!(#arg)
                ));
            }
        }
    }
}

impl ToTokens for Sp<expr::NodeId> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("_{}", self.t.id), self.span);
        toks.extend(quote!( #id ));
    }
}
