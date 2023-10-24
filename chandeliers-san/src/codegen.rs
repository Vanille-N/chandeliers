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
                    #[allow(non_upper_case_globals)]
                    const #c ;
                });
            }
            Self::Node(n) => toks.extend(quote! {
                #n
            }),
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
        let value = value.const_expr();
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
impl ToTokens for decl::Node {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        } = self;

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
        let inputs_ty = inputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()));
        let inputs_vs = inputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.name_of()));

        let outputs_ty = outputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.base_type_of()));
        let outputs_vs = outputs
            .t
            .iter()
            .map(|sv| sv.as_ref().map(|_, v| v.name_of()));

        toks.extend(quote! {
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            pub struct #name {
                __clock: usize,
                #( #pos_inputs_decl , )*
                #( #pos_outputs_decl , )*
                #( #pos_locals_decl , )*
                __nodes: ( #( #blocks , )* ),
            }

            #[allow(non_snake_case)]
            impl #name {
                pub fn update_mut(
                    &mut self,
                    #( #inputs_vs : chandeliers_sem::ty!(#inputs_ty) ),*
                ) -> (
                    #( chandeliers_sem::ty!(#outputs_ty) ),*
                ) {
                    #( #stmts ; )*
                    chandeliers_sem::tick!(self);
                    #( chandeliers_sem::update!(self, #pos_inputs_use ); )*
                    #( chandeliers_sem::update!(self, #pos_outputs_use ); )*
                    #( chandeliers_sem::update!(self, #pos_locals_use ); )*
                    ( #( #outputs_vs ),* )
                }
            }
        });
    }
}

impl Sp<decl::NodeName> {
    fn as_ident(&self) -> Ident {
        Ident::new_raw(&self.t.0.t, self.t.0.span)
    }
}

impl decl::TyVar {
    fn strictly_positive(&self) -> bool {
        self.ty.t.depth.dt > 0
    }
}

impl ToTokens for Sp<expr::GlobalVar> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new_raw(&self.t.name.t, self.t.name.span);
        toks.extend(quote!( #id ));
    }
}

impl ToTokens for Sp<expr::LocalVar> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("_local_{}", &self.t.name.t), self.t.name.span);
        toks.extend(quote!( #id ));
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
                toks.extend(quote!( #n ));
            }
            Self::Global(v) => {
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
            Self::Global(v) => quote!( #v ),
        }
    }
}

/// Candle specifies that variables in the past can be invoqued through the
/// notation `var!(self <~ dt; v)`.
impl ToTokens for expr::ClockVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { var, depth } = self;
        toks.extend(quote! {
            chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

impl ToTokens for decl::TyVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        toks.extend(quote! {
            #name : chandeliers_sem::ty!(#ty)
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
                let s = format!("{}", &e);
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
                toks.extend(quote!(chandeliers_sem::float!(#arg)));
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
