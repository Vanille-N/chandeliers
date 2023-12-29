//! Codegen for `const` declarations.
//!
//! Since fewer constructs are available and since we need to use only
//! things that Rustc considers const-computable, we represent expressions in
//! a different way depending on whether they are in a `const` or in a local
//! variable.

use proc_macro2::TokenStream;
use quote::quote;

use chandeliers_err as err;

use crate::ast::{expr, var};
use crate::sp::{Sp, Span, WithSpan};

/// Trait to generate `const` expressions.
pub trait ConstExprTokens {
    /// Generate a `const` value for this expression.
    fn const_expr_tokens(&self, span: Span) -> TokenStream;
}

/// Helper trait for `Sp<_>` to impement `ConstExprTokens` with an added span.
pub trait ConstExprSpanTokens {
    /// Generate a `const` value for this expression.
    fn const_expr_tokens(&self) -> TokenStream;
}

impl<T: ConstExprTokens> ConstExprSpanTokens for Sp<T> {
    fn const_expr_tokens(&self) -> TokenStream {
        self.t.const_expr_tokens(self.span).with_span(self.span)
    }
}

impl<T: ConstExprTokens> ConstExprTokens for Box<T> {
    fn const_expr_tokens(&self, span: Span) -> TokenStream {
        let inner = self.as_ref().const_expr_tokens(span);
        quote!( #inner )
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
    fn const_expr_tokens(&self, _: Span) -> TokenStream {
        match self {
            Self::Lit(l) => {
                let l = l.const_expr_tokens();
                quote!( #l )
            }
            Self::Reference(refer) => {
                let refer = refer.const_expr_tokens();
                quote!( #refer )
            }
            Self::DummyPre(e) => {
                let v = e.const_expr_tokens();
                quote!( #v )
            }
            Self::DummyParen(e) => {
                let v = e.const_expr_tokens();
                quote!( #v )
            }
            Self::Bin { op, lhs, rhs } => {
                let lhs = lhs.const_expr_tokens();
                let rhs = rhs.const_expr_tokens();
                quote!( (#lhs #op #rhs) )
            }
            Self::Un { op, inner } => {
                let inner = inner.const_expr_tokens();
                quote!( (#op #inner) )
            }
            Self::Cmp { op, lhs, rhs } => {
                let lhs = lhs.const_expr_tokens();
                let rhs = rhs.const_expr_tokens();
                quote!( (#lhs #op #rhs) )
            }
            Self::Tuple(t) => {
                let ts =
                    t.t.iter()
                        .map(ConstExprSpanTokens::const_expr_tokens)
                        .collect::<Vec<_>>();
                quote!( ( #( #ts ),* ) )
            }
            Self::Later { .. } => err::abort!("Later is not valid in const contexts, which should have been caught during typecheck"),
            Self::Substep { .. } => err::abort!("Substep is not valid in const contexts, which should have been caught during typecheck"),
            Self::Ifx { cond, yes, no } => {
                let cond = cond.const_expr_tokens();
                let yes = yes.const_expr_tokens();
                let no = no.const_expr_tokens();
                quote! {
                    if #cond { #yes } else { #no }
                }
            }
            Self::Merge { .. } => err::abort!("Merge is not valid in const contexts, which should have been caught during typecheck"),
            Self::Clock { .. } => err::abort!("Clock is not valid in const contexts, which should have been caught during typecheck"),
        }
    }
}

/// It feels wrong to go back-and-forth between `i64`/`f64`/`bool` and
/// the same as `LitInt`/`LitFloat`/`LitBool`.
/// Maybe future implementations will change the internals of `expr::Lit`
/// to not perform parsing to base10.
impl ConstExprTokens for expr::Lit {
    fn const_expr_tokens(&self, span: Span) -> TokenStream {
        let lit = match self {
            Self::Int(i) => syn::Lit::Int(syn::LitInt::new(&format!("{i}i64"), span.unwrap())),
            Self::Float(f) => {
                syn::Lit::Float(syn::LitFloat::new(&format!("{f}f64"), span.unwrap()))
            }
            Self::Bool(b) => syn::Lit::Bool(syn::LitBool::new(*b, span.unwrap())),
        };
        quote!( #lit )
    }
}

impl ConstExprTokens for var::Reference {
    fn const_expr_tokens(&self, _span: Span) -> TokenStream {
        match self {
            Self::Var(_) => err::abort!(
                "Var is invalid in const contexts, which should have been caught during typecheck"
            ),
            Self::Global(v) => {
                let g = v.as_sanitized_ident();
                quote!( #g )
            }
        }
    }
}
