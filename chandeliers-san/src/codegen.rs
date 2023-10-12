use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use super::ast::*;
use super::candle;

pub trait Codegen {
    fn codegen(self) -> TokenStream;
}

impl<T: ToTokens> ToTokens for Sp<T> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { span, t } = &self;
        toks.extend(quote_spanned! {*span=>
            #t
        })
    }
}

impl ToTokens for decl::Prog {
    fn to_tokens(&self, toks: &mut TokenStream) {
        for decl in &self.decls {
            decl.to_tokens(toks);
        }
    }
}

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

impl decl::Node {
    fn declare_node(&self) -> TokenStream {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        } = self;
        let name = name.as_ident();
        let pos_inputs = inputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs = outputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_locals = locals.t.elems.iter().filter(|v| v.t.strictly_positive());
        let blocks = blocks.iter().map(|n| n.as_ident()).collect::<Vec<_>>();
        quote! {
            #[derive(Debug, Default)]
            #[allow(non_camel_case_types)]
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
            blocks,
            stmts,
        } = self;
        let name = name.as_ident();
        let inputs_sep = inputs.comma_separated_decls();
        let outputs_ty = outputs.type_tuple();
        let outputs_vs = outputs.name_tuple();
        let pos_inputs = inputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs = outputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_locals = locals.t.elems.iter().filter(|v| v.t.strictly_positive());
        quote! {
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
        Ident::new(&self.t.0, self.span)
    }
}

impl Sp<decl::Var> {
    fn as_ident(&self) -> Ident {
        self.t.name.as_ident()
    }
}

impl decl::Var {
    fn strictly_positive(&self) -> bool {
        self.ty.t.depth.dt > 0
    }
}

impl Sp<expr::Var> {
    fn as_ident(&self) -> Ident {
        Ident::new(&self.t.name, self.span)
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

impl Sp<expr::Builtin> {
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
                quote!( (#op #inner) )
            }
            Self::CmpOp { op, lhs, rhs } => {
                let lhs = lhs.const_expr();
                let rhs = rhs.const_expr();
                quote!( (#lhs #op #rhs) )
            }
            Self::Tuple(t) => {
                let ts = t.t.elems.iter().map(|e| e.const_expr()).collect::<Vec<_>>();
                quote!( #( #ts ),* )
            }
            Self::Later { .. } => unreachable!("Later is not valid in const contexts"),
            Self::Builtin(b) => {
                let b = b.const_expr();
                quote!( #b )
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

impl expr::Builtin {
    fn const_expr(&self) -> TokenStream {
        unimplemented!("Builtin in const contexts")
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

impl ToTokens for expr::Reference {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Var(v) => {
                toks.extend(quote! {
                    #v
                });
            }
            Self::Node(n) => {
                let ident = Ident::new(&format!("_{}", n.t.id), n.span);
                toks.extend(quote! { #ident });
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
            Self::Var(v) => unreachable!("Invalid in const contexts"),
            Self::Node(n) => {
                let ident = Ident::new(&format!("_{}", n.t.id), n.span);
                quote! { #ident }
            }
            Self::Global(v) => {
                quote! {
                    #v
                }
            }
        }
    }
}

impl ToTokens for expr::ClockVar {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { var, depth } = self;
        toks.extend(quote! {
            chandeliers_sem::var!(self <~ #depth; #var)
        });
    }
}

impl ToTokens for Sp<expr::Var> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&self.t.name, self.span);
        toks.extend(quote! {
            #id
        })
    }
}

impl ToTokens for decl::Var {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        let name = name.as_ident();
        let ty = ty.as_base_ty();
        toks.extend(quote! {
            #name : chandeliers_sem::ty!(#ty)
        });
    }
}

impl Tuple<Sp<decl::Var>> {
    fn strictly_positive_comma_sep(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            if v.t.ty.t.depth.dt > 0 {
                toks.extend(quote! { #v , });
            }
        }
        toks
    }
    fn strictly_positive_update(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            if v.t.ty.t.depth.dt > 0 {
                toks.extend(quote! { update(self, #v); });
            }
        }
        toks
    }
}

impl Sp<Tuple<Sp<decl::Var>>> {
    fn type_tuple(&self) -> TokenStream {
        let toks = self.t.type_tuple();
        quote_spanned! {self.span=> #toks }
    }

    fn name_tuple(&self) -> TokenStream {
        let toks = self.t.name_tuple();
        quote_spanned! {self.span=> #toks }
    }

    fn comma_separated_decls(&self) -> TokenStream {
        let toks = self.t.comma_separated_decls();
        quote_spanned! {self.span=> #toks }
    }
}

impl Tuple<Sp<decl::Var>> {
    fn type_tuple(&self) -> TokenStream {
        if self.elems.len() == 1 {
            let ty = self.elems[0].t.ty.as_base_ty();
            quote! { chandeliers_sem::ty!(#ty) }
        } else {
            let tup = self.comma_separated_types();
            quote! {
                ( #tup )
            }
        }
    }

    fn name_tuple(&self) -> TokenStream {
        if self.elems.len() == 1 {
            let name = self.elems[0].t.name.as_ident();
            quote! { #name }
        } else {
            let tup = self.comma_separated_names();
            quote! {
                ( #tup )
            }
        }
    }

    fn comma_separated_types(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            let ty = v.t.ty.as_base_ty();
            toks.extend(quote! { chandeliers_sem::ty!(#ty) , });
        }
        toks
    }

    fn comma_separated_names(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            let name = v.t.name.as_ident();
            toks.extend(quote! { #name , });
        }
        toks
    }

    fn comma_separated_decls(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            toks.extend(quote! { #v , });
        }
        toks
    }
}

impl Sp<ty::Stream> {
    fn as_base_ty(&self) -> Ident {
        Ident::new(&format!("{}", self.t.base), self.span)
    }
}

impl Sp<ty::TyBase> {
    fn as_base_ty(&self) -> Ident {
        Ident::new(&format!("{}", self.t), self.span)
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
            Self::Substep { clk, id, args } => {
                let id_lit = syn::LitInt::new(&format!("{}", id.t.id), id.span);
                let args = &args.t.elems;
                toks.extend(quote! {
                    let #id = chandeliers_sem::substep!(
                        self <~ #clk;
                        #id_lit => {
                            #( #args , )*
                        }
                    )
                });
            }
            Self::Trace { msg, fmt } => {
                unimplemented!("Trace");
            }
            Self::Assert(e) => {
                unimplemented!("Assert");
            }
        }
    }
}

impl ToTokens for stmt::VarTuple {
    fn to_tokens(&self, toks: &mut TokenStream) {
        match self {
            Self::Single(s) => {
                let s = s.as_ident();
                toks.extend(quote! {
                    #s
                });
            }
            Self::Multiple(m) => {
                let m = &m.t.elems;
                toks.extend(quote! {
                    ( #( #m ),* )
                });
            }
        }
    }
}

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
                let elems = &t.t.elems;
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

impl ToTokens for expr::Builtin {
    fn to_tokens(&self, toks: &mut TokenStream) {
        unimplemented!("Builtin")
    }
}

impl ToTokens for Sp<expr::NodeId> {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let id = Ident::new(&format!("_{}", self.t.id), self.span);
        toks.extend(quote!( #id ));
    }
}
