use proc_macro2::{TokenStream, Ident};
use quote::{ToTokens, quote_spanned, quote};

use super::ast::*;

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
            Self::ExtConst(_) => {},
            Self::ExtNode(_) => {},
            Self::Const(c) => toks.extend(quote! {
                const #c ;
            }),
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
        let value = value.const_expr();
        let ty = ty.as_base_ty();
        toks.extend(quote! {
            #name : #ty = #value
        });
    }
}

impl decl::Node {
    fn declare_node(&self) -> TokenStream {
        let Self { name, inputs, outputs, locals, blocks, stmts } = self;
        let name = name.as_ident();
        let pos_inputs = inputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_outputs = outputs.t.elems.iter().filter(|v| v.t.strictly_positive());
        let pos_locals = locals.t.elems.iter().filter(|v| v.t.strictly_positive());
        let blocks = blocks.iter().map(|n| n.as_ident()).collect::<Vec<_>>();
        quote! {
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
        let Self { name, inputs, outputs, locals, blocks, stmts } = self;
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
                    tick(self);
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

impl expr::Expr {
    fn const_expr(&self) -> TokenStream {
        unimplemented!("Expr")
    }
}

impl ToTokens for expr::Var {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name } = self;
        toks.extend(quote! {
            #name
        })
    }
}

impl ToTokens for decl::Var {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let Self { name, ty } = self;
        let name = name.as_ident();
        let ty = ty.as_base_ty();
        toks.extend(quote! {
            #name : #ty
        });
    }
}

impl Tuple<Sp<decl::Var>> {
    fn strictly_positive_comma_sep(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            if v.t.ty.t.depth.dt > 0 {
                toks.extend(quote!{ #v , });
            }
        }
        toks
    }
    fn strictly_positive_update(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            if v.t.ty.t.depth.dt > 0 {
                toks.extend(quote!{ update(self, #v); });
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
            quote!{ #ty }
        } else {
            let tup = self.comma_separated_types();
            quote!{
                ( #tup )
            }
        }
    }

    fn name_tuple(&self) -> TokenStream {
        if self.elems.len() == 1 {
            let name = self.elems[0].t.name.as_ident();
            quote!{ #name }
        } else {
            let tup = self.comma_separated_names();
            quote!{
                ( #tup )
            }
        }
    }


    fn comma_separated_types(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            let ty = v.t.ty.as_base_ty();
            toks.extend(quote!{ #ty , });
        }
        toks
    }

    fn comma_separated_names(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            let name = v.t.name.as_ident();
            toks.extend(quote!{ #name , });
        }
        toks
    }

    fn comma_separated_decls(&self) -> TokenStream {
        let mut toks = TokenStream::new();
        for v in &self.elems {
            toks.extend(quote!{ #v , });
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
        toks.extend(quote! {
            todo!();
        });
    }
}
