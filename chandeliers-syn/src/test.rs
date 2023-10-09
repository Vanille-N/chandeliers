#![cfg(test)]

use crate::*;

use proc_macro2::TokenStream;
use syn::parse::Parse;
use syn::parse_macro_input;
use syn::Token;

macro_rules! success {
    ($obj:expr) => {{
        $obj.unwrap()
    }};
}
macro_rules! failure {
    ($obj:expr) => {{
        assert!($obj.is_err())
    }};
}

macro_rules! parse_as {
    ($typ:ty, $from:expr) => {{
        syn::parse_str::<$typ>($from)
    }};
}

#[test]
fn type_keywords() {
    success!(parse_as!(BaseType, "bool"));
    success!(parse_as!(BaseType, "int"));
    success!(parse_as!(BaseType, "float"));
    failure!(parse_as!(BaseType, "foobar"));
}

#[test]
fn struct_keywords() {
    success!(parse_as!(kw::node, "node"));
    success!(parse_as!(kw::returns, "returns"));
    success!(parse_as!(kw::var, "var"));
    success!(parse_as!(kw::tel, "tel"));
    success!(parse_as!(Token![let], "let"));
}

#[test]
fn opers() {
    success!(parse_as!(punct::Neq, "<>"));
    success!(parse_as!(kw::fby, "fby"));
    success!(parse_as!(kw::and, "and"));
    success!(parse_as!(kw::pre, "pre"));
    success!(parse_as!(Token![->], "->"));
    success!(parse_as!(kw::not, "not"));
}

#[test]
fn r#type() {
    success!(parse_as!(Type, "int"));
    failure!(parse_as!(Type, "truc"));
}

#[test]
fn decls() {
    success!(parse_as!(Decls, "x"));
    success!(parse_as!(Decls, "x, y, z "));
    // No trailing commas in Decls
    failure!(parse_as!(Decls, "x, y, z, "));
    // Must be nonempty
    failure!(parse_as!(Decls, ""));
}

#[test]
fn args_ty() {
    success!(parse_as!(ArgsTy, "x, y, z : float"));
    success!(parse_as!(ArgsTy, "x : bool"));
    failure!(parse_as!(ArgsTy, "x :"));
    failure!(parse_as!(ArgsTy, "x"));
}

#[test]
fn args_tys() {
    success!(parse_as!(ArgsTys, "x, y : float; w:bool; s : float;"));
    success!(parse_as!(ArgsTys, "x : bool"));
    success!(parse_as!(ArgsTys, ""));
}

#[test]
fn target_expr() {
    success!(parse_as!(TargetExpr, "x"));
    success!(parse_as!(TargetExpr, "(x, y)"));
    success!(parse_as!(TargetExpr, "(((s, t), u), (v, (w, x, y), z))"));
}

#[test]
fn expr() {
    success!(parse_as!(Expr, "0"));
    success!(parse_as!(Expr, "5476"));
    success!(parse_as!(Expr, "3.1415"));
    /*FIXME*/
    success!(parse_as!(Expr, "\"abc\""));
    /*FIXME*/
    success!(parse_as!(Expr, "'x'"));
    success!(parse_as!(Expr, "x"));
    success!(parse_as!(Expr, "(x)"));
    success!(parse_as!(Expr, "((x))"));
    success!(parse_as!(Expr, "-x"));
    success!(parse_as!(Expr, "-x + y"));
    success!(parse_as!(Expr, "-x - y"));
    success!(parse_as!(Expr, "-x - -y"));
    success!(parse_as!(Expr, "(-x)"));
    success!(parse_as!(Expr, "---(x)"));
    success!(parse_as!(Expr, "x + y"));
    success!(parse_as!(Expr, "x + y + z"));
    success!(parse_as!(Expr, "x + y - z - w"));
    success!(parse_as!(Expr, "f()"));
    success!(parse_as!(Expr, "f(x, y,)"));
    success!(parse_as!(Expr, "f(x, y, g(z))"));
    success!(parse_as!(Expr, "f(x) + g(y) + h(z)"));
    success!(parse_as!(Expr, "x -> y"));
    success!(parse_as!(Expr, "x -> y -> z"));
    success!(parse_as!(Expr, "x fby f(z)"));
    success!(parse_as!(Expr, "x = y = z"));
    success!(parse_as!(Expr, "x >= y > z"));
    success!(parse_as!(Expr, "not x = y"));
    success!(parse_as!(Expr, "a and b < c and d = e"));
}
#[test]
fn expr_perf() {
    success!(parse_as!(Expr, "x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x"));
}
