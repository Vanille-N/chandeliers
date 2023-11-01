//! Codegen options for toplevel declarations.

/// Options Available for a node.
#[derive(Debug, Clone)]
pub struct Node {
    /// `#[trace]`: display debug information on the inputs and outputs
    pub trace: bool,
    /// `#[export]`: this struct is public.
    pub export: bool,
    /// `#[main(n)]` generate a main function for this node that runs `n` times.
    pub main: Option<usize>,
    /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
    /// to silence Rustc warnings.
    pub rustc_allow: Vec<syn::Ident>,
}

/// Options Available for an extern node.
#[derive(Debug, Clone)]
pub struct ExtNode {
    /// `#[trace]`: display debug information on the inputs and outputs
    pub trace: bool,
    /// `#[main(n)]` generate a main function for this node that runs `n` times.
    pub main: Option<usize>,
    /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
    /// to silence Rustc warnings.
    pub rustc_allow: Vec<syn::Ident>,
}

/// Options available for a const.
#[derive(Debug, Clone)]
pub struct Const {
    /// `#[export]`: this const is public.
    pub export: bool,
    /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
    /// to silence Rustc warnings.
    pub rustc_allow: Vec<syn::Ident>,
}

/// Options available for an extern const.
#[derive(Debug, Clone)]
pub struct ExtConst {
    /// `#[rustc_allow("foo")]`: generates a `#[allow(foo)]` on the definition
    /// to silence Rustc warnings.
    pub rustc_allow: Vec<syn::Ident>,
}
