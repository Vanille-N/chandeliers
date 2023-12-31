//! Check proper causality (acyclicity of dependencies)
//!
//! Most of the hard work is delegated to `graph` for cycle detection
//! and `depends` for constraint definition.
//! Here we only need to recurse into the program AST deep enough for
//! the structures that need to have their dependencies analyzed,
//! which is done by the `Causality` trait.

use chandeliers_err::EAccum;

use crate::ast;
use crate::sp::Sp;

pub mod depends;
pub mod graph;

use depends::Reference;
use graph::Graph;

/// Sort the internals of `Self` to remove cyclic dependencies and allow
/// typechecking in the right order.
pub trait Causality: Sized {
    /// Rearrange the internal contents in a causally consistent ordering.
    /// # Errors
    /// Fails if there is a cycle (including of length 1).
    fn causality(self, eaccum: &mut EAccum) -> Option<Self>;
}

impl Causality for ast::decl::Prog {
    /// Sort the declarations of the program to remove cycles.
    fn causality(self, eaccum: &mut EAccum) -> Option<Self> {
        let Self { decls } = self;
        let mut g = Graph::default();
        for decl in decls {
            g.insert(decl.causality(eaccum)?);
        }
        let decls = g.scheduling(eaccum)?;
        Some(Self { decls })
    }
}

impl<T: Causality> Causality for Sp<T> {
    /// Trivial projection.
    fn causality(self, eaccum: &mut EAccum) -> Option<Self> {
        self.map(|_, t| t.causality(eaccum)).transpose()
    }
}

impl Causality for ast::decl::Decl {
    /// Trivial projection.
    fn causality(self, eaccum: &mut EAccum) -> Option<Self> {
        Some(match self {
            Self::Node(n) => Self::Node(n.causality(eaccum)?),
            _ => self,
        })
    }
}

impl Causality for ast::decl::Node {
    /// Sort the statements of a node to
    /// - verify that no inputs are redefined
    /// - verify that all locals and outputs are well-defined
    /// - ensure that there is no dependency cycle.
    fn causality(self, eaccum: &mut EAccum) -> Option<Self> {
        let Self {
            name,
            options,
            inputs,
            outputs,
            locals,
            blocks,
            deptys,
            stmts,
            registers,
            flips,
        } = self;
        let mut g = Graph::default();
        for i in inputs.t.iter() {
            g.already_provided(&Reference::LocalVarName(
                i.t.name.as_ref().map(|_, t| t.repr.t.clone()),
            ));
        }
        for stmt in stmts {
            g.insert(stmt);
        }
        // Define interface
        for l in locals.t.iter() {
            let unit = Reference::LocalVarName(l.t.name.as_ref().map(|_, t| t.repr.t.clone()));
            g.must_provide(eaccum, &unit)?;
        }
        for o in outputs.t.iter() {
            let unit = Reference::LocalVarName(o.t.name.as_ref().map(|_, t| t.repr.t.clone()));
            g.must_provide(eaccum, &unit)?;
        }
        // Sort internals
        let stmts = g.scheduling(eaccum)?;
        Some(Self {
            name,
            options,
            inputs,
            outputs,
            locals,
            blocks,
            deptys,
            stmts,
            registers,
            flips,
        })
    }
}
