//! Check proper causality (acyclicity of dependencies)
//!
//! Most of the hard work is delegated to `graph` for cycle detection
//! and `depends` for constraint definition.
//! Here we only need to recurse into the program AST deep enough for
//! the structures that need to have their dependencies analyzed,
//! which is done by the `Causality` trait.

use crate::ast;
use chandeliers_err as err;

pub mod depends;
pub mod graph;
use depends::Reference;
use graph::Graph;

type CycResult<T> = Result<T, err::Error>;

/// Sort the internals of `Self` to remove cyclic dependencies and allow
/// typechecking in the right order.
pub trait Causality: Sized {
    fn causality(self) -> CycResult<Self>;
}

impl Causality for ast::decl::Prog {
    /// Sort the declarations of the program to remove cycles.
    fn causality(self) -> CycResult<Self> {
        let Self { decls } = self;
        let mut g = Graph::default();
        for decl in decls {
            g.insert(decl.causality()?)?;
        }
        let decls = g.scheduling()?;
        Ok(Self { decls })
    }
}

impl<T: Causality> Causality for ast::Sp<T> {
    /// Trivial projection.
    fn causality(self) -> CycResult<Self> {
        self.map(|_, t| t.causality()).transpose()
    }
}

impl Causality for ast::decl::Decl {
    /// Trivial projection.
    fn causality(self) -> CycResult<Self> {
        Ok(match self {
            Self::Node(n) => Self::Node(n.causality()?),
            _ => self,
        })
    }
}

impl Causality for ast::decl::Node {
    /// Sort the statements of a node to
    /// - verify that no inputs are redefined
    /// - verify that all locals and outputs are well-defined
    /// - ensure that there is no dependency cycle.
    fn causality(self) -> CycResult<Self> {
        let Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        } = self;
        let mut g = Graph::default();
        for i in inputs.t.iter() {
            g.already_provided(Reference::LocalVarName(
                i.t.name.as_ref().map(|_, t| t.name.t.clone()),
            ));
        }
        for stmt in stmts {
            g.insert(stmt)?;
        }
        for l in locals.t.iter() {
            let unit = Reference::LocalVarName(l.t.name.as_ref().map(|_, t| t.name.t.clone()));
            g.must_provide(unit)?;
        }
        for o in outputs.t.iter() {
            let unit = Reference::LocalVarName(o.t.name.as_ref().map(|_, t| t.name.t.clone()));
            g.must_provide(unit)?;
        }
        let stmts = g.scheduling()?;
        Ok(Self {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        })
    }
}
