//! Verify positivity of all variables (absence of Nil)
//!
//! Although the uninitialized value exists in the runtime semantics of
//! Candle (and Lustre), a correct program should not be able to build a Nil.
//! This is given by the fact that in any expression we are never allowed
//! to have more `pre` in front of a variable than there are `->`.

use std::collections::HashMap;

use chandeliers_err::{self as err, IntoError, Result};

use crate::ast::*;
use crate::sp::Sp;

struct VarsDepth<'i> {
    max_known: &'i mut HashMap<expr::LocalVar, usize>,
    current: usize,
}

macro_rules! fork {
    ($depths:ident) => {
        VarsDepth {
            max_known: &mut *$depths.max_known,
            current: $depths.current,
        }
    };
}

impl VarsDepth<'_> {
    fn with(mut self, nb: usize) -> Self {
        self.current = nb;
        self
    }

    fn update(self, var: &expr::LocalVar, depth: usize) {
        if let Some(old) = self.max_known.get(var) {
            if *old >= depth {
                return;
            }
        }
        self.max_known.insert(var.clone(), depth);
    }
}

trait CheckPositive {
    fn check_positive(&self, depth: VarsDepth<'_>) -> Result<()>;
}

/// Compute the required depth of variables so that the program is positive
/// (i.e. does not attempt to read a variable that has not been initialized yet).
/// This requires mutability because it will edit the `depth` field of streams
/// everywhere in the tree.
pub trait MakePositive {
    /// Mutate this object to make all its streams deep enough.
    fn make_positive(&mut self) -> Result<()>;
}

impl<T: CheckPositive> CheckPositive for Sp<T> {
    fn check_positive(&self, depth: VarsDepth<'_>) -> Result<()> {
        self.t.check_positive(depth)
    }
}

impl<T: CheckPositive> CheckPositive for Box<T> {
    fn check_positive(&self, depth: VarsDepth<'_>) -> Result<()> {
        self.as_ref().check_positive(depth)
    }
}

impl<T: MakePositive> MakePositive for Sp<T> {
    fn make_positive(&mut self) -> Result<()> {
        self.t.make_positive()
    }
}

impl MakePositive for decl::Prog {
    fn make_positive(&mut self) -> Result<()> {
        for decl in &mut self.decls {
            decl.make_positive()?;
        }
        Ok(())
    }
}

impl MakePositive for decl::Decl {
    fn make_positive(&mut self) -> Result<()> {
        match self {
            Self::Const(_) => Ok(()),
            Self::ExtConst(_) => Ok(()),
            Self::ExtNode(_) => Ok(()),
            Self::Node(n) => n.make_positive(),
        }
    }
}

impl MakePositive for decl::Node {
    fn make_positive(&mut self) -> Result<()> {
        let mut depths = HashMap::new();
        let view = VarsDepth {
            max_known: &mut depths,
            current: 0,
        };
        for stmt in &self.stmts {
            stmt.check_positive(fork!(view))?;
        }
        for vars in [&mut self.inputs, &mut self.outputs, &mut self.locals] {
            for var in vars.t.iter_mut() {
                var.t.ty.t.depth.t.dt = *depths.get(&var.t.name.t).unwrap_or(&0);
            }
        }
        Ok(())
    }
}

impl CheckPositive for stmt::Statement {
    fn check_positive(&self, depths: VarsDepth<'_>) -> Result<()> {
        assert_eq!(depths.current, 0, "Statement expects depth to be 0");
        match self {
            Self::Let { source, .. } => source.check_positive(depths),
            Self::Assert(e) => e.check_positive(depths),
        }
    }
}

impl CheckPositive for expr::Expr {
    fn check_positive(&self, depths: VarsDepth<'_>) -> Result<()> {
        match self {
            // First let's get all the trivial cases out of the picture.
            Self::Lit(_) => Ok(()),
            Self::Tuple(tup) => tup.check_positive(depths),
            Self::BinOp { lhs, rhs, .. } => {
                lhs.check_positive(fork!(depths))?;
                rhs.check_positive(fork!(depths))?;
                Ok(())
            }
            Self::UnOp { inner, .. } => inner.check_positive(depths),
            Self::CmpOp { lhs, rhs, .. } => {
                lhs.check_positive(fork!(depths))?;
                rhs.check_positive(fork!(depths))?;
                Ok(())
            }
            Self::Ifx { cond, yes, no } => {
                cond.check_positive(fork!(depths))?;
                yes.check_positive(fork!(depths))?;
                no.check_positive(fork!(depths))?;
                Ok(())
            }
            // Now we finally get to the interesting cases.
            // Later relaxes the constraints, and we get to increase the
            // depth for the right side by as much as the later.
            Self::Later { clk, before, after } => {
                before.check_positive(fork!(depths))?;
                after.check_positive(fork!(depths).with(clk.t.dt + 1))?;
                Ok(())
            }
            Self::Substep { args, .. } => args.check_positive(depths),
            // Reference is also an interesting case, but its impl is separate.
            Self::Reference(refer) => refer.check_positive(depths),
            Self::ClockOp {
                activate,
                op: _,
                inner,
            } => {
                activate.check_positive(fork!(depths))?;
                inner.check_positive(fork!(depths))?;
                Ok(())
            }
            Self::Merge { switch, on, off } => {
                switch.check_positive(fork!(depths))?;
                on.check_positive(fork!(depths))?;
                off.check_positive(fork!(depths))?;
                Ok(())
            }
        }
    }
}

impl<T: CheckPositive> CheckPositive for Tuple<T> {
    fn check_positive(&self, depths: VarsDepth<'_>) -> Result<()> {
        for elem in self.iter() {
            elem.check_positive(fork!(depths))?;
        }
        Ok(())
    }
}

impl CheckPositive for expr::Reference {
    fn check_positive(&self, depths: VarsDepth<'_>) -> Result<()> {
        match self {
            // We get globals for free.
            Self::Global(_) => Ok(()),
            // Finally Variables have an associated clock that corresponds
            // to how many `pre` we found before the variable when performing
            // the translation.
            Self::Var(v) => {
                if v.t.depth.t.dt <= depths.current {
                    depths.update(&v.t.var.t, v.t.depth.t.dt);
                    Ok(())
                } else {
                    Err(err::NotPositive {
                        var: &v.t.var,
                        site: v.span,
                        available_depth: depths.current,
                        attempted_depth: v.t.depth.t.dt,
                    }
                    .into_err())
                }
            }
        }
    }
}
