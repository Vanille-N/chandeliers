//! Verify positivity of all variables (absence of Nil)
//!
//! Although the uninitialized value exists in the runtime semantics of
//! Candle (and Lustre), a correct program should not be able to build a Nil.
//! This is given by the fact that in any expression we are never allowed
//! to have more `pre` in front of a variable than there are `->`.

use std::collections::HashMap;

use chandeliers_err::{self as err, Acc, Result};

use crate::ast::{decl, expr, stmt, var, Tuple};
use crate::sp::Sp;

/// Helper to record the depth during the traversal.
struct DepthCtx<'i> {
    /// Maximum observed depth for each known variable.
    max_known: &'i mut HashMap<var::Local, usize>,
    /// Current depth of the exploration.
    current: usize,
}

/// Duplicate a `DepthCtx` (which is not `Clone` or `Copy` so
/// duplication produces a reborrow).
macro_rules! fork {
    ($depths:ident) => {
        DepthCtx {
            max_known: &mut *$depths.max_known,
            current: $depths.current,
        }
    };
}

impl DepthCtx<'_> {
    /// Visit at depth `nb` from now on.
    fn with(mut self, nb: usize) -> Self {
        self.current = nb;
        self
    }

    /// Check if the depth required is smaller than the current depth
    /// of the exploration.
    fn allows(&self, req: usize) -> bool {
        req <= self.current
    }

    /// Record a new depth `depth` observed for variable `var`,
    /// and update the known maximum if it is greater.
    fn update(self, var: &var::Local, depth: usize) {
        if let Some(old) = self.max_known.get(var) {
            if *old >= depth {
                return;
            }
        }
        self.max_known.insert(var.clone(), depth);
    }
}

/// Without modifying the object, check that the depth of variables is
/// acceptable and that no uninitialized variables are accessed.
trait CheckPositive {
    /// Verify that this item is deep enough.
    fn check_positive(&self, acc: &mut Acc, depth: DepthCtx<'_>) -> Result<()>;
}

/// Compute the required depth of variables so that the program is positive
/// (i.e. does not attempt to read a variable that has not been initialized yet).
/// This requires mutability because it will edit the `depth` field of streams
/// everywhere in the tree.
pub trait MakePositive {
    /// Mutate this object to make all its streams deep enough.
    ///
    /// # Errors
    /// Failure means that it is impossible to make this positive and can
    /// occur in exactly one place: putting too many `pre` in front of a variable
    /// with not enough `->` before.
    fn make_positive(&mut self, acc: &mut Acc) -> Result<()>;
}

impl<T: CheckPositive> CheckPositive for Sp<T> {
    fn check_positive(&self, acc: &mut Acc, depth: DepthCtx<'_>) -> Result<()> {
        self.t.check_positive(acc, depth)
    }
}

impl<T: CheckPositive> CheckPositive for Box<T> {
    fn check_positive(&self, acc: &mut Acc, depth: DepthCtx<'_>) -> Result<()> {
        self.as_ref().check_positive(acc, depth)
    }
}

impl<T: MakePositive> MakePositive for Sp<T> {
    fn make_positive(&mut self, acc: &mut Acc) -> Result<()> {
        self.t.make_positive(acc)
    }
}

impl MakePositive for decl::Prog {
    fn make_positive(&mut self, acc: &mut Acc) -> Result<()> {
        for decl in &mut self.decls {
            decl.make_positive(acc)?;
        }
        Ok(())
    }
}

impl MakePositive for decl::Decl {
    fn make_positive(&mut self, acc: &mut Acc) -> Result<()> {
        match self {
            Self::Node(n) => n.make_positive(acc),
            _ => Ok(()),
        }
    }
}

impl MakePositive for decl::Node {
    fn make_positive(&mut self, acc: &mut Acc) -> Result<()> {
        let mut depths = HashMap::new();
        let view = DepthCtx {
            max_known: &mut depths,
            current: 0,
        };
        for stmt in &self.stmts {
            stmt.check_positive(acc, fork!(view))?;
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
    fn check_positive(&self, acc: &mut Acc, depths: DepthCtx<'_>) -> Result<()> {
        err::consistency!(depths.current == 0, "Statement expects depth to be 0");
        match self {
            Self::Let { source, .. } => source.check_positive(acc, depths),
            Self::Assert(e) => e.check_positive(acc, depths),
        }
    }
}

impl CheckPositive for expr::Expr {
    fn check_positive(&self, acc: &mut Acc, depths: DepthCtx<'_>) -> Result<()> {
        match self {
            // First let's get all the trivial cases out of the picture.
            Self::Lit(_) => Ok(()),
            Self::Tuple(tup) => tup.check_positive(acc, depths),
            Self::Bin { lhs, rhs, .. } => {
                lhs.check_positive(acc, fork!(depths))?;
                rhs.check_positive(acc, fork!(depths))?;
                Ok(())
            }
            Self::Un { inner, .. } => inner.check_positive(acc, depths),
            Self::Cmp { lhs, rhs, .. } => {
                lhs.check_positive(acc, fork!(depths))?;
                rhs.check_positive(acc, fork!(depths))?;
                Ok(())
            }
            Self::Ifx { cond, yes, no } => {
                cond.check_positive(acc, fork!(depths))?;
                yes.check_positive(acc, fork!(depths))?;
                no.check_positive(acc, fork!(depths))?;
                Ok(())
            }
            // Now we finally get to the interesting cases.
            // Later relaxes the constraints, and we get to increase the
            // depth for the right side by as much as the later.
            Self::Later { clk, before, after } => {
                before.check_positive(acc, fork!(depths))?;
                after.check_positive(acc, fork!(depths).with(clk.t.dt + 1))?;
                Ok(())
            }
            Self::Substep { args, .. } => args.check_positive(acc, depths),
            // Reference is also an interesting case, but its impl is separate.
            Self::Reference(refer) => refer.check_positive(acc, depths),
            Self::Clock {
                activate,
                op: _,
                inner,
            } => {
                activate.check_positive(acc, fork!(depths))?;
                inner.check_positive(acc, fork!(depths))?;
                Ok(())
            }
            Self::Merge { switch, on, off } => {
                switch.check_positive(acc, fork!(depths))?;
                on.check_positive(acc, fork!(depths))?;
                off.check_positive(acc, fork!(depths))?;
                Ok(())
            }
        }
    }
}

impl<T: CheckPositive> CheckPositive for Tuple<T> {
    fn check_positive(&self, acc: &mut Acc, depths: DepthCtx<'_>) -> Result<()> {
        for elem in self.iter() {
            elem.check_positive(acc, fork!(depths))?;
        }
        Ok(())
    }
}

impl CheckPositive for var::Reference {
    fn check_positive(&self, acc: &mut Acc, depths: DepthCtx<'_>) -> Result<()> {
        match self {
            // We get globals for free.
            Self::Global(_) => Ok(()),
            // Finally Variables have an associated clock that corresponds
            // to how many `pre` we found before the variable when performing
            // the translation.
            Self::Var(v) => {
                if depths.allows(v.t.depth.t.dt) {
                    depths.update(&v.t.var.t, v.t.depth.t.dt);
                    Ok(())
                } else {
                    acc.error(err::NotPositive {
                        var: &v.t.var,
                        site: v.span,
                        available_depth: depths.current,
                        attempted_depth: v.t.depth.t.dt,
                    })
                }
            }
        }
    }
}
