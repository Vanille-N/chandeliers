//! Verify positivity of all variables (absence of Nil)
//!
//! Although the uninitialized value exists in the runtime semantics of
//! Candle (and Lustre), a correct program should not be able to build a Nil.
//! This is given by the fact that in any expression we are never allowed
//! to have more `pre` in front of a variable than there are `->`.

use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote_spanned;

type TokResult<T> = Result<T, TokenStream>;

use crate::ast::*;

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
    fn check_positive(&self, depth: VarsDepth<'_>) -> TokResult<()>;
}

pub trait MakePositive {
    fn make_positive(&mut self) -> TokResult<()>;
}

impl<T: CheckPositive> CheckPositive for Sp<T> {
    fn check_positive(&self, depth: VarsDepth<'_>) -> TokResult<()> {
        self.t.check_positive(depth)
    }
}

impl<T: MakePositive> MakePositive for Sp<T> {
    fn make_positive(&mut self) -> TokResult<()> {
        self.t.make_positive()
    }
}

impl MakePositive for decl::Prog {
    fn make_positive(&mut self) -> TokResult<()> {
        for decl in &mut self.decls {
            decl.make_positive()?;
        }
        Ok(())
    }
}

impl MakePositive for decl::Decl {
    fn make_positive(&mut self) -> TokResult<()> {
        match self {
            Self::Const(_) => Ok(()),
            Self::ExtConst(_) => Ok(()),
            Self::ExtNode(_) => Ok(()),
            Self::Node(n) => n.make_positive(),
        }
    }
}

impl MakePositive for decl::Node {
    fn make_positive(&mut self) -> TokResult<()> {
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
                var.t.ty.t.depth.dt = *depths.get(&var.t.name.t).unwrap_or(&0);
            }
        }
        Ok(())
    }
}

impl CheckPositive for stmt::Statement {
    fn check_positive(&self, depths: VarsDepth<'_>) -> TokResult<()> {
        assert_eq!(depths.current, 0, "Statement expects depth to be 0");
        match self {
            Self::Let { source, .. } => source.check_positive(depths),
            Self::Trace { .. } => unimplemented!("CheckPositive for Trace"),
            Self::Assert(e) => e.check_positive(depths),
            Self::Substep { clk, args, .. } => {
                // Substeps are executed at level 0, but in reality
                // their output is only used at a deeper level.
                // What's important is to check that the inputs are positive
                // when the subnode's personal clock is positive.
                for arg in args.t.iter() {
                    arg.check_positive(fork!(depths).with(clk.dt))?;
                }
                Ok(())
            }
        }
    }
}

impl CheckPositive for expr::Expr {
    fn check_positive(&self, depths: VarsDepth<'_>) -> TokResult<()> {
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
            Self::Builtin(b) => b.check_positive(depths),
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
                after.check_positive(fork!(depths).with(clk.dt + 1))?;
                Ok(())
            }
            // Reference is also an interesting case, but its impl is separate.
            Self::Reference(refer) => refer.check_positive(depths),
        }
    }
}

impl<T: CheckPositive> CheckPositive for Tuple<T> {
    fn check_positive(&self, depths: VarsDepth<'_>) -> TokResult<()> {
        for elem in self.iter() {
            elem.check_positive(fork!(depths))?;
        }
        Ok(())
    }
}

impl CheckPositive for expr::Builtin {
    fn check_positive(&self, depths: VarsDepth<'_>) -> TokResult<()> {
        match self {
            Self::Float(arg) => arg.check_positive(depths),
        }
    }
}

impl CheckPositive for expr::Reference {
    fn check_positive(&self, depths: VarsDepth<'_>) -> TokResult<()> {
        match self {
            // We get globals for free.
            Self::Global(_) => Ok(()),
            // Nodes look easy here, but that's because they were already
            // extracted to their own statement where the positivity of
            // their arguments is checked against their personal clock
            // (of which the dt was computed exactly by the depth we are currently at)
            Self::Node(_) => Ok(()),
            // Finally Variables have an associated clock that corresponds
            // to how many `pre` we found before the variable when performing
            // the translation.
            Self::Var(v) => {
                if v.t.depth.dt <= depths.current {
                    depths.update(&v.t.var.t, v.t.depth.dt);
                    Ok(())
                } else {
                    let s = format!("Variable {} is not positive at this depth ({} steps into the past at depth {})", v.t.var, v.t.depth.dt, depths.current);
                    Err(quote_spanned! {v.span=>
                        compile_error!(#s);
                    })
                }
            }
        }
    }
}
