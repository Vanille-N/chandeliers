use super::ast as lus;
use chandeliers_san::candle::ast as candle;

#[must_use]
#[derive(Debug, Clone, Default)]
pub struct TrError {
    msgs: Vec<String>,
}

impl TrError {
    fn new<T>() -> Result<T, Self> {
        Err(Self::default())
    }

    fn with(mut self, msg: String) -> Self {
        self.msgs.push(msg);
        self
    }
}

pub type TrResult<T> = Result<T, TrError>;

impl lus::Node {
    pub fn translate(self) -> TrResult<candle::decl::Node> {
        let name = candle::decl::NodeName(self.name.to_string());
        let inputs = self.inputs.translate()?;
        let outputs = self.outputs.translate()?;
        let locals = self.locals.translate()?;
        let mut blocks = Vec::new();
        let mut stmts = Vec::new();
        for def in self.defs.into_iter() {
            def.translate(&mut blocks, &mut stmts)?;
        }
        Ok(candle::decl::Node {
            name,
            inputs,
            outputs,
            locals,
            blocks,
            stmts,
        })
    }
}

impl lus::ArgsTys {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        let mut vs = candle::Tuple::default();
        for item in self.items {
            item.translate(&mut vs)?;
        }
        Ok(vs)
    }
}

impl lus::ArgsTy {
    pub fn translate(self, vars: &mut candle::Tuple<candle::decl::Var>) -> TrResult<()> {
        let ty = self.ty.translate()?;
        self.args.translate(vars, ty)?;
        Ok(())
    }
}

impl lus::Decls {
    pub fn translate(
        self,
        vars: &mut candle::Tuple<candle::decl::Var>,
        ty: candle::ty::TyBase,
    ) -> TrResult<()> {
        for id in self.ids {
            vars.elems.push(candle::decl::Var {
                name: candle::expr::Var {
                    name: id.to_string(),
                },
                ty: candle::ty::Stream {
                    base: ty,
                    depth: candle::clock::Depth {
                        dt: 0, // FIXME
                    },
                },
            });
        }
        Ok(())
    }
}

impl lus::Type {
    pub fn translate(self) -> TrResult<candle::ty::TyBase> {
        self.base.translate()
    }
}

impl lus::BaseType {
    pub fn translate(self) -> TrResult<candle::ty::TyBase> {
        Ok(match self {
            Self::Int(_) => candle::ty::TyBase::Int,
            Self::Float(_) => candle::ty::TyBase::Float,
            Self::Bool(_) => candle::ty::TyBase::Bool,
        })
    }
}

impl lus::OptionalVarsDecl {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        match self {
            Self::Decls(d) => d.translate(),
            Self::None => Ok(candle::Tuple::default()),
        }
    }
}

impl lus::VarsDecl {
    pub fn translate(self) -> TrResult<candle::Tuple<candle::decl::Var>> {
        self.decls.translate()
    }
}

impl lus::Statement {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
    ) -> TrResult<()> {
        match self {
            Self::Assert(ass) => ass.translate(blocks, stmts),
            Self::Def(def) => def.translate(blocks, stmts),
        }
    }
}

impl lus::Assertion {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
    ) -> TrResult<()> {
        let e = self.expr.translate(blocks, stmts, 0)?;
        stmts.push(candle::stmt::Statement::Assert(e));
        Ok(())
    }
}

impl lus::Def {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
    ) -> TrResult<()> {
        let target = self.target.translate()?;
        let source = self.source.translate(blocks, stmts, 0)?;
        stmts.push(candle::stmt::Statement::Let { target, source });
        Ok(())
    }
}

impl lus::TargetExpr {
    pub fn translate(
        self,
    ) -> TrResult<candle::stmt::VarTuple> {
        match self {
            Self::Var(i) => Ok(candle::stmt::VarTuple::Single(candle::expr::Var { name: i.to_string() })),
            Self::Tuple(ts) => ts.translate(),
        }
    }
}

impl lus::TargetExprTuple {
    pub fn translate(
        self,
    ) -> TrResult<candle::stmt::VarTuple> {
        let mut vs = candle::Tuple::default();
        for t in self.fields {
            vs.elems.push(t.translate()?);
        }
        Ok(candle::stmt::VarTuple::Multiple(vs))
    }
}

impl lus::Expr {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
        depth: usize,
    ) -> TrResult<candle::expr::Expr> {
        self.inner.translate(blocks, stmts, depth)
    }
}

impl lus::expr::IfLevelExpr {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
        depth: usize,
    ) -> TrResult<candle::expr::Expr> {
        unimplemented!()
    }
}

impl lus::expr::IfExpr {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
        depth: usize,
    ) -> TrResult<candle::expr::Expr> {
        unimplemented!()
    }
}

impl lus::expr::OrExpr {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
        depth: usize,
    ) -> TrResult<candle::expr::Expr> {
        let mut it = self.items.into_iter();
        let mut or = it.next().unwrap().translate(blocks, stmts, depth)?;
        for e in it {
            let e = e.translate(blocks, stmts, depth)?;
            or = candle::expr::Expr::BinOp {
                op: candle::expr::BinOp::BitOr,
                lhs: Box::new(or),
                rhs: Box::new(e),
            }
        }
        Ok(or)
    }
}

impl lus::expr::AndExpr {
    pub fn translate(
        self,
        blocks: &mut Vec<candle::decl::NodeName>,
        stmts: &mut Vec<candle::stmt::Statement>,
        depth: usize,
    ) -> TrResult<candle::expr::Expr> {
        unimplemented!()
    }
}


