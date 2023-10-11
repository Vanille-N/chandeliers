//! Check proper causality (acyclicity of dependencies)

use std::hash::Hash;

use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

use crate::candle::ast;
pub type CycResult<T> = Result<T, TokenStream>;

pub trait Causality: Sized {
    fn causality(self) -> CycResult<Self>;
}

impl Causality for ast::decl::Prog {
    fn causality(self) -> CycResult<Self> {
        let Self { decls } = self;
        let mut g = Graph::default(|dec: &ast::Sp<ast::decl::Decl>| dec.span);
        for decl in decls {
            g.insert(decl.causality()?)?;
        }
        let decls = g.scheduling()?;
        Ok(Self { decls })
    }
}

impl<T: Causality> Causality for ast::Sp<T> {
    fn causality(self) -> CycResult<Self> {
        self.map(|_, t| t.causality()).transpose()
    }
}

impl Causality for ast::decl::Decl {
    fn causality(self) -> CycResult<Self> {
        Ok(match self {
            Self::Node(n) => Self::Node(n.causality()?),
            _ => self,
        })
    }
}

impl Causality for ast::decl::Node {
    fn causality(self) -> CycResult<Self> {
        use impl_depends::Reference;
        let Self { name, inputs, outputs, locals, blocks, stmts } = self;
        let mut g = Graph::default(|stmt: &ast::Sp<ast::stmt::Statement>| stmt.span);
        for i in &inputs.t.elems {
            g.already_provided(Reference::VarName(i.t.name.t.name.clone()));
        }
        for stmt in stmts {
            g.insert(stmt)?;
        }
        for l in &locals.t.elems {
            g.must_provide(l.span, Reference::VarName(l.t.name.t.name.clone()))?;
        }
        for o in &outputs.t.elems {
            g.must_provide(o.span, Reference::VarName(o.t.name.t.name.clone()))?;
        }
        let stmts = g.scheduling()?;
        Ok(Self { name, inputs, outputs, locals, blocks, stmts })
    }
}


#[derive(Debug, Clone)]
struct Graph<Obj, Unit, ObjSpan> {
    elements: Vec<Obj>,
    atomics: (Vec<Unit>, HashMap<Unit, usize>),
    provide: Vec<Vec<usize>>,
    require: Vec<Vec<usize>>,
    provided_by_ext: HashSet<usize>,
    provided_for_ext: HashSet<usize>,
    span: ObjSpan,
}

trait Depends {
    type Output;
    fn provides(&self, v: &mut Vec<Self::Output>);
    fn requires(&self, v: &mut Vec<Self::Output>);
}

impl<Obj, Unit, ObjSpan> Graph<Obj, Unit, ObjSpan>
where
    Obj: Depends<Output = Unit> + std::fmt::Debug,
    Unit: Clone + Hash + PartialEq + Eq + std::fmt::Debug + std::fmt::Display,
    ObjSpan: Fn(&Obj) -> Span,
{
    fn default(span: ObjSpan) -> Self {
        Self {
            elements: Default::default(),
            atomics: Default::default(),
            provide: Default::default(),
            require: Default::default(),
            provided_by_ext: Default::default(),
            provided_for_ext: Default::default(),
            span,
        }
    }

    fn get_or_insert_atomic(&mut self, o: Unit) -> usize {
        match self.atomics.1.get(&o) {
            Some(uid) => *uid,
            None => {
                let uid = self.atomics.0.len();
                self.atomics.0.push(o.clone());
                self.atomics.1.insert(o.clone(), uid);
                uid
            }
        }
    }

    fn already_provided(&mut self, o: Unit) {
        let uid = self.get_or_insert_atomic(o);
        self.provided_by_ext.insert(uid);
    }

    fn must_provide(&mut self, span: Span, o: Unit) -> CycResult<()> {
        let uid = self.get_or_insert_atomic(o.clone());
        if !self.provided_for_ext.contains(&uid) {
            let s = format!("No definition provided for {}", o);
            Err(quote_spanned! {span=>
                compile_error!(#s);
            })
        } else {
            Ok(())
        }
    }

    fn insert(&mut self, t: Obj) -> CycResult<()> {
        let mut provide_units = Vec::new();
        let mut require_units = Vec::new();
        let mut provide_uids = Vec::new();
        let mut require_uids = Vec::new();
        t.provides(&mut provide_units);
        t.requires(&mut require_units);
        for prov in provide_units {
            let uid = self.get_or_insert_atomic(prov);
            provide_uids.push(uid);
            self.provided_for_ext.insert(uid);
        }
        for req in require_units {
            let uid = self.get_or_insert_atomic(req);
            require_uids.push(uid);
        }
        assert!(self.elements.len() == self.provide.len());
        assert!(self.elements.len() == self.require.len());
        self.provide.push(provide_uids);
        self.require.push(require_uids);
        self.elements.push(t);
        Ok(())
    }

    fn scheduling(self) -> CycResult<Vec<Obj>> {
        let nb = self.atomics.0.len();
        let (provider, constraints) = {
            let mut provider = vec![None; nb];
            let mut constraints = vec![vec![]; nb];
            for (id, provs) in self.provide.iter().enumerate() {
                for &p in provs {
                    if self.provided_by_ext.contains(&p) {
                        let s = format!("Cannot redefine {} (already provided by the context)", self.atomics.0[p]);
                        return Err(quote_spanned! {(self.span)(&self.elements[id])=>
                            compile_error!(#s);
                        });
                    }
                    if provider[p].is_some() {
                        // FIXME: also show first definition site
                        let s = format!("{} is declared twice", self.atomics.0[p]);
                        return Err(quote_spanned! {(self.span)(&self.elements[id])=>
                            compile_error!(#s);
                        });
                    } else {
                        provider[p] = Some(id);
                    }
                    for &r in &self.require[id] {
                        if r == p {
                            let s = format!("{} depends on itself", self.atomics.0[p]);
                            return Err(quote_spanned! {(self.span)(&self.elements[id])=>
                                compile_error!(#s);
                            });
                        }
                        constraints[p].push(r);
                    }
                }
            }
            (provider, constraints)
        };
        //dbg!(&self.atomics);
        //dbg!(&constraints);
        //dbg!(&provider);
        // Tarjan
        let scc = {
            #[derive(Default, Clone)]
            struct Extra {
                on_stack: bool,
                low_link: Option<usize>,
                index: Option<usize>,
            }
            struct SccExplorer {
                index: usize,
                stk: Vec<usize>,
                extra: Vec<Extra>,
                scc: Vec<Vec<usize>>,
            }
            let mut expl = SccExplorer {
                index: 0,
                stk: Vec::new(),
                extra: vec![Extra::default(); nb],
                scc: Vec::new(),
            };
            for v in 0..nb {
                if expl.extra[v].index.is_none() {
                    explore_scc(v, &constraints, &mut expl);
                }
            }

            fn explore_scc(v: usize, graph: &[Vec<usize>], expl: &mut SccExplorer) {
                expl.extra[v].index = Some(expl.index);
                expl.extra[v].low_link = Some(expl.index);
                expl.index += 1;
                expl.stk.push(v);
                expl.extra[v].on_stack = true;

                for &w in &graph[v] {
                    if expl.extra[w].index.is_none() {
                        explore_scc(w, graph, expl);
                        expl.extra[v].low_link = expl.extra[v].low_link.min(expl.extra[w].low_link);
                    } else if expl.extra[w].on_stack {
                        expl.extra[v].low_link = expl.extra[v].low_link.min(expl.extra[w].index);
                    }
                }

                if expl.extra[v].low_link == expl.extra[v].index {
                    let mut scc = Vec::new();
                    while let Some(w) = expl.stk.pop() {
                        expl.extra[w].on_stack = false;
                        scc.push(w);
                        if w == v {
                            break;
                        }
                    }
                    expl.scc.push(scc);
                }
            }
            expl.scc
        };
        // Check that all |scc| = 1
        let mut schedule = Vec::new();
        let mut unused: Vec<Option<Obj>> = self.elements.into_iter().map(Some).collect();
        for c in &scc {
            assert!(!c.is_empty(), "Malformed SCC of size 0");
            if c.len() > 1 {
                let looping = *c.last().expect("Length > 1");
                let cprov = provider[looping].expect("Element of SCC must have a provider");
                let s = format!("{} is part of a dependency cycle", self.atomics.0[looping]);
                // FIXME: print the entire cycle
                return Err(quote_spanned! {(self.span)(&unused[cprov].as_ref().expect("Not already taken"))=>
                    compile_error!(#s);
                });
            } else {
                let id = *c.last().expect("Length == 1");
                if let Some(Some(prov)) = provider.get(id) {
                    if let Some(obj) = unused[*prov].take() {
                        schedule.push(obj);
                    }
                }
            }
        }
        Ok(schedule)
    }
}

mod impl_depends {
    use std::fmt;
    use super::Depends;
    use crate::candle::ast::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(super) enum Reference {
        NodeId(usize),
        VarName(String),
        FunName(String),
    }

    impl fmt::Display for Reference {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::NodeId(id) => write!(f, "Block #{id}"),
                Self::FunName(fun) => write!(f, "Node `{fun}`"),
                Self::VarName(var) => write!(f, "Variable `{var}`"),
            }
        }
    }

    macro_rules! provide_by_match {
        ( $( $pat:pat => $($y:ident),* ; )* ) => {
            fn provides(&self, v: &mut Vec<Self::Output>) {
                match self {
                    $( $pat  => { $( $y.t.provides(v); )* } )*
                }
            }
        }
    }

    macro_rules! require_by_match {
        ( $( $pat:pat => $($y:ident),* ; )* ) => {
            fn requires(&self, v: &mut Vec<Self::Output>) {
                match self {
                    $( $pat  => { $( $y.t.requires(v); )* } )*
                }
            }
        }
    }

    macro_rules! provide_in_fields {
        ( $( $field:ident ),* ) => {
            fn provides(&self, v: &mut Vec<Self::Output>) {
                $( self.$field.provides(v); )*
            }
        }
    }

    macro_rules! require_in_fields {
        ( $( $field:ident ),* ) => {
            fn requires(&self, v: &mut Vec<Self::Output>) {
                $( self.$field.requires(v); )*
            }
        }
    }

    macro_rules! provide_nothing {
        () => {
            fn provides(&self, _v: &mut Vec<Self::Output>) {}
        }
    }

    macro_rules! require_nothing {
        () => {
            fn requires(&self, _v: &mut Vec<Self::Output>) {}
        }
    }

    macro_rules! provide_this {
        ( $fun:expr ) => {
            fn provides(&self, v: &mut Vec<Self::Output>) {
                v.push(($fun)(self));
            }
        }
    }

    macro_rules! require_this {
        ( $fun:expr ) => {
            fn requires(&self, v: &mut Vec<Self::Output>) {
                v.push(($fun)(self));
            }
        }
    }

    impl<T: Depends> Depends for Sp<T> {
        type Output = T::Output;
        provide_in_fields!(t);
        require_in_fields!(t);
    }

    impl<T: Depends> Depends for Vec<T> {
        type Output = T::Output;
        fn provides(&self, v: &mut Vec<Self::Output>) {
            for i in self {
                i.provides(v);
            }
        }
        fn requires(&self, v: &mut Vec<Self::Output>) {
            for i in self {
                i.requires(v);
            }
        }
    }

    impl Depends for decl::Decl {
        type Output = Reference;
        provide_by_match! {
            Self::Const(c) => c;
            Self::Node(n) => n;
            Self::ExtConst(c) => c;
            Self::ExtNode(n) => n;
        }
        require_by_match! {
            Self::Const(c) => c;
            Self::Node(n) => n;
            Self::ExtConst(c) => c;
            Self::ExtNode(n) => n;
        }
    }

    impl Depends for decl::Const {
        type Output = Reference;
        provide_in_fields!(name);
        require_in_fields!(value);
    }

    impl Depends for decl::ExtConst {
        type Output = Reference;
        provide_in_fields!(name);
        require_nothing!();
    }

    impl Depends for decl::Node {
        type Output = Reference;
        provide_in_fields!(name);
        require_in_fields!(blocks, stmts);
    }

    impl Depends for decl::ExtNode {
        type Output = Reference;
        provide_in_fields!(name);
        require_nothing!();
    }

    impl Depends for decl::NodeName {
        type Output = Reference;
        provide_this!(|this: &Self| Reference::FunName(this.0.clone()));
        require_this!(|this: &Self| Reference::FunName(this.0.clone()));
    }

    impl Depends for stmt::Statement {
        type Output = Reference;
        provide_by_match! {
            Self::Let { target, .. } => target;
            Self::Substep { id, .. } => id;
            Self::Trace { .. } => ;
            Self::Assert(_) => ;
        }
        require_by_match! {
            Self::Let { source, .. } => source;
            Self::Substep { args, .. } => args;
            Self::Trace { .. } => ;
            Self::Assert(e) => e;
        }
    }

    impl Depends for expr::Expr {
        type Output = Reference;
        provide_nothing!();
        require_by_match! {
            Self::Lit(_) => ;
            Self::Reference(r) => r;
            Self::Tuple(t) => t;
            Self::BinOp { lhs, rhs, .. } => lhs, rhs;
            Self::CmpOp { lhs, rhs, .. } => lhs, rhs;
            Self::UnOp { inner, .. } => inner;
            Self::Later { before, after, .. } => before, after;
            Self::Builtin(_) => ;
            Self::Ifx { cond, yes, no } => cond, yes, no;
        }
    }

    impl Depends for expr::Var {
        type Output = Reference;
        provide_this!(|this: &Self| Reference::VarName(this.name.clone()));
        require_this!(|this: &Self| Reference::VarName(this.name.clone()));
    }

    impl Depends for expr::Reference {
        type Output = Reference;
        provide_nothing!();
        require_by_match! {
            Self::Var(v) => v;
            Self::Node(n) => n;
        }
    }

    impl Depends for expr::NodeId {
        type Output = Reference;
        provide_this!(|this: &Self| Reference::NodeId(this.id));
        require_this!(|this: &Self| Reference::NodeId(this.id));
    }

    impl Depends for expr::ClockVar {
        type Output = Reference;
        provide_nothing!();
        fn requires(&self, v: &mut Vec<Reference>) {
            if self.depth.dt == 0 {
                self.var.requires(v);
            }
        }
    }

    impl<T: Depends> Depends for Tuple<T> {
        type Output = T::Output;
        provide_in_fields!(elems);
        require_in_fields!(elems);
    }

    impl Depends for stmt::VarTuple {
        type Output = Reference;
        provide_by_match! {
            Self::Single(s) => s;
            Self::Multiple(m) => m;
        }
        require_by_match! {
            Self::Single(s) => s;
            Self::Multiple(m) => m;
        }
    }
}
