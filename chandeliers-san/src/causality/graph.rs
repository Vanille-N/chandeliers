//! A scheduler implemented as a graph with a cycle detection algorithm.
//!
//! `Graph` can provide scheduling for any object that implements
//! `GraphError` and `Depends`.
//!
//! The methods of `Graph` provide ways to define
//! (`insert`, `already_provided`, `must_provide`)
//! and resolve (`scheduling`) dependencies between objects of type
//! `Obj`, using `Unit` as an identifier for dependency units.
//!
//! Typically if you are trying to resolve dependencies between variable
//! assignments of the form `a = f(b, c, d)`, you would declare `Unit`
//! to be the type of variable identifiers and `Obj` to be the type of
//! assignments.
//!
//! An implementation of `Depends` in the above example would say that
//! the assignment `a = f(b, c, d)` provides `[a]` and requires `[b, c, d]`.
//! No two `Obj` can provide the same `Unit`, but an `Obj` can require arbitrarily
//! many `Unit`s.
//!
//! You may additionally specify external constraints, such as
//! - `must_provide(u: Unit)`: constraint resolution will fail if no `Obj`
//!   provides `u`.
//!   (typical use-case: `u` is a binding required by the environment)
//! - `already_provided(u : Unit)`: constraint resolution will fail if some
//!   `Obj` provides `u`.
//!   (typical use-case: `u` is a global value that must not be overriden)
//!
//! Once all constraints have been specified, if you invoque `scheduling`
//! on the `Graph`, it will return either
//! - a scheduling of all `insert`ed `Obj`s such that all `Unit`s are
//!   provided before they are required, or
//! - a proof that this is impossible, in the form of a `Unit` that
//!   is part of a dependency cycle.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

use super::depends::Depends;

/// Describes how to convert objects that the graph can manipulate
/// (`Obj`, `Unit`) into ones that the environment wants to use as error messages.
pub trait GraphError {
    /// The type of errors.
    ///
    /// Typically something like `String` or `TokenStream` that will
    /// print the failure readably.
    type Error;
    /// Produce an error message.
    fn emit(&self, msg: String) -> Self::Error;
}

/// The identifier of a `Unit`.
///
/// To avoid out of bounds accesses it is very important to only ever use
/// indexes provided by `register_new_unit`.
#[derive(Debug, Clone, Copy)]
struct UnitIdx(usize);

/// The identifier of an `Obj`.
///
/// To avoid out of bounds accesses it is very important to only ever use
/// indexes provided by `register_new_obj`.
#[derive(Debug, Clone, Copy)]
struct ObjIdx(usize);

/// A scheduler to resolve dependency requirements.
#[derive(Debug, Clone)]
pub struct Graph<Obj, Unit> {
    /// Known objects, this is the Vec that will be returned by `schedule`
    /// once sorted.
    elements: Vec<Obj>,
    /// Bidirectional map of the building blocks that describe dependencies.
    atomics: (Vec<Unit>, HashMap<Unit, usize>),
    /// `provide[k]` is the list of `Unit`s provided by `elements[k]`.
    provide: Vec<Vec<usize>>,
    /// `require[k]` is the list of `Unit`s required by `elements[k]`.
    require: Vec<Vec<usize>>,
    /// The set of all `Unit`s that must not be provided.
    provided_by_ext: HashSet<usize>,
    /// The set of all `Unit`s that have been provided.
    provided_for_ext: HashSet<usize>,
}

impl<O, U> Default for Graph<O, U> {
    fn default() -> Self {
        Self {
            elements: Default::default(),
            atomics: Default::default(),
            provide: Default::default(),
            require: Default::default(),
            provided_by_ext: Default::default(),
            provided_for_ext: Default::default(),
        }
    }
}

impl<Obj, Unit, E> Graph<Obj, Unit>
where
    Obj: Depends<Output = Unit> + GraphError<Error = E> + fmt::Debug,
    Unit: Clone + Hash + PartialEq + Eq + GraphError<Error = E> + fmt::Debug + fmt::Display,
{
    /// Get the identifier of a `Unit` if it exists,
    /// or insert a new one with a fresh id.
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

    /// Declare a `Unit` that must not be provided by any of
    /// the future `Obj` to be inserted.
    ///
    /// This only has an effect at time of calling `scheduling`,
    /// so it is fine to provide these constraints before or after
    /// all `insert`.
    pub fn already_provided(&mut self, o: Unit) {
        let uid = self.get_or_insert_atomic(o);
        self.provided_by_ext.insert(uid);
    }

    /// Verify that a given `Unit` has already been provided by
    /// some `Obj`.
    ///
    /// This verification is not deferred and will immediately error
    /// if the `Unit` in question has not already been encountered,
    /// so this method should be called after all insertions are
    /// complete.
    pub fn must_provide(&mut self, o: Unit) -> Result<(), E> {
        let uid = self.get_or_insert_atomic(o.clone());
        if !self.provided_for_ext.contains(&uid) {
            let s = format!("No definition provided for {}", &o);
            Err(o.emit(s))
        } else {
            Ok(())
        }
    }

    /// Declare a new set of constraints.
    pub fn insert(&mut self, t: Obj) -> Result<(), E> {
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

    /// Resolve all previously inserted constraints and return a scheduling
    /// without temporal inconsistencies.
    pub fn scheduling(self) -> Result<Vec<Obj>, E> {
        let nb = self.atomics.0.len();
        let (provider, constraints) = {
            // First step is to create some reverse mappings that will
            // make it easier to navigate the graph.
            //
            // For now we have maps from an `Obj` to all the `Unit`s it
            // relates to (both provides and requires), but it is not yet
            // convenient to find the `Unit`s that another `Unit` depends on
            // because we don't have a `Unit -> Obj` map.
            // Fortunately building one is straightforward.

            // `provider[i]` is which `Obj` provides `i`.
            let mut provider = vec![None; nb];
            // `constraints[i]` is the list of `Unit`s that `i` depends on.
            let mut constraints = vec![vec![]; nb];
            for (id, provs) in self.provide.iter().enumerate() {
                for &p in provs {
                    // First we check that the `Unit` is provided only once.
                    // This involves both `provided_by_ext` and `provider`
                    // currently being computed.
                    if self.provided_by_ext.contains(&p) {
                        let s = format!(
                            "Cannot redefine {} (already provided by the context)",
                            self.atomics.0[p]
                        );
                        return Err(self.elements[id].emit(s));
                    }
                    if provider[p].is_some() {
                        // FIXME: also show first definition site
                        let s = format!("{} is declared twice", self.atomics.0[p]);
                        return Err(self.elements[id].emit(s));
                    } else {
                        // Constraint is valid, record its provider.
                        provider[p] = Some(id);
                    }
                    // Check for cycles of length 1.
                    // This is the best time to do it since the default SCC algorithm
                    // is fine with connected components of size 1.
                    for &r in &self.require[id] {
                        if r == p {
                            let s = format!("{} depends on itself", self.atomics.0[p]);
                            return Err(self.elements[id].emit(s));
                        }
                        constraints[p].push(r);
                    }
                }
            }
            (provider, constraints)
        };
        // Tarjan's algorithm for computing the SCCs of a graph.
        // This does two things:
        //   1. an SCC of size >1 is a cycle, we don't want those.
        //   2. the order in which the SCCs are closed gives a scheduling.
        let scc = {
            /// Per-node information on the status of the traversal.
            #[derive(Default, Clone)]
            struct Extra {
                /// This node is currently being explored.
                on_stack: bool,
                /// Best temporary approximation of the lowest index
                /// of a node in this one's SCC.
                low_link: Option<usize>,
                /// Unique identifier.
                index: Option<usize>,
            }
            /// Stack of the traversal, and other mutable state.
            struct SccExplorer {
                /// Count nodes and attribute IDs by order of DFS traversal.
                index: usize,
                /// Nodes left to handle.
                stk: Vec<usize>,
                /// Extra info for each node (visited status).
                extra: Vec<Extra>,
                /// Building the SCC.
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

            /// Exploration procedure for one node.
            fn explore_scc(v: usize, graph: &[Vec<usize>], expl: &mut SccExplorer) {
                // Register unique identifiers for this new node.
                expl.extra[v].index = Some(expl.index);
                expl.extra[v].low_link = Some(expl.index);
                expl.index += 1;
                expl.stk.push(v);
                expl.extra[v].on_stack = true;

                // Explore all children.
                // Compare `low_link` to find cycles: because `low_link` is initially
                // the (unique) index, a child with a smaller `low_link` gives a cycle.
                for &w in &graph[v] {
                    if expl.extra[w].index.is_none() {
                        // Not explored at all, explore it.
                        explore_scc(w, graph, expl);
                        expl.extra[v].low_link = expl.extra[v].low_link.min(expl.extra[w].low_link);
                    } else if expl.extra[w].on_stack {
                        // Exploration still ongoing, `low_link` has not yet been fully computed,
                        // so use `index` instead.
                        expl.extra[v].low_link = expl.extra[v].low_link.min(expl.extra[w].index);
                    }
                }

                // Pop the scc, as given by all the nodes above `v` on the stack.
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
        // We're going to remove all elements one at a time, let's make that easier.
        let mut unused: Vec<Option<Obj>> = self.elements.into_iter().map(Some).collect();
        for c in &scc {
            assert!(!c.is_empty(), "Malformed SCC of size 0");
            if c.len() > 1 {
                // Oops, here's a loop.
                let looping = *c.last().expect("Length > 1");
                let cprov = provider[looping].expect("Element of SCC must have a provider");
                let s = format!("{} is part of a dependency cycle", self.atomics.0[looping]);
                // FIXME: print the entire cycle
                return Err(unused[cprov].as_ref().unwrap().emit(s));
            } else {
                // Correctly of size 1, we can use it next.
                let id = *c.last().expect("Length == 1");
                // This is faillible because we have `Unit`s that are not provided by anyone.
                if let Some(Some(prov)) = provider.get(id) {
                    if let Some(obj) = unused[*prov].take() {
                        schedule.push(obj);
                    }
                }
            }
        }
        for o in unused.into_iter().flatten() {
            // These are statements that provide nothing, but they could require things.
            // e.g. assertions. Let's put them last.
            schedule.push(o);
        }
        Ok(schedule)
    }
}

#[cfg(test)]
mod test {
    use std::fmt;

    use super::super::depends::Depends;
    use super::{Graph, GraphError};

    #[derive(Debug, PartialEq)]
    struct Triplet(char, char, char);

    #[derive(Debug, Clone, Eq, Hash, PartialEq)]
    struct Unit(char);

    impl fmt::Display for Triplet {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({}, {}, {})", self.0, self.1, self.2)
        }
    }

    impl fmt::Display for Unit {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({})", self.0)
        }
    }

    impl GraphError for Triplet {
        type Error = String;
        fn emit(&self, msg: String) -> Self::Error {
            msg
        }
    }
    impl GraphError for Unit {
        type Error = String;
        fn emit(&self, msg: String) -> Self::Error {
            msg
        }
    }
    impl Depends for Triplet {
        type Output = Unit;
        fn provides(&self, v: &mut Vec<Self::Output>) {
            v.push(Unit(self.0));
        }
        fn requires(&self, v: &mut Vec<Self::Output>) {
            v.push(Unit(self.1));
            v.push(Unit(self.2));
        }
    }

    fn schedule(pairs: Vec<Triplet>) -> Result<Vec<Triplet>, String> {
        let mut g = Graph::default();
        for pair in pairs {
            g.insert(pair)?;
        }
        Ok(g.scheduling()?)
    }

    #[test]
    fn schedule_test() {
        assert_eq!(
            schedule(vec![
                Triplet('a', 'b', 'c'),
                Triplet('b', 'd', 'z'),
                Triplet('c', 'b', 'w')
            ]),
            Ok(vec![
                Triplet('b', 'd', 'z'),
                Triplet('c', 'b', 'w'),
                Triplet('a', 'b', 'c')
            ]),
        );

        assert_eq!(
            schedule(vec![Triplet('a', 'b', 'a')]),
            Err(String::from("(a) depends on itself")),
        );

        assert_eq!(
            schedule(vec![Triplet('a', 'b', 'c'), Triplet('b', 'a', 'c')]),
            Err(String::from("(a) is part of a dependency cycle")),
        );
    }
}
