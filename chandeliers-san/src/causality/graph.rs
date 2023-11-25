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
//! If you require the absence of unused variables or missing definitions,
//! it is crucial that you use the methods above, as by default the scheduling
//! algorithm will assume that unused variables are useless and that missing
//! dependencies are provided by other means.
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

use chandeliers_err::{self as err, EAccum, TrySpan};

use crate::causality::depends::Depends;
use crate::sp::Span;

/// The manipulations in this file are prone to out-of-bounds accesses,
/// and because we are in a proc macro we won't get a nice error message
/// if that happens.
/// It is strongly discouraged to access any array other than through
/// this macro that will at the very least produce an actionable error message.
macro_rules! at {
    ( $arr:expr, $idx:expr ) => {{
        let arr = &$arr;
        let idx: usize = $idx;
        if let Some(val) = arr.get(idx) {
            val
        } else {
            chandeliers_err::abort!(
                "Out-of-bounds access: no index {} on array of length {}",
                idx,
                arr.len()
            )
        }
    }};
    ( mut $arr:expr, $idx:expr ) => {{
        let arr = &mut $arr;
        let idx: usize = $idx;
        if let Some(val) = arr.get_mut(idx) {
            val
        } else {
            chandeliers_err::abort!(
                "Out-of-bounds access: no index {} on array of length {}",
                idx,
                arr.len()
            )
        }
    }};
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

/// Data that may be accompanied by a span, typically representing the place
/// that the value was constructed so that the definition site can be shown
/// in error messages.
#[derive(Debug, Clone, Copy)]
struct WithDefSite<Unit> {
    /// Payload (may itself contain a span, usually the place that it is being used).
    contents: Unit,
    /// Where it was defined.
    def_site: Option<Span>,
}

impl<Unit> WithDefSite<Unit> {
    /// Wrap data without span.
    fn without(o: &Unit) -> Self
    where
        Unit: Clone,
    {
        Self {
            contents: o.clone(),
            def_site: None,
        }
    }

    /// Wrap data with something that might be a span.
    fn try_with(o: &Unit, sp: impl TrySpan) -> Self
    where
        Unit: Clone,
    {
        Self {
            contents: o.clone(),
            def_site: sp.try_span(),
        }
    }

    /// Replace the span, but only if there is not already a span
    /// defined.
    fn try_override_span(&mut self, o: &Unit)
    where
        Unit: TrySpan,
    {
        if self.def_site.is_none() {
            self.def_site = o.try_span();
        }
    }

    /// Explode this into the data and the span separately.
    fn elements(&self) -> (&Unit, Option<Span>) {
        (&self.contents, self.def_site)
    }
}

/// A scheduler to resolve dependency requirements.
#[derive(Debug, Clone)]
pub struct Graph<Obj, Unit> {
    /// Known objects, this is the Vec that will be returned by `schedule`
    /// once sorted.
    elements: Vec<Obj>,
    /// Bidirectional map of the building blocks that describe dependencies.
    atomics: (Vec<WithDefSite<Unit>>, HashMap<Unit, usize>),
    /// `provide[k]` is the list of `Unit`s provided by `elements[k]`.
    provide: Vec<Vec<WithDefSite<usize>>>,
    /// `require[k]` is the list of `Unit`s required by `elements[k]`.
    require: Vec<Vec<WithDefSite<usize>>>,
    /// The set of all `Unit`s that must not be provided.
    provided_by_ext: HashMap<usize, WithDefSite<()>>,
    /// The set of all `Unit`s that have been provided.
    provided_for_ext: HashSet<usize>,
}

impl<O, U> Default for Graph<O, U> {
    fn default() -> Self {
        Self {
            elements: Vec::default(),
            atomics: Default::default(),
            provide: Vec::default(),
            require: Vec::default(),
            provided_by_ext: HashMap::default(),
            provided_for_ext: HashSet::default(),
        }
    }
}

/// Exploration helper.
///
/// This has no knowledge of the objects that it is working on, so at the
/// level of this module all nodes are `usize`.
///
/// It expects a graph represented as a `[[usize]]` where `graph[i]` is the
/// list of vertices accessible from `i`, and will produce a vector of SCCs.
mod explore {
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
    pub struct SccExplorer {
        /// Count nodes and attribute IDs by order of DFS traversal.
        index: usize,
        /// Nodes left to handle.
        stk: Vec<usize>,
        /// Extra info for each node (visited status).
        extra: Vec<Extra>,
        /// Building the SCC.
        scc: Vec<Vec<usize>>,
    }

    impl SccExplorer {
        /// Exploration procedure for one node.
        pub fn run(&mut self, v: usize, graph: &[Vec<super::WithDefSite<usize>>]) {
            // Register unique identifiers for this new node.
            at!(mut self.extra, v).index = Some(self.index);
            at!(mut self.extra, v).low_link = Some(self.index);
            self.index += 1;
            self.stk.push(v);
            at!(mut self.extra, v).on_stack = true;

            // Explore all children.
            // Compare `low_link` to find cycles: because `low_link` is initially
            // the (unique) index, a child with a smaller `low_link` gives a cycle.
            for w in at!(graph, v) {
                if at!(self.extra, w.contents).index.is_none() {
                    // Not explored at all, explore it.
                    self.run(w.contents, graph);
                    at!(mut self.extra, v).low_link = at!(self.extra, v)
                        .low_link
                        .min(at!(self.extra, w.contents).low_link);
                } else if at!(self.extra, w.contents).on_stack {
                    // Exploration still ongoing, `low_link` has not yet been fully computed,
                    // so use `index` instead.
                    at!(mut self.extra, v).low_link = at!(self.extra, v)
                        .low_link
                        .min(at!(self.extra, w.contents).index);
                }
            }

            // Pop the scc, as given by all the nodes above `v` on the stack.
            if at!(self.extra, v).low_link == at!(self.extra, v).index {
                let mut scc = Vec::new();
                while let Some(w) = self.stk.pop() {
                    at!(mut self.extra, w).on_stack = false;
                    scc.push(w);
                    if w == v {
                        break;
                    }
                }
                self.scc.push(scc);
            }
        }

        /// Construct a new explorer for a graph of given size.
        pub fn for_graph_of_size(nb: usize) -> Self {
            Self {
                index: 0,
                stk: Vec::new(),
                extra: vec![Extra::default(); nb],
                scc: Vec::new(),
            }
        }

        /// Determine if a vertex was already explored or not.
        pub fn unexplored(&self, idx: usize) -> bool {
            at!(self.extra, idx).index.is_none()
        }

        /// Get the SCC out at the end of the exploration.
        pub fn extract(self) -> Vec<Vec<usize>> {
            self.scc
        }
    }
}

impl<Obj, Unit> Graph<Obj, Unit>
where
    Obj: Depends<Output = Unit> + fmt::Debug,
    Unit: TrySpan,
    Obj: TrySpan,
    Unit: Clone + Hash + PartialEq + Eq + fmt::Debug + fmt::Display,
{
    /// Get the identifier of a `Unit` if it exists,
    /// or insert a new one with a fresh id.
    fn get_or_insert_atomic(&mut self, o: &Unit, override_span: bool) -> usize {
        let uid = if let Some(uid) = self.atomics.1.get(o) {
            *uid
        } else {
            let uid = self.atomics.0.len();
            self.atomics.0.push(WithDefSite::without(o));
            self.atomics.1.insert(o.clone(), uid);
            uid
        };
        if override_span {
            at!(mut self.atomics.0, uid).try_override_span(o);
        }
        uid
    }

    /// Declare a `Unit` that must not be provided by any of
    /// the future `Obj` to be inserted.
    ///
    /// This only has an effect at time of calling `scheduling`,
    /// so it is fine to provide these constraints before or after
    /// all `insert`.
    pub fn already_provided(&mut self, o: Unit) {
        let uid = self.get_or_insert_atomic(&o, true);
        self.provided_by_ext
            .insert(uid, WithDefSite::try_with(&(), o));
    }

    /// Verify that a given `Unit` has already been provided by
    /// some `Obj`.
    ///
    /// # Errors
    /// This fails if the `Unit` has not been defined.
    /// The verification is not deferred and will immediately error
    /// if the `Unit` in question has not already been encountered,
    /// so this method should be called after all insertions are
    /// complete.
    pub fn must_provide(&mut self, eaccum: &mut EAccum, o: &Unit) -> Option<()> {
        let uid = self.get_or_insert_atomic(o, false);
        if self.provided_for_ext.contains(&uid) {
            Some(())
        } else {
            eaccum.error(err::GraphUnitUndeclared { unit: &o })
        }
    }

    /// Declare a new set of constraints.
    pub fn insert(&mut self, t: Obj) {
        let mut provide_units = Vec::new();
        let mut require_units = Vec::new();
        let mut provide_uids = Vec::new();
        let mut require_uids = Vec::new();
        t.provides(&mut provide_units);
        t.requires(&mut require_units);
        for prov in provide_units {
            let uid = self.get_or_insert_atomic(&prov, true);
            provide_uids.push(WithDefSite::try_with(&uid, prov));
            self.provided_for_ext.insert(uid);
        }
        for req in require_units {
            let uid = self.get_or_insert_atomic(&req, false);
            require_uids.push(WithDefSite::try_with(&uid, req));
        }
        err::consistency!(
            self.elements.len() == self.provide.len() && self.elements.len() == self.require.len(),
            "Internal arrays of Graph must have matching lengths"
        );
        self.provide.push(provide_uids);
        self.require.push(require_uids);
        self.elements.push(t);
    }

    /// Resolve all previously inserted constraints and return a scheduling
    /// without temporal inconsistencies.
    ///
    /// # Errors
    /// Fails if a cycle is encountered.
    pub fn scheduling(self, eaccum: &mut EAccum) -> Option<Vec<Obj>> {
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
            let mut provider: Vec<Option<usize>> = vec![None; nb];
            // `constraints[i]` is the list of `Unit`s that `i` depends on.
            let mut constraints: Vec<Vec<WithDefSite<usize>>> = vec![vec![]; nb];
            for (id, provs) in self.provide.iter().enumerate() {
                for p in provs {
                    // First we check that the `Unit` is provided only once.
                    // This involves both `provided_by_ext` and `provider`
                    // currently being computed.
                    if let Some(prior) = self.provided_by_ext.get(&p.contents) {
                        eaccum.error(err::GraphUnitDeclTwice {
                            unit: &at!(self.atomics.0, p.contents).contents,
                            prior: "the context",
                            new_site: &at!(self.elements, id),
                            prior_site: prior.def_site,
                        })?;
                    }
                    if let Some(prov) = at!(provider, p.contents) {
                        eaccum.error(err::GraphUnitDeclTwice {
                            unit: &at!(self.atomics.0, p.contents).contents,
                            prior: "a prior item",
                            new_site: &at!(self.elements, id),
                            prior_site: &at!(self.elements, *prov),
                        })?;
                    }
                    // Constraint is valid, record its provider.
                    *at!(mut provider, p.contents) = Some(id);
                    // Check for cycles of length 1.
                    // This is the best time to do it since the default SCC algorithm
                    // is fine with connected components of size 1.
                    for r in at!(self.require, id) {
                        if r.contents == p.contents {
                            eaccum.error(err::GraphUnitDependsOnItself {
                                unit: &at!(self.atomics.0, p.contents).contents,
                                def_site: at!(self.elements, id),
                                usage: r.def_site,
                            })?;
                        }
                        at!(mut constraints, p.contents).push(*r);
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
            let mut expl = explore::SccExplorer::for_graph_of_size(nb);
            for v in 0..nb {
                if expl.unexplored(v) {
                    expl.run(v, &constraints);
                }
            }
            expl.extract()
        };
        // Check that all |scc| = 1
        let mut schedule = Vec::new();
        // We're going to remove all elements one at a time, let's make that easier.
        let mut unused: Vec<Option<Obj>> = self.elements.into_iter().map(Some).collect();
        for c in &scc {
            err::consistency!(
                !c.is_empty(),
                "It should be impossible to have SCCs of size zero"
            );
            if c.len() > 1 {
                // Oops, here's a loop.
                let looping = c.iter().map(|l| at!(self.atomics.0, *l).elements());
                eaccum.error(err::Cycle { items: looping })?;
            }
            // Correctly of size 1, we can use it next.
            let Some(&id) = c.last() else {
                err::malformed!();
            };
            // This is faillible because we have `Unit`s that are not provided by anyone.
            if let Some(Some(prov)) = provider.get(id) {
                if let Some(obj) = at!(mut unused, *prov).take() {
                    schedule.push(obj);
                }
            }
        }
        for o in unused.into_iter().flatten() {
            // These are statements that provide nothing, but they could require things.
            // e.g. assertions. Let's put them last.
            schedule.push(o);
        }
        Some(schedule)
    }
}

#[cfg(test)]
mod test {
    use std::fmt;

    use super::super::depends::Depends;
    use super::Graph;
    use chandeliers_err::{self as err, EAccum};

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

    impl err::TrySpan for Triplet {} // Default impl is None
    impl err::TrySpan for Unit {}

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

    fn vec_proj1<T, U>(v: Vec<(T, U)>) -> Vec<T> {
        v.into_iter().map(|(t, _)| t).collect()
    }

    fn schedule(pairs: Vec<Triplet>) -> Result<Vec<Triplet>, Vec<String>> {
        let mut eaccum = EAccum::default();
        let mut g = Graph::default();
        for pair in pairs {
            g.insert(pair);
        }
        let sched = g.scheduling(&mut eaccum);
        sched.ok_or_else(|| vec_proj1(eaccum.fetch().0.into_iter().next().unwrap()))
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
            Err(vec![
                String::from("(a) depends on itself"),
                String::from("used here within its own definition")
            ]),
        );

        assert_eq!(
            schedule(vec![Triplet('a', 'b', 'c'), Triplet('b', 'a', 'c')]),
            Err(vec![
                String::from("(b) was found to be part of a dependency cycle"),
                String::from("The cycle also goes through (a)")
            ]),
        );
    }
}
