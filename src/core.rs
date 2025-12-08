use indexmap::{IndexMap, IndexSet};
use petgraph::Direction;
use petgraph::stable_graph as pg;
use petgraph::visit::{EdgeRef, IntoEdgeReferences};
use std::collections::HashMap;
use std::fmt;

////////////////////////////////////////////////////////////////////////////////
// Nodes

/// Node identifiers (must be unique within a graph)
pub type NodeId = String;

/// &nodeid is a reference to a node identifier
#[allow(non_camel_case_types)]
pub type nodeid = str;

/// Indicates whether a node is an AND node or an OR node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    And,
    Or,
}

impl fmt::Display for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::And => write!(f, "AND"),
            Self::Or => write!(f, "OR"),
        }
    }
}

/// A node in an AND-OR graph
#[derive(Debug, Clone)]
pub struct Node {
    id: NodeId,
    label: Option<String>,
    kind: NodeKind,
}

impl Node {
    /// Constructs a new node
    pub fn new(id: NodeId, label: Option<String>, kind: NodeKind) -> Self {
        Self { id, label, kind }
    }

    /// Returns the node's identifier
    pub fn id(&self) -> &nodeid {
        &self.id
    }

    /// Returns a reference to the node's label, if it exists
    pub fn label(&self) -> Option<&nodeid> {
        self.label.as_ref().map(|x| x.as_str())
    }

    /// Returns the node's kind
    pub fn kind(&self) -> NodeKind {
        self.kind
    }

    /// Returns true iff the node is an AND node
    pub fn is_and(&self) -> bool {
        self.kind == NodeKind::And
    }

    /// Returns true iff the node is an OR node
    pub fn is_or(&self) -> bool {
        self.kind == NodeKind::Or
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.label() {
            Some(s) => write!(f, "{}", s),
            None => write!(f, "{}", self.id()),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Edges

#[derive(Debug, Clone)]
struct Edge;

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

////////////////////////////////////////////////////////////////////////////////
// Graphs

/// An AND-OR graph (main data structure for this crate)
///
/// AND nodes are referenced by [`AIdx`] indexes and OR nodes are referenced by
/// [`OIdx`] indexes. An edge exists from an AND node _A_ to an OR node _O_ if
/// _O_ is a premise of _A_. An edge exists from an OR node _O_ to an AND node
/// _A_ if the conclusion of _A_ is _O_.
///
/// OR nodes can have any number of children (providers) and parents
/// (consumders). AND nodes can have any number of children (premises), but
/// only one parent (conclusion).
#[derive(Debug, Clone)]
pub struct Graph {
    pg: pg::StableGraph<Node, Edge>,
    goal: pg::NodeIndex,
}

/// An index to an AND node in an AND-OR graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AIdx(pg::NodeIndex);

/// An index to an OR node in an AND-OR graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OIdx(pg::NodeIndex);

/// # Creating an AND-OR graph
impl Graph {
    /// Creates a new AND-OR graph
    ///
    /// This function performs a variety of safety checks to ensure that
    /// a graph is returned only if it is valid (e.g., bipartite, no duplicate
    /// node identifiers, etc.).
    pub fn new(
        nodes: impl Iterator<Item = Node>,
        edges: impl Iterator<Item = (NodeId, NodeId)>,
        goal: &nodeid,
    ) -> Result<Self, String> {
        let mut pg = pg::StableGraph::new();

        let mut translation = IndexMap::new();

        for node in nodes {
            let nid = node.id.to_owned();
            if translation.contains_key(&nid) {
                return Err(format!("Duplicate node '{}'", nid));
            }
            let pid = pg.add_node(node);
            translation.insert(nid, pid);
        }

        for (source_nid, target_nid) in edges {
            let source_pid = *translation
                .get(&source_nid)
                .ok_or(format!("Source '{}' is not a node", source_nid))?;

            let target_pid = *translation
                .get(&target_nid)
                .ok_or(format!("Target '{}' is not a node", target_nid))?;

            match (&pg[source_pid].kind, &pg[target_pid].kind) {
                (NodeKind::And, NodeKind::And) => {
                    return Err(format!(
                        "Cannot connect AND node '{}' to AND node '{}'",
                        source_nid, target_nid
                    ));
                }
                (NodeKind::Or, NodeKind::Or) => {
                    return Err(format!(
                        "Cannot connect Or node '{}' to Or node '{}'",
                        source_nid, target_nid
                    ));
                }
                (NodeKind::And, NodeKind::Or)
                | (NodeKind::Or, NodeKind::And) => (),
            };

            let _ = pg.add_edge(source_pid, target_pid, Edge);
        }

        let goal = match translation.get(goal).map(|pid| (pid, pg[*pid].kind)) {
            Some((pid, NodeKind::Or)) => *pid,
            _ => return Err(format!("Goal node '{}' is not an OR node", goal)),
        };

        for pid in pg.node_indices() {
            let node = &pg[pid];
            if !node.is_and() {
                continue;
            }
            let mut it = pg.edges_directed(pid, Direction::Incoming);
            if !it.next().is_some() {
                return Err(format!(
                    "AND node '{}' has no conclusion",
                    node.id
                ));
            }
            if it.next().is_some() {
                return Err(format!(
                    "AND node '{}' has more than one conclusion",
                    node.id
                ));
            }
        }

        Ok(Self { pg, goal })
    }
}

/// # Basic operations
impl Graph {
    /// Returns an iterator of indexes to all OR nodes in the AND-OR graph
    pub fn or_indexes(&self) -> impl Iterator<Item = OIdx> + '_ {
        self.pg.node_indices().filter_map(|pid| {
            if self.pg[pid].is_or() {
                Some(OIdx(pid))
            } else {
                None
            }
        })
    }

    /// Returns an iterator of indexes to all AND nodes in the AND-OR graph
    pub fn and_indexes(&self) -> impl Iterator<Item = AIdx> + '_ {
        self.pg.node_indices().filter_map(|pid| {
            if self.pg[pid].is_and() {
                Some(AIdx(pid))
            } else {
                None
            }
        })
    }

    /// Returns an iterator of all nodes in the AND-OR graph
    pub fn nodes(&self) -> impl Iterator<Item = &Node> {
        self.pg.node_weights()
    }

    /// Returns an iterator of all edges in the AND-OR graph
    pub fn edges(&self) -> impl Iterator<Item = (&Node, &Node)> {
        self.pg
            .edge_references()
            .map(|e| (&self.pg[e.source()], &self.pg[e.target()]))
    }

    /// Returns an index to the goal OR node in the AND-OR graph
    pub fn goal(&self) -> OIdx {
        OIdx(self.goal)
    }

    /// Returns all AND nodes in the graph with no premises (the unconditionally
    /// true AND nodes)
    pub fn sources(&self) -> impl Iterator<Item = AIdx> + '_ {
        self.pg.externals(Direction::Outgoing).filter_map(|pid| {
            let node = &self.pg[pid];
            if node.is_and() { Some(AIdx(pid)) } else { None }
        })
    }

    /// Returns all AND nodes in the graph with no premises (the unconditionally
    /// true AND nodes)
    pub fn or_leaves(&self) -> impl Iterator<Item = OIdx> + '_ {
        self.pg.externals(Direction::Outgoing).filter_map(|pid| {
            let node = &self.pg[pid];
            if node.is_or() { Some(OIdx(pid)) } else { None }
        })
    }

    /// Returns an index to an OR node given a node identifier, if that
    /// identifier can be found
    pub fn find_or_by_id(&self, id: &nodeid) -> Option<OIdx> {
        self.pg.node_indices().find_map(|pid| {
            let n = &self.pg[pid];
            if n.is_or() && n.id() == id {
                Some(OIdx(pid))
            } else {
                None
            }
        })
    }
}

/// # Indexing operations
impl Graph {
    /// Returns an AND [`Node`] given an index to it
    pub fn and_at(&self, a: AIdx) -> &Node {
        &self.pg[a.0]
    }

    /// Returns an OR [`Node`] given an index to it
    pub fn or_at(&self, o: OIdx) -> &Node {
        &self.pg[o.0]
    }

    /// Returns the premises of an AND node
    pub fn premises(&self, a: AIdx) -> impl Iterator<Item = OIdx> + '_ {
        self.pg
            .edges_directed(a.0, Direction::Outgoing)
            .map(|er| OIdx(er.target()))
    }

    /// Returns the conclusion of an AND node
    pub fn conclusion(&self, a: AIdx) -> OIdx {
        OIdx(
            self.pg
                .edges_directed(a.0, Direction::Incoming)
                .next()
                .unwrap()
                .source(),
        )
    }

    /// Returns the providers of an OR node
    pub fn providers(&self, o: OIdx) -> impl Iterator<Item = AIdx> + '_ {
        self.pg
            .edges_directed(o.0, Direction::Outgoing)
            .map(|er| AIdx(er.target()))
    }

    /// Returns the consumer of an OR node
    pub fn consumers(&self, o: OIdx) -> impl Iterator<Item = AIdx> + '_ {
        self.pg
            .edges_directed(o.0, Direction::Incoming)
            .map(|er| AIdx(er.source()))
    }

    /// Returns all OR-node descendents of an OR node
    pub fn provider_cone(&self, o: OIdx) -> IndexSet<OIdx> {
        self.providers(o)
            .flat_map(|a| self.premises(a))
            .flat_map(|o| std::iter::once(o).chain(self.provider_cone(o)))
            .collect()
    }
}

/// # Modification operations
impl Graph {
    /// Sets the goal of an AND-OR graph
    pub fn set_goal(&mut self, o: OIdx) {
        self.goal = o.0;
    }

    /// Axiomitizes an OR node by adding an AND node without premises whose
    /// conclusion is the OR node
    pub fn make_axiom(&mut self, o: OIdx) {
        let ax_pid = self.pg.add_node(Node {
            id: format!("ax:{}", self.pg[o.0].id),
            label: None,
            kind: NodeKind::And,
        });
        let _ = self.pg.add_edge(o.0, ax_pid, Edge);
    }

    /// Axiomitizes multiple OR nodes (see [`Graph::make_axiom`])
    pub fn make_axioms(&mut self, oidxs: impl Iterator<Item = OIdx>) {
        for o in oidxs {
            self.make_axiom(o)
        }
    }

    /// Removes an OR node from an AND-OR graph (also removes all AND nodes
    /// whose conclusion is that OR node)
    pub fn or_remove(&mut self, oidx: OIdx) {
        for aidx in self.providers(oidx).collect::<Vec<_>>() {
            self.pg.remove_node(aidx.0);
        }
        self.pg.remove_node(oidx.0);
    }
}

/// DOT formatting
impl Graph {
    fn node_format(
        highlights: &HashMap<OIdx, String>,
        pid: pg::NodeIndex,
        node: &Node,
    ) -> String {
        let base = match node.kind {
            NodeKind::Or => {
                "color=darkslateblue,fontcolor=darkslateblue,penwidth=2"
                    .to_string()
            }
            NodeKind::And => {
                "shape=rectangle,color=gray35,fontcolor=gray35,margin=0"
                    .to_string()
            }
        };
        base + &match highlights.get(&OIdx(pid)) {
            Some(c) => format!(",style=filled,fillcolor={}", c),
            None => "".to_string(),
        }
    }

    /// Returns a representation of the AND-OR graph in the
    /// [DOT](https://graphviz.org/doc/info/lang.html)
    /// markup language (OR nodes are highlighted using the provided colors)
    pub fn dot(&self, highlights: &HashMap<OIdx, String>) -> String {
        let get_node_attrs =
            |_, (pid, node)| Self::node_format(highlights, pid, node);

        let d = petgraph::dot::Dot::with_attr_getters(
            &self.pg,
            &[petgraph::dot::Config::EdgeNoLabel],
            &|g, e| match g[e.source()].kind {
                NodeKind::And => "color=red".to_string(),
                NodeKind::Or => "color=blue, style=dashed".to_string(),
            },
            &get_node_attrs,
        );
        format!("{}", d)
    }
}

////////////////////////////////////////////////////////////////////////////////
// AND sets and OR sets

/// A set of AND nodes in an AND-OR graph
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AndSet {
    pub set: IndexSet<AIdx>,
}

impl AndSet {
    /// Creates a new, empty set of AND nodes
    pub fn new() -> Self {
        AndSet {
            set: IndexSet::new(),
        }
    }

    /// Creates a singleton set from an AND node
    pub fn singleton(aidx: AIdx) -> Self {
        AndSet {
            set: IndexSet::from([aidx]),
        }
    }

    /// Returns the node identifiers of the AND-node set
    pub fn ids(&self, graph: &Graph) -> IndexSet<NodeId> {
        self.set
            .iter()
            .map(|aidx| graph.and_at(*aidx).id.to_owned())
            .collect()
    }

    /// Pretty-prints the AND-node set (gets labels from graph)
    pub fn show(&self, graph: &Graph) -> String {
        if self.set.is_empty() {
            "∅".to_owned()
        } else {
            let mut first = true;
            let mut s = "".to_owned();
            for aidx in &self.set {
                let ax = graph.and_at(*aidx);
                s += &format!("{}{}", if first { "{" } else { ", " }, ax);
                first = false;
            }
            s + "}"
        }
    }
}

/// A set of OR nodes in an AND-OR graph
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrSet {
    pub set: IndexSet<OIdx>,
}

impl OrSet {
    /// Creates a new, empty set of OR nodes
    pub fn new() -> Self {
        OrSet {
            set: IndexSet::new(),
        }
    }

    /// Creates a singleton set from an OR node
    pub fn singleton(oidx: OIdx) -> Self {
        OrSet {
            set: IndexSet::from([oidx]),
        }
    }

    /// Returns the node identifiers of the OR-node set
    pub fn ids(&self, graph: &Graph) -> IndexSet<NodeId> {
        self.set
            .iter()
            .map(|oidx| graph.or_at(*oidx).id.to_owned())
            .collect()
    }

    /// Pretty-prints the OR-node set (gets labels from graph)
    pub fn show(&self, graph: &Graph) -> String {
        if self.set.is_empty() {
            "∅".to_owned()
        } else {
            let mut first = true;
            let mut s = "".to_owned();
            for oidx in &self.set {
                let ax = graph.or_at(*oidx);
                s += &format!("{}{}", if first { "{" } else { ", " }, ax);
                first = false;
            }
            s + "}"
        }
    }
}
