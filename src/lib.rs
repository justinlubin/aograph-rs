//! AND-OR graph data structures and algorithms.
//!
//! The main data structure, [`Graph`], is a bipartite graph of AND nodes
//! (representing _rules_ in a proof system) and OR nodes (representing
//! _propositions_ in a proof system). [`Graph`]s have a designated _goal_ OR
//! node that represents the goal to be proven.
//!
//! A convenient way to construct and store these graphs is to use the
//! [`From<jsongraph::Graph>`] trait implementation in [`Graph`]. Otherwise,
//! AND-OR graphs can be created with [`Graph::new`].

/// Algorithms for operating on AND-OR graphs (e.g. provability, simplification)
pub mod algo;

mod convert;
mod core;

pub use core::*;
