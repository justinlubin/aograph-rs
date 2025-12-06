use crate::core::*;
use indexmap::IndexSet;
use std::collections::HashMap;

// Uses forward chaining
// Reference: https://courses.cs.washington.edu/courses/cse473/12au/slides/lect10.pdf
pub fn provable_or_nodes(graph: &Graph) -> OrSet {
    let mut count: HashMap<AIdx, usize> = HashMap::new();
    let mut inferred: IndexSet<OIdx> = IndexSet::new();
    let mut agenda: Vec<OIdx> =
        graph.sources().map(|aid| graph.conclusion(aid)).collect();

    while let Some(p) = agenda.pop() {
        if !inferred.insert(p) {
            continue;
        }

        for c in graph.consumers(p) {
            *count.entry(c).or_insert_with(|| graph.premises(c).count()) -= 1;

            if count[&c] == 0 {
                agenda.push(graph.conclusion(c))
            }
        }
    }

    OrSet { set: inferred }
}

// TODO switch to using backward reasoning
pub fn provable(graph: &Graph, oid: OIdx) -> bool {
    provable_or_nodes(graph).set.contains(&oid)
}

pub fn reduce(graph: &mut Graph) {
    for oidx in provable_or_nodes(graph).set {
        graph.or_remove(oidx);
    }
}
