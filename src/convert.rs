use crate::core::*;

use indexmap::IndexMap;

impl TryFrom<jsongraph::Graph> for Graph {
    type Error = String;

    fn try_from(value: jsongraph::Graph) -> Result<Self, Self::Error> {
        let jgf_nodes = value.nodes.ok_or("Missing graph nodes")?;
        let jgf_edges = value.edges.ok_or("Missing graph edges")?;

        let goal = value
            .metadata
            .ok_or("Missing graph metadata")?
            .get("goal")
            .ok_or("Missing 'goal' metadata for graph")?
            .as_str()
            .ok_or("'goal' metadata for graph is not a string")?
            .to_owned();

        let mut nodes = vec![];

        for (node_id, node_val) in jgf_nodes {
            let metadata = node_val
                .metadata
                .ok_or(format!("Missing metadata for node '{}'", node_id))?;
            let kind = match metadata
                .get("kind")
                .ok_or(format!(
                    "Missing 'kind' metadata for node '{}'",
                    node_id
                ))?
                .as_str()
                .ok_or(format!(
                    "'kind' metadata for node '{}' is not a string",
                    node_id
                ))?
                .to_ascii_uppercase()
                .as_str()
            {
                "AND" => NodeKind::And,
                "OR" => NodeKind::Or,
                k => {
                    return Err(format!(
                        "Unknown 'kind' metadata '{}' for node '{}'",
                        k, node_id
                    ));
                }
            };
            nodes.push(Node::new(node_id, node_val.label, kind));
        }

        Ok(Graph::new(
            nodes.into_iter(),
            jgf_edges.into_iter().map(|e| (e.source, e.target)),
            &goal,
        )?)
    }
}

impl TryFrom<Graph> for jsongraph::Graph {
    type Error = String;

    fn try_from(ao: Graph) -> Result<Self, Self::Error> {
        let mut nodes = IndexMap::new();

        for node in ao.nodes() {
            nodes.insert(
                node.id().to_owned(),
                jsongraph::Node {
                    label: node.label().map(|x| x.to_owned()),
                    metadata: Some(IndexMap::from([(
                        "kind".to_owned(),
                        serde_json::Value::String(node.kind().to_string()),
                    )])),
                },
            );
        }

        Ok(jsongraph::Graph {
            id: None,
            label: None,
            directed: true,
            graph_type: None,
            metadata: Some(IndexMap::from([(
                "goal".to_owned(),
                serde_json::Value::String(ao.or_at(ao.goal()).id().to_owned()),
            )])),
            nodes: Some(nodes),
            edges: Some(
                ao.edges()
                    .map(|(source, target)| jsongraph::Edge {
                        id: None,
                        source: source.id().to_owned(),
                        target: target.id().to_owned(),
                        relation: None,
                        directed: true,
                        label: None,
                        metadata: None,
                    })
                    .collect(),
            ),
        })
    }
}
