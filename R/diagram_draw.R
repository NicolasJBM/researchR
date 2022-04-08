#' @name diagram_draw
#' @title Create a diagram
#' @author Nicolas Mangin
#' @description Function creating a diagram object from tibbles specifying nodes, relations, and moderations.
#' @param nodes       Tibble. Nodes with their properties.
#' @param relations   Tibble. Edges with their properties.
#' @param moderations Tibble. Moderations with their properties.
#' @return A DiagrammeR object ready for rendering.
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom tibble rowid_to_column
#' @importFrom DiagrammeR add_node
#' @importFrom DiagrammeR node_aes
#' @importFrom DiagrammeR create_graph
#' @importFrom DiagrammeR add_edge
#' @importFrom DiagrammeR edge_aes
#' @export

diagram_draw <- function(nodes, relations, moderations) {

  # Bind variables
  from_x <- NULL
  from_y <- NULL
  include <- NULL
  label <- NULL
  node_id <- NULL
  relation <- NULL
  relation_id <- NULL
  target <- NULL
  to_x <- NULL
  to_y <- NULL
  x <- NULL
  y <- NULL
  
  # Necessary information to tables for connections and create graph:

  nodes <- nodes |>
    dplyr::mutate_if(is.factor, as.character) |>
    dplyr::filter(include == TRUE) |>
    tibble::rowid_to_column("node_id")

  
  if (nrow(relations) > 0) {
    relations <- relations |>
      dplyr::mutate_if(is.factor, as.character) |>
      dplyr::filter(include == TRUE) |>
      tibble::rowid_to_column(var = "relation_id") |>
      dplyr::left_join(
        dplyr::select(nodes, source = label, from = node_id, from_x = x, from_y = y),
        by = "source"
      ) |>
      dplyr::left_join(
        dplyr::select(nodes, target = label, to = node_id, to_x = x, to_y = y),
        by = "target"
      ) |>
      dplyr::mutate(
        by = nrow(nodes) + relation_id,
        by_x = (from_x + to_x) / 2,
        by_y = (from_y + to_y) / 2
      )
  }

  if (nrow(moderations) > 0) {
    moderations <- moderations |>
      dplyr::mutate_if(is.factor, as.character) |>
      dplyr::filter(include == TRUE) |>
      dplyr::left_join(
        dplyr::select(nodes, source = label, from = node_id),
        by = "source"
      ) |>
      dplyr::left_join(
        dplyr::select(relations, target = relation, to = by),
        by = "target"
      ) |>
      dplyr::select(-source, -target)
  }

  graph <- DiagrammeR::create_graph()


  # Add the nodes:

  for (i in 1:nrow(nodes)) {
    graph <- DiagrammeR::add_node(
      graph,
      label = nodes$label[[i]],
      node_aes = DiagrammeR::node_aes(
        shape = nodes$shape[[i]],
        x = nodes$x[[i]],
        y = nodes$y[[i]],
        width = nodes$width[[i]],
        height = nodes$height[[i]],
        penwidth = nodes$penwidth[[i]],
        color = nodes$color[[i]],
        fillcolor = nodes$fillcolor[[i]],
        fontsize = nodes$fontsize[[i]],
        fontcolor = nodes$fontcolor[[i]]
      )
    )
  }

  # Add the junctions between nodes:

  if (nrow(relations) > 0){
    for (i in 1:nrow(relations)) {
      graph <- DiagrammeR::add_node(
        graph,
        label = "",
        node_aes = DiagrammeR::node_aes(
          x = relations$by_x[[i]],
          y = relations$by_y[[i]],
          width = 0,
          height = 0,
          penwidth = 0,
          color = "grey50",
          fillcolor = "grey50"
        )
      )
    }
  }

  # Add the relationships:

  if (nrow(relations) > 0){
    for (i in 1:nrow(relations)) {
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = relations$from[[i]],
        to = relations$by[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = relations$style[[i]],
          color = relations$color[[i]],
          fontcolor = relations$fontcolor[[i]],
          fontsize = relations$fontsize[[i]],
          penwidth = relations$penwidth[[i]],
          arrowhead = "none",
          label = relations$label[[i]]
        )
      )
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = relations$by[[i]],
        to = relations$to[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = relations$style[[i]],
          color = relations$color[[i]],
          penwidth = relations$penwidth[[i]],
          arrowhead = relations$arrowhead[[i]],
          label = ""
        )
      )
    }
  }

  # Add the moderations:

  if (nrow(moderations) > 0) {
    for (i in 1:nrow(moderations)) {
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = moderations$from[[i]],
        to = moderations$to[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = moderations$style[[i]],
          color = moderations$color[[i]],
          fontcolor = moderations$fontcolor[[i]],
          fontsize = moderations$fontsize[[i]],
          penwidth = moderations$penwidth[[i]],
          arrowhead = moderations$arrowhead[[i]],
          label = moderations$label[[i]]
        )
      )
    }
  }

  return(graph)
}
