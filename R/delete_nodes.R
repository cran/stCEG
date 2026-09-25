#' Compute an Event Tree After Node Deletion
#'
#' Creates a modified event tree by removing one or more specified nodes and
#' reconnecting the remaining tree structure.
#'
#' For each deleted node, incoming edges are redirected to its descendants so
#' that paths through the tree remain connected where possible. After
#' deletion, edge counts are aggregated, orphaned nodes are removed, and node
#' identifiers are renumbered sequentially.
#'
#' The function accepts both \code{"event_tree"} and \code{"staged_tree"}
#' objects and returns the modified structure as a new event tree.
#'
#' @param tree_obj An object of class \code{"event_tree"} or
#'   \code{"staged_tree"}.
#'
#' @param nodes_to_delete Character vector containing the node IDs to remove.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Identifies the specified nodes for deletion.
#'   \item Redirects incoming edges to the deleted node's descendants.
#'   \item Removes deleted nodes and associated edges.
#'   \item Aggregates duplicate edges sharing the same originating node and
#'   edge label.
#'   \item Removes unused nodes.
#'   \item Renumbers remaining nodes sequentially.
#' }
#'
#' The returned object contains the modified graph structure while preserving
#' the original dataset.
#'
#' @return
#' An object of class \code{"event_tree"} containing:
#' \itemize{
#'   \item \code{nodes}: updated node information.
#'   \item \code{edges}: updated edge information.
#'   \item \code{data}: the original dataset.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#'
#' modified_et <- compute_deleted_nodes(et, nodes_to_delete = c("s11", "s12"))
#'
#' print(modified_et)
#' summary(modified_et)
#' plot(modified_et)
#'
#'
#' @seealso
#' \code{\link{create_event_tree}},
#' \code{\link{plot.event_tree}}
#'
#' @export
compute_deleted_nodes <- function(tree_obj, nodes_to_delete) {

  # Accept both event_tree and staged_tree
  if (!inherits(tree_obj, "event_tree") &&
      !inherits(tree_obj, "staged_tree")) {
    stop("Input must be an object of class 'event_tree' or 'staged_tree'.")
  }

  if (length(nodes_to_delete) == 0) {
    stop("No nodes specified for deletion.")
  }

  nodes <- tree_obj$nodes
  edges <- tree_obj$edges



  ## ----------------------------------------------------------------------
  ## Helper: redirect edges around deleted node
  ## ----------------------------------------------------------------------

  redirect_edges <- function(node_id, nodes, edges) {

    outgoing <- edges[edges$from == node_id, ]
    incoming <- edges[edges$to == node_id, ]

    if (nrow(outgoing) == 0 || nrow(incoming) == 0) {
      return(edges)
    }

    new_edges <- edges

    for (i in seq_len(nrow(outgoing))) {
      for (j in seq_len(nrow(incoming))) {

        ne <- outgoing[i, ]
        ne$from <- incoming$from[j]
        ne$to   <- outgoing$to[i]

        missing <- setdiff(names(edges), names(ne))
        ne[missing] <- NA

        new_edges <- rbind(new_edges, ne)
      }
    }

    new_edges <- new_edges[new_edges$from != node_id & new_edges$to != node_id, ]
    new_edges
  }

  ## ----------------------------------------------------------------------
  ## Delete nodes sequentially
  ## ----------------------------------------------------------------------

  for (node in nodes_to_delete) {

    edges <- redirect_edges(node, nodes, edges)

    nodes <- nodes[nodes$id != node, ]
    edges <- edges[edges$from != node & edges$to != node, ]
  }

  ## ----------------------------------------------------------------------
  ## Remove orphan nodes (no incoming AND no outgoing edges)
  ## ----------------------------------------------------------------------

  ## ----------------------------------------------------------------------
  ## Merge edges with same from + label1
  ## ----------------------------------------------------------------------

  edges <- edges %>%
    dplyr::group_by(from, label1) %>%
    dplyr::summarise(
      label2 = sum(as.numeric(label2), na.rm = TRUE),
      label3 = paste(dplyr::first(label1), "\n", label2),
      label  = label3,
      to = dplyr::first(to),
      arrows = dplyr::first(arrows),
      color = dplyr::first(color),
      .groups = "drop"
    ) %>%
    dplyr::distinct()

  used <- unique(c(edges$from, edges$to))
  nodes <- nodes[nodes$id %in% used, ]

  ## ----------------------------------------------------------------------
  ## Reassign node IDs sequentially
  ## ----------------------------------------------------------------------

  old_ids <- nodes$id
  new_ids <- paste0("s", seq_len(nrow(nodes)) - 1)
  id_map <- setNames(new_ids, old_ids)

  nodes$id <- id_map[nodes$id]
  nodes$label <- nodes$id

  edges$from <- id_map[edges$from]
  edges$to   <- id_map[edges$to]
  ## ----------------------------------------------------------------------
  ## Return updated event tree object
  ## ----------------------------------------------------------------------

  structure(
    list(
      nodes = nodes,
      edges = edges,
      data = tree_obj$data,
      variables = tree_obj$variables,
      n_nodes = tree_obj$n_nodes,
      n_edges = tree_obj$n_edges
    ),
    class = "event_tree"
  )
}


