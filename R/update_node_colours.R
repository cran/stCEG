#' Update Node Colours in an Event Tree
#'
#' Assigns stage colours to nodes in an event tree or staged tree and returns
#' an updated staged tree object.
#'
#' Nodes assigned the same colour are interpreted as belonging to the same
#' stage. The function validates that nodes sharing a colour also share the
#' same outgoing edge structure, ensuring consistency with staged tree
#' definitions.
#'
#' @param event_tree_obj An object of class \code{"event_tree"} or
#'   \code{"staged_tree"}.
#'
#' @param node_groups A list of character vectors. Each vector contains the
#'   node identifiers belonging to a single stage.
#'
#' @param colours Character vector of colour values corresponding to
#'   \code{node_groups}. The lengths of \code{node_groups} and
#'   \code{colours} must be equal.
#'
#' @param level_separation Numeric value controlling spacing between graph
#'   levels. Included for consistency with earlier versions.
#'
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Included for consistency with earlier versions.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Assigns colours to the specified node groups.
#'   \item Prevents nodes appearing in multiple groups.
#'   \item Computes outgoing edge labels for every node.
#'   \item Verifies that nodes sharing a colour have identical outgoing edge
#'   labels.
#'   \item Returns the result as an object of class
#'   \code{"staged_tree"}.
#' }
#'
#' If a colour is assigned to nodes with different outgoing edge structures,
#' an error is produced.
#'
#' @return
#' An object of class \code{"staged_tree"} containing:
#' \itemize{
#'   \item \code{nodes}: node information including assigned colours.
#'   \item \code{edges}: edge information inherited from the original tree.
#'   \item \code{data}: the original dataset.
#'   \item \code{metadata}: staged tree summary information.
#' }
#'
#' @examples
#'
#' et <- create_event_tree(homicides, c(1:3))
#'
#' st <- update_node_colours(
#'   et,
#'   node_groups = list(c("s1", "s2")),
#'   colours = "#BBA0CA"
#' )
#'
#' print(st)
#' summary(st)
#' plot(st)
#'
#'
#' @seealso
#' \code{\link{create_event_tree}},
#' \code{\link{create_staged_tree}},
#' \code{\link{ahc_colouring}}
#'
#' @export
update_node_colours <- function(event_tree_obj,
                                node_groups,
                                colours,
                                level_separation = 1000,
                                node_distance   = 300) {

  # Correct S3 extraction
  if (inherits(event_tree_obj, "event_tree")) {
    nodes <- dplyr::as_tibble(event_tree_obj$nodes)
    edges <- dplyr::as_tibble(event_tree_obj$edges)
    filtereddf <- event_tree_obj$data

  } else if (inherits(event_tree_obj, "staged_tree")) {
    nodes <- dplyr::as_tibble(event_tree_obj$nodes)
    edges <- dplyr::as_tibble(event_tree_obj$edges)
    filtereddf <- event_tree_obj$data

  } else {
    stop("Input must be an event_tree or staged_tree object.")
  }

  nodes$number <- 1

  # Length check
  if (length(node_groups) != length(colours)) {
    stop("Length of node_groups must match length of colours.")
  }

  node_groups <- lapply(node_groups, function(x) {
    if (length(x) == 1 && grepl(",", x)) {
      trimws(strsplit(x, ",")[[1]])
    } else {
      x
    }
  })

  # Duplicate check
  all_node_ids <- unlist(node_groups)
  #print("all_node_ids")
  #print(all_node_ids)
  duplicate_nodes <- all_node_ids[duplicated(all_node_ids)]
  if (length(duplicate_nodes) > 0) {
    stop(
      paste(
        "Error: The following node IDs appear in multiple node_groups:",
        paste(duplicate_nodes, collapse = ", ")
      )
    )
  }

  # Apply colours
  for (i in seq_along(node_groups)) {
    nodes$color[nodes$id %in% node_groups[[i]]] <- colours[i]
    nodes$method[nodes$id %in% node_groups[[i]]] <- "manual"
  }
  #print("nodes")
  #print(nodes)
  nodes <- nodes %>% dplyr::select(-dplyr::any_of("outgoing_labels"))
  nodes <- nodes %>% dplyr::select(-dplyr::any_of("outgoing_edges"))
  nodes <- nodes %>% dplyr::select(-dplyr::any_of("outgoing_edges2"))
  # Outgoing edge labels
  outgoing_edges_labels <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::summarize(
      outgoing_labels = paste(sort(unique(label1)), collapse = ","),
      outgoing_edges2 = dplyr::n(),
      .groups = "drop"
    )

  nodes <- dplyr::left_join(nodes, outgoing_edges_labels, by = c("id" = "from"))
  #print("nodes")
  #print(nodes)
  # Conflict check
  nodes <- dplyr::as_tibble(nodes)


  conflicting_nodes <- nodes %>%
    dplyr::filter(color != "#FFFFFF") %>%
    dplyr::group_by(color) %>%
    dplyr::filter(dplyr::n_distinct(outgoing_labels) > 1) %>%
    dplyr::pull(id) %>%
    unique()

  if (length(conflicting_nodes) > 0) {
    stop(
      paste(
        "Error: The following nodes have the same colour but different outgoing edge labels:",
        paste(conflicting_nodes, collapse = ", ")
      )
    )
  }

  # Tag method


  # Build staged_tree object (data only; plotting via plot.staged_tree)
  staged_tree <- create_staged_tree(
    nodes      = nodes,
    edges      = edges,
    filtereddf = filtereddf,
    method     = "manual"
  )

  return(staged_tree)
}

