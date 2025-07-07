#' Update Node Colours in an Event Tree
#'
#' This function updates the colours of nodes in an event tree based on user-defined
#' groups while ensuring that nodes with the same colour have consistent outgoing edge labels.
#'
#' @param event_tree_obj A list containing the event tree object (possibly with node deletion). It should include:
#'   - `$eventtree$x$nodes`: A data frame of nodes.
#'   - `$eventtree$x$edges`: A data frame of edges.
#'   - `$filtereddf`: A filtered data frame associated with the event tree.
#' @param node_groups A list of character vectors, where each vector contains node IDs belonging to a specific group.
#' @param colours A character vector of colour codes corresponding to each node group. The length of `colours` must match `node_groups`.
#' @param level_separation Numeric value specifying the spacing between levels in the hierarchical layout.
#'   Default is `1000`.
#' @param node_distance Numeric value specifying the distance between nodes in the layout.
#'   Default is `300`.
#'
#' @return A list containing:
#'   - `$stagedtree`: A `visNetwork` object representing the updated event tree with coloured nodes.
#'   - `$filtereddf`: The filtered data frame, returned invisibly.
#'
#' @details
#' The function follows these steps:
#' - Assigns colours to nodes based on the specified `node_groups`.
#' - Ensures that no node appears in multiple groups (raises an error if duplicates exist).
#' - Checks that all nodes with the same colour have identical outgoing edge labels.
#' - Updates the event tree visualization using `visNetwork`.
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' event_tree
#'
#' # Define node groups and colours
#' node_groups <- list(c("s1", "s2"), c("s3", "s4"))
#' colours <- c("#BBA0CA", "#8AC6D0")
#'
#' # Apply colours to the event tree
#' coloured_tree <- update_node_colours(event_tree, node_groups, colours)
#' coloured_tree
#'
#' @import visNetwork
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @export
update_node_colours <- function(event_tree_obj, node_groups, colours, level_separation = 1000, node_distance = 300) {

  if (!is.null(event_tree_obj$eventtree)) {
    tree <- event_tree_obj$eventtree
  } else if (!is.null(event_tree_obj$stagedtree)) {
    tree <- event_tree_obj$stagedtree
  } else {
    stop("Neither eventtree nor stagedtree exists in delete_nodes_obj")
  }

  # Extract nodes and edges
  nodes2 <- tree$x$nodes
  edges2 <- tree$x$edges
  nodes2$number <- 1

  filtereddf <- event_tree_obj$filtereddf

  # Check that node_groups and colours are the same length
  if (length(node_groups) != length(colours)) {
    stop("Length of node_groups must match length of colours")
  }

  # Flatten the node_groups to a single vector and check for duplicates
  all_node_ids <- unlist(node_groups)
  duplicate_nodes <- all_node_ids[duplicated(all_node_ids)]

  # Raise an error if there are duplicate node IDs across groups
  if (length(duplicate_nodes) > 0) {
    stop(paste("Error: The following node IDs appear in multiple node_groups:",
               paste(duplicate_nodes, collapse = ", ")))
  }

  # Apply each colour to its corresponding group
  for (i in seq_along(node_groups)) {
    nodes2$color[nodes2$id %in% node_groups[[i]]] <- colours[i]
  }

  # Create a dataframe of outgoing edge labels for each node
  outgoing_edges_labels <- edges2 %>%
    group_by(from) %>%
    summarize(outgoing_labels = paste(sort(unique(label1)), collapse = ","), outgoing_edges2 = n(), .groups = "drop")

  # Merge outgoing edge labels with nodes data
  nodes2 <- left_join(nodes2, outgoing_edges_labels, by = c("id" = "from"))

  # Check if the columns exist before mutating
  if ("outgoing_labels.x" %in% colnames(nodes2) & "outgoing_labels.y" %in% colnames(nodes2)) {
    nodes2 <- nodes2 %>%
      mutate(outgoing_labels = coalesce(outgoing_labels.x, outgoing_labels.y)) %>%
      select(-outgoing_labels.x, -outgoing_labels.y)
  }

  if ("outgoing_edges2.x" %in% colnames(nodes2) & "outgoing_edges2.y" %in% colnames(nodes2)) {
    nodes2 <- nodes2 %>%
      mutate(outgoing_edges2 = coalesce(outgoing_edges2.x, outgoing_edges2.y)) %>%
      select(-outgoing_edges2.x, -outgoing_edges2.y)
  }

  # Check for conflicts: Nodes with the same colour but different outgoing edge labels
  conflicting_nodes <- nodes2 %>%
    filter(color != "#FFFFFF") %>%  # Ignore white-coloured nodes
    group_by(color) %>%
    filter(n_distinct(outgoing_labels) > 1) %>%
    pull(id) %>%
    unique()

  # Raise an error if any conflicts exist
  if (length(conflicting_nodes) > 0) {
    stop(paste("Error: The following nodes have the same colour but different outgoing edge labels:",
               paste(conflicting_nodes, collapse = ", ")))
  }

  # Ensure nodes are movable in Y direction but fixed in X
  nodes2 <- nodes2 %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))

  # Return the updated visNetwork plot
  network_plot <- visNetwork(nodes = nodes2, edges = edges2) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
    visOptions(
      manipulation = list(
        enabled = FALSE,
        addEdgeCols = FALSE,
        addNodeCols = FALSE,
        editEdgeCols = FALSE,
        editNodeCols = c("color"),
        multiselect = TRUE
      ),
      nodesIdSelection = FALSE
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      multiselect = TRUE,
      navigationButtons = TRUE
    ) %>%
    visPhysics(hierarchicalRepulsion = list(nodeDistance = node_distance), stabilization = TRUE) %>%
    visEvents(
      selectNode = "function(params) { /* Node selection code */ }",
      deselectNode = "function(params) { /* Deselect code */ }"
    ) %>%
    visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")

  # Create the result list (with invisible filtereddf)
  result <- invisible(filtereddf)


  # Return both the network plot and the result
  output <- list(stagedtree = network_plot, filtereddf = result)

  class(output) <- "staged_tree"
  return(output)

}
