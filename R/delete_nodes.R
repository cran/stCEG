#' Delete Nodes from an Event Tree
#'
#' This function removes specified nodes from an event tree, updating edges to maintain
#' the tree structure while ensuring that node IDs remain sequential.
#'
#' @param event_tree_obj A list containing the event tree object, which includes:
#'   - `$eventtree$x`: A list with `nodes` and `edges` data frames.
#'   - `$filtereddf`: The data frame used to create the event tree.
#' @param nodes_to_delete A character vector of node IDs to delete from the event tree.
#' @param level_separation Numeric value specifying the spacing between levels in the hierarchical layout.
#'   Default is `1000`.
#' @param node_distance Numeric value specifying the distance between nodes in the layout.
#'   Default is `300`.
#'
#' @return A list containing:
#'   - `$eventtree`: A `visNetwork` object representing the updated event tree.
#'   - `$filtereddf`: The filtered data frame, returned invisibly.
#'
#' @details
#' The function performs the following steps:
#' - Identifies outgoing and incoming edges for each node marked for deletion.
#' - Redirects outgoing edges to the sources of the incoming edges.
#' - Updates the edges and nodes data frames to reflect the new connections.
#' - Ensures node IDs are sequentially re-assigned.
#' - Adjusts outgoing edge counts for affected nodes.
#' - Removes orphaned nodes (nodes with no connections).
#' - Returns an updated `visNetwork` visualization of the event tree.
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' event_tree
#'
#' updated_tree <- delete_nodes(event_tree, nodes_to_delete = c("s14", "s18"))
#' updated_tree
#'
#' @import visNetwork
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @export
#'
delete_nodes <- function(event_tree_obj, nodes_to_delete, level_separation = 1000, node_distance = 300) {

  # Extract nodes and edges from the create_event_tree object
  if (!is.null(event_tree_obj$eventtree)) {
    data2 <- event_tree_obj$eventtree$x
  } else if (!is.null(event_tree_obj$stagedtree)) {
    data2 <- event_tree_obj$stagedtree$x
  } else {
    stop("Neither eventtree nor stagedtree exists")
  }

  filtereddf <- event_tree_obj$filtereddf

  if (!is.null(nodes_to_delete) && length(nodes_to_delete) > 0) {
    data_before <- list(nodes = data2$nodes, edges = data2$edges)

    # Loop through selected nodes to delete them
    for (node in nodes_to_delete) {

      # Find the outgoing edges of the node to be deleted
      outgoing_edges <- data2$edges[data2$edges$from == node, ]

      # Find the incoming edges to the node to be deleted
      incoming_edges <- data2$edges[data2$edges$to == node, ]

      if (nrow(outgoing_edges) > 0 && nrow(incoming_edges) > 0) {
        # Redirect the outgoing edges to connect to the source of the incoming edges
        for (i in 1:nrow(outgoing_edges)) {
          for (j in 1:nrow(incoming_edges)) {

            old_edge_indices <- which(data2$edges$to == node)

            new_edge <- data.frame(
              from = incoming_edges$from[j],
              to = outgoing_edges$to[i],
              label = outgoing_edges$label[i],
              label1 = outgoing_edges$label1[i],
              label2 = outgoing_edges$label2[i],
              label3 = outgoing_edges$label3[i],
              arrows = outgoing_edges$arrows[i],
              font.size = outgoing_edges$font.size[i],
              color = "#000000",  # Set colour to black
              stringsAsFactors = FALSE
            )

            # Add any other required columns to the new edge to match the structure of data$edges
            missing_cols <- setdiff(names(data2$edges), names(new_edge))
            new_edge[missing_cols] <- NA  # Assign NA to missing columns if necessary

            for (k in seq_along(old_edge_indices)) {
              data2$edges <- rbind(
                data2$edges[1:(old_edge_indices[k]), ],  # Edges before the old ones
                new_edge,                                     # The new edge
                data2$edges[(old_edge_indices[k] + 1):nrow(data2$edges), ]  # Edges after the old ones
              )
            }

          }
        }
      }

      # Remove the node and its edges (including end nodes)
      data2$nodes <- data2$nodes[data2$nodes$id != node, ]
      data2$edges <- data2$edges[data2$edges$from != node & data2$edges$to != node, ]
    }

    # Update the reactive graph data
    deleted_edges <- setdiff(paste(data_before$edges$from, data_before$edges$to),
                             paste(data2$edges$from, data2$edges$to))

    added_edges <- setdiff(paste(data2$edges$from, data2$edges$to),
                           paste(data_before$edges$from, data_before$edges$to))

    #cat("Edges removed:\n")
    #print(data_before$edges[with(data_before$edges, paste(from, to)) %in% deleted_edges, ])

    #cat("Edges added:\n")
    #print(data2$edges[with(data2$edges, paste(from, to)) %in% added_edges, ])

    # Extract numeric parts of the node IDs
    get_numeric_part <- function(x) as.numeric(gsub("[^0-9]", "", x))

    # Reset the row numbers to match the new order
    rownames(data2$edges) <- seq_len(nrow(data2$edges))

    # Find the unique 'from' nodes in the added edges
    unique_from_nodes <- unique(data2$edges$from[with(data2$edges, paste(from, to)) %in% added_edges])

    # Increment the 'level' for each of these unique nodes in data$nodes
    data2$nodes$level2[data2$nodes$id %in% unique_from_nodes] <- data2$nodes$level2[data2$nodes$id %in% unique_from_nodes] + 1

    deleted_from_counts <- table(data_before$edges$from[with(data_before$edges, paste(from, to)) %in% deleted_edges])
    added_from_counts <- table(data2$edges$from[with(data2$edges, paste(from, to)) %in% added_edges])


    # Update outgoing edges for deleted edges
    for (node in names(deleted_from_counts)) {
      data2$nodes$outgoing_edges[data2$nodes$id == node] <-
        data2$nodes$outgoing_edges[data2$nodes$id == node] - deleted_from_counts[node]
    }

    # Update outgoing edges for added edges
    for (node in names(added_from_counts)) {
      data2$nodes$outgoing_edges[data2$nodes$id == node] <-
        data2$nodes$outgoing_edges[data2$nodes$id == node] + added_from_counts[node]
    }

    # Reassign node IDs sequentially
    old_ids <- data2$nodes$label
    new_ids <- paste0("s", seq(0, nrow(data2$nodes) - 1))
    id_mapping <- setNames(new_ids, old_ids)

    # Update node IDs in the nodes dataframe
    data2$nodes$id <- new_ids
    data2$nodes$label <- new_ids

    # Ensure edges are updated with the new node IDs
    data2$edges$from <- id_mapping[as.character(data2$edges$from)]
    data2$edges$to <- id_mapping[as.character(data2$edges$to)]
    data2$edges <- data2$edges %>%
      group_by(from, label1) %>%
      reframe(
        label2 = sum(as.numeric(label2), na.rm = TRUE),  # Sum label2 values
        label3 = paste(label1, "\n", label2),             # Merge label3 values (optional)
        label = first(label3),                            # Keep the first 'label'
        font.size = first(font.size),                     # Keep the first font.size
        color = first(color),                             # Keep the first colour
        arrows = first(arrows),                           # Keep the first arrows value
        to = first(to)                                   # Keep the first 'to'
      )


    # Remove duplicate rows
    data2$edges <- data2$edges %>%
      distinct()

    # Step 1: Identify the node IDs that appear in the edges
    used_node_ids <- unique(c(data2$edges$from, data2$edges$to))

    # Step 2: Remove any rows from nodes data frame where node ID does not appear in the edges
    data2$nodes <- data2$nodes[data2$nodes$id %in% used_node_ids, ]

    old_ids <- data2$nodes$label
    new_ids <- paste0("s", seq(0, nrow(data2$nodes) - 1))
    id_mapping <- setNames(new_ids, old_ids)

    data2$nodes$id <- id_mapping[data2$nodes$label]
    data2$nodes$label <- id_mapping[data2$nodes$label]
    data2$edges$from <- id_mapping[data2$edges$from]
    data2$edges$to <- id_mapping[data2$edges$to]

    # Check the updated data
    #print(data2$nodes)
    #print(data2$edges)

    data2$nodes <- data2$nodes %>%
      mutate(fixed = list(list(x = TRUE, y = FALSE)))

    # Return the updated visNetwork plot
    network_plot <- visNetwork(nodes = data2$nodes, edges = data2$edges) %>%
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
    output <- list(eventtree = network_plot, filtereddf = result)

    class(output) <- "event_tree"
    return(output)


  } else {
    message("No nodes specified for deletion.")
  }
}
