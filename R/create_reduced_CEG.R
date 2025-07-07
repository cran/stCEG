#' Create a Reduced Chain Event Graph (CEG)
#'
#' This function generates a reduced version of a Chain Event Graph (CEG) based on a set of starting labels.
#' It extracts connected nodes and edges from the input CEG object, and visualizes the result using the visNetwork package.
#'
#' @param ceg_object A list containing the Chain Event Graph (CEG) data, including nodes and edges.
#'   The `ceg_object` must have the structure `ceg_object$ceg$x$nodes` and `ceg_object$ceg$x$edges`.
#' @param start_labels A character vector containing the labels of the nodes from which the floret extraction should start.
#' @param level_separation A numeric value determining the separation between levels in the hierarchical layout. Default is 1200.
#' @param node_distance A numeric value controlling the distance between nodes in the hierarchical layout. Default is 300.
#'
#' @return A visNetwork object representing the reduced CEG, with interactive visualization features,
#'   such as node selection, highlighting of edges, and layout manipulation.
#'
#' @import visNetwork
#' @import igraph
#' @import sf
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @importFrom igraph graph_from_data_frame
#' @export
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#'
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' ceg <- create_ceg(staged_tree, view_table = TRUE)
#' create_reduced_CEG(ceg, "Adult")
#'
#'
create_reduced_CEG <- function(ceg_object, start_labels, level_separation = 1200, node_distance = 300) {

  extract_floret <- function(nodes, edges, start_label1) {
    # Find all 'from' edges associated with the start_label1
    start_edges <- edges[edges$label1 == start_label1, ]

    if (nrow(start_edges) == 0) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }

    # Initialize sets for floret nodes and edges
    floret_nodes <- list()
    floret_edges <- data.frame()
    visited_nodes <- c()

    # Recursive function to traverse and collect connected nodes and edges
    collect_floret <- function(current_node) {
      # Find edges starting from the current node
      outgoing_edges <- edges[edges$from == current_node, ]

      # Eliminate edges leading into or before the starting node
      outgoing_edges <- outgoing_edges[outgoing_edges$to != current_node, ]

      # Add the new edges to the floret set
      if (nrow(outgoing_edges) > 0) {
        floret_edges <<- rbind(floret_edges, outgoing_edges)
      }

      # Get the 'to' nodes from these edges
      to_nodes <- outgoing_edges$to

      # Add new nodes to the floret set if not already added
      new_nodes <- to_nodes[!to_nodes %in% visited_nodes]
      visited_nodes <<- c(visited_nodes, new_nodes)

      floret_nodes <<- c(floret_nodes, setNames(as.list(new_nodes), new_nodes))

      # Recursively process each new node
      for (node in new_nodes) {
        collect_floret(node)
      }
    }

    # Process all starting edges
    for (i in seq_len(nrow(start_edges))) {
      start_node <- start_edges$to[i]

      if (!start_node %in% visited_nodes) {
        visited_nodes <- c(visited_nodes, start_node)
        floret_nodes <- c(floret_nodes, setNames(list(start_node), start_node))
        collect_floret(start_node)
      }
    }

    # Filter the nodes dataframe to include only nodes in the floret
    floret_nodes_df <- nodes[nodes$id %in% names(floret_nodes), ]

    list(nodes = floret_nodes_df, edges = floret_edges)
  }



  extract_florets <- function(ceg_object, start_labels) {
    nodes <- ceg_object$ceg$x$nodes
    edges <- ceg_object$ceg$x$edges
    # Initialize sets for floret nodes and edges
    all_floret_nodes <- data.frame()
    all_floret_edges <- data.frame()

    for (start_label in start_labels) {
      floret <- extract_floret(nodes, edges, start_label)

      # Combine results
      all_floret_nodes <- rbind(all_floret_nodes, floret$nodes)
      all_floret_edges <- rbind(all_floret_edges, floret$edges)
    }

    # Ensure unique nodes and edges
    all_floret_nodes <- unique(all_floret_nodes)
    all_floret_edges <- unique(all_floret_edges)

    list(nodes = all_floret_nodes, edges = all_floret_edges)
  }

  reduced_data <- extract_florets(ceg_object, start_labels)

  # Generate the visNetwork visualization
  network_plot <- visNetwork(nodes = reduced_data$nodes, edges = reduced_data$edges) %>%
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
    visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }") %>%
    visEvents(
      selectNode = "function(params) {
        var selectedNodeIds = params.nodes; // Array of selected node IDs

        // Store the original colours of the edges
        var edges = this.body.data.edges.get();
        edges.forEach(function(edge) {
          if (edge.originalcolour === undefined) {
            edge.originalcolour = edge.color; // Store the original edge colour
          }
          if (edge.originalFontcolour === undefined) {
            edge.originalFontcolour = (edge.font && edge.font.color) || '#000000'; // Store the original label colour
          }
        });

        // Reset all edges to their original colours
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalcolour || '#000000'; // Reset to original or default black
          edge.font = { colour: edge.originalFontcolour || '#000000' }; // Reset to original or default black
          return edge;
        }));

        // Highlight edges based on selected nodes
        selectedNodeIds.forEach(function(selectedNodeId) {
          // Highlight edges going into the selected node (blue)
          var incomingEdges = this.body.data.edges.get({
            filter: function(edge) {
              return edge.to === selectedNodeId;
            }
          });
          incomingEdges.forEach(function(edge) {
            edge.color = '#0000FF'; // Set colour to blue
            edge.font = { color: '#0000FF' }; // Set label colour to blue
          });
          this.body.data.edges.update(incomingEdges);

          // Highlight edges going out from the selected node (red)
          var outgoingEdges = this.body.data.edges.get({
            filter: function(edge) {
              return edge.from === selectedNodeId;
            }
          });
          outgoingEdges.forEach(function(edge) {
            edge.color = '#FF0000'; // Set colour to red
            edge.font = { color: '#FF0000' }; // Set label colour to red
          });
          this.body.data.edges.update(outgoingEdges);
        }, this); // Bind `this` to the function to access visNetwork context

        // Redraw network to apply changes
        this.redraw();
      }",
      deselectNode = "function(params) {
        // When deselecting, reset all edges to their original colours
        var edges = this.body.data.edges.get();
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalcolour || '#000000'; // Reset to original or default black
          edge.font = { color: edge.originalFontcolour || '#000000' }; // Reset to original or default black
          return edge;
        }));
        this.redraw();
      }"
    )%>%
    visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")

  # Print the network plot
  return(list(reduced_ceg = network_plot))
}
