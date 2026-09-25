#' Compute a Reduced Chain Event Graph
#'
#' Extracts one or more florets from a Chain Event Graph (CEG) beginning at
#' specified edge labels and returns the downstream subgraph as a reduced CEG.
#'
#' For each supplied edge label, the function identifies all matching edges and
#' recursively traverses descendant situations and transitions. The resulting
#' graph contains only the nodes and edges reachable from the selected starting
#' labels.
#'
#' @param ceg An object of class \code{"ceg"} created by
#'   \code{\link{compute_ceg}}.
#'
#' @param start_labels A character vector containing one or more edge labels
#'   from which the reduced CEG should begin.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Locates edges whose \code{label1} values match the supplied
#'   \code{start_labels}.
#'   \item Extracts the descendant floret associated with each matching edge.
#'   \item Recursively traverses all reachable downstream situations.
#'   \item Combines and deduplicates nodes and edges from all extracted
#'   florets.
#' }
#'
#' @return
#' An object of class \code{"reduced_ceg"} containing:
#' \itemize{
#'   \item \code{nodes}: nodes contained in the reduced graph.
#'   \item \code{edges}: edges contained in the reduced graph.
#'   \item \code{start_labels}: labels used to generate the reduction.
#' }
#'
#' @examples
#' \dontrun{
#' data("Medical_Trial")
#'
#' et <- create_event_tree(Medical_Trial)
#' st <- ahc_colouring(et)
#' st_priors <- compute_staged_tree_priors(st)
#' ceg <- compute_ceg(st_priors)
#'
#' reduced_ceg <- compute_reduced_ceg(
#'   ceg,
#'   start_labels = "Recovered"
#' )
#'
#' print(reduced_ceg)
#' summary(reduced_ceg)
#' }
#'
#' @seealso
#' \code{\link{compute_ceg}},
#' \code{\link{plot.reduced_ceg}}
#'
#' @export
compute_reduced_ceg <- function(ceg, start_labels) {

  if (!inherits(ceg, "ceg")) {
    stop("Input must be an object of class 'ceg'.")
  }

  nodes <- ceg$nodes
  edges <- ceg$edges

  ## ----------------------------------------------------------------------
  ## Helper: extract floret from a single start label
  ## ----------------------------------------------------------------------

  extract_floret <- function(start_label) {

    start_edges <- edges[edges$label1 == start_label, ]
    if (nrow(start_edges) == 0) {
      return(list(nodes = nodes[0, ], edges = edges[0, ]))
    }

    visited <- character(0)
    floret_edges <- edges[0, ]

    collect <- function(node_id) {

      out_edges <- edges[edges$from == node_id, ]
      out_edges <- out_edges[out_edges$to != node_id, ]

      if (nrow(out_edges) > 0) {
        floret_edges <<- rbind(floret_edges, out_edges)
      }

      new_nodes <- out_edges$to
      new_nodes <- new_nodes[!new_nodes %in% visited]

      visited <<- c(visited, new_nodes)

      for (n in new_nodes) collect(n)
    }

    for (i in seq_len(nrow(start_edges))) {
      to_node <- start_edges$to[i]
      if (!to_node %in% visited) {
        visited <- c(visited, to_node)
        collect(to_node)
      }
    }

    floret_nodes <- nodes[nodes$id %in% visited, ]
    list(nodes = floret_nodes, edges = floret_edges)
  }

  ## ----------------------------------------------------------------------
  ## Extract all florets
  ## ----------------------------------------------------------------------

  all_nodes <- nodes[0, ]
  all_edges <- edges[0, ]

  for (lab in start_labels) {
    fl <- extract_floret(lab)
    all_nodes <- rbind(all_nodes, fl$nodes)
    all_edges <- rbind(all_edges, fl$edges)
  }

  all_nodes <- unique(all_nodes)
  all_edges <- unique(all_edges)

  ## ----------------------------------------------------------------------
  ## Return S3 object
  ## ----------------------------------------------------------------------

  structure(
    list(
      nodes = all_nodes,
      edges = all_edges,
      start_labels = start_labels
    ),
    class = "reduced_ceg"
  )
}

#' Plot a Reduced Chain Event Graph
#'
#' Produces an interactive visualisation of a reduced Chain Event Graph using
#' \pkg{visNetwork}.
#'
#' Edge labels may display observed counts, prior values, posterior values,
#' prior probabilities, posterior probabilities or the original edge labels.
#'
#' Selecting a node highlights incoming and outgoing transitions to aid
#' interpretation of the local graph structure.
#'
#' @param x An object of class \code{"reduced_ceg"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @param label_type Character string specifying which edge labels to display.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"posterior_mean"} (default)
#'     \item \code{"posterior"}
#'     \item \code{"prior_mean"}
#'     \item \code{"prior"}
#'     \item any other value displays the original edge labels
#'   }
#'
#' @param level_separation Numeric value controlling spacing between graph
#'   levels. Default is \code{1200}.
#'
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Default is \code{400}.
#'
#' @param font_size Numeric value controlling edge-label font size.
#'   Default is \code{80}.
#'
#' @return
#' A \pkg{visNetwork} htmlwidget.
#'
#' @examples
#' \dontrun{
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' priors <- specify_priors(
#'   st,
#'   prior_type = "Uniform"
#' )
#'
#' st_priors <- compute_staged_tree_priors(
#'   st,
#'   priors
#' )
#'
#' ceg <- compute_ceg(st_priors)
#'
#' reduced_ceg <- compute_reduced_ceg(
#'   ceg,
#'   start_labels = "Adult"
#' )
#'
#' plot(reduced_ceg)
#'
#' plot(reduced_ceg, label_type = "posterior")
#'
#' plot(reduced_ceg, label_type = "prior_mean")
#' }
#'
#' @seealso
#' \code{\link{compute_reduced_ceg}}
#'
#' @method plot reduced_ceg
#' @export
plot.reduced_ceg <- function(x,
                             label_type = "posterior_mean",
                             level_separation = 1200,
                             node_distance = 400,
                             font_size = 80,
                             ...) {


  nodes <- x$nodes
  edges <- x$edges
  edges$font.size <- font_size

  if (label_type == "posterior") {
    edges$label <- edges$label_individuals
  } else if (label_type == "posterior_mean") {
    edges$label <- edges$label_posterior
  } else if (label_type == "prior_mean") {
    edges$label <- edges$label_prior_mean
  } else if (label_type == "prior") {
    edges$label <- edges$label_prior
  } else {
    edges$label <- edges$label1
  }

  print(edges$label)

  visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5)), smooth = TRUE) %>%
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
}

#' Print a Reduced Chain Event Graph
#'
#' Prints a concise summary of a reduced Chain Event Graph including the number
#' of nodes, edges and starting labels used to construct the graph.
#'
#' @param x An object of class \code{"reduced_ceg"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied \code{"reduced_ceg"} object, invisibly.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' priors <- specify_priors(
#'   st,
#'   prior_type = "Uniform"
#' )
#'
#' st_priors <- compute_staged_tree_priors(
#'   st,
#'   priors
#' )
#'
#' ceg <- compute_ceg(st_priors)
#'
#' reduced_ceg <- compute_reduced_ceg(
#'   ceg,
#'   start_labels = "Adult"
#' )
#'
#' print(reduced_ceg)
#'
#'
#' @method print reduced_ceg
#' @export
print.reduced_ceg <- function(x, ...) {

  cat("Reduced Chain Event Graph\n")
  cat("=========================\n")

  cat("Nodes:   ", nrow(x$nodes), "\n")
  cat("Edges:   ", nrow(x$edges), "\n")
  cat("Start labels: ", paste(x$start_labels, collapse = ", "), "\n\n")

  cat("Use summary(x) for details.\n")
  cat("Use plot(x) to visualize.\n")

  invisible(x)
}

#' Summarise a Reduced Chain Event Graph
#'
#' Produces a summary of a reduced Chain Event Graph including graph size,
#' levels present and stage colours represented in the extracted subgraph.
#'
#' @param object An object of class \code{"reduced_ceg"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_reduced_ceg"} containing:
#' \itemize{
#'   \item Number of nodes.
#'   \item Number of edges.
#'   \item Starting labels used for extraction.
#'   \item Levels represented in the reduced graph.
#'   \item Stage colours present in the reduced graph.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' priors <- specify_priors(
#'   st,
#'   prior_type = "Uniform"
#' )
#'
#' st_priors <- compute_staged_tree_priors(
#'   st,
#'   priors
#' )
#'
#' ceg <- compute_ceg(st_priors)
#'
#' reduced_ceg <- compute_reduced_ceg(
#'   ceg,
#'   start_labels = "Adult"
#' )
#' summary(reduced_ceg)
#'
#'
#' @method summary reduced_ceg
#' @export
summary.reduced_ceg <- function(object, ...) {

  out <- list(
    n_nodes = nrow(object$nodes),
    n_edges = nrow(object$edges),
    start_labels = object$start_labels,
    levels = sort(unique(object$nodes$level)),
    colours = unique(object$nodes$color)
  )

  class(out) <- "summary_reduced_ceg"
  out
}

