#' Create a Staged Tree
#'
#' Creates an object of class \code{"staged_tree"} from node and edge data
#' produced by a staged-tree colouring procedure.
#'
#' A staged tree is an event tree in which situations have been grouped into
#' stages. Situations assigned to the same stage are represented by a common
#' node colour and are assumed to share the same conditional transition
#' structure.
#'
#' This function is typically called internally by stage-colouring methods such
#' as \code{\link{ahc_colouring}} or manual colouring procedures rather than
#' directly by users.
#'
#' @param nodes A data frame containing node information, including stage
#'   colours and identifiers.
#' @param edges A data frame containing edge information inherited from the
#'   event tree.
#' @param filtereddf The dataset used to construct the original event tree.
#' @param method Character string describing the primary colouring method used
#'   to generate the staged tree.
#'
#' @return
#' An object of class \code{"staged_tree"} containing:
#' \itemize{
#'   \item \code{nodes}: staged node information.
#'   \item \code{edges}: edge information.
#'   \item \code{data}: underlying dataset.
#'   \item \code{metadata}: information describing the staged tree.
#' }
#'
#' @examples
#'
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' @seealso
#' \code{\link{ahc_colouring}},
#' \code{\link{plot.staged_tree}}
#'
#' @export
create_staged_tree <- function(nodes, edges, filtereddf, method) {
  structure(
    list(
      nodes      = nodes,
      edges      = edges,
      data = filtereddf,
      metadata   = list(
        n_nodes       = nrow(nodes),
        n_edges       = nrow(edges),
        methods_used = sort(unique(stats::na.omit(nodes$method))),
        primary_method = method
      )
    ),
    class = "staged_tree"
  )
}

#' Print a Staged Tree
#'
#' Prints a concise summary of a staged tree, including the number of nodes,
#' edges and colouring methods used.
#'
#' @param x An object of class \code{"staged_tree"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied \code{"staged_tree"} object, invisibly.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' print(st)
#'
#' @method print staged_tree
#' @export
print.staged_tree <- function(x, ...) {
  cat("Staged Tree\n")
  cat("-----------\n")
  cat("Nodes:", x$metadata$n_nodes, "\n")
  cat("Edges:", x$metadata$n_edges, "\n")
  cat("Methods:", x$metadata$methods_used, "\n")
  cat("Primary Method:", x$metadata$primary_method, "\n\n")
  invisible(x)
}

#' Summarise a Staged Tree
#'
#' Produces a summary of a staged tree, including graph size, colouring
#' methods and previews of node and edge information.
#'
#' @param object An object of class \code{"staged_tree"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item Number of nodes.
#'   \item Number of edges.
#'   \item Preview of node information.
#'   \item Preview of edge information.
#'   \item Colouring methods used.
#'   \item Primary colouring method.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' summary(st)
#'
#' @method summary staged_tree
#' @export
summary.staged_tree <- function(object, ...) {
  list(
    n_nodes       = object$metadata$n_nodes,
    n_edges       = object$metadata$n_edges,
    nodes_preview = utils::head(object$nodes),
    edges_preview = utils::head(object$edges),
    methods_used  = object$metadata$methods_used,
    primary_method = object$metadata$primary_method
  )
}

#' Plot a Staged Tree
#'
#' Produces an interactive visualisation of a staged tree using
#' \pkg{visNetwork}.
#'
#' Nodes belonging to the same stage share a common colour. Edge labels may
#' display transition names, observed frequencies or prior information,
#' depending on the selected label type.
#'
#' @param x An object of class \code{"staged_tree"}.
#'
#' @param label_type Character string specifying the edge label format.
#'   Supported values include:
#'   \itemize{
#'     \item \code{"both"} (default), displaying transition labels and counts.
#'     \item \code{"names"}, displaying transition labels only.
#'     \item \code{"priormeans"}, displaying prior means.
#'     \item \code{"priorfrac"}, displaying prior fractions.
#'   }
#'
#' @param level_separation Numeric value controlling spacing between graph
#'   levels. Default is \code{1600}.
#'
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Default is \code{400}.
#'
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A \pkg{visNetwork} htmlwidget representing the staged tree.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#'
#' plot(st)
#'
#' @seealso
#' \code{\link{create_event_tree}},
#' \code{\link{ahc_colouring}}
#'
#' @method plot staged_tree
#' @export
plot.staged_tree <- function(x,
                             label_type = "both",
                             level_separation = 1600,
                             node_distance = 400,
                             ...) {

  edges <- x$edges

  edges$label <- switch(
    label_type,
    names = edges$label1,
    both  = edges$label3,
    priormeans = edges$label_prior_mean,
    priorfrac = edges$label_prior_frac,
    edges$label3
  )

  edges$font.size <- node_distance * 0.2

  nodes <- x$nodes

  nodes <- nodes %>%
    dplyr::mutate(fixed = list(list(x = TRUE, y = FALSE)))

  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges
  ) %>%
    visNetwork::visHierarchicalLayout(
      direction = "LR",
      levelSeparation = level_separation
    ) %>%
    visNetwork::visNodes(
      label = nodes$label,
      scaling = list(min = 10, max = 10),
      font = list(vadjust = -170)
    ) %>%
    visNetwork::visEdges(
      arrows = list(
        to = list(
          enabled = TRUE,
          scaleFactor = 5
        )
      )
    ) %>%
    visNetwork::visInteraction(
      dragNodes        = TRUE,
      multiselect      = TRUE,
      navigationButtons = TRUE
    ) %>%
    visNetwork::visPhysics(
      hierarchicalRepulsion = list(
        nodeDistance = node_distance),
      stabilization = TRUE
    ) %>%
    visNetwork::visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")
}


