#' Compute Prior-Adjusted Values for a Staged Tree
#'
#' Combines a staged tree and a stage-level prior specification to create a
#' staged tree with prior, prior mean and prior variance information attached
#' to both nodes and edges.
#'
#' The function distributes stage-level Dirichlet priors across the nodes
#' belonging to each stage and computes the corresponding prior means and
#' variances. Prior information is then propagated to outgoing edges, creating
#' edge-level prior labels suitable for visualisation and subsequent Chain
#' Event Graph construction.
#'
#' @param staged_tree_obj An object of class \code{"staged_tree"}.
#'
#' @param prior_table An object of class \code{"prior_table"} created by
#'   \code{\link{specify_priors}}.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Matches stages using stage colour and tree level.
#'   \item Allocates stage-level Dirichlet parameters across nodes within each
#'   stage.
#'   \item Computes prior means and Dirichlet variances.
#'   \item Assigns prior information to outgoing edges.
#'   \item Creates labels suitable for visualisation using
#'   \code{\link{plot.staged_tree_priors}}.
#' }
#'
#' The resulting object is typically used as input to
#' \code{\link{compute_ceg}}.
#'
#' @return
#' An object of class \code{"staged_tree_priors"} containing:
#' \itemize{
#'   \item \code{nodes}: node data with prior information.
#'   \item \code{edges}: edge data with prior values and prior means.
#'   \item \code{prior_table}: the stage-level prior table.
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
#' print(st_priors)
#' summary(st_priors)
#'
#' @seealso
#' \code{\link{specify_priors}},
#' \code{\link{compute_ceg}}
#'
#' @export
compute_staged_tree_priors <- function(staged_tree_obj, prior_table) {

  if (!inherits(staged_tree_obj, "staged_tree")) {
    stop("Input must be an object of class 'staged_tree'.")
  }

  if (!inherits(prior_table, "prior_table")) {
    stop("prior_table must be an object created by specify_priors().")
  }

  nodes_df <- staged_tree_obj$nodes
  edges_df <- staged_tree_obj$edges

  # Helper functions
  convertPrior <- function(prior) as.numeric(strsplit(prior, ",")[[1]])
  calculateRatios <- function(alpha) round(alpha / sum(alpha), 3)
  calculateVariance <- function(alpha) {
    A <- sum(alpha)
    round((alpha * (A - alpha)) / (A^2 * (A + 1)), 3)
  }

  # Add new columns
  nodes_df$adjusted_prior <- NA_character_
  nodes_df$prior_mean <- NA_character_
  nodes_df$prior_variance <- NA_character_

  # Apply priors stage-wise
  for (i in seq_len(nrow(prior_table$table))) {

    row <- prior_table$table[i, ]
    col <- row$Colour
    lvl <- row$Level
    prior <- row$Prior

    alpha <- convertPrior(prior)

    group_nodes <- nodes_df[nodes_df$color == col & nodes_df$level2 == lvl, ]

    if (nrow(group_nodes) > 0) {

      count <- nrow(group_nodes)
      adjusted <- round(alpha / count, 3)
      ratios <- calculateRatios(alpha)
      variances <- calculateVariance(alpha)

      for (id in group_nodes$id) {
        idx <- which(nodes_df$id == id)
        nodes_df$adjusted_prior[idx] <- paste(adjusted, collapse = ",")
        nodes_df$prior_mean[idx] <- paste(ratios, collapse = ",")
        nodes_df$prior_variance[idx] <- paste(variances, collapse = ",")
      }
    }
  }

  # Assign edge-level priors
  edges_df$prior_value <- NA_real_
  edges_df$prior_mean <- NA_real_
  edges_df$label_prior_frac <- NA_real_
  edges_df$label_prior_mean <- NA_real_

  for (i in seq_len(nrow(nodes_df))) {

    from <- nodes_df$id[i]
    adj <- nodes_df$adjusted_prior[i]
    mean <- nodes_df$prior_mean[i]

    if (!is.na(adj)) {
      alpha_adj <- convertPrior(adj)
      mean_adj <- convertPrior(mean)

      idx <- which(edges_df$from == from)

      if (length(alpha_adj) >= length(idx)) {
        edges_df$prior_value[idx] <- alpha_adj[seq_along(idx)]
        edges_df$prior_mean[idx] <- mean_adj[seq_along(idx)]
        for (j in 1:length(idx)) {
          edges_df$label_prior_frac[idx[j]] <- paste(edges_df$label1[idx[j]], "\n", alpha_adj[j])
          edges_df$label_prior_mean[idx[j]] <- paste(edges_df$label1[idx[j]], "\n", mean_adj[j])
          edges_df$label3[idx[j]] <- alpha_adj[j]
        }
      }
    }
  }

  # Return structured object
  structure(
    list(
      nodes = nodes_df,
      edges = edges_df,
      prior_table = prior_table
    ),
    class = "staged_tree_priors"
  )
}

#' Print a Staged Tree with Priors
#'
#' Prints a concise overview of a \code{"staged_tree_priors"} object,
#' including the number of nodes, edges and prior type.
#'
#' @param x An object of class \code{"staged_tree_priors"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied object, invisibly.
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
#' print(st_priors)
#'
#' @method print staged_tree_priors
#' @export
print.staged_tree_priors <- function(x, ...) {

  if (!inherits(x, "staged_tree_priors")) {
    stop("Object must be of class 'staged_tree_priors'.")
  }

  cat("Staged Tree with Prior Information\n")
  cat("=================================\n")
  cat("Number of nodes: ", nrow(x$nodes), "\n")
  cat("Number of edges: ", nrow(x$edges), "\n")
  cat("Prior type:      ", unique(x$prior_table$table$Prior_Type), "\n\n")

  cat("Use summary(x) for detailed information.\n")
  cat("Use plot(x) to visualize the tree.\n")

  invisible(x)
}

#' Summarise a Staged Tree with Priors
#'
#' Produces a summary of a \code{"staged_tree_priors"} object including graph
#' size, prior types and stage information.
#'
#' @param object An object of class \code{"staged_tree_priors"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_staged_tree_priors"} containing:
#' \itemize{
#'   \item Number of nodes.
#'   \item Number of edges.
#'   \item Prior type information.
#'   \item Numbers of uniform and custom priors.
#'   \item Levels represented in the tree.
#'   \item Stage colours.
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
#' summary(st_priors)
#'
#' @method summary staged_tree_priors
#' @export
summary.staged_tree_priors <- function(object, ...) {

  if (!inherits(object, "staged_tree_priors")) {
    stop("Object must be of class 'staged_tree_priors'.")
  }

  nodes <- object$nodes
  edges <- object$edges
  pt    <- object$prior_table$table

  out <- list(
    n_nodes = nrow(nodes),
    n_edges = nrow(edges),
    prior_type_global = unique(pt$Prior_Type),
    prior_types_rowwise = pt$Prior_Type,
    n_custom = sum(pt$Prior_Type == "Custom"),
    n_uniform = sum(pt$Prior_Type == "Uniform"),
    levels = sort(unique(nodes$level2)),
    colours = unique(nodes$color)
  )

  class(out) <- "summary_staged_tree_priors"
  out
}

#' Plot a Staged Tree with Priors
#'
#' Produces an interactive visualisation of a staged tree containing prior
#' information using \pkg{visNetwork}.
#'
#' Node tooltips display prior distributions, prior means and prior variances.
#' Edge labels may display prior values or prior means.
#'
#' @param x An object of class \code{"staged_tree_priors"}.
#'
#' @param level_separation Numeric value controlling spacing between graph
#'   levels. Default is \code{1600}.
#'
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Default is \code{400}.
#'
#' @param label_type Character string specifying the edge labels to display.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"priors"} (default)
#'     \item \code{"priormeans"}
#'     \item \code{"names"}
#'   }
#'
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A \pkg{visNetwork} htmlwidget.
#'
#' @examples
#'
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
#' plot(st_priors)
#'
#'
#' @seealso
#' \code{\link{compute_staged_tree_priors}},
#' \code{\link{compute_ceg}}
#'
#' @method plot staged_tree_priors
#' @export
plot.staged_tree_priors <- function(x, level_separation = 1600,
                                    node_distance = 400,
                                    label_type = "priors", ...) {

  nodes <- x$nodes
  edges <- x$edges

  print(edges)


  nodes$title <- ifelse(
    is.na(nodes$adjusted_prior) | nodes$adjusted_prior == "",
    "Leaf nodes have no prior",
    paste0(
      "Prior: ", nodes$adjusted_prior, "<br>",
      "Prior Mean: ", nodes$prior_mean, "<br>",
      "Prior Variance: ", nodes$prior_variance
    )
  )


  edges$label <- switch(
    label_type,
    names = edges$label1,
    priors  = edges$label_prior_frac,
    priormeans = edges$label_prior_mean,
    edges$label_prior_frac  # Default to priors if unknown type
  )


  edges$font.size <- node_distance * 0.2

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
    visNetwork::visEvents(
      selectNode = "function(params) { /* Node selection code */ }",
      deselectNode = "function(params) { /* Deselect code */ }"
    ) %>%
    visNetwork::visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")
}


