#' Summary of an Event Tree
#'
#' This function provides a summary of an object of class `event_tree`. It outputs key information about the nodes and edges of the event tree, including the number of nodes and edges, unique levels and labels, and the labels of the edges (with newlines replaced by spaces).
#'
#' @param object An object of class `event_tree`. This object should have a list structure with `eventtree` containing a nested list where `x` is a dataframe with `nodes` and `edges` attributes.
#' @param ... Additional arguments passed to or from other methods (ignored).
#'
#' @return Prints a summary of the event tree to the console, including:
#'   - The number of nodes and the unique node levels
#'   - The number of edges and the unique labels in the edges
#'   - The labels of the edges, with newlines replaced by spaces
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' homicides_ET_summary <- summary(event_tree)
#' @export
summary.event_tree <- function(object, ...) {
  # Ensure the object is of class 'event_tree'
  if (!inherits(object, "event_tree")) {
    stop("Object must be of class 'event_tree'")
  }

  # Extract the event tree structure
  object <- object$eventtree$x

  # Summarize the nodes
  cat("Summary of Nodes\n")
  cat("================\n")
  cat("Number of nodes:", nrow(object$nodes), "\n")
  cat("Unique node levels:", length(unique(object$nodes$level)), "\n")

  # Summarize the edges
  cat("\nSummary of Edges:\n")
  cat("===================\n")
  cat("Number of edges:", nrow(object$edges), "\n")
  cat("Unique labels in edges:", length(unique(object$edges$label1)), "\n")

  # Replace newlines with spaces in edge labels before printing  # Use newline to separate labels
  edge_labels <- gsub("\n", " ", unique(object$edges$label1))
  edge_labels <- paste(edge_labels, collapse = "\n") # Replace newlines with spaces in the labels themselves

  # Print each label on a new line
  cat("Edge labels:\n", "==============\n", edge_labels)

}


#' Summarise a Staged Tree Object
#'
#' Provides a visual and textual summary of a staged tree object, including counts of nodes and edges, node colour distributions, and uncoloured nodes in intermediate levels.
#'
#' @param object An object of class `staged_tree`, containing components `nodes` and `edges` in `object$stagedtree$x`, and optionally a `priortable`.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' Invisibly returns a list containing summary components:
#' \itemize{
#'   \item \code{num_nodes}: Total number of nodes in the staged tree.
#'   \item \code{num_edges}: Total number of edges in the staged tree.
#'   \item \code{num_uncoloured_middle}: Number of white nodes not in the first or last level.
#'   \item \code{color_counts}: A named table of node colour frequencies.
#'   \item \code{priortable}: The prior table, if available; otherwise \code{NULL}.
#' }
#' Also prints a human-readable summary to the console.
#'
#' @details
#' This function #prints a summary that includes:
#' \itemize{
#'   \item Total number of nodes and edges.
#'   \item Number of nodes still uncoloured (white) that are not in the first or last level.
#'   \item A count of nodes by hex colour, optionally with coloured terminal output using the `crayon` package.
#'   \item The prior table if it exists.
#' }
#'
#' If the `crayon` package is available, the function displays background colour blocks in the console to represent node colours.
#'
#' @note
#' White-coloured nodes (`#FFFFFF`) that are not in the minimum or maximum level are flagged as "Nodes left to be coloured."
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' homicides_ST_summary <- summary(staged_tree)
#'
#' @importFrom grDevices col2rgb
#' @importFrom crayon make_style
#' @export
summary.staged_tree <- function(object, ...) {
  nodes <- object$stagedtree$x$nodes
  edges <- object$stagedtree$x$edges

  # Basic summary counts
  num_nodes <- nrow(nodes)
  num_edges <- nrow(edges)

  # Level range
  min_level <- min(nodes$level, na.rm = TRUE)
  max_level <- max(nodes$level, na.rm = TRUE)

  # Colour count
  color_counts <- table(nodes$color)

  # White nodes not in min or max level
  num_uncoloured_middle <- sum(
    nodes$color == "#FFFFFF" &
      !(nodes$level %in% c(min_level, max_level))
  )

  cat("Summary of Staged Tree Object\n")
  cat("=============================\n")
  cat("Total nodes:", num_nodes, "\n")
  cat("Total edges:", num_edges, "\n")
  cat("Nodes left to be coloured:", num_uncoloured_middle, "\n")

  # Load crayon
  if (!requireNamespace("crayon", quietly = TRUE)) {
    cat("Install the 'crayon' package to view colored output.\n")
  } else {
    hex_to_style <- function(hex) {
      rgb <- grDevices::col2rgb(hex)
      crayon::make_style(rgb, bg = TRUE)
    }

    cat("\nNode colour counts:\n")
    for (hex in names(color_counts)) {
      style <- hex_to_style(hex)
      cat(style(sprintf("     ")), " ", hex, sprintf(" (%d nodes)\n", color_counts[[hex]]))
    }
  }

  if (!is.null(object$priortable)) {
    cat("\nPrior Table:\n")
    print(object$priortable)
  }

}


#' Summarise a Chain Event Graph Model
#'
#' Computes the total log marginal likelihood, effective sample size (ESS), and per-stage log scores for a fitted Chain Event Graph (CEG) model using conjugate prior/posterior updates.
#'
#' @param object An object of class `chain_event_graph`, which must contain an `update_table` with prior and data columns for each stage.
#' @param ... Additional arguments (currently unused).
#'
#' @return An invisible object of class `summary.chain_event_graph`, which is a list containing:
#' \describe{
#'   \item{total_log_marginal_likelihood}{Total log marginal likelihood across all stages.}
#'   \item{per_stage_log_scores}{A data frame with log scores and effective sample sizes (ESS) for each stage.}
#' }
#'
#' @details
#' The log marginal likelihood is computed using the Dirichlet-multinomial formula.
#'
#' The effective sample size for a stage is defined as \eqn{ESS = \sum_j (\alpha_{ij} + y_{ij})}.
#' It represents the amount of information (prior + observed) available for that stage.
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#'
#' # Cannot run this whole chunk at once as specify_priors needs user input
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' ceg <- create_ceg(staged_tree, view_table = TRUE)
#' homicides_CEG_summary <- summary(ceg)
#'
#' @export
summary.chain_event_graph <- function(object, ...) {
  if (is.null(object$update_table)) {
    stop("The chain_event_graph object does not contain an update_table.")
  }

  update_table <- object$update_table
  total_score <- 0
  stage_scores <- numeric(nrow(update_table))
  effective_sample_sizes <- numeric(nrow(update_table))

  for (i in 1:nrow(update_table)) {
    prior <- as.numeric(unlist(strsplit(update_table$Prior[i], ",")))
    data <- as.numeric(unlist(strsplit(update_table$Data[i], ",")))

    prior <- ifelse(prior == 0, 1e-10, prior)
    data <- ifelse(data == 0, 1e-10,data)

    alpha_sum <- sum(prior)
    x_sum <- sum(data)
    posterior_sum <- alpha_sum + x_sum

    term1 <- lgamma(alpha_sum) - lgamma(posterior_sum)
    term2 <- sum(lgamma(prior + data) - lgamma(prior))

    stage_score <- term1 + term2
    #if (is.nan(stage_score)) stage_score <- 0
    stage_scores[i] <- stage_score
    total_score <- total_score + stage_score

    # Effective Sample Size = sum of posterior alpha values
    alpha_star <- prior + data
    effective_sample_sizes[i] <- sum(alpha_star)
  }

  # Create separate flag column for low ESS
  ess_flags <- ifelse(effective_sample_sizes < 100, "**", "")

  df <- data.frame(
    Stage = update_table$Stage,
    LogScore = round(stage_scores, 3),
    ESS = round(effective_sample_sizes, 2),
    Flag = ess_flags,  # temporary name
    stringsAsFactors = FALSE
  )

  # Rename the last column to have no name
  names(df)[4] <- ""

  # Store in result
  result <- list(
    total_log_marginal_likelihood = total_score,
    per_stage_log_scores = df
  )

  cat("Chain Event Graph Summary\n")
  cat("--------------------------\n")
  cat("Total Log Marginal Likelihood: ", round(total_score, 3), "\n\n")
  cat("Per-stage metrics:\n")
  print(result$per_stage_log_scores, row.names = FALSE)

  cat("\nNote: ESS (Effective Sample Size) reflects the total information (prior + data) available for each stage.\n")
  cat("      Stages with ESS < 100 are flagged with '**' as potentially low-information stages.\n")
  cat("      Increasing the strength of the prior would help this.\n")

  class(result) <- "summary.chain_event_graph"
  return(invisible(result))
}

