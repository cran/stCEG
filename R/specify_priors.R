#' Specify Stage Priors for a Staged Tree
#'
#' Generates prior distributions for each stage of a staged tree.
#'
#' Prior distributions are specified at the stage level and may be generated
#' automatically using a Uniform prior, a Phantom Individuals prior, or
#' supplied manually through custom Dirichlet parameters.
#'
#' The resulting object is used when constructing posterior distributions and
#' Chain Event Graphs.
#'
#' @param staged_tree_obj An object of class \code{"staged_tree"}.
#'
#' @param prior_type Character string specifying the prior construction
#'   method. Supported values are:
#'   \itemize{
#'     \item \code{"Uniform"}: assigns equal Dirichlet parameters to each
#'     outgoing edge.
#'     \item \code{"Phantom"}: computes a Phantom Individuals prior based on
#'     the staged tree structure.
#'   }
#'
#' @param custom_priors Optional named list of custom prior vectors.
#' Names must correspond to stage names (e.g. \code{"u1"}, \code{"u2"}).
#'
#' @details
#' The function groups situations into stages and constructs a stage-level
#' prior table containing:
#' \itemize{
#'   \item Stage identifiers.
#'   \item Stage colours.
#'   \item Tree levels.
#'   \item Number of outgoing edges.
#'   \item Dirichlet prior parameters.
#'   \item Corresponding prior means.
#' }
#'
#' If \code{custom_priors} is supplied, the supplied values override the
#' selected \code{prior_type}.
#'
#' @return
#' An object of class \code{"prior_table"} containing a stage-level summary
#' of prior distributions.
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
#' print(priors)
#' summary(priors)
#'
#' @seealso
#' \code{\link{edit_priors}},
#' \code{\link{compute_ceg}}
#'
#' @export
specify_priors <- function(staged_tree_obj,
                           prior_type = "Uniform",
                           custom_priors = NULL) {

  if (!inherits(staged_tree_obj, "staged_tree")) {
    stop("Input must be an object of class 'staged_tree'.")
  }

  nodes <- staged_tree_obj$nodes
  edges <- staged_tree_obj$edges

  required_cols <- c("color", "level2", "outgoing_edges2")
  if (!all(required_cols %in% colnames(nodes))) {
    stop("Nodes must contain: color, level2, outgoing_edges2.")
  }

  # Remove terminal level
  max_level <- max(nodes$level2, na.rm = TRUE)
  nodes <- dplyr::filter(nodes, level2 != max_level)

  # Build stage table
  stage_df <- nodes |>
    dplyr::mutate(number_nodes = 1) |>
    dplyr::group_by(color, level2, outgoing_edges2) |>
    dplyr::summarise(number_nodes = sum(number_nodes), .groups = "drop") |>
    dplyr::arrange(level2) |>
    dplyr::mutate(stage = paste0("u", dplyr::row_number()))

  # Custom priors
  if (!is.null(custom_priors)) {
    if (!all(names(custom_priors) %in% stage_df$stage)) {
      stop("Names of custom_priors must match stage names (e.g., 'u1', 'u2').")
    }

    stage_df$prior <- purrr::map_chr(stage_df$stage, function(stg) {
      if (!is.null(custom_priors[[stg]])) {
        paste(custom_priors[[stg]], collapse = ",")
      } else {
        NA_character_
      }
    })

  } else if (prior_type == "Uniform") {

    stage_df$prior <- purrr::pmap_chr(
      list(stage_df$outgoing_edges2, stage_df$number_nodes),
      function(k, n) paste(rep(n, k), collapse = ",")
    )

  } else if (prior_type == "Phantom") {

    nodes2 <- staged_tree_obj$nodes
    edges2 <- staged_tree_obj$edges

    max_level <- max(nodes2$level2, na.rm = TRUE)
    phantom_df <- dplyr::filter(nodes2, level2 != max_level)

    equivsize <- max(phantom_df$outgoing_edges2, na.rm = TRUE)
    phantom_df$prior <- NA_real_
    phantom_df$prior[1] <- equivsize

    for (i in seq_len(nrow(phantom_df))) {
      id <- phantom_df$id[i]
      k  <- phantom_df$outgoing_edges2[i]
      if (!is.na(id) && k > 0) {
        new_prior <- phantom_df$prior[i] / k
        to_nodes <- edges2$to[edges2$from == id]
        phantom_df$prior[phantom_df$id %in% to_nodes] <- new_prior
      }
    }

    phantom_stage <- phantom_df |>
      dplyr::group_by(color, level2, outgoing_edges2) |>
      dplyr::summarise(total_prior = sum(prior, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(
        prior = purrr::map_chr(
          seq_len(n()),
          ~ paste(
            rep(round(total_prior[.x] / outgoing_edges2[.x], 3),
                outgoing_edges2[.x]),
            collapse = ", "
          )
        )
      )

    stage_df <- dplyr::left_join(stage_df, phantom_stage,
                                 by = c("color", "level2", "outgoing_edges2")) #|>
      #dplyr::mutate(prior = dplyr::coalesce(prior.y, prior.x)) |>
      #dplyr::select(-prior.x, -prior.y)
  }

  # Prior means
  compute_prior_mean <- function(prior_string) {
    vals <- as.numeric(strsplit(prior_string, ",")[[1]])
    paste(round(vals / sum(vals), 3), collapse = ",")
  }

  stage_df <- stage_df |>
    dplyr::mutate(prior_mean = purrr::map_chr(prior, compute_prior_mean))

  # Return structured object
  structure(
    list(
      table = dplyr::select(
        stage_df,
        Stage = stage,
        Colour = color,
        Level = level2,
        Outgoing_Edges = outgoing_edges2,
        Nodes = number_nodes,
        Prior = prior,
        Prior_Mean = prior_mean
      ) |>
        dplyr::mutate(Prior_Type = prior_type)
    ),
    class = "prior_table"
  )
}

#' Edit Priors in a Prior Table
#'
#' Modifies one or more rows of a prior table created by
#' \code{\link{specify_priors}}.
#'
#' The function validates that the number of supplied Dirichlet parameters
#' matches the number of outgoing edges associated with each stage.
#'
#' @param prior_table An object of class \code{"prior_table"}.
#' @param rows Integer vector specifying rows to modify.
#' @param new_priors List containing replacement prior specifications.
#'
#' @return
#' An updated object of class \code{"prior_table"}.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#' priors <- specify_priors(st, "Uniform")
#'
#' priors <- edit_priors(
#'   priors,
#'   rows = 1,
#'   new_priors = list("2,3,4,5")
#' )
#'
#' print(priors)
#'
#' @seealso
#' \code{\link{specify_priors}}
#'
#' @export
edit_priors <- function(prior_table, rows, new_priors) {

  if (!inherits(prior_table, "prior_table")) {
    stop("Input must be a prior_table object.")
  }

  tbl <- prior_table$table

  if (length(rows) != length(new_priors)) {
    stop("rows and new_priors must have the same length.")
  }

  for (i in seq_along(rows)) {
    r <- rows[i]
    np <- new_priors[[i]]

    # Validate number of values
    k <- tbl$Outgoing_Edges[r]
    vals <- as.numeric(strsplit(np, ",")[[1]])

    if (length(vals) != k) {
      stop(paste0("Row ", r, ": expected ", k, " values."))
    }

    # Update prior
    tbl$Prior[r] <- np

    # Update prior mean
    tbl$Prior_Mean[r] <- paste(round(vals / sum(vals), 3), collapse = ",")

    # Mark this row as Custom
    tbl$Prior_Type[r] <- "Custom"
  }

  prior_table$table <- tbl
  prior_table
}


#' Print a Prior Table
#'
#' Prints the stage-level prior table contained within a
#' \code{"prior_table"} object.
#'
#' @param x An object of class \code{"prior_table"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied object, invisibly.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#' priors <- specify_priors(st, "Uniform")
#'
#' print(priors)
#'
#' @method print prior_table
#' @export
print.prior_table <- function(x, ...) {
  if (!inherits(x, "prior_table")) {
    stop("Object must be of class 'prior_table'.")
  }

  print(x$table)
  invisible(x)
}

#' Summarise a Prior Table
#'
#' Produces a summary of a prior table including the number of stages,
#' levels, colours and prior types represented.
#'
#' @param object An object of class \code{"prior_table"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_prior_table"}.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#' st <- ahc_colouring(et)
#' priors <- specify_priors(st, "Uniform")
#'
#' summary(priors)
#'
#' @method summary prior_table
#' @export
summary.prior_table <- function(object, ...) {

  if (!inherits(object, "prior_table")) {
    stop("Object must be of class 'prior_table'.")
  }

  tbl <- object$table

  out <- list(
    n_stages = nrow(tbl),
    prior_type = unique(tbl$Prior_Type),
    stages = unique(tbl$Stage),
    levels = unique(tbl$Level),
    colours = unique(tbl$Colour)
  )

  class(out) <- "summary_prior_table"
  out
}

