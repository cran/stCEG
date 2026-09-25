#' Create an Event Tree from a Dataset
#'
#' Constructs an event tree from a categorical dataset. Each unique path
#' through the selected variables is represented as a sequence of situations
#' and edges, with edge counts corresponding to observed frequencies in the
#' data.
#'
#' The resulting object can be visualised using \code{plot()} and inspected
#' using \code{print()} and \code{summary()}.
#'
#' @param dataset A data frame containing categorical variables.
#' @param columns A vector of column names or column indices specifying which
#'   variables should be used to construct the event tree. Defaults to all
#'   columns in the dataset.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Constructs a rooted event tree from the selected variables.
#'   \item Creates one edge for each possible transition between levels.
#'   \item Calculates frequencies for all observed and unobserved paths.
#'   \item Stores node and edge information in a format suitable for
#'   visualisation with \pkg{visNetwork}.
#' }
#'
#' @return
#' An object of class \code{"event_tree"} containing:
#' \itemize{
#'   \item \code{nodes}: node information for the event tree.
#'   \item \code{edges}: edge information and frequencies.
#'   \item \code{data}: the filtered dataset used to construct the tree.
#'   \item \code{variables}: variables used in the tree.
#'   \item \code{n_nodes}: total number of nodes.
#'   \item \code{n_edges}: total number of edges.
#' }
#'
#' @examples
#'
#' et <- create_event_tree(homicides, c(1:3))
#'
#' print(et)
#' summary(et)
#'
#' @seealso
#' \code{\link{plot.event_tree}},
#' \code{\link{summary.event_tree}}
#'
#' @export

create_event_tree <- function(dataset,
                              columns = seq_along(dataset)) {

  g <- igraph::make_empty_graph()
  parent <- "s0"

  selected_data <- dataset[, columns]
  filtereddf <- selected_data

  num_vars <- length(columns)

  unique_values_list <- vector("list", num_vars)
  state_names_list  <- vector("list", num_vars)

  start_index  <- 1
  total_states <- 1

  for (i in seq_len(num_vars)) {
    col_values <- unique(selected_data[[i]])
    col_values <- sort(col_values, na.last = TRUE)

    unique_values_list[[i]] <- col_values

    state_names_list[[i]] <- paste0(
      "s",
      start_index:(start_index + length(col_values) * total_states - 1)
    )

    total_states <- total_states * length(col_values)
    start_index  <- start_index + total_states
  }

  g <- igraph::add_vertices(g, 1, name = "s0")

  for (i in seq_len(num_vars)) {
    g <- igraph::add_vertices(g,
                              length(state_names_list[[i]]),
                              name = state_names_list[[i]])
  }

  generate_combinations <- function(df, cols) {
    col_names <- colnames(df)[cols]

    all_combinations <- expand.grid(lapply(df[col_names], unique))

    counts <- dplyr::group_by(df, dplyr::across(dplyr::all_of(col_names)))
    counts <- dplyr::summarise(counts, count = dplyr::n(), .groups = "drop")

    full_data <- dplyr::full_join(all_combinations, counts, by = col_names)

    full_data$count[is.na(full_data$count)] <- 0

    full_data <- dplyr::arrange(full_data, dplyr::across(dplyr::all_of(col_names)))
    full_data
  }

  counts_list <- lapply(seq_len(num_vars),
                        function(x) generate_combinations(selected_data, 1:x))

  edges <- character(0)

  for (i in seq_len(num_vars)) {
    num_states <- length(state_names_list[[i]])
    prev_total_states <- if (i > 1) length(state_names_list[[i - 1]]) else 1

    start_index <- 1
    end_index   <- num_states / prev_total_states

    if (i == 1) {
      for (j in seq_len(num_states)) {
        edges <- c(edges, "s0", state_names_list[[i]][j])
      }
    } else {
      for (j in seq_len(prev_total_states)) {
        parent_state <- state_names_list[[i - 1]][j]
        child_states <- state_names_list[[i]][start_index:end_index]

        for (k in seq_along(child_states)) {
          edges <- c(edges, parent_state, child_states[k])
        }

        start_index <- end_index + 1
        end_index   <- start_index + (num_states / prev_total_states) - 1
      }
    }
  }

  g <- igraph::add_edges(g, edges)

  data <- visNetwork::toVisNetworkData(g)

  num_levels <- num_vars + 1
  data$nodes$level <- rep(
    seq_len(num_levels),
    times = c(1, vapply(seq_len(num_vars),
                        function(x) length(state_names_list[[x]]),
                        integer(1)))
  )

  data$nodes$shape            <- "dot"
  data$nodes$size             <- 100
  data$nodes$color.background <- "#FFFFFF"
  data$nodes$font             <- "80px"
  data$nodes$title            <- data$nodes$id
  data$nodes$color            <- "#FFFFFF"
  data$nodes$level2           <- data$nodes$level
  data$nodes$method           <- NA_character_

  all_column_names <- unique(unlist(lapply(counts_list, colnames)))

  align_columns <- function(df, all_column_names) {
    missing_cols <- setdiff(all_column_names, colnames(df))
    df[missing_cols] <- "IGNORE"
    cols_ordered <- c(setdiff(all_column_names, "count"), "count")
    df[cols_ordered]
  }

  counts_list_aligned <- lapply(counts_list, align_columns, all_column_names)
  df_flat <- do.call(rbind, counts_list_aligned)

  get_last_non_zero_na_rowwise <- function(df) {
    non_count_cols <- colnames(df)[colnames(df) != "count"]

    get_last_non_zero <- function(row) {
      vals <- row[non_count_cols]
      keep <- (vals != 0 | is.na(vals)) & vals != "IGNORE"
      non_zero_entries <- vals[keep]

      if (length(non_zero_entries) > 0) {
        last_non_zero <- tail(non_zero_entries, 1)
      } else {
        last_non_zero <- NA
      }

      ifelse(is.na(last_non_zero), "NA", last_non_zero)
    }

    apply(df, 1, get_last_non_zero)
  }

  last_entries <- get_last_non_zero_na_rowwise(df_flat)

  data$edges$label1 <- last_entries
  data$edges$label2 <- df_flat$count
  data$edges$label3 <- paste(data$edges$label1, "\n", data$edges$label2)


  data$edges$color     <- "#000000"
  data$edges$arrows    <- "to"

  obj <- list(
    nodes      = data$nodes,
    edges      = data$edges,
    data       = filtereddf,
    variables  = colnames(selected_data),
    n_nodes    = nrow(data$nodes),
    n_edges    = nrow(data$edges)
  )

  class(obj) <- "event_tree"
  obj
}

#' Print an Event Tree
#'
#' Prints a concise summary of an event tree including the variables used,
#' number of nodes and number of edges.
#'
#' @param x An object of class \code{"event_tree"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied \code{"event_tree"} object, invisibly.
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#'
#' print(et)
#'
#' @method print event_tree
#' @export
print.event_tree <- function(x, ...) {
  cat("Event Tree\n")
  cat("-----------\n")
  cat("Variables:", paste(x$variables, collapse = ", "), "\n")
  cat("Nodes:", x$n_nodes, "\n")
  cat("Edges:", x$n_edges, "\n")
  cat("\n")
  invisible(x)
}

#' Summarise an Event Tree
#'
#' Produces a summary of an event tree including variable names, graph size,
#' and previews of node and edge information.
#'
#' @param object An object of class \code{"event_tree"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item Variables used to construct the tree.
#'   \item Number of nodes.
#'   \item Number of edges.
#'   \item Preview of node information.
#'   \item Preview of edge information.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#'
#' summary(et)
#'
#' @method summary event_tree
#' @export
summary.event_tree <- function(object, ...) {
  list(
    variables     = object$variables,
    n_nodes       = object$n_nodes,
    n_edges       = object$n_edges,
    nodes_preview = utils::head(object$nodes),
    edges_preview = utils::head(object$edges)
  )
}

#' Plot an Event Tree
#'
#' Produces an interactive visualisation of an event tree using
#' \pkg{visNetwork}.
#'
#' Edge labels may display either transition names only or both transition
#' names and observed frequencies.
#'
#' @param x An object of class \code{"event_tree"}.
#' @param label_type Character string specifying the edge label format.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"both"} (default), displaying labels and frequencies.
#'     \item \code{"names"}, displaying labels only.
#'   }
#' @param level_separation Numeric value controlling spacing between levels.
#'   Default is \code{1600}.
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Default is \code{400}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A \pkg{visNetwork} htmlwidget.
#'
#' @examples
#' \dontrun{
#' et <- create_event_tree(homicides, c(1:3))
#'
#' plot(et)
#' }
#'
#' @seealso
#' \code{\link{create_event_tree}}
#'
#' @method plot event_tree
#' @export
plot.event_tree <- function(x,
                            label_type = "both",
                            level_separation = 1600,
                            node_distance = 400,
                            ...) {

  edges <- x$edges

  edges$label <- switch(label_type,
                        names = edges$label1,
                        both  = edges$label3,
                        edges$label3)

  edges$font.size <- node_distance*0.2

  nodes <- x$nodes

  nodes <- nodes %>%
    dplyr::mutate(fixed = list(list(x = TRUE, y = FALSE)))


  visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
    visNetwork::visHierarchicalLayout(direction = "LR",
                                      levelSeparation = level_separation) %>%
    visNetwork::visNodes(
      label = nodes$label,
      scaling = list(min = 10, max = 10),
      font    = list(vadjust = -170),
      fixed   = TRUE
    ) %>%
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 5))
    ) %>%
    visNetwork::visOptions(
      manipulation = list(
        enabled       = FALSE,
        addEdgeCols   = FALSE,
        addNodeCols   = FALSE,
        editEdgeCols  = FALSE,
        editNodeCols  = c("color"),
        multiselect   = TRUE
      ),
      nodesIdSelection = FALSE
    ) %>%
    visNetwork::visInteraction(
      dragNodes        = TRUE,
      multiselect      = TRUE,
      navigationButtons = TRUE
    ) %>%
    visNetwork::visPhysics(
      hierarchicalRepulsion = list(nodeDistance = node_distance),
      stabilization         = TRUE,
    ) %>%
    visNetwork::visEvents(
      stabilizationIterationsDone = "function() {
      this.physics.options.enabled = false;
    }"
    )
}







