#' Compute a Chain Event Graph (CEG)
#'
#' Constructs a Chain Event Graph (CEG) from a
#' \code{"staged_tree_priors"} object by contracting situations that share
#' equivalent future developments. The resulting graph contains contracted
#' vertices, aggregated edge information, posterior and prior summaries, and
#' a stage-level summary table.
#'
#' During construction, nodes with equivalent stage colours and downstream
#' structures are recursively merged to form the final CEG representation.
#' Edge counts, prior values, posterior values, and corresponding probability
#' summaries are aggregated across contracted situations.
#'
#' @param staged_tree_priors An object of class
#'   \code{"staged_tree_priors"} created by
#'   \code{\link{compute_staged_tree_priors}}.
#'
#' @details
#' The procedure:
#' \enumerate{
#'   \item Assigns contraction identifiers to nodes based on stage colours and
#'   downstream structure.
#'   \item Contracts equivalent situations into CEG vertices.
#'   \item Aggregates edge counts, prior information and posterior information.
#'   \item Calculates prior and posterior transition probabilities for each
#'   stage.
#'   \item Creates a stage-level summary table suitable for downstream model
#'   inspection and comparison.
#' }
#'
#' @return
#' An object of class \code{"ceg"} containing:
#' \itemize{
#'   \item \code{nodes}: contracted CEG vertices.
#'   \item \code{edges}: aggregated transition edges with counts, priors,
#'   posteriors and probability summaries.
#'   \item \code{table}: stage-level summary table containing data, prior,
#'   posterior and probability information.
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
#' print(ceg)
#' summary(ceg)
#'
#' @seealso
#' \code{\link{plot.ceg}},
#' \code{\link{summary.ceg}}
#'
#' @export
compute_ceg <- function(staged_tree_priors) {

  if (!inherits(staged_tree_priors, "staged_tree_priors")) {
    stop("Input must be an object of class 'staged_tree_priors'.")
  }

  nodes <- staged_tree_priors$nodes
  edges <- staged_tree_priors$edges
  prior_table <- staged_tree_priors$prior_table$table
  #print(prior_table)

  ## 1. Initialise contract IDs ---------------------------------------------

  nodes$contract_id <- paste0(nodes$level2, "-", nodes$color)

  update_contract_ids <- function(nodes, edges) {

    nodes$contract_id[nodes$level2 == min(nodes$level2, na.rm = TRUE)] <-
      paste0(min(nodes$level2, na.rm = TRUE), "-#FFFFFF")

    nodes$contract_id[nodes$level2 == max(nodes$level2, na.rm = TRUE)] <-
      paste0(max(nodes$level2, na.rm = TRUE), "-#FFFFFF")

    for (lvl in sort(unique(nodes$level2), decreasing = TRUE)) {

      if (lvl %in% c(min(nodes$level2, na.rm = TRUE),
                     max(nodes$level2, na.rm = TRUE))) next

      current <- nodes[nodes$level2 == lvl, ]

      for (i in seq_len(nrow(current))) {

        node <- current[i, ]
        ce <- edges[edges$from == node$id | edges$to == node$id, ]
        cn <- unique(c(ce$from, ce$to))
        cn <- cn[cn != node$id]

        if (length(cn) > 0) {
          cid <- nodes$contract_id[nodes$id %in% cn]
          clvl <- nodes$level2[nodes$id %in% cn]
          higher <- cid[clvl >= node$level2]
          nodes$contract_id[nodes$id == node$id] <-
            paste0(nodes$contract_id[nodes$id == node$id],
                   "-", paste(higher, collapse = "-"))
        }
      }
    }

    nodes
  }

  nodes <- update_contract_ids(nodes, edges)

  ## 2. Contracted nodes -----------------------------------------------------

  contracted_nodes <- nodes |>
    dplyr::group_by(contract_id) |>
    dplyr::summarise(
      ids   = paste(id, collapse = ", "),
      label = dplyr::first(label),
      level = dplyr::first(level2),
      color = dplyr::first(color),
      .groups = "drop"
    )

  contracted_nodes <- contracted_nodes[
    order(as.numeric(gsub("[^0-9]", "", contracted_nodes$label))), ]

  n_nodes <- nrow(contracted_nodes)
  contracted_nodes$label <- paste0("w", 0:(n_nodes - 1))
  contracted_nodes$label[n_nodes] <- paste0("w", "\u221E")
  contracted_nodes$id <- contracted_nodes$label
  contracted_nodes$font <- "80px"
  contracted_nodes$size <- 100

  ## 3. Map original IDs to contracted IDs ----------------------------------

  id_mapping <- lapply(seq_len(nrow(contracted_nodes)), function(i) {
    ids <- unlist(strsplit(contracted_nodes$ids[i], ",\\s*"))
    ids <- trimws(ids)
    stats::setNames(rep(contracted_nodes$label[i], length(ids)), ids)
  })

  id_mapping <- unlist(id_mapping, use.names = TRUE)

  updated_edges <- edges
  updated_edges$from <- id_mapping[as.character(updated_edges$from)]
  updated_edges$to   <- id_mapping[as.character(updated_edges$to)]

  updated_edges <- updated_edges |>
    dplyr::select(-color) |>
    dplyr::left_join(
      contracted_nodes |>
        dplyr::select(id, color),
      by = c("from" = "id")
    ) |>
    dplyr::rename(colour_from = color)


  #print(updated_edges)
  #print("___________________")
  ## 4. Merge and summarise edges -------------------------------------------

  merged_edges <- updated_edges |>
    dplyr::group_by(from, to, label1, colour_from) |>
    dplyr::summarise(
      sumlabel2 = sum(label2, na.rm = TRUE),
      sumlabel3 = sum(as.numeric(label3), na.rm = TRUE),
      total     = sumlabel2 + sumlabel3,
      label_individuals = paste(dplyr::first(label1), "\n", total),
      font.size = "80px",
      colour_from = dplyr::first(colour_from),
      .groups = "drop"
    )

  merged_edges <- merged_edges |>
    dplyr::group_by(colour_from) |>
    dplyr::mutate(stage_total_posterior = sum(total, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(colour_from, label1) |>
    dplyr::mutate(posterior_total = sum(total, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(colour_from) |>
    dplyr::mutate(stage_total_prior = sum(sumlabel3, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(colour_from, label1) |>
    dplyr::mutate(prior_total = sum(sumlabel3, na.rm = TRUE)) |>
    dplyr::ungroup()

  merged_edges$prior_mean <- round(
    merged_edges$prior_total / merged_edges$stage_total_prior, 3
  )

  merged_edges$posterior_mean <- round(
    merged_edges$posterior_total / merged_edges$stage_total_posterior, 3
  )

  merged_edges$label_posterior   <- paste(merged_edges$label1, "\n",
                                          merged_edges$posterior_mean)
  merged_edges$label_prior_mean  <- paste(merged_edges$label1, "\n",
                                          merged_edges$prior_mean)
  merged_edges$label_prior       <- paste(merged_edges$label1, "\n",
                                          merged_edges$prior_total)
  merged_edges$color <- "#000000"

  curvature_values <- merged_edges %>%
    group_by(from, to) %>%
    mutate(
      curvature = seq(-0.3, 0.3, length.out = n()) # Ensure curvature is evenly spaced
    ) %>%
    ungroup()

  merged_edges$smooth <- pmap(curvature_values, function(from, to, curvature, ...) {
    list(enabled = TRUE, type = "curvedCW", roundness = curvature)
  })

  #print(merged_edges)

  merged_edges <- merged_edges |>
    dplyr::left_join(
      contracted_nodes |>
        dplyr::select(label, level),
      by = c("from" = "label")
    )

  #print(merged_edges)
  ## 5. Aggregated stage-level table ----------------------------------------

  aggregated_df <- merged_edges |>
    dplyr::group_by(colour_from, level, label1) |>
    dplyr::summarise(
      data      = sum(sumlabel2, na.rm = TRUE),
      prior     = sum(sumlabel3, na.rm = TRUE),
      posterior = sum(total, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::arrange(level, label1)

  aggregated_df <- aggregated_df |>
    dplyr::group_by(colour_from, level) |>
    dplyr::summarise(
      data      = paste(data, collapse = ","),
      prior     = paste(prior, collapse = ","),
      posterior = paste(posterior, collapse = ","),
      prior_mean = paste(
        round(
          as.numeric(unlist(strsplit(prior, ","))) /
            sum(as.numeric(unlist(strsplit(prior, ",")))), 3
        ),
        collapse = ","
      ),
      posterior_mean = paste(
        round(
          as.numeric(unlist(strsplit(posterior, ","))) /
            sum(as.numeric(unlist(strsplit(posterior, ",")))), 3
        ),
        collapse = ","
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(level)


  contracted_nodes <- contracted_nodes %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))
  ## 6. Merge with prior_table ----------------------------------------------

  prior_table2 <- prior_table |>
    dplyr::rename(colour_from = Colour, level = Level)


  merged_table <- prior_table2 |>
    dplyr::left_join(aggregated_df, by = c("colour_from", "level")) |>
    dplyr::select(
      Stage, colour_from, level,
      data, prior, prior_mean,
      posterior, posterior_mean, Prior_Type
    ) |>
    dplyr::mutate(
      dplyr::across(c(prior, posterior), ~ purrr::map_chr(.x, function(s) {
        vals <- as.numeric(unlist(strsplit(s, ",")))
        if (any(abs(vals %% 1 - 0.999) < 1e-6 | abs(vals %% 1 - 0.001) < 1e-6)) {
          vals <- round(vals)
        }
        paste(vals, collapse = ",")
      }))
    ) |>
    dplyr::rename(
      Colour          = colour_from,
      Level           = level,
      Data            = data,
      Prior           = prior,
      Prior_Mean    = prior_mean,
      Posterior       = posterior,
      Posterior_Mean = posterior_mean
    )

  merged_table$Colour <- as.character(merged_table$Colour)

  ## 7. Return CEG object ----------------------------------------------------

  structure(
    list(
      nodes = contracted_nodes,
      edges = merged_edges,
      table = merged_table
    ),
    class = "ceg"
  )
}

#' Plot a Chain Event Graph
#'
#' Produces an interactive visualisation of a Chain Event Graph (CEG) using
#' \pkg{visNetwork}. Edge labels can display observed counts, prior values,
#' posterior values, prior probabilities, or posterior probabilities.
#'
#' Selecting a node highlights incoming and outgoing edges, allowing local
#' graph structure to be explored interactively.
#'
#' @param x An object of class \code{"ceg"}.
#' @param label Character string specifying the edge label type to display.
#'   One of:
#'   \itemize{
#'     \item \code{"posterior_mean"} (default)
#'     \item \code{"posterior"}
#'     \item \code{"prior_mean"}
#'     \item \code{"prior"}
#'     \item any other value displays the original edge labels
#'   }
#' @param level_separation Numeric value controlling spacing between graph
#'   levels. Default is \code{1200}.
#' @param node_distance Numeric value controlling spacing between nodes.
#'   Default is \code{400}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A \pkg{visNetwork} htmlwidget representing the Chain Event Graph.
#'
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
#' plot(ceg)
#'
#' plot(
#'   ceg,
#'   label = "posterior_mean"
#' )
#'
#' plot(
#'   ceg,
#'   label = "prior_mean"
#' )
#'
#' plot(
#'   ceg,
#'   label = "posterior"
#' )
#' }
#'
#' @seealso
#' \code{\link{compute_ceg}},
#' \code{\link{summary.ceg}}
#'
#' @method plot ceg
#' @export
plot.ceg <- function(x,
                     label = "posterior_mean",
                     level_separation = 1200,
                     node_distance = 400,
                     ...) {

  if (!inherits(x, "ceg")) {
    stop("Object must be of class 'ceg'.")
  }

  nodes <- x$nodes
  #print("nodes")
  #print(nodes)
  edges <- x$edges
  #print("edges")
  #print(edges)
  edges$font.size <- 100
  ## ----------------------------------------------------------------------
  ## 1. Build tooltips (leaf nodes included)
  ## ----------------------------------------------------------------------

#  nodes$title <- ifelse(
#    is.na(nodes$prior_Mean) | nodes$Prior_Mean == "",
#    "Leaf nodes have no prior",
#    paste0(
#      "Prior: ", nodes$Prior, "<br>",
#      "Prior Mean: ", nodes$Prior_Mean, "<br>",
#      "Prior Variance: ", nodes$Prior_Variance
#    )
#  )

  ## ----------------------------------------------------------------------
  ## 2. Select edge label type
  ## ----------------------------------------------------------------------

  if (label == "posterior") {
    edges$label <- edges$label_individuals
  } else if (label == "posterior_mean") {
    edges$label <- edges$label_posterior
  } else if (label == "prior_mean") {
    edges$label <- edges$label_prior_mean
  } else if (label == "prior") {
    edges$label <- edges$label_prior
  } else {
    edges$label <- edges$label1
  }

  ## ----------------------------------------------------------------------
  ## 3. Build visNetwork plot
  ## ----------------------------------------------------------------------

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

#' Summarise a Chain Event Graph
#'
#' Generates a concise summary of a Chain Event Graph including the number of
#' contracted vertices, edges, stages, levels and available prior and posterior
#' information.
#'
#' @param object An object of class \code{"ceg"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_ceg"} containing:
#' \itemize{
#'   \item Number of vertices.
#'   \item Number of edges.
#'   \item Graph levels.
#'   \item Stage colours.
#'   \item Stage identifiers.
#'   \item Prior type information.
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
#' summary(ceg)
#'
#' @method summary ceg
#' @export
summary.ceg <- function(object, ...) {

  if (!inherits(object, "ceg")) {
    stop("Object must be of class 'ceg'.")
  }

  nodes <- object$nodes
  edges <- object$edges
  table <- object$table
  #print(nodes)
  #print("------")
  #print(table)

  out <- list(
    n_nodes = nrow(nodes),
    n_edges = nrow(edges),
    levels = sort(unique(nodes$level)),
    colours = unique(nodes$color),
    stages = unique(table$Stage),
    n_stages = length(unique(table$Stage)),
    prior_types = unique(table$Prior_Type %||% NA),
    has_prior_info = !all(is.na(table$Prior)),
    has_posterior_info = !all(is.na(table$Posterior))
  )

  class(out) <- "summary_ceg"
  out
}


#' Print a Chain Event Graph
#'
#' Prints a concise overview of a Chain Event Graph including the number of
#' vertices, edges and stages.
#'
#' @param x An object of class \code{"ceg"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied \code{"ceg"} object, invisibly.
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
#' print(ceg)
#'
#' @method print ceg
#' @export
print.ceg <- function(x, ...) {

  if (!inherits(x, "ceg")) {
    stop("Object must be of class 'ceg'.")
  }

  cat("Chain Event Graph\n")
  cat("=================\n")

  cat("Nodes:   ", nrow(x$nodes), "\n")
  cat("Edges:   ", nrow(x$edges), "\n")
  cat("Stages:  ", length(unique(x$table$Stage)), "\n\n")

  cat("Use summary(x) for detailed information.\n")
  cat("Use plot(x) to visualize the graph.\n")

  invisible(x)
}

#' Compare Two Chain Event Graph Models
#'
#' Compares two fitted Chain Event Graph (CEG) models using their log marginal
#' likelihoods and calculates the corresponding Bayes Factor.
#'
#' Evidence in favour of each model is quantified and classified according to
#' Jeffreys' scale for Bayes Factors.
#'
#' @param ceg1 An object of class
#'   \code{"ceg"}.
#' @param ceg2 An object of class
#'   \code{"ceg"}.
#'
#' @details
#' The Bayes Factor is computed as:
#'
#' \deqn{
#' BF = \exp(\log p(D \mid M_1) - \log p(D \mid M_2))
#' }
#'
#' where \eqn{p(D \mid M)} denotes the marginal likelihood of a model.
#'
#' The resulting Bayes Factor is interpreted using Jeffreys' evidence scale.
#'
#' @return
#' An object of class \code{"compare_ceg_models"} containing:
#' \itemize{
#'   \item Log marginal likelihoods for both models.
#'   \item Log Bayes Factor.
#'   \item Bayes Factor.
#'   \item Preferred model.
#'   \item Evidence measures for each model.
#'   \item Jeffreys evidence category.
#' }
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
#' ceg_model1 <- compute_ceg(st_priors)
#'
#' priors2 <- specify_priors(
#'   st,
#'   prior_type = "Phantom"
#' )
#'
#' st_priors2 <- compute_staged_tree_priors(
#'   st,
#'   priors2
#' )
#'
#' ceg_model2 <- compute_ceg(st_priors2)
#'
#' comparison <- compare_ceg_models(
#'   ceg_model1,
#'   ceg_model2
#' )
#'
#' print(comparison)
#' summary(comparison)
#' }
#'
#' @seealso
#' \code{\link{summary.compare_ceg_models}}
#'
#' @export
compare_ceg_models <- function(ceg1, ceg2) {

  # ------------------------------------------------------------
  # Check that both inputs are chain event graph objects
  # ------------------------------------------------------------

  if (!inherits(ceg1, "ceg") ||
      !inherits(ceg2, "ceg")) {
    stop("Both inputs must be 'ceg' objects.")
  }


  # ------------------------------------------------------------
  # Function to calculate log marginal likelihood
  # and per-stage log scores
  # ------------------------------------------------------------

  calculate_log_scores <- function(ceg) {

    if (is.null(ceg$table)) {
      stop(
        "The ceg object does not contain an update_table."
      )
    }

    update_table <- ceg$table

    total_score <- 0

    stage_scores <- numeric(nrow(update_table))
    effective_sample_sizes <- numeric(nrow(update_table))


    # ----------------------------------------------------------
    # Calculate score for each stage
    # ----------------------------------------------------------

    for (i in 1:nrow(update_table)) {

      prior <- as.numeric(
        unlist(strsplit(update_table$Prior[i], ","))
      )

      data <- as.numeric(
        unlist(strsplit(update_table$Data[i], ","))
      )


      # Replace zero values
      prior <- ifelse(prior == 0, 1e-10, prior)
      data  <- ifelse(data == 0, 1e-10, data)


      alpha_sum <- sum(prior)
      x_sum <- sum(data)

      posterior_sum <- alpha_sum + x_sum


      # --------------------------------------------------------
      # Log marginal likelihood for this stage
      # --------------------------------------------------------

      term1 <- lgamma(alpha_sum) -
        lgamma(posterior_sum)

      term2 <- sum(
        lgamma(prior + data) -
          lgamma(prior)
      )

      stage_score <- term1 + term2

      stage_scores[i] <- stage_score

      total_score <- total_score + stage_score


      # --------------------------------------------------------
      # Effective sample size
      # --------------------------------------------------------

      alpha_star <- prior + data

      effective_sample_sizes[i] <- sum(alpha_star)
    }


    # ----------------------------------------------------------
    # Per-stage results
    # ----------------------------------------------------------

    per_stage_scores <- data.frame(
      Stage = update_table$Stage,
      LogScore = round(stage_scores, 3),
      ESS = round(effective_sample_sizes, 2),
      stringsAsFactors = FALSE
    )


    # ----------------------------------------------------------
    # Return results
    # ----------------------------------------------------------

    list(
      total_log_marginal_likelihood = total_score,
      per_stage_log_scores = per_stage_scores
    )
  }


  # ============================================================
  # CALCULATE SCORES FOR BOTH CEGs
  # ============================================================

  scores1 <- calculate_log_scores(ceg1)

  scores2 <- calculate_log_scores(ceg2)


  # ============================================================
  # TOTAL LOG MARGINAL LIKELIHOOD
  # ============================================================

  log_marginal_1 <-
    scores1$total_log_marginal_likelihood

  log_marginal_2 <-
    scores2$total_log_marginal_likelihood


  # ============================================================
  # BAYES FACTOR
  # ============================================================

  logBF <- log_marginal_1 - log_marginal_2

  BF <- exp(logBF)


  # ============================================================
  # EVIDENCE MEASURES
  # ============================================================

  evidence_model1 <- if (logBF > 0) {
    logBF
  } else {
    0
  }

  evidence_model2 <- if (logBF < 0) {
    -logBF
  } else {
    0
  }


  # ============================================================
  # JEFFREYS SCALE
  # ============================================================

  jeffreys_scale <- function(BF) {

    if (is.na(BF) || BF <= 0) {
      return("Invalid Bayes Factor")

    } else if (BF < 1) {
      return("Evidence against Model 1")

    } else if (BF < 3) {
      return("Barely worth mentioning")

    } else if (BF < 10) {
      return("Substantial evidence")

    } else if (BF < 30) {
      return("Strong evidence")

    } else if (BF < 100) {
      return("Very strong evidence")

    } else {
      return("Decisive evidence")
    }
  }


  # ============================================================
  # CREATE OUTPUT
  # ============================================================

  out <- list(

    log_marginal_1 = log_marginal_1,

    log_marginal_2 = log_marginal_2,

    log_scores_1 =
      scores1$per_stage_log_scores,

    log_scores_2 =
      scores2$per_stage_log_scores,

    log_Bayes_factor = logBF,

    Bayes_factor = BF,

    preferred_model =
      ifelse(
        logBF > 0,
        "Model 1",
        "Model 2"
      ),

    evidence_model1 = evidence_model1,

    evidence_model2 = evidence_model2,

    jeffreys_category =
      jeffreys_scale(BF)
  )


  class(out) <- "compare_ceg_models"

  return(out)
}


#' Summarise a CEG Model Comparison
#'
#' Produces a summary of a Bayes Factor comparison between two Chain Event
#' Graph models.
#'
#' @param object An object of class \code{"compare_ceg_models"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_compare_ceg_models"} containing Bayes
#' Factor statistics, preferred model information and evidence measures.
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
#' ceg_model1 <- compute_ceg(st_priors)
#'
#' priors2 <- specify_priors(
#'   st,
#'   prior_type = "Phantom"
#' )
#'
#' st_priors2 <- compute_staged_tree_priors(
#'   st,
#'   priors2
#' )
#'
#' ceg_model2 <- compute_ceg(st_priors2)
#'
#' comparison <- compare_ceg_models(
#'   ceg_model1,
#'   ceg_model2
#' )
#'
#' summary(comparison)
#'
#' @method summary compare_ceg_models
#' @export
summary.compare_ceg_models <- function(object, ...) {

  out <- list(
    log_marginal_1   = object$log_marginal_1,
    log_marginal_2   = object$log_marginal_2,
    log_Bayes_factor = object$log_Bayes_factor,
    Bayes_factor     = object$Bayes_factor,
    preferred_model  = object$preferred_model,
    evidence_model1  = object$evidence_model1,
    evidence_model2  = object$evidence_model2,
    jeffreys_category = object$jeffreys_category
  )

  class(out) <- "summary_compare_ceg_models"
  out
}



