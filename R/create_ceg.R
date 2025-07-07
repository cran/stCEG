#' Create a Chain Event Graph (CEG)
#'
#' This function generates a Chain Event Graph (CEG) from a staged tree object and a prior table.
#' It allows for node contraction and mapping, adjusts edge and node attributes,
#' and visualizes the graph with specific customisations for labels, colour, and node arrangement.
#'
#' @param staged_tree_obj A staged tree object containing nodes and edges. It should have the following structure:
#' \itemize{
#'   \item `staged_tree_obj$x$nodes` : A data frame of node attributes with columns such as `id`, `level`, `colour`, `label`, etc.
#'   \item `staged_tree_obj$x$edges` : A data frame of edge attributes with columns such as `from`, `to`, `label1`, `label2`, `label3`, etc.
#' }
#' @param level_separation Numeric. The level separation value for hierarchical layout in the visualised graph. Default is 1200.
#' @param node_distance Numeric. The node distance value for hierarchical layout in the visualised graph. Default is 400.
#' @param label A character string specifying the type of label to display on edges. Options include:
#'   - `"prior"`: Uses posterior label information.
#'   - `"prior_mean"`: Uses posterior label information.
#'   - `"posterior"`: Uses posterior label information.
#'   - `"posterior_mean"`: Uses posterior mean label information.
#'   - `"none"`: No labels on edges.
#'   Default is `"posterior_mean"`.
#' @param view_table Logical. Whether to display the summary table of the aggregated CEG data in the console. Default is `FALSE`.
#'
#' @return A `visNetwork` object representing the Chain Event Graph, including contracted nodes and updated edges.If view_table = TRUE, a coloured table is returned that can be displayed in the viewer, instead of just an update_table which can be viewed in the console.
#'
#' @details This function processes the staged tree and prior table, contracts nodes based on connected nodes,
#' creates aggregated edge summaries, computes posterior and prior mean values, and visualizes the CEG
#' with hierarchical layout, customizable labels, and node distance adjustments.
#'
#' The function also provides detailed printing of updated edges and contracted nodes for debugging purposes.
#'
#' @import stringr
#' @import DT
#' @import htmltools
#' @import visNetwork
#' @importFrom purrr pmap map_chr
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
#'
#'
#' @export
#'
create_ceg <- function(staged_tree_obj, level_separation = 1200, node_distance = 400, label = "posterior", view_table = FALSE) {
  nodes <- staged_tree_obj$stagedtreewithpriors$x$nodes
  edges <- staged_tree_obj$stagedtreewithpriors$x$edges
  nodes$size = 400

  # Initialize contract IDs
  nodes$contract_id <- paste0(nodes$level2, "-", nodes$color)

  # Function to update contract IDs by appending connected nodes' contract IDs
  update_contract_ids <- function(nodes, edges) {
    # Contract nodes at levels 1 and 5 separately
    nodes$contract_id[nodes$level2 == 1] <- "1-#FFFFFF"
    nodes$contract_id[nodes$level2 == max(nodes$level2)] <- paste0(max(nodes$level2), "-#FFFFFF")

    for (level2 in sort(unique(nodes$level2), decreasing = TRUE)) {
      if (level2 == 1 || level2 == max(nodes$level2)) next

      current_level_nodes <- nodes[nodes$level2 == level2, ]

      for (i in 1:nrow(current_level_nodes)) {
        node <- current_level_nodes[i, ]
        connected_edges <- edges[edges$from == node$id | edges$to == node$id, ]

        connected_nodes <- unique(c(connected_edges$from, connected_edges$to))
        connected_nodes <- connected_nodes[connected_nodes != node$id]

        if (length(connected_nodes) > 0) {
          connected_contract_ids <- nodes$contract_id[nodes$id %in% connected_nodes]
          connected_levels <- nodes$level[nodes$id %in% connected_nodes]
          higher_or_same_level_ids <- connected_contract_ids[connected_levels >= node$level2]
          nodes$contract_id[nodes$id == node$id] <- paste0(nodes$contract_id[nodes$id == node$id], "-", paste(higher_or_same_level_ids, collapse = "-"))
        }
      }
    }
    return(nodes)
  }

  # Apply contract IDs update
  nodes <- update_contract_ids(nodes, edges)


  # Contracted nodes aggregation
  contracted_nodes <- nodes %>%
    group_by(contract_id) %>%
    summarise(
      ids = paste(id, collapse = ", "),
      label = first(label),
      level = first(level2),
      color = first(color),
      #prior_variance = first(priorvariance),
      .groups = 'drop'
    )


  # Sort contracted nodes by labels numerically
  contracted_nodes <- contracted_nodes[order(as.numeric(gsub("[^0-9]", "", contracted_nodes$label))), ]

  # Reassign labels sequentially from w0 to w(n-1) and set the last label as w∞
  num_nodes <- nrow(contracted_nodes)
  contracted_nodes$label <- paste0("w", 0:(num_nodes - 1))
  contracted_nodes$label[num_nodes] <- paste0("w", "\u221E")  # Unicode for ∞
  contracted_nodes$id <- contracted_nodes$label
  contracted_nodes$font <- "80px"
  contracted_nodes$size <- 100


  # Mapping individual node IDs to contracted node IDs
  id_mapping <- lapply(1:nrow(contracted_nodes), function(i) {
    # Split the ids string into individual ids
    ids <- unlist(strsplit(contracted_nodes$ids[i], ",\\s*"))
    ids <- trimws(ids)  # Remove any extra spaces
    # Create a named vector where each id is mapped to the corresponding label
    setNames(rep(contracted_nodes$label[i], length(ids)), ids)
  })

  # Flatten the list into a single vector
  id_mapping <- unlist(id_mapping, use.names = TRUE)

  #print("contracted_nodes")
  #print(contracted_nodes)

  # Copy edges and replace IDs with contracted IDs
  updated_edges <- edges
  updated_edges$from <- id_mapping[as.character(updated_edges$from)]
  updated_edges$to <- id_mapping[as.character(updated_edges$to)]

  # Print the updated edges
  #print("Updated Edges with Contracted Node Labels:")
  #print(updated_edges)

  updated_edges <- updated_edges %>%
    select(-color) %>%
    left_join(contracted_nodes %>% select(id, color), by = c("from" = "id")) %>%  # Drop the existing 'colour' column from edges
    rename(colour_from = color)
  # Check the column names of updated_edges to ensure 'colour' is the correct name
  #print(colnames(updated_edges))

#print(updated_edges)

  # Merge and summarize edges
  merged_edges <- updated_edges %>%
    group_by(from, to, label1, colour_from) %>%
    summarise(
      sumlabel2 = sum(label2),
      sumlabel3 = sum(as.numeric(label3)),
      total = (sumlabel2 + sumlabel3),
      label_individuals = paste(first(label1), "\n", (sumlabel2 + sumlabel3)),
      font.size = first(font.size),
      colour_from = first(colour_from),
      .groups = 'drop'
    )

  #print(merged_edges)

  merged_edges <- merged_edges %>%
    group_by(colour_from) %>%
    mutate(stage_total_posterior = sum(total, na.rm = TRUE)) %>%
    ungroup()


  merged_edges <- merged_edges %>%
    group_by(colour_from, label1) %>%
    mutate(posterior_total = sum(total, na.rm = TRUE)) %>%
    ungroup()

  merged_edges <- merged_edges %>%
    group_by(colour_from) %>%
    mutate(stage_total_prior = sum(sumlabel3, na.rm = TRUE)) %>%
    ungroup()

  merged_edges <- merged_edges %>%
    group_by(colour_from, label1) %>%
    mutate(prior_total = sum(sumlabel3, na.rm = TRUE)) %>%
    ungroup()



  merged_edges$prior_mean <- round(merged_edges$prior_total/merged_edges$stage_total_prior,3)

  merged_edges$posterior_mean <- round(merged_edges$posterior_total/merged_edges$stage_total_posterior,3)
  merged_edges$label_posterior = paste(merged_edges$label1, "\n", merged_edges$posterior_mean)
  merged_edges$label_prior_mean = paste(merged_edges$label1, "\n", merged_edges$prior_mean)
  merged_edges$label_prior = paste(merged_edges$label1, "\n", merged_edges$prior_total)
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


  merged_edges <- merged_edges %>%
    left_join(contracted_nodes %>% select(label, level), by = c("from" = "label"))

  # Return the contracted nodes and edges

  #print(contracted_nodes)
  #print(merged_edges)

  if (label == "posterior") {
    merged_edges$label <- merged_edges$label_individuals  # Assign "names" (label1)
  } else if (label == "posterior_mean") {
    merged_edges$label <-  merged_edges$label_posterior  # Assign "priors" (label_prior_frac)
  } else if (label == "none") {
    merged_edges$label <-  merged_edges$label1  # Assign "priors" (label_prior_frac)
  } else if (label == "prior_mean") {
    merged_edges$label <-  merged_edges$label_prior_mean # Assign "priors" (label_prior_frac)
  } else if (label == "prior") {
    merged_edges$label <-  merged_edges$label_prior # Assign "priors" (label_prior_frac)
  }

  aggregated_df <- merged_edges %>%
    group_by(colour_from, level, label1) %>%
    summarise(
      data = sum(sumlabel2, na.rm = TRUE),
      prior = sum(sumlabel3, na.rm = TRUE),
      posterior = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(level, label1)

  aggregated_df <- aggregated_df %>%
    group_by(colour_from, level) %>%
    summarise(
      data = paste(data, collapse = ","),
      prior = paste(prior, collapse = ","),
      posterior = paste(posterior, collapse = ","),
      prior_mean = paste(round(as.numeric(unlist(strsplit(prior, ","))) /
                                 sum(as.numeric(unlist(strsplit(prior, ",")))), 3), collapse = ","),
      posterior_mean = paste(round(as.numeric(unlist(strsplit(posterior, ","))) /
                                     sum(as.numeric(unlist(strsplit(posterior, ",")))), 3), collapse = ","),
      .groups = "drop"
    ) %>%
    arrange(level)

  #print(colnames(aggregated_df))

  contracted_nodes <- contracted_nodes %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))

  # Rename the columns in prior_table to match aggregated_df
  prior_table <- staged_tree_obj$priortable %>%
    rename(colour_from = Colour)

  prior_table <- prior_table %>%
    rename(level = Level)

  # Perform the left join
  merged_table <- prior_table %>%
    left_join(aggregated_df, by = c("colour_from", "level")) %>%
    select(Stage, colour_from, level, data, prior, prior_mean, posterior, posterior_mean) %>%
    mutate(
      across(c(prior, posterior), ~ map_chr(.x, ~ {
        values <- as.numeric(unlist(strsplit(.x, ",")))  # Convert string to numeric vector
        if (any(abs(values %% 1 - 0.999) < 1e-6 | abs(values %% 1 - 0.001) < 1e-6)) {
          values <- round(values)  # Round if any number ends in .999 or .001
        }
        paste(values, collapse = ",")  # Rejoin into a string
      }))
    ) %>% rename(
      Stage = Stage,
      Colour = colour_from,
      Level = level,
      Data = data,
      Prior = prior,
      `Prior Mean` = prior_mean,
      Posterior = posterior,
      `Posterior Mean` = posterior_mean
    )

  #print("mergedtable")
  #print(merged_table)

  #  for (col in merged_table$Colour) {
  #    styled_text <- make_style(col, bg = TRUE)
  #    cat(styled_text(paste(col)), "\n")
  #  }

  # Ensure 'Colour' is a character vector
  merged_table$Colour <- as.character(merged_table$Colour)

  # Count number of unique stages
  num_stages <- length(unique(merged_table$Stage))

  # Create the datatable with styling
  UpdateTable <- DT::datatable(
    merged_table,
    escape = FALSE,
    class = 'stripe hover row-border compact',  # modern minimal style
    options = list(
      pageLength = num_stages,
      dom = 't<"bottom"i>',  # modern layout: table body, then info and pagination at bottom
      columnDefs = list(
        list(targets = which(names(merged_table) == "Colour"), visible = FALSE)
      ),
      initComplete = JS(  # modern header styling
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f9f9f9', 'color': '#333', 'font-family': 'Segoe UI, sans-serif', 'font-size': '14px'});",
        "}"
      )
    )
  ) %>%
    formatStyle(
      'Stage',
      backgroundColor = styleEqual(merged_table$Stage, merged_table$Colour),
      fontFamily = 'Segoe UI, sans-serif',
      fontSize = '13px',
      color = 'black',
      fontWeight = '500',
      padding = '6px'
    )

  ChainEventGraph <- visNetwork(nodes = contracted_nodes, edges = merged_edges) %>%
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
      selectNode = "function(params) { /* Node selection code */ }",
      deselectNode = "function(params) { /* Deselect code */ }"
    ) %>%
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

  # Create the result list (with invisible filtereddf)
  result <- merged_table

  #print(contracted_nodes)
  #print(merged_edges)

  # Return both the network plot and the result


  if (view_table) {
    output <- list(ceg = ChainEventGraph, coloured_table = UpdateTable, update_table = result)
  } else {
    output <- list(ceg = ChainEventGraph, update_table = result)
  }
  class(output) <- "chain_event_graph"
  return(output)

}





#' Compare Two Chain Event Graph Models Using Bayes Factors
#'
#' This function compares two fitted Chain Event Graph (CEG) models by evaluating their total log marginal likelihoods and computing the Bayes factor.
#' The comparison identifies the preferred model and reports the strength of evidence using Jeffreys' scale.
#'
#' @param summary1 An object of class `summary.chain_event_graph`, typically the output of `summary()` applied to a fitted CEG model.
#' @param summary2 Another object of class `summary.chain_event_graph` to compare with `summary1`.
#'
#' @return An invisible list of class `ceg_model_comparison` containing:
#' \describe{
#'   \item{log_marginal_1}{Log marginal likelihood of model 1.}
#'   \item{log_marginal_2}{Log marginal likelihood of model 2.}
#'   \item{log_Bayes_factor}{The log Bayes factor comparing model 1 to model 2.}
#'   \item{Bayes_factor}{The Bayes factor (on the original scale).}
#'   \item{preferred_model}{The model preferred based on the Bayes factor.}
#' }
#'
#' @details
#' The Bayes factor is calculated as the ratio of marginal likelihoods of the two models: \eqn{BF = \exp(\log BF)}.
#' Interpretation of the Bayes factor is guided by Jeffreys' scale:
#' \itemize{
#'   \item \strong{< 1:} Evidence against the alternative model
#'   \item \strong{1–3:} Weak evidence
#'   \item \strong{3–10:} Moderate evidence
#'   \item \strong{10–30:} Strong evidence
#'   \item \strong{30–100:} Very strong evidence
#'   \item \strong{> 100:} Decisive evidence
#' }
#'
#' The function prints the log marginal likelihoods, log Bayes factor, Bayes factor, and preferred model.
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#'
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' ceg <- create_ceg(staged_tree, view_table = TRUE)
#'
#'
#' # Define node groups and colours
#' node_groups <- list(c("s1", "s2"), c("s3", "s4"))
#' colours <- c("#BBA0CA", "#8AC6D0")
#'
#' # Apply colours to the event tree
#' custom_tree <- update_node_colours(event_tree, node_groups, colours)
#' custom_coloured_tree <- ahc_colouring(custom_tree)
#'
#' # Cannot run this whole chunk at once as specify_priors needs user input
#' custom_tree_priors <- specify_priors(custom_coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' custom_staged_tree <- staged_tree_prior(custom_coloured_tree, custom_tree_priors)
#' ceg2 <- create_ceg(custom_staged_tree, view_table = TRUE)
#' model1_summary <- summary(ceg)
#' model2_summary <- summary(ceg2)
#' compare_ceg_models(model1_summary, model2_summary)
#'
#'
#' @export
compare_ceg_models <- function(summary1, summary2) {
  if (!inherits(summary1, "summary.chain_event_graph") ||
      !inherits(summary2, "summary.chain_event_graph")) {
    stop("Both inputs must be of class 'summary.chain_event_graph'.")
  }

  log_marginal_1 <- summary1$total_log_marginal_likelihood
  log_marginal_2 <- summary2$total_log_marginal_likelihood

  log_BF <- log_marginal_1 - log_marginal_2
  BF <- exp(log_BF)
  preferred_model = ifelse(log_BF > 0, "Model 1", "Model 2")

  result <- list(
    log_marginal_1 = log_marginal_1,
    log_marginal_2 = log_marginal_2,
    log_Bayes_factor = log_BF,
    Bayes_factor = BF,
    preferred_model = ifelse(log_BF > 0, "Model 1", "Model 2")
  )

  jeffreys_scale <- function(BF) {
    if (BF <= 0 || is.na(BF)) {
      return("Invalid Bayes Factor")
    }

    evidence_strength <- function(x) {
      if (x >= 1 & x < 3) {
        return("Weak")
      } else if (x >= 3 & x < 10) {
        return("Moderate")
      } else if (x >= 10 & x < 30) {
        return("Strong")
      } else if (x >= 30 & x < 100) {
        return("Very strong")
      } else {
        return("Decisive")
      }
    }

    if (BF == 1) {
      return("No evidence either way")
    } else if (BF > 1) {
      return(paste(evidence_strength(BF), "evidence for Model 2 over Model 1"))
    } else {
      return(paste(evidence_strength(1 / BF), "evidence for Model 1 over Model 2"))
    }
  }

  # Print the requested values
  cat("Log marginal of model 1: ", round(log_marginal_1, 3), "\n")
  cat("Log marginal of model 2: ", round(log_marginal_2, 3), "\n")
  cat("Log Bayes factor of Model 1 vs Model 2: ", round(log_BF, 3), "\n")
  #cat("Bayes factor of Model 1 vs Model 2: ", BF, "\n")
  #cat("Jeffreys interpretation: ", jeffreys_scale(BF), "\n")
  cat("Preferred Model:", preferred_model, "\n")

  class(result) <- "ceg_model_comparison"
  invisible(result)
}

