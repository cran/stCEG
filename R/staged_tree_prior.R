#' Apply Priors to a Staged Tree for Visualization
#'
#' This function applies prior distributions to a staged tree, adjusting priors and calculating the mean and variance of the Dirichlet distributions for each node. The function also generates a visualization using the `visNetwork` package, showing the tree with additional information like prior distributions, means, and variances.
#'
#' @param staged_tree_obj A list containing a staged tree object with the components "nodes" and "edges".
#' @param prior_table A data frame with columns "Colour", "Level", and "Prior", containing prior distributions for each stage.
#' @param level_separation A numeric value determining the separation between levels in the hierarchical layout of the tree. Default is 1000.
#' @param node_distance A numeric value for the distance between nodes. Default is 300.
#' @param label_type A string indicating the label type for edges. It can be one of "names", "priors", or "priormeans". Default is "names".
#'
#' @return A `visNetwork` object that displays the staged tree with adjusted prior distributions and additional information in the tooltips.
#'
#' @details
#' - The function assumes that the staged tree object is structured in a specific format, where the nodes and edges are contained in the "stagedtree" component.
#' - Priors are adjusted based on the number of nodes in each group, and Dirichlet priors are split and normalized.
#' - Tooltips for nodes show the prior distribution, mean, and variance.
#' - Edge labels can show the names, priors, or prior means based on the `label_type` parameter.
#'
#'@examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#' # cannot run this whole chunk as one, as specify_priors needs user input
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' staged_tree
#'
#' @import visNetwork
#'
#' @export
staged_tree_prior <- function(staged_tree_obj, prior_table,level_separation = 1500, node_distance = 250, label_type = "priors") {

  # Error if not a staged_tree object
  if (!("stagedtree" %in% names(staged_tree_obj)) || !("x" %in% names(staged_tree_obj$stagedtree))) {
    stop("Error: Need a staged tree object.")
  }

  nodes_df <- staged_tree_obj$stagedtree$x$nodes
  edges_df <- staged_tree_obj$stagedtree$x$edges

  #Unlist prior for Dirichlet calculations
  convertPrior <- function(prior) {
    as.numeric(unlist(strsplit(prior, ",")))
  }

  adjustPriors <- function(prior, count) {
    prior_values <- convertPrior(prior)
    adjusted <- round(prior_values / count, 3)
    return(paste(adjusted, collapse = ","))
  }

  splitPriors <- function(prior) {
    as.numeric(unlist(strsplit(prior, ",")))
  }

  calculateRatios <- function(priors) {
    total <- sum(priors)
    if (total == 0) return(rep(0, length(priors)))
    return(round(priors / total, 3))
  }

  calculateVariance <- function(priors) {
    total <- sum(priors)
    if (total == 0) return(rep(0, length(priors)))
    return(round((priors * (total - priors)) / (total^2 * (total + 1)), 3))
  }

  # Create tooltip when hovering
  createTooltipWithPrior <- function(prior, ratio, priorvariance) {
    if (!is.na(prior) && prior != "") {
      tooltip_text <- paste(
        "Prior Distribution: Dirichlet(", prior, ")<br>",
        "Prior Mean: (", ratio, ")<br>",
        "Prior Variance: (", priorvariance, ")"
      )
      return(tooltip_text)
    } else {
      return("Leaf nodes have no prior")
    }
  }

  # Initialize new columns
  nodes_df$adjusted_prior <- ""
  nodes_df$priormean <- ""
  nodes_df$priorvariance <- ""

  # Loop through unique (colour, level2) groups
  for (grp in unique(paste(nodes_df$color, nodes_df$level2, sep = "_"))) {
    color_level <- unlist(strsplit(grp, "_"))
    color_match <- color_level[1]
    level2_match <- color_level[2]

    # Find matching row in prior_table
    prior_row <- prior_table[prior_table$"Colour" == color_match & prior_table$"Level" == level2_match, ]

    if (nrow(prior_row) > 0) {
      prior_value <- prior_row$Prior[1]

      # Get the matching nodes
      same_group_nodes <- nodes_df[nodes_df$color == color_match & nodes_df$level2 == level2_match, ]

      if (nrow(same_group_nodes) > 0) {
        count <- nrow(same_group_nodes)

        for (i in 1:nrow(same_group_nodes)) {
          node_index <- which(nodes_df$id == same_group_nodes$id[i])

          if (!is.na(prior_value) && prior_value != "") {
            nodes_df$adjusted_prior[node_index] <- adjustPriors(prior_value, count)

            # Calculate ratios
            prior_values <- splitPriors(prior_value)
            nodes_df$priormean[node_index] <- paste(calculateRatios(prior_values), collapse = ", ")

            # Calculate variance
            nodes_df$priorvariance[node_index] <- paste(calculateVariance(prior_values), collapse = ", ")
          }
        }
      }
    }
  }

  # Assign prior values to edges based on from-node
  for (i in 1:nrow(nodes_df)) {
    from_node <- nodes_df$id[i]
    adj_prior <- nodes_df$adjusted_prior[i]
    adj_mean <- nodes_df$priormean[i]

    if (adj_prior != "") {
      adj_prior_values <- convertPrior(adj_prior)
      adj_mean_values <- convertPrior(adj_mean)
      edges_from_node <- which(edges_df$from == from_node)

      if (length(adj_prior_values) >= length(edges_from_node)) {
        for (j in 1:length(edges_from_node)) {
          edges_df$label_prior_frac[edges_from_node[j]] <- paste(edges_df$label1[edges_from_node[j]], "\n", adj_prior_values[j])
          edges_df$label_prior_mean[edges_from_node[j]] <- paste(edges_df$label1[edges_from_node[j]], "\n", adj_mean_values[j])
          edges_df$label3[edges_from_node[j]] <- adj_prior_values[j]
        }
      }
    }
  }

  nodes_df$title <- apply(nodes_df, 1, function(row) {
    createTooltipWithPrior(row['adjusted_prior'], row['priormean'], row['priorvariance'])
  })

  if (label_type == "names") {
    edges_df$label <- edges_df$label1  # Assign "names" (label1)
  } else if (label_type == "priors") {
    edges_df$label <- edges_df$label_prior_frac  # Assign "priors" (label_prior_frac)
  }
  else if (label_type == "priormeans") {
    edges_df$label <- edges_df$label_prior_mean  # Assign "priors" (label_prior_frac)
  }

# VisNetwork visualisation

  network_plot <- visNetwork(nodes = nodes_df, edges = edges_df) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE, title = nodes_df$title) %>%
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
    result <- invisible(prior_table)


    # Return both the network plot and the result
    output <- list(stagedtreewithpriors = network_plot, priortable = result)

    class(output) <- "staged_tree"
    return(output)
}
