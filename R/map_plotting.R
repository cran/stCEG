#' Calculate Path Products in a Chain Event Graph (CEG)
#'
#' This function calculates the products of probabilities along all paths from a specified root node
#' in a Chain Event Graph (CEG). It traverses the graph, multiplying the posterior means of the edges
#' for each path, and returns the resulting paths and their products.
#'
#' @param nodes_df A data frame representing the nodes in the CEG.
#' @param edges_df A data frame representing the edges in the CEG, containing columns for `from`, `to`,
#'   `label1`, and `posterior_mean`.
#' @param root_node A string specifying the label of the root node from which to start the traversal. Default is "w0".
#'
#' @return A data frame with two columns:
#'   \item{path}{The sequence of nodes in the path, as a string of node labels separated by " -> "}
#'   \item{product}{The product of the posterior means along the path}
#'
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @export
#'
#'
calculate_path_products <- function(nodes_df, edges_df, root_node = "w0") {

  # Initialize a list to store paths and products
  paths_list <- list()

  # Find all paths from root node (could use graph traversal here)
  traverse_paths <- function(node, path, product) {
    # Find outgoing edges from current node
    next_edges <- edges_df[edges_df$from == node, ]

    if (nrow(next_edges) == 0) {
      # If no outgoing edges, it's a terminal node, save the path and product
      paths_list <<- append(paths_list, list(list(path = path, product = product)))
    } else {
      # Otherwise, traverse the next nodes
      for (i in 1:nrow(next_edges)) {
        # Access the posteriormean from the edge
        posteriormean <- as.numeric(next_edges$posterior_mean[i])

        # Append the label1 (from edges_df) to the path
        new_path <- c(path, next_edges$label1[i])

        # Continue traversal with the new node
        traverse_paths(next_edges$to[i], new_path, product * posteriormean)
      }
    }
  }

  # Start traversal from the root node
  traverse_paths(root_node, path = character(0), product = 1)

  # Convert list of paths and products into a data frame
  path_df <- do.call(rbind, lapply(paths_list, function(x) data.frame(path = paste(x$path, collapse = " -> "), product = x$product)))

  return(path_df)
}


#' Calculate Conditional Probability in a Chain Event Graph (CEG)
#'
#' This function calculates the conditional probability of a specified group of conditions occurring,
#' given a set of unique values, based on the calculated path products.
#'
#' @param path_df A data frame containing paths and their corresponding products,
#'   as returned by the `calculate_path_products` function.
#' @param unique_values A character vector containing the unique values (e.g., labels or categories)
#'   to condition on.
#' @param selected_indices A numeric vector of indices corresponding to the groupings of the unique values.
#' @param last_group A string representing the last group for which the conditional probability is to be calculated.
#'
#' @return A numeric value representing the conditional probability P(last_group | unique_values).
#'
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @export
#'
calculate_conditional_prob <- function(path_df, unique_values, selected_indices, last_group) {
  # Ensure unique_values is a character vector
  unique_values <- as.character(unique_values)

  # Group values by their assigned index
  grouped_conditions <- split(unique_values, selected_indices)

  # Step 1: Filter paths directly based on AND/OR logic
  condition_paths <- path_df[sapply(path_df$path, function(path) {
    path_components <- unlist(strsplit(path, " -> "))  # Convert path to vector

    # Apply AND/OR logic for each group
    all(sapply(grouped_conditions, function(group) {
      if (length(group) == 1) {
        group %in% path_components  # AND condition: must be present
      } else {
        any(group %in% path_components)  # OR condition: at least one must be present
      }
    }))
  }), , drop = FALSE]  # Prevent accidental list conversion

  # If no matching paths exist, return 0 probability
  if (nrow(condition_paths) == 0) {
    print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = 0"))
    return(0)
  }

  # Step 2: Compute joint probability P(last_group and unique_values)
  joint_prob <- sum(condition_paths$product[sapply(condition_paths$path, function(path) {
    last_group %in% unlist(strsplit(path, " -> "))
  })])

  # Step 3: Compute marginal probability P(unique_values)
  marginal_prob <- sum(condition_paths$product)

  # Step 4: Compute conditional probability P(last_group | unique_values)
  conditional_prob <- ifelse(marginal_prob > 0, joint_prob / marginal_prob, 0)

  #print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = ", conditional_prob, sep = ""))
  return(conditional_prob)
}


#' Calculate Area Probabilities in a Chain Event Graph (CEG)
#'
#' This function calculates the conditional probability of a given last group for each area, based on the
#' path products and area-specific paths. It leverages the `calculate_conditional_prob` function for each area.
#'
#' @param path_df A data frame containing paths and their corresponding products,
#'   as returned by the `calculate_path_products` function.
#' @param unique_values A character vector containing the unique values (e.g., labels or categories)
#'   to condition on.
#' @param selected_indices A numeric vector of indices corresponding to the groupings of the unique values.
#' @param last_group A string representing the last group for which the conditional probability is to be calculated.
#' @param shapefile_vals A vector of area names (or other geographical identifiers) from the shapefile data.
#'
#' @return A list with area names as keys and their respective conditional probabilities as values.
#'   If no paths are found for a specific area, the probability is returned as `NA`.
#'
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#' @export
#'
calculate_area_probabilities <- function(path_df, unique_values, selected_indices, last_group, shapefile_vals) {
  area_probs <- list()  # Store probabilities for each area

  # Loop over each area in shapefile_vals
  for (area in shapefile_vals) {
    # Filter paths for the current area
    area_paths <- path_df[sapply(path_df$path, function(path) {
      area %in% unlist(strsplit(path, " -> "))  # Ensure area is in path
    }), , drop = FALSE]

    # If no paths exist for this area, assign probability 0
    if (nrow(area_paths) == 0) {
      area_probs[[area]] <- NA
    } else {
      # Compute probability for the given area
      area_probs[[area]] <- calculate_conditional_prob(area_paths, unique_values, selected_indices, last_group)
    }
  }

  return(area_probs)  # Return a named list of probabilities
}



#' Generate a Leaflet Map for a Chain Event Graph (CEG)
#'
#' This function generates an interactive map using the Leaflet package to visualize the probability of each
#' area in the Chain Event Graph (CEG). The map is color-coded based on area-specific probabilities, which
#' are calculated from the path products and conditional probabilities.
#'
#' @param shapefile A Simple Features (sf) object representing the shapefile data for the geographical areas.
#' @param ceg_object A list containing the Chain Event Graph (CEG) data, including nodes and edges.
#' @param conditionals A character vector containing the conditions (labels) to condition on when calculating
#'   area probabilities. Default is the unique edge labels from the `ceg_object`.
#' @param colour_by A string specifying the label by which to color the map. Default is `NULL`, which colors by
#'   the label with the maximum level.
#' @param color_palette A string specifying the color palette to use for the map. Default is "viridis".
#'
#' @return A Leaflet map object with color-coded polygons representing the areas, with a legend indicating
#'   the probability values for each area.
#'
#' @import leaflet
#' @import sf
#' @importFrom viridis scale_color_viridis scale_colour_viridis scale_fill_viridis viridis
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join semi_join rowwise across everything case_when
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(9,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#'
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' ceg <- create_ceg(staged_tree, view_table = TRUE)
#' generate_CEG_map(bcu_shapefile, ceg)
#'
#' @export
generate_CEG_map <- function(shapefile, ceg_object, conditionals = unique(ceg_object$x$edges$label1), colour_by = NULL, color_palette = "viridis") {

  nodes <- ceg_object$ceg$x$nodes
  edges <- ceg_object$ceg$x$edges


  # If start_label1 is NULL, set it to the first label1 with max level
  if (is.null(colour_by)) {
    max_level <- max(edges$level, na.rm = TRUE)
    max_level_rows <- edges[edges$level == max_level, ]
    colour_by <- max_level_rows$label1[1]  # First label1 with max level
  }

  # Step 1: Load Shapefile Data
  shape_data <- shapefile
  shape_data <- st_transform(shape_data, crs = 4326)
  # Step 2: Calculate Path Products (Assume your function is loaded)
  path_df <- calculate_path_products(nodes, edges)

  get_levels_from_conditionals <- function(conditionals, edges_df) {
    levels <- edges_df %>%
      filter(label1 %in% conditionals) %>%  # Filter for given label1 values
      group_by(label1) %>%
      slice(1) %>%  # Take the first row for each label1
      ungroup() %>%
      mutate(label1 = factor(label1, levels = conditionals)) %>%  # Maintain original order
      arrange(label1) %>%  # Arrange by the factor to keep input order
      pull(level)  # Extract the level column

    return(levels) # Return levels in the correct order
  }

  selected_indices <- get_levels_from_conditionals(conditionals, edges)

  # Step 3: Calculate Area Probabilities
  area_probs <- calculate_area_probabilities(
    path_df, conditionals, selected_indices, colour_by, shape_data[[1]]
  )

  # Convert area probabilities to a named vector
  area_probs_vec <- unlist(area_probs)

  # Match areas and assign probabilities to shapefile data
  shape_data$area_probs <- area_probs_vec[shape_data[[1]]]

  # Step 4: Assign Colors Based on Area Probabilities
  assign_colors <- function(probs, palette_name) {
    color_func <- colorNumeric(viridis(100, option = palette_name), domain = c(1, 0))

    sapply(probs, function(prob) {
      if (is.na(prob)) "#FFFFFF" else color_func(prob)  # White for NA values
    })
  }

  # Assign colors based on area probabilities
  shape_data$color_assignment <- assign_colors(shape_data$area_probs, color_palette)

  # Step 5: Generate the Leaflet Map
  map <- leaflet(data = shape_data) %>%
    addTiles() %>%
    addPolygons(
      layerId = shape_data[[1]],
      fillColor = shape_data$color_assignment,
      color = "black",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 1, fillOpacity = 0.7, bringToFront = TRUE
      ),
      opacity = 1, fillOpacity = 0.7,
      label = ~paste0(as.character(shape_data[[1]]), ": ", round(shape_data$area_probs, 3))
    ) %>%
    addLegend(
      pal = colorNumeric(viridis(100, option = color_palette), domain = c(0, 1)),
      values = c(0, 1),
      title = "Probability", position = "bottomright",
      labFormat = labelFormat(transform = function(x) round(x, 2))
    )

  transposed_df <- as.data.frame(area_probs)
  transposed_df <- as.data.frame(t(transposed_df))

  # Optional: Set column name
  colnames(transposed_df) <- "Probability"

  return(list(map = map, conditional_probabilities = transposed_df))
}


