#' Calculate Path Probabilities in a Chain Event Graph
#'
#' Calculates the probability associated with every root-to-leaf path in a
#' Chain Event Graph (CEG) by recursively multiplying the posterior transition
#' probabilities along each path.
#'
#' The resulting output can be used for conditional probability calculations
#' and geographical probability mapping.
#'
#' @param nodes_df A node data frame from a \code{"ceg"} object.
#' @param edges_df An edge data frame from a \code{"ceg"} object containing
#'   posterior transition probabilities in the \code{posterior_mean} column.
#' @param root_node Character string identifying the root node. Default is
#'   \code{"w0"}.
#'
#' @return
#' A data frame containing:
#' \itemize{
#'   \item \code{path}: sequence of edge labels defining the path.
#'   \item \code{product}: probability associated with the path.
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
#' path_df <- calculate_path_products(
#'   ceg$nodes,
#'   ceg$edges
#' )
#'
#' head(path_df)
#'
#'
#' @export
calculate_path_products <- function(nodes_df, edges_df, root_node = "w0") {

  paths <- list()

  traverse <- function(node, path, product) {
    next_edges <- edges_df[edges_df$from == node, ]
    if (nrow(next_edges) == 0) {
      paths[[length(paths) + 1]] <<- list(
        path = path,
        product = product
      )
      return()
    }

    for (i in seq_len(nrow(next_edges))) {
      pm <- as.numeric(next_edges$posterior_mean[i])
      new_path <- c(path, next_edges$label1[i])
      traverse(next_edges$to[i], new_path, product * pm)
    }
  }

  traverse(root_node, character(0), 1)

  out <- do.call(
    rbind,
    lapply(paths, function(x) {
      data.frame(
        path = paste(x$path, collapse = " -> "),
        product = x$product,
        stringsAsFactors = FALSE
      )
    })
  )

  out
}



#' Calculate a Conditional Probability from CEG Paths
#'
#' Computes a conditional probability using path probabilities obtained from a
#' Chain Event Graph.
#'
#' Given a set of conditioning variables and a target outcome, the function
#' evaluates:
#'
#' \deqn{
#' P(\mathrm{last\_group}\mid \mathrm{conditions})
#' }
#'
#' by summing the probabilities of all relevant paths.
#'
#' @param path_df Output from \code{\link{calculate_path_products}}.
#' @param unique_values Character vector of values appearing in the CEG paths.
#' @param selected_indices Numeric vector indicating which values belong to the
#'   same conditioning group.
#' @param last_group Character string corresponding to the event whose
#'   conditional probability is required.
#'
#' @return
#' A numeric value between 0 and 1.
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
#' path_df <- calculate_path_products(
#'   ceg$nodes,
#'   ceg$edges
#' )
#'
#' head(path_df)
#'
#' calculate_conditional_prob(
#'   path_df,
#'   unique_values = c("Adult", "Male"),
#'   selected_indices = c(1, 2),
#'   last_group = "Shooting"
#' )
#'
#'
#' @seealso
#' \code{\link{calculate_path_products}}
#'
#' @export
calculate_conditional_prob <- function(path_df, unique_values, selected_indices, last_group) {

  unique_values <- as.character(unique_values)
  grouped_conditions <- split(unique_values, selected_indices)

  condition_paths <- path_df[
    sapply(path_df$path, function(p) {
      comps <- unlist(strsplit(p, " -> "))
      all(sapply(grouped_conditions, function(group) {
        if (length(group) == 1) {
          group %in% comps
        } else {
          any(group %in% comps)
        }
      }))
    }),
    ,
    drop = FALSE
  ]

  if (nrow(condition_paths) == 0) {
    return(0)
  }

  joint_prob <- sum(condition_paths$product[
    sapply(condition_paths$path, function(p) {
      last_group %in% unlist(strsplit(p, " -> "))
    })
  ])

  marginal_prob <- sum(condition_paths$product)

  if (marginal_prob == 0) return(0)

  joint_prob / marginal_prob
}


#' Calculate Area-Specific Conditional Probabilities
#'
#' Calculates a conditional probability for each geographical area represented
#' within a Chain Event Graph.
#'
#' The function evaluates a specified conditional probability separately for
#' each area and returns the resulting probabilities as a named list.
#'
#' @param path_df Output from \code{\link{calculate_path_products}}.
#' @param unique_values Character vector of conditioning values.
#' @param selected_indices Numeric vector indicating grouping structure for the
#'   conditioning values.
#' @param last_group Character string specifying the outcome of interest.
#' @param shapefile_vals Character vector containing area identifiers.
#'
#' @return
#' A named list of conditional probabilities indexed by area.
#'
#' @examples
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' path_df <- calculate_path_products(
#'   ceg$nodes,
#'   ceg$edges
#' )
#'
#' probs <- calculate_area_probabilities(
#'   path_df,
#'   unique_values = c("Adult"),
#'   selected_indices = c(2),
#'   last_group = "Female",
#'   shapefile_vals = c("West", "South East")
#' )
#'
#'
#' @seealso
#' \code{\link{calculate_conditional_prob}}
#'
#' @export
calculate_area_probabilities <- function(path_df, unique_values, selected_indices, last_group, shapefile_vals) {

  area_probs <- vector("list", length(shapefile_vals))
  names(area_probs) <- shapefile_vals

  for (area in shapefile_vals) {

    area_paths <- path_df[
      sapply(path_df$path, function(p) {
        area %in% unlist(strsplit(p, " -> "))
      }),
      ,
      drop = FALSE
    ]

    if (nrow(area_paths) == 0) {
      area_probs[[area]] <- NA_real_
    } else {
      area_probs[[area]] <- calculate_conditional_prob(
        area_paths,
        unique_values,
        selected_indices,
        last_group
      )
    }
  }

  area_probs
}




#' Generate a Chain Event Graph Probability Map
#'
#' Creates an interactive leaflet map displaying area-level probabilities
#' derived from a Chain Event Graph (CEG).
#'
#' Conditional probabilities are calculated for each geographical region and
#' displayed using a colour scale. Areas that do not appear in the CEG are
#' reported separately and excluded from colouring.
#'
#' @param shapefile An \code{sf} object containing polygon geometries.
#' @param ceg_object An object of class \code{"ceg"}.
#' @param conditionals Character vector specifying the conditioning variables.
#'   By default all unique edge labels are used.
#' @param colour_by Character string specifying the outcome label whose
#'   conditional probability should be visualised. If \code{NULL}, the first
#'   label appearing at the deepest level of the CEG is used.
#' @param color_palette Character string specifying the viridis palette option.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calculates all root-to-leaf path probabilities.
#'   \item Computes conditional probabilities for each geographical region.
#'   \item Joins probabilities to the supplied shapefile.
#'   \item Creates an interactive leaflet map with colour-coded polygons.
#' }
#'
#' @return
#' An object of class \code{"ceg_map"} containing:
#' \itemize{
#'   \item \code{map}: leaflet map object.
#'   \item \code{conditional_probabilities}: probability table.
#'   \item \code{colour_by}: outcome used for colouring.
#'   \item \code{conditionals}: conditioning variables.
#'   \item \code{excluded_polygons}: polygons not present in the CEG.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' map_obj <- generate_CEG_map(
#'   shapefile = bcu_shapefile,
#'   ceg_object = ceg,
#'   colour_by = "Female"
#' )
#'
#'
#' @seealso
#' \code{\link{plot.ceg_map}},
#' \code{\link{summary.ceg_map}}
#'
#' @export
generate_CEG_map <- function(
    shapefile,
    ceg_object,
    conditionals = unique(ceg_object$edges$label1),
    colour_by = NULL,
    color_palette = "viridis"
) {

  nodes <- ceg_object$nodes
  edges <- ceg_object$edges

  if (is.null(colour_by)) {
    max_level <- max(edges$level, na.rm = TRUE)
    colour_by <- edges$label1[edges$level == max_level][1]
  }

  shape_data <- sf::st_transform(shapefile, crs = 4326)


  path_df <- calculate_path_products(nodes, edges)

  if (is.null(conditionals)) {
    # Colour by max-level label
    max_level <- max(edges$level, na.rm = TRUE)
    conditionals <- edges$label1[edges$level == max_level]
  }

  if (!is.null(conditionals)) {
    conditionals <- unique(conditionals)
  }

  get_levels_from_conditionals <- function(conditionals, edges_df) {
    edges_df %>%
      dplyr::filter(label1 %in% conditionals) %>%
      dplyr::group_by(label1) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label1 = factor(label1, levels = conditionals)) %>%
      dplyr::arrange(label1) %>%
      dplyr::pull(level)
  }

  selected_indices <- get_levels_from_conditionals(conditionals, edges)

  area_probs <- calculate_area_probabilities(
    path_df,
    conditionals,
    selected_indices,
    colour_by,
    shape_data[[1]]
  )

  area_probs_vec <- unlist(area_probs)


  # Remove NA area labels from the CEG output
  valid_names <- names(area_probs_vec)[!is.na(names(area_probs_vec))]

  # Polygons in the shapefile that are NOT in the CEG
  missing <- setdiff(shapefile[[1]], valid_names)

  # Now filter shapefile polygons to those present in the CEG
  shape_data <- shape_data[shape_data[[1]] %in% valid_names, ]

  # Assign probabilities only to matching polygons
  shape_data$area_probs <- area_probs_vec[shape_data[[1]]]


  assign_colors <- function(probs, palette_name) {
    color_func <- leaflet::colorNumeric(
      viridis::viridis(100, option = palette_name),
      domain = c(0, 1)
    )
    sapply(probs, function(p) {
      if (is.na(p)) "#FFFFFF" else color_func(p)
    })
  }

  shape_data$color_assignment <- assign_colors(shape_data$area_probs, color_palette)

  map <- leaflet::leaflet(data = shape_data) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      layerId = shape_data[[1]],
      fillColor = shape_data$color_assignment,
      color = "black",
      weight = 1,
      highlightOptions = leaflet::highlightOptions(
        weight = 1, fillOpacity = 0.7, bringToFront = TRUE
      ),
      opacity = 1,
      fillOpacity = 0.7,
      label = ~paste0(as.character(shape_data[[1]]), ": ", round(shape_data$area_probs, 3))
    ) %>%
    leaflet::addLegend(
      pal = leaflet::colorNumeric(
        viridis::viridis(100, option = color_palette),
        domain = c(0, 1)
      ),
      values = c(0, 1),
      title = "Probability",
      position = "bottomright",
      labFormat = leaflet::labelFormat(transform = function(x) round(x, 2))
    )

  prob_df <- data.frame(
    Area = names(area_probs),
    Probability = unlist(area_probs),
    row.names = NULL
  )

  out <- list(
    map = map,
    valid_names = valid_names,
    conditional_probabilities = prob_df,
    colour_by = colour_by,
    conditionals = conditionals,
    excluded_polygons = missing
  )

  class(out) <- "ceg_map"
  out
}

#' Summarise a CEG Probability Map
#'
#' Produces a summary of a \code{"ceg_map"} object, including the number of
#' mapped areas, probability range and any excluded polygons.
#'
#' @param object An object of class \code{"ceg_map"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' An object of class \code{"summary_ceg_map"}.
#'
#' @examples
#' \dontrun{
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' map_obj <- generate_CEG_map(
#'   shapefile = bcu_shapefile,
#'   ceg_object = ceg,
#'   colour_by = "Female"
#' )
#'
#' summary(map_obj)
#' }
#'
#' @method summary ceg_map
#' @export
summary.ceg_map <- function(object, ...) {

  out <- list(
    n_areas = length(object$valid_names),
    colour_by = object$colour_by,
    n_conditionals = length(object$conditionals),
    min_prob = min(object$conditional_probabilities$Probability, na.rm = TRUE),
    max_prob = max(object$conditional_probabilities$Probability, na.rm = TRUE),
    missing_polygons = object$excluded_polygons
  )

  class(out) <- "summary_ceg_map"
  out
}

#' Print a Summary of a CEG Probability Map
#'
#' Prints a concise summary of a \code{"summary_ceg_map"} object.
#'
#' @param x An object of class \code{"summary_ceg_map"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied summary object, invisibly.
#'
#' @examples
#' \dontrun{
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' map_obj <- generate_CEG_map(
#'   shapefile = bcu_shapefile,
#'   ceg_object = ceg,
#'   colour_by = "Female"
#' )
#'
#' print(summary(map_obj))
#' }
#'
#' @method print summary_ceg_map
#' @export
print.summary_ceg_map <- function(x, ...) {

  cat("Summary of CEG Map\n")
  cat("===================\n")
  cat("Number of areas:        ", x$n_areas, "\n")
  cat("Coloured by:        ", x$colour_by, "\n")
  cat("Number of conditionals: ", x$n_conditionals, "\n")
  cat("Minimum probability:    ", x$min_prob, "\n")
  cat("Maximum probability:    ", x$max_prob, "\n")
  cat("Missing polygons:       ", ifelse(length(x$missing_polygons) == 0, "None", paste(x$missing_polygons, collapse = ", ")), "\n")

  invisible(x)
}

#' Plot a CEG Probability Map
#'
#' Displays the interactive leaflet map stored within a
#' \code{"ceg_map"} object.
#'
#' @param x An object of class \code{"ceg_map"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' A leaflet map widget.
#'
#' @examples
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' map_obj <- generate_CEG_map(
#'   shapefile = bcu_shapefile,
#'   ceg_object = ceg,
#'   colour_by = "Female"
#' )
#'
#' plot(map_obj)
#'
#'
#' @method plot ceg_map
#' @export
plot.ceg_map <- function(x, ...) {
  x$map
}

#' Print a CEG Probability Map
#'
#' Prints a concise description of a \code{"ceg_map"} object including the
#' outcome being mapped, probability range and the number of excluded
#' geographical regions.
#'
#' @param x An object of class \code{"ceg_map"}.
#' @param ... Additional arguments passed to S3 methods.
#'
#' @return
#' The supplied \code{"ceg_map"} object, invisibly.
#'
#' @examples
#' \dontrun{
#' et <- create_event_tree(homicides, c(9,1:3))
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
#' map_obj <- generate_CEG_map(
#'   shapefile = bcu_shapefile,
#'   ceg_object = ceg,
#'   colour_by = "Female"
#' )
#'
#' print(map_obj)
#' }
#'
#' @method print ceg_map
#' @export
print.ceg_map <- function(x, ...) {

  cat("CEG Map\n")
  cat("==============\n")

  cat("Coloured by:        ",
      x$colour_by, "\n")

  cat("Number of conditionals: ",
      length(x$conditionals), "\n")

  rng <- range(x$conditional_probabilities$Probability, na.rm = TRUE)
  cat("Probability range:      ",
      sprintf("%.3f-%.3f", rng[1], rng[2]),
      "\n")

  cat("Number of missing polygons:   ",
      length(x$excluded_polygons), "\n")

  cat("\nUse summary(x) for detailed information.\n")
  cat("Use plot(x) to display the map.\n")

  invisible(x)
}

