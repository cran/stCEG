#' Specify Priors for a Staged Tree Object
#'
#' This function assigns priors to the nodes of a staged tree object. The user can choose from different types of priors, including "Uniform", "Phantom", and "Custom".
#'
#' @param staged_tree_obj A staged tree object containing the nodes and edges data for the tree.
#' @param prior_type A character string indicating the prior type. Options are:
#'   - "Uniform": Assigns a Uniform (1,1) prior based on the outgoing edges.
#'   - "Phantom": Calculates a Phantom Individuals prior by initialising an alpha based on the maximum number of outgoing edges in the tree and dividing that evenly throughout the tree.
#'   - "Custom": Allows the user to manually specify the prior values for each node.
#' @param ask_edit If TRUE, this allows you to edit priors after choosing an uninformative prior type.
#' @param print_colours If TRUE, this prints a colour key for stages. Default is TRUE.
#'
#' @return A data frame containing the updated nodes data with the specified priors and their means.
#'
#' @details
#' The function checks if the necessary columns and structure are present in the input staged tree object. It performs validation on the nodes' levels and ensures that no nodes with non-minimum or non-maximum levels have the colour `#FFFFFF`.
#' For the "Uniform" and "Phantom" priors, the function calculates the priors for each node based on their outgoing edges and propagates them through connected nodes. The user can edit these priors if desired.
#' The "Custom" option allows the user to manually input prior values for each row. If incorrect values are provided, an error will be raised.
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' coloured_tree <- ahc_colouring(event_tree)
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform", ask_edit = FALSE)
#' @export
#'
specify_priors <- function(staged_tree_obj, prior_type = "Uniform", ask_edit = TRUE, print_colours = TRUE) {
  # Ensure necessary columns exist
  # Check if staged_tree_obj contains the necessary structure
  if (!("stagedtree" %in% names(staged_tree_obj)) || !("x" %in% names(staged_tree_obj$stagedtree))) {
    stop("Error: Need a staged tree object.")
  }

  staged_tree_obj$stagedtree$x$nodes$number_nodes <- 1
  edges_df <- staged_tree_obj$stagedtree$x$edges
  nodes_df <- staged_tree_obj$stagedtree$x$nodes
  required_cols <- c("color", "level2", "outgoing_edges2", "number_nodes")

  min_level <- min(nodes_df$level2, na.rm = TRUE)
  max_level <- max(nodes_df$level2, na.rm = TRUE)

  if (!all(required_cols %in% colnames(staged_tree_obj$stagedtree$x$nodes))) {
    stop("Dataframe must contain the required columns: color, level2, outgoing_edges2, and number_nodes.")
  }

  # Error message to catch uncoloured nodes
  if (any(nodes_df$level2 != min_level & nodes_df$level2 != max_level & nodes_df$color == "#FFFFFF")) {
    stop("Error: Nodes with a non-min/max level cannot have color #FFFFFF.")
  }

  nodes_df <- nodes_df[nodes_df$level2 != max_level, ]
  nodes_df <- nodes_df[c("color", "level2", "outgoing_edges2", "number_nodes")]

  # Calculate number of nodes for each colour
  nodes_df <- nodes_df %>%
    group_by(color, level2, outgoing_edges2) %>%
    summarise(number_nodes = sum(number_nodes, na.rm = TRUE), .groups = "drop")

  if (!"prior" %in% colnames(nodes_df)) {
    nodes_df$prior <- ""
  }

  nodes_df <- arrange(nodes_df, level2)
  nodes_df$stage <- paste0("u", seq_len(nrow(nodes_df)))

  if (!requireNamespace("crayon", quietly = TRUE)) {
    cat("Install the 'crayon' package to view colored output.\n")
  } else {
    unique_colors <- unique(nodes_df$color)

    hex_to_style <- function(hex) {
      rgb <- grDevices::col2rgb(hex)
      crayon::make_style(rgb, bg = TRUE)
    }

    # Display key to match colour codes
    if (print_colours) {
      cat("\nStage Colour Key:\n")
      for (hex in unique_colors) {
        style <- hex_to_style(hex)
        cat(style("     "), " ", hex, "\n")
      }
    }
  }

  if (prior_type == "Custom") {
    print_nodes_df <- nodes_df
    colnames(print_nodes_df) <- c("Color", "Level", "Outgoing Edges", "Nodes", "Prior", "Stage")
    print(print_nodes_df)

    # Loop through each row and prompt the user for the prior
    for (i in 1:nrow(nodes_df)) {
      cat(paste0("Enter new prior for row ", i, " (", nodes_df$outgoing_edges2[i], " values, comma-separated): "))
      new_prior <- scan(what = character(), nmax = 1, quiet = TRUE)

      # Split the input values by comma
      values <- unlist(strsplit(new_prior, ","))

      # Check if the number of values matches the expected outgoing edges
      if (length(values) != nodes_df$outgoing_edges2[i]) {
        stop(paste0("Row ", i, ": Incorrect number of values. Expected ", nodes_df$outgoing_edges2[i], "."))
      }

      # Store the entered prior for the row
      nodes_df$prior[i] <- new_prior
    }
  }
  else if (prior_type %in% c("Uniform", "Phantom")) {
    if (prior_type == "Uniform") {
      nodes_df$prior <- ifelse(nodes_df$prior == "",
                               mapply(function(edges, nodes) paste(rep(1 * nodes, edges), collapse = ","),
                                      nodes_df$outgoing_edges2, nodes_df$number_nodes),
                               nodes_df$prior)
    } else if (prior_type == "Phantom") {

      # Extract nodes and edges data
      nodes_df2 <- staged_tree_obj$stagedtree$x$nodes
      edges_df <- staged_tree_obj$stagedtree$x$edges
      # Find the maximum level (to exclude the max level from prior calculation)
      max_level <- max(nodes_df2$level2, na.rm = TRUE)

      # Filter out rows where level is the maximum level
      nodes_df_filtered <- filter(nodes_df2, level2 != max_level)

      # Initialize the 'prior' column with NA
      nodes_df_filtered$prior <- NA

      # Set the prior for the first row based on outgoing edges
      equivsize <- max(nodes_df_filtered$outgoing_edges2, na.rm = TRUE)  # Assuming outgoing_edges2 is not NA
      nodes_df_filtered$prior[1] <- equivsize  # Set the first row prior value

      # Loop through each node and calculate the prior
      for (i in 1:nrow(nodes_df_filtered)) {
        current_id <- nodes_df_filtered$id[i]  # Current node ID
        outgoing_edges2 <- nodes_df_filtered$outgoing_edges2[i]  # Number of outgoing edges for this node

        if (!is.na(current_id) && outgoing_edges2 > 0) {  # Ensure no division by zero
          current_prior <- nodes_df_filtered$prior[i]  # Get current prior
          new_prior <- current_prior / outgoing_edges2  # Divide by outgoing edges

          # Find the 'to' nodes connected from the current node (edges where from == current_id)
          to_nodes <- edges_df$to[edges_df$from == current_id]

          # Update the prior for each 'to' node based on the current prior
          for (j in to_nodes) {
            nodes_df_filtered$prior[nodes_df_filtered$id == j] <- new_prior
          }
        }
      }

      # Group by colour and outgoing_edges2 to summarize the total prior
      df_grouped_by_colour <- nodes_df_filtered %>%
        group_by(color, level2, outgoing_edges2) %>%
        summarise(total_prior = sum(prior, na.rm = TRUE), .groups = "drop")


      # Initialize the prior column for the grouped dataframe
      df_grouped_by_colour$prior <- NA

      # Adjust the prior for each group based on the total prior
      for (i in 1:nrow(df_grouped_by_colour)) {
        df_grouped_by_colour$prior[i] <- paste(rep(round(as.numeric(df_grouped_by_colour$total_prior[i]) / as.numeric(df_grouped_by_colour$outgoing_edges2[i]), 3), as.numeric(df_grouped_by_colour$outgoing_edges2[i])), collapse = ", ")
      }

      # Select only the columns that are present in nodes_df
      nodes_df <- full_join(nodes_df, df_grouped_by_colour, by = c("level2", "color", "outgoing_edges2")) %>%
        mutate(
          prior.x = na_if(prior.x, ""),  # Convert empty strings to NA
          prior.y = na_if(prior.y, ""),
          prior = coalesce(prior.x, prior.y)  # Take the non-missing value
        ) %>%
        select(-prior.x, -prior.y)  # Remove redundant columns




    }



    if (ask_edit) {
      cat("Calculated priors:\n")
      print_nodes_df <- nodes_df
      colnames(print_nodes_df) <- c("Color", "Level", "Outgoing Edges", "Nodes", "Prior", "Stage")
      print(print_nodes_df)

      cat("\nDo you want to edit specific rows? (yes/no): ")
      edit_choice <- scan(what = character(), nmax = 1, quiet = TRUE)

      if (tolower(edit_choice) == "yes") {
        cat("Enter row numbers to edit (comma-separated, e.g., 1,3,5): ")
        edit_rows <- scan(what = character(), nmax = 1, quiet = TRUE)

        if (nzchar(edit_rows)) {
          edit_indices <- as.numeric(unlist(strsplit(edit_rows, ",")))
          edit_indices <- edit_indices[edit_indices %in% seq_len(nrow(nodes_df))]

          if (length(edit_indices) > 0) {
            for (i in edit_indices) {
              cat(paste0("Enter new prior for row ", i, " (", nodes_df$outgoing_edges2[i], " values, comma-separated): "))
              new_prior <- scan(what = character(), nmax = 1, quiet = TRUE)

              values <- unlist(strsplit(new_prior, ","))
              if (length(values) != nodes_df$outgoing_edges2[i]) {
                stop(paste0("Row ", i, ": Incorrect number of values. Expected ", nodes_df$outgoing_edges2[i], "."))
              }

              nodes_df$prior[i] <- new_prior
            }
          }
        }
      } else {
        cat("No edits made \n")
      }
    }



  }


  # Function to compute prior mean
  compute_prior_mean <- function(prior_string) {
    prior_values <- as.numeric(strsplit(prior_string, ",")[[1]])  # Convert to numeric
    prior_sum <- sum(prior_values)  # Get total
    prior_mean <- prior_values / prior_sum  # Normalize
    return(paste(round(prior_mean, 2), collapse = ","))  # Format as string
  }

  # Apply function to nodes_table
  nodes_df <- nodes_df %>%
    mutate(prior_mean = sapply(prior, compute_prior_mean))


nodes_df_table <- nodes_df %>%
  select(
    Stage = stage,
    Colour = color,
    Level = level2,
    "Outgoing Edges" = outgoing_edges2,
    Nodes = number_nodes,
    Prior = prior,
    "Prior Mean" = prior_mean
  )


# Return the table
return(nodes_df_table)

}

