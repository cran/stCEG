#' Create an Event Tree from a Dataset
#'
#' This function constructs an event tree from a given dataset based on user-specified categorical variables.
#' The resulting tree is represented as a graph, where each unique combination of variable values corresponds
#' to a distinct path in the tree.
#'
#' @param dataset A data frame containing categorical variables.
#' @param columns A vector of column indices or names specifying which variables to use for constructing the event tree.
#' @param label_type A character string specifying how edge labels should be displayed. Options are `"names"` (variable names),
#' `"both"` (variable names and counts). Default is `"both"`.
#' @param level_separation A numeric value indicating the spacing between levels in the visualization. Default is `1000`.
#' @param node_distance A numeric value specifying the horizontal distance between nodes in the visualization. Default is `300`.
#'
#' @return A `visNetwork` object displaying the event tree.
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Extracts the specified columns from the dataset.
#'   \item Determines unique values for each variable and generates state names dynamically.
#'   \item Constructs a directed acyclic graph (DAG) using `igraph`, where each state represents a unique combination of variable values.
#'   \item Computes transition counts between states.
#'   \item Uses `visNetwork` to generate an interactive visualization of the event tree.
#' }
#'
#' @import igraph
#' @import visNetwork
#' @importFrom dplyr %>% select filter mutate arrange summarise summarise_all group_by ungroup distinct rename pull relocate bind_rows bind_cols left_join right_join inner_join full_join anti_join all_of semi_join rowwise across everything case_when
#' @importFrom tidyr pivot_longer pivot_wider separate unite drop_na replace_na fill complete nesting expand_grid expand

#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' event_tree
#' @export
create_event_tree <- function(dataset, columns = seq_along(dataset), label_type = "both", level_separation = 1000, node_distance = 300) {
  g <- make_empty_graph()
  parent <- "s0"

  # Filter dataset based on specified columns
  homicide_data2 <- dataset[, columns]
  filtereddf <- homicide_data2

  num_vars <- length(columns)  # Number of variables chosen by the user

  # Initialize lists to store unique values and state names
  unique_values_list <- vector("list", num_vars)
  state_names_list <- vector("list", num_vars)

  start_index <- 1
  total_states <- 1

  # Generate state names and keep track of the indices
  for (i in 1:num_vars) {
    # Get unique values from column without sorting or excluding NAs
    col_values <- unique(homicide_data2[[i]])

    # Sort values and keep NAs if they exist
    col_values <- sort(col_values, na.last = TRUE)

    # Store the sorted values with NAs in unique_values_list
    unique_values_list[[i]] <- col_values

    # Generate state names based on unique values
    state_names_list[[i]] <- paste0("s", start_index:(start_index + length(col_values) * (total_states) - 1))

    total_states <- total_states * length(col_values)
    start_index <- start_index + total_states
  }

 #print(unique_values_list)
 #print(state_names_list)

  # Add vertices to the graph: starting with the root node "s0"
  g <- add_vertices(g, 1, name = "s0")

  # Add vertices dynamically for each column based on the calculated states
  for (i in 1:num_vars) {
    g <- add_vertices(g, length(state_names_list[[i]]), name = state_names_list[[i]])
  }
  vertex_names <- V(g)$name
  #print("Vertex names of the graph:")
  #print(vertex_names)

  # Function to generate combinations and counts dynamically
  generate_combinations <- function(df, cols) {
    col_names <- colnames(df)[cols]

    # Generate all possible combinations
    all_combinations <- expand.grid(lapply(df[col_names], unique))

    # Remove any combinations containing NA
    #all_combinations <- all_combinations %>%
    #filter(!if_any(everything(), is.na))

    # Calculate counts for the actual data
    counts <- df %>%
      group_by(across(all_of(col_names))) %>%
      summarise(count = n(), .groups = 'drop')

    # Merge combinations with counts
    full_data <- full_join(all_combinations, counts, by = col_names)

    # Replace any NA counts with 0
    full_data$count[is.na(full_data$count)] <- 0

    # Arrange the final dataset
    full_data <- full_data %>%
      arrange(across(all_of(col_names)))

    return(full_data)
  }


  # Calculate counts dynamically
  counts_list <- lapply(1:num_vars, function(x) generate_combinations(homicide_data2, 1:x))
  # Add edges between parent and child nodes dynamically
  edges <- c()
  state_indices <- rep(1, num_vars)

  edges <- c()

  # Loop through each column/variable to generate edges
  for (i in 1:num_vars) {
    num_states <- length(state_names_list[[i]])

    # Compute the total number of states in the previous columns
    prev_total_states <- if (i > 1) length(state_names_list[[i - 1]]) else 1

    # Adjust start and end indices for the current variable's states
    start_index <- 1
    end_index <- num_states / prev_total_states

    # Add edges
    if (i == 1) {
      # First column: connect to "s0" (root node)
      for (j in 1:num_states) {
        edges <- c(edges, "s0", state_names_list[[i]][j])
      }
    } else {
      # Subsequent columns: connect to the previous column's states
      for (j in 1:prev_total_states) {
        parent_state <- state_names_list[[i - 1]][j]
        child_states <- state_names_list[[i]][start_index:end_index]

        # Create edges from the parent state to its corresponding child states
        for (k in 1:length(child_states)) {
          edges <- c(edges, parent_state, child_states[k])
        }

        # Update start and end indices for the next parent node
        start_index <- end_index + 1
        end_index <- start_index + (num_states / prev_total_states) - 1
      }
    }
  }



  g <- add_edges(g, edges)
  # Plot the graph
  layout <- layout_as_tree(g)
  layout <- -layout[, 2:1]

  data <- toVisNetworkData(g)
  #print(data)
  # Check that state_names_list has the correct structure
  #print(state_names_list)

  # Adjust the times argument in the rep function
  # `1` is for the root node, and the rest corresponds to the number of states at each level
  num_levels <- num_vars + 1  # Including the root node
  #print("state_names_list")
  data$nodes$level <- rep(1:num_levels, times = c(1, sapply(1:num_vars, function(x) length(state_names_list[[x]]))))
  #print(data$edges)
  data$nodes$shape <- 'dot'
  data$nodes$size <- 100
  data$nodes$color.background <- "#FFFFFF"
  data$nodes$font <- "80px"
  data$nodes$title <- data$nodes$id
  # Check number of edges
  #print(nrow(data$edges))  # Number of edges in the graph

  # Flatten the data and extract second-to-last column (label1) and count (label2)
  # Ensure all data frames have the same number of columns by padding with NA

  # Step 1: Find the union of all column names across the data frames in counts_list
  all_column_names <- unique(unlist(lapply(counts_list, colnames)))
  # Step 2: Function to add missing columns to each data frame and ensure "count" is last
  align_columns <- function(df, all_column_names) {
    missing_cols <- setdiff(all_column_names, colnames(df))  # Find columns that are missing
    df[missing_cols] <- "IGNORE"  # Add missing columns filled with NA

    # Reorder columns to match the union and ensure "count" is the last column
    cols_ordered <- c(setdiff(all_column_names, "count"), "count")
    return(df[cols_ordered])
  }

  # Step 3: Apply the function to each data frame in counts_list
  counts_list_aligned <- lapply(counts_list, align_columns, all_column_names)

  # Step 4: Flatten the list of aligned data frames into one using rbind
  df_flat <- do.call(rbind, counts_list_aligned)

  get_last_non_zero_na_rowwise <- function(df) {
    # Exclude the 'count' column
    non_count_cols <- colnames(df)[-which(colnames(df) == "count")]

    # Function to get the last non-zero (and non-NA) entry in a row
    get_last_non_zero <- function(row) {
      # Identify non-zero entries, keeping NAs but ignoring "IGNORE" values
      non_zero_entries <- row[non_count_cols][(row[non_count_cols] != 0 | is.na(row[non_count_cols])) & row[non_count_cols] != "IGNORE"]

      # Check if there are any entries after filtering
      if (length(non_zero_entries) > 0) {
        last_non_zero <- tail(non_zero_entries, 1)
      } else {
        last_non_zero <- NA
      }

      # Replace NA with "NA" as a text string
      last_non_zero <- ifelse(is.na(last_non_zero), "NA", last_non_zero)

      return(last_non_zero)
    }


    # Apply the function to each row
    last_non_zero_entries <- apply(df, 1, get_last_non_zero)

    return(last_non_zero_entries)
  }

  last_entries <- get_last_non_zero_na_rowwise(df_flat)
  # Add this vector as a new column in the data$edges dataframe
  data$edges$label1 <- last_entries
  data$edges$label2 <- df_flat$count
  data$edges$label3 <- paste(data$edges$label1, "\n", data$edges$label2)

  # Assign labels to edges in the graph
  #data$edges$label1 <- label1
  #data$edges$label2 <- label2
  #data$edges$label3 <- label3

  data$edges$label <- switch(label_type,
                             names = data$edges$label1,
                             both = data$edges$label3)

  data$edges$font.size <- 100
  data$edges$color <- "#000000"
  data$edges$arrows <- "to"
  data$nodes$level2 <- data$nodes$level
  data$nodes$color <- "#FFFFFF"

  data$nodes <- data$nodes %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))

  # Generate the visNetwork visualization
  network_plot <- visNetwork(nodes = data$nodes, edges = data$edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE) %>%
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
  result <- invisible(filtereddf)

  # Return both the network plot and the result
  output <- list(eventtree = network_plot, filtereddf = result)

  class(output) <- "event_tree"
  return(output)

}








