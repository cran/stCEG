#' Agglomerative Hierarchical Clustering (AHC) Colouring for Event Trees
#'
#' This function applies Agglomerative Hierarchical Clustering (AHC) to colour the nodes of an event tree, taking into account their outgoing edges and stage information. It returns a coloured event tree based on the computed priors.
#'
#' @param event_tree_obj A list containing an event tree or staged tree, and other relevant data for the event tree processing.
#' @param level_separation Numeric value defining the level separation between nodes in the event tree (default is 1000).
#' @param node_distance Numeric value defining the distance between nodes (default is 300).
#'
#' @return A `visNetwork` object representing the staged tree.
#'
#' @details
#' This function processes an event tree or partial staged tree, calculates priors based on the outgoing edges from each node, and performs Agglomerative Hierarchical Clustering (AHC) to colour the nodes of the event tree. It returns a `visNetwork` object that can be visualized as a coloured event tree. The function also computes the likelihood and scores based on merging stages in the event tree.
#'
#'
#' @examples
#' data <- homicides
#' event_tree <- create_event_tree(data, columns = c(1,2,4,5), "both")
#' event_tree
#' coloured_tree <- ahc_colouring(event_tree)
#' coloured_tree
#'
#' @export
ahc_colouring <- function(event_tree_obj, level_separation = 1000, node_distance = 300) {

  if (!requireNamespace("randomcoloR", quietly = TRUE)) {
    stop("Package 'randomcoloR' needed for this function to work. Please install it.", call. = FALSE)
  }

  exampledata <- event_tree_obj$filtereddf

# Setting error if incorrect format
  if (!is.null(event_tree_obj$eventtree)) {
    tree <- event_tree_obj$eventtree
  } else if (!is.null(event_tree_obj$stagedtree)) {
    tree <- event_tree_obj$stagedtree
  } else {
    stop("Neither eventtree nor stagedtree exists")
  }

  # Extract nodes and edges
  nodes <- tree$x$nodes
  edges <- tree$x$edges

  # Get unique levels from nodes
  unique_levels <- unique(nodes$level)

  # Define levels to filter out (maximum level)
  levels_to_exclude <- max(unique_levels)

  # Filter out nodes at level 1 or max level
  nodes_to_consider <- nodes[!(nodes$level %in% levels_to_exclude), ]

  nodes_to_consider$id2 <- 1:nrow(nodes_to_consider)
  nodes_to_consider2 <- nodes_to_consider$id

  edges_to_consider <- edges %>%
    group_by(from) %>%
    summarize(
      label2_list = paste(label2, collapse = ", ")
    )
  label_matching <- edges %>%
    group_by(from) %>%
    summarize(
      label_list = paste(label1, collapse = ", ")
    )

  # Count outgoing edges for each 'from' node
  outgoing_edges <- edges %>%
    count(from, name = "outgoing_edges")
  edges_to_consider <- inner_join(edges_to_consider, label_matching, by = join_by(from == from))
  nodes_to_consider <- inner_join(nodes_to_consider, outgoing_edges, by = join_by(id == from), keep = FALSE)
  nodes_to_consider <- inner_join(nodes_to_consider, edges_to_consider, by = join_by(id == from), keep = FALSE)

  # Add the outgoing edges information as a new column in the nodes dataframe

  convert_to_matrix <- function(label2_list_str) {
    # Split the string into a numeric vector
    num_vec <- as.numeric(unlist(strsplit(label2_list_str, ", ")))

    # Convert the numeric vector to a matrix with 1 row
    mat <- matrix(num_vec, nrow = 1, byrow = TRUE)

    # If we have more than one element, return the matrix as-is.
    # If there's only one element, wrap it into a matrix format
    if (length(num_vec) == 2) {
      return(mat)
    } else {
      return(matrix(num_vec, nrow = 1))
    }
  }

  # Apply the conversion function to the 'label2_list' column

  # Ensure all columns are factors
  exampledata[] <- lapply(exampledata, function(x) {
    if (!is.factor(x)) as.factor(x) else x
  })
  #print("exampledata")
  #print(exampledata)



  # Calculate number of variables
  numbvariables <- ncol(exampledata)
  #print("numvars:")
  #print(numbvariables)

  # Calculate number of categories for each column
  numbcat <- sapply(exampledata, nlevels)
  #print("numcat:")
  #print(as.vector(numbcat))

  # Determine the size of the largest category
  equivsize <- max(nodes_to_consider$outgoing_edges)
  #print("equivsize:")
  #print(equivsize)

  # Calculate the number of combinations
  numb <- numeric(numbvariables)
  numb[1] <- 1  # The number of combinations for 1 variable is 1

  for (i in 2:numbvariables) {
    numb[i] <- prod(numbcat[1:(i-1)])
  }


  nodes_to_consider$prior <- 0

  # Set the prior for the first row to equivsize
  nodes_to_consider$prior[1] <- equivsize
  prior<-c()
  for (i in 1:nrow(nodes_to_consider)) {

    # Get the current row's 'id' from nodes_to_consider
    current_id <- nodes_to_consider$id[i]
    #print(current_id)
    # Step 1: Get the number of outgoing edges for this node (this could be a count of edges with 'from' = current_id)
    outgoing_edges <- nodes_to_consider$outgoing_edges[i]
    #print(outgoing_edges)

    # Step 2: Calculate the new prior (divide current prior by the number of outgoing edges)
    current_prior <- nodes_to_consider$prior[i]  # Assuming prior column exists
    new_prior <- current_prior / outgoing_edges
    #print(new_prior)

    # Step 3: Update the 'prior' for rows in edges where 'from' equals the current 'id'
    # and update the corresponding 'prior' in nodes_to_consider based on 'to'
    to_nodes <- edges$to[edges$from == current_id]  # Get all 'to' nodes where 'from' equals current_id

    # Update the 'prior' for corresponding nodes in nodes_to_consider
    for (j in to_nodes) {
      nodes_to_consider$prior[nodes_to_consider$id == j] <- new_prior
    }
    prior<-c(prior,list(rbind(rep(nodes_to_consider$prior[i]/outgoing_edges,outgoing_edges))))
  }


  #Datalist1: list of the number of individuals going from the stage along a particular edge in C_{0}
  data <- lapply(nodes_to_consider$label2_list, convert_to_matrix)
  # Print the resulting list of matrices
  # print("data")
  # print(data)

  #List of the stages that can be merged in the first step
  comparisonset <- nodes_to_consider %>%
    group_by(level2, label_list) %>%
    summarise(node_ids = list(id2), .groups = "keep") %>%
    pull(node_ids)  # Extract the list of node IDs

  # Print the resulting list of vectors
  # print(comparisonset)
  # print("end of comparisonset")
  # Initialize labelling as an empty matrix with 0 rows and columns

  # Print the levels of the factor
  #print(levels(column_vector))
  # Extract and sort levels
  #sorted_levels <- sort(levels(factor_levels))

  # Print sorted levels
  #print(sorted_levels)

  #print(sorted_levels)
  # Initialize labelling matrix
  labelling <-c()
  labelling <- NULL

  for (k in 1:(numbvariables - 1)) {
    # Alphabetically sort the levels of the current variable
    sorted_levels <- sort(levels(factor(exampledata[[k]])))
    #print("sorted levels")
    #print(sorted_levels)

    # Create the initial label with "NA" and appropriate repetitions
    label <- c("NA", rep("NA", sum(numb[1:k]) - 1))
    label <- c(label, rep(sorted_levels, numb[k]))
    #print(label)

    # If not the last variable, continue adding labels for subsequent variables
    if (k < (numbvariables - 1)) {
      for (i in (k + 1):(numbvariables - 1)) {
        label <- c(label, rep(sorted_levels, each = numb[i + 1] / numb[k + 1], numb[k + 1] / numbcat[k]))
      }
    }

    labelling <- cbind(labelling, label)

  }

  labelling <- nodes_to_consider$label_list


  row_numbers <- nodes_to_consider$id

  # Combine the sequence with the `labelling` matrix
  # Use `matrix` to ensure the row numbers are a column vector with correct dimensions
  labelling <- cbind(labelling, row_numbers)
  #print("labelling")
  #print(labelling)

  mergedlist <-c()
  for (i in 1:nrow(nodes_to_consider)){
    mergedlist<-c(mergedlist,list(labelling[i,]))
  }
  #print("mergedlist")
  #print(mergedlist)
  merged1<-c()
  lik <-0
  for( i in 1: nrow(nodes_to_consider)){
    alpha<-unlist(prior[i])
    #print("alpha")
    #print(alpha)
    N<-unlist(data[i])
    #print(N)
    lik<-lik+sum(lgamma(alpha+N)-lgamma(alpha))+sum(lgamma(sum(alpha))-lgamma(sum(alpha+N)))
  }
  score<-c(lik)
  #At each step we calculate the difference between the current CEG and the CEG in which two stages in the current comparison set have been merged.
  #We go through every possible combination of stages that can be merged. k is an index for the comparisonset we are in,
  #and i and j the position of the stages within the comparison set.
  diff.end<-1 #to start the algorithm
  while(diff.end>0){ #We stop when no positive difference is obtained by merging two stages
    #while(length(unlist(comparisonset))>3){
    difference <-0
    for (k in 1:length(comparisonset)){
      if(length(comparisonset[[k]])>1){ #can only merge if more than one stage in the comparisonset
        for (i in 1:(length(comparisonset[[k]])-1)){
          for (j in (i+1):length(comparisonset[[k]])){
            #to compare
            compare1<-comparisonset[[k]][i]
            compare2<-comparisonset[[k]][j]
            #we calculate the difference between the CEG where two stages are merged
            result<-lgamma(sum(prior[[compare1]]+prior[[compare2]]))-lgamma(sum(prior[[ compare1]]+data[[compare1]]+prior[[compare2]]+data[[compare2]]))+
              sum(lgamma(prior[[compare1]]+data[[compare1]]+prior[[compare2]]+data[[ compare2]]))-sum(lgamma(prior[[compare1]]+prior[[compare2]]))-
              #and the CEG where the two stages are not merged
              (lgamma(sum(prior[[compare1]]))-lgamma(sum(prior[[compare1]]+data[[compare1 ]]))+sum(lgamma(prior[[compare1]]+data[[compare1]]))-
                 sum(lgamma(prior[[compare1]]))+lgamma(sum(prior[[compare2]]))-lgamma(sum( prior[[compare2]]+data[[compare2]]))+
                 sum(lgamma(prior[[compare2]]+data[[compare2]]))-sum(lgamma(prior[[compare2]])))
            #if the resulting difference is greater than the current difference then we replace it
            if (result > difference){
              difference<-result
              merged<-c(compare1,compare2,k)
            }
          }
        }
      }
    }
    diff.end<-difference
    #We update our priorlist, datalist and comparisonset to obtain the priorlist , datalist and comparisonlist for C_{1}
    if(diff.end >0){
      prior[[merged[1]]]<-prior[[merged[1]]]+prior[[merged[2]]]
      prior[[merged[2]]]<-cbind(NA,NA)
      data[[merged[1]]]<-data[[merged[1]]]+data[[merged[2]]]
      data[[merged[2]]]<-cbind(NA,NA)
      comparisonset[[merged[3]]]<-comparisonset[[merged[3]]][-(which(comparisonset[[merged[3]]]==merged[2]))]
      mergedlist[[merged[1]]]<-cbind(mergedlist[[merged[1]]],mergedlist[[merged[2]]])
      mergedlist[[merged[2]]]<-cbind(NA,NA)
      lik<-lik+diff.end
      score<-c(score,lik)
      merged1<-cbind(merged1,merged)
    }
  }
  # Output: stages of the finest partition to be combined to obtain the most probable CEG structure
  stages<-c(1)
  for (i in 2:numbvariables){
    stages<-c(stages,comparisonset[[i-1]])
  }
  result<-mergedlist[stages]
  newlist<-list(prior=prior,data=data,stages=stages,result=result,score=score,merged=merged1 ,comparisonset=comparisonset ,mergedlist=mergedlist ,lik=lik)
  mergedlist
  row_numbers_list <- list()

  # Loop through each sublist in mergedlist
  for (i in 1:length(mergedlist)) {
    sublist <- mergedlist[[i]]

    # Initialize an empty vector to hold the row_numbers from this sublist
    sublist_row_numbers <- c()

    # Check if the sublist is not empty and not NULL
    if (!is.null(sublist) && length(sublist) > 0) {
      # Check if sublist is a matrix and contains "row_numbers"
      if (is.matrix(sublist)) {
        # Extract "row_numbers" from the matrix
        if (any(grepl("^row_numbers$", rownames(sublist)))) {
          row_numbers <- sublist[grepl("^s\\d+$", sublist)]
          if (length(row_numbers) > 0) {
            sublist_row_numbers <- c(sublist_row_numbers, row_numbers)
          }
        }
      } else if (is.list(sublist)) {
        # If sublist is a list, check each element for "row_numbers"
        for (j in 1:length(sublist)) {
          # Ensure the sublist element is not NULL
          if (!is.null(sublist[[j]])) {
            # Check for named "row_numbers" entry or row_numbers in matrix rownames
            if (names(sublist)[j] == "row_numbers" || (is.matrix(sublist[[j]]) && any(grepl("^row_numbers$", rownames(sublist[[j]]))))) {
              row_numbers <- sublist[[j]][grepl("^s\\d+$", sublist[[j]])]
              if (length(row_numbers) > 0) {
                sublist_row_numbers <- c(sublist_row_numbers, row_numbers)
              }
            }
          }
        }
      }
    }

    # Add the extracted row_numbers to the main list if any were found
    if (length(sublist_row_numbers) > 0) {
      row_numbers_list[[length(row_numbers_list) + 1]] <- sublist_row_numbers
    }
  }

  # Flatten the nested lists into simple vectors and print them
  flattened_list <- lapply(row_numbers_list, function(x) unlist(x))
  included_ids <- unlist(flattened_list)
  #print(flattened_list)
  #print(included_ids)
  #print("rownumbers")
  #print(row_numbers)

  # Identify missing IDs
  missing_ids <- setdiff(nodes_to_consider2, included_ids)
  #print("missing:")
  #print(missing_ids)

  # Add each missing ID as an individual sublist to flattened_list
  for (row_number in missing_ids) {
    flattened_list <- append(flattened_list, list(row_number))
  }

  # Print the updated flattened_list
  #print(flattened_list)

  num_colours <- length(flattened_list) # Number of groups
  colors <- randomcoloR::distinctColorPalette(num_colours)

  # Update the nodes dataframe with these colours
  for (i in 1:num_colours) {
    group <- flattened_list[[i]]
    color <- colors[i]

    # Update the colour for each node in the group
    #nodes$colour <- "#ffffff"
    nodes[nodes$id %in% group & nodes$color == "#FFFFFF", "color"] <- color
  }
  nodes$color[nodes$level == 1] <- "#FFFFFF"
  nodes$color[nodes$level == levels_to_exclude] <- "#FFFFFF"
  nodes$number <- 1

  # Create a dataframe of outgoing edge labels for each node
  outgoing_edges_labels <- edges %>%
    group_by(from) %>%
    summarize(
      outgoing_labels = paste(sort(unique(label1)), collapse = ","),
      outgoing_edges2 = n(),
      .groups = "drop"
    )

  # Check if outgoing_labels and outgoing_edges2 exist in nodes
  if (!("outgoing_labels" %in% colnames(nodes))) {
    nodes <- left_join(nodes, outgoing_edges_labels, by = c("id" = "from"))
  } else {
    # Only update missing values
    nodes <- nodes %>%
      left_join(outgoing_edges_labels, by = c("id" = "from")) %>%
      mutate(
        outgoing_labels = ifelse(is.na(outgoing_labels.x), outgoing_labels.y, outgoing_labels.x),
        outgoing_edges2 = ifelse(is.na(outgoing_edges2.x), outgoing_edges2.y, outgoing_edges2.x)
      ) %>%
      select(-outgoing_labels.y, -outgoing_labels.x, -outgoing_edges2.y, -outgoing_edges2.x)
  }

  # Merge outgoing edge labels with nodes data
  # Check for conflicts: Nodes with the same colour but different outgoing edge labels
  conflicting_nodes <- nodes %>%
    filter(color != "#FFFFFF") %>%  # Ignore white-coloured nodes
    group_by(color) %>%
    filter(n_distinct(outgoing_labels) > 1) %>%
    pull(id) %>%
    unique()

  # Raise an error if any conflicts exist
  if (length(conflicting_nodes) > 0) {
    stop(paste("Error: The following nodes have the same colour but different outgoing edge labels:",
               paste(conflicting_nodes, collapse = ", ")))
  }

  # Save updated nodes & edges to environment
  #stagedtreedf <- list(nodes = nodes, edges = edges)
  #assign("stagedtreedf", stagedtreedf, envir = .GlobalEnv)

  # Ensure nodes are movable in Y direction but fixed in X
  nodes <- nodes %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))

  # Return the updated visNetwork plot
  network_plot <- visNetwork(nodes = nodes, edges = edges) %>%
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
  result <- invisible(exampledata)


  # Return both the network plot and the result
  output <- list(stagedtree = network_plot, filtereddf = result)

  class(output) <- "staged_tree"
  return(output)

}
