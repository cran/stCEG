#' Apply Agglomerative Hierarchical Clustering (AHC) Stage Colouring
#'
#' Applies an Agglomerative Hierarchical Clustering (AHC) stage-merging
#' algorithm to an event tree or staged tree. The algorithm groups situations
#' with equivalent floret structures and iteratively merges stages according to
#' a Bayesian scoring criterion based on Dirichlet-Multinomial likelihoods.
#'
#' Nodes assigned to the same stage are coloured identically, and the resulting
#' stage allocation is returned as a \code{"staged_tree"} object suitable for
#' further analysis, visualisation, or CEG construction.
#'
#' The algorithm only considers non-terminal situations when forming stages.
#' Root and sink nodes are always assigned the default white colour and are not
#' included in stage merging.
#'
#' @param event_tree_obj An object of class \code{"event_tree"} or
#'   \code{"staged_tree"} containing node and edge information together with
#'   the underlying dataset stored in \code{$data}.
#'
#' @param level_separation Numeric value controlling the separation between
#'   levels when plotting the resulting staged tree. Included for compatibility
#'   with staged tree visualisation methods. Default is \code{1000}.
#'
#' @param node_distance Numeric value controlling the spacing between nodes when
#'   plotting the resulting staged tree. Included for compatibility with staged
#'   tree visualisation methods. Default is \code{300}.
#'
#' @details
#' The procedure:
#' \enumerate{
#'   \item Extracts node, edge, and dataset information from the supplied
#'   event tree or staged tree object.
#'   \item Identifies comparable situations based on their level and outgoing
#'   edge labels.
#'   \item Constructs Dirichlet prior vectors and floret count vectors for each
#'   situation.
#'   \item Iteratively merges candidate stages whenever doing so increases the
#'   Bayesian score.
#'   \item Assigns a unique colour to each resulting stage using the
#'   \pkg{randomcoloR} package.
#'   \item Returns a new \code{"staged_tree"} object containing the updated
#'   stage colouring.
#' }
#'
#' A consistency check is performed before returning the staged tree to ensure
#' that nodes sharing the same stage colour have identical outgoing edge
#' structures. An error is raised if such conflicts are detected.
#'
#' @return
#' An object of class \code{"staged_tree"} containing:
#' \itemize{
#'   \item Updated node colours representing the inferred stage structure.
#'   \item Original edge information.
#'   \item The underlying dataset.
#'   \item Node-level method annotations identifying stages generated using
#'   the AHC procedure.
#' }
#'
#' @examples
#' et <- create_event_tree(homicides, c(1:3))
#'
#' st <- ahc_colouring(et)
#'
#' print(st)
#' summary(st)
#' plot(st)
#'
#'
#' @seealso
#' \code{\link{create_staged_tree}},
#' \code{\link{plot.staged_tree}},
#' \code{\link{summary.staged_tree}}
#'
#' @export
ahc_colouring <- function(event_tree_obj,
                          level_separation = 1000,
                          node_distance = 300) {

  if (!requireNamespace("randomcoloR", quietly = TRUE)) {
    stop("Package 'randomcoloR' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Correct S3 extraction
  if (inherits(event_tree_obj, "event_tree")) {
    nodes <- dplyr::as_tibble(event_tree_obj$nodes)
    edges <- dplyr::as_tibble(event_tree_obj$edges)
    filtereddf <- event_tree_obj$data

  } else if (inherits(event_tree_obj, "staged_tree")) {
    nodes <- dplyr::as_tibble(event_tree_obj$nodes)
    edges <- dplyr::as_tibble(event_tree_obj$edges)
    filtereddf <- event_tree_obj$data

  } else {
    stop("Input must be an event_tree or staged_tree object.")
  }

  #nodes$method <- NA_character_

  # Levels and nodes to consider (exclude root and terminal level)
  unique_levels <- unique(nodes$level)
  levels_to_exclude <- max(unique_levels)
  nodes_to_consider <- nodes[!(nodes$level %in% levels_to_exclude), ]
  nodes_to_consider$id2 <- 1:nrow(nodes_to_consider)
  nodes_to_consider2 <- nodes_to_consider$id

  # Edge summaries
  edges_to_consider <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::summarize(
      label2_list = paste(label2, collapse = ", ")
    )


  label_matching <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::summarize(
      label_list = paste(label1, collapse = ", ")
    )

  if (!"outgoing_edges" %in% colnames(nodes)) {
    outgoing_edges <- dplyr::count(edges, from, name = "outgoing_edges")
    nodes_to_consider <- dplyr::inner_join(
      nodes_to_consider,
      outgoing_edges,
      by = c("id" = "from"),
      keep = FALSE
    )
  } else {

  }


  edges_to_consider <- dplyr::inner_join(
    edges_to_consider,
    label_matching,
    by = "from"
  )

  nodes_to_consider <- dplyr::inner_join(
    nodes_to_consider,
    edges_to_consider,
    by = c("id" = "from"),
    keep = FALSE
  )

  # Helper: convert label2_list string to numeric matrix
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

  # Ensure all columns in filtereddf are factors
  filtereddf[] <- lapply(filtereddf, function(x) {
    if (!is.factor(x)) as.factor(x) else x
  })

  numbvariables <- ncol(filtereddf)
  numbcat <- sapply(filtereddf, nlevels)

  equivsize <- max(nodes_to_consider$outgoing_edges)

  numb <- numeric(numbvariables)
  numb[1] <- 1

  for (i in 2:numbvariables) {
    numb[i] <- prod(numbcat[1:(i - 1)])
  }

  nodes_to_consider$prior <- 0
  nodes_to_consider$prior[1] <- equivsize

  prior <- vector("list", nrow(nodes_to_consider))

  prior<-c()

  for (i in 1:nrow(nodes_to_consider)) {

    # Get the current row's 'id' from nodes_to_consider
    current_id <- nodes_to_consider$id[i]
    # Step 1: Get the number of outgoing edges for this node (this could be a count of edges with 'from' = current_id)
    outgoing_edges <- nodes_to_consider$outgoing_edges[i]

    # Step 2: Calculate the new prior (divide current prior by the number of outgoing edges)
    current_prior <- nodes_to_consider$prior[i]  # Assuming prior column exists
    new_prior <- current_prior / outgoing_edges


    # Step 3: Update the 'prior' for rows in edges where 'from' equals the current 'id'
    # and update the corresponding 'prior' in nodes_to_consider based on 'to'
    to_nodes <- edges$to[edges$from == current_id]  # Get all 'to' nodes where 'from' equals current_id

    # Update the 'prior' for corresponding nodes in nodes_to_consider
    for (j in to_nodes) {
      nodes_to_consider$prior[nodes_to_consider$id == j] <- new_prior
    }
    prior<-c(prior,list(rbind(rep(nodes_to_consider$prior[i]/outgoing_edges,outgoing_edges))))
  }



  data_list <- lapply(nodes_to_consider$label2_list, convert_to_matrix)


  comparisonset <- nodes_to_consider %>% dplyr::group_by(level2, label_list)
  comparisonset <- dplyr::summarise(
    comparisonset,
    node_ids = list(id2),
    .groups = "keep"
  )
  comparisonset <- comparisonset$node_ids

  labelling <-c()
  labelling <- NULL

  for (k in 1:(numbvariables - 1)) {
    # Alphabetically sort the levels of the current variable
    sorted_levels <- sort(levels(factor(filtereddf[[k]])))

    # Create the initial label with "NA" and appropriate repetitions
    label <- c("NA", rep("NA", sum(numb[1:k]) - 1))
    label <- c(label, rep(sorted_levels, numb[k]))

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


  mergedlist <-c()
  for (i in 1:nrow(nodes_to_consider)){
    mergedlist<-c(mergedlist,list(labelling[i,]))
  }

  merged1<-c()
  lik <-0
  for( i in 1: nrow(nodes_to_consider)){
    alpha<-unlist(prior[i])

    N<-unlist(data_list[i])

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
            result<-lgamma(sum(prior[[compare1]]+prior[[compare2]]))-lgamma(sum(prior[[ compare1]]+data_list[[compare1]]+prior[[compare2]]+data_list[[compare2]]))+
              sum(lgamma(prior[[compare1]]+data_list[[compare1]]+prior[[compare2]]+data_list[[ compare2]]))-sum(lgamma(prior[[compare1]]+prior[[compare2]]))-
              #and the CEG where the two stages are not merged
              (lgamma(sum(prior[[compare1]]))-lgamma(sum(prior[[compare1]]+data_list[[compare1 ]]))+sum(lgamma(prior[[compare1]]+data_list[[compare1]]))-
                 sum(lgamma(prior[[compare1]]))+lgamma(sum(prior[[compare2]]))-lgamma(sum( prior[[compare2]]+data_list[[compare2]]))+
                 sum(lgamma(prior[[compare2]]+data_list[[compare2]]))-sum(lgamma(prior[[compare2]])))
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
      data_list[[merged[1]]]<-data_list[[merged[1]]]+data_list[[merged[2]]]
      data_list[[merged[2]]]<-cbind(NA,NA)
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
  newlist<-list(prior=prior,data_list=data_list,stages=stages,result=result,score=score,merged=merged1 ,comparisonset=comparisonset ,mergedlist=mergedlist ,lik=lik)
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


  # Identify missing IDs
  missing_ids <- setdiff(nodes_to_consider2, included_ids)

  # Add each missing ID as an individual sublist to flattened_list
  for (row_number in missing_ids) {
    flattened_list <- append(flattened_list, list(row_number))
  }

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
  if (!"method" %in% colnames(nodes)) {
    nodes$method <- NA_character_
  }

  nodes <- dplyr::as_tibble(nodes)

  # Create a dataframe of outgoing edge labels for each node
  outgoing_edges_labels <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::summarize(
      outgoing_labels = paste(sort(unique(label1)), collapse = ","),
      outgoing_edges2 = dplyr::n(),
      .groups = "drop"
    )


  # Check if outgoing_labels and outgoing_edges2 exist in nodes
  if (!("outgoing_labels" %in% colnames(nodes))) {
    nodes <- dplyr::left_join(nodes, outgoing_edges_labels, by = c("id" = "from"))
  } else if (("outgoing_labels.y" %in% colnames(nodes))){
    # Only update missing values
    nodes <- dplyr::left_join(nodes, outgoing_edges_labels, by = c("id" = "from")) %>%
      dplyr::mutate(
        outgoing_labels = dplyr::coalesce(
          outgoing_labels,
          outgoing_labels.x,
          outgoing_labels.y
        ),
        outgoing_edges2 = dplyr::coalesce(
          outgoing_edges2,
          outgoing_edges2.x,
          outgoing_edges2.y
        )
      ) %>%
      dplyr::select(
        -dplyr::matches("\\.x$"),
        -dplyr::matches("\\.y$")
      )

  }

  #nodes <- dplyr::as_tibble(nodes)

  # Check for conflicts: Nodes with the same colour but different outgoing edge labels
  conflicting_nodes <- nodes %>%
    dplyr::filter(color != "#FFFFFF") %>%  # Ignore white-coloured nodes
    dplyr::group_by(color) %>%
    dplyr::filter(dplyr::n_distinct(outgoing_labels) > 1) %>%
    dplyr::pull(id) %>%
    unique()

  # Raise an error if any conflicts exist
  if (length(conflicting_nodes) > 0) {
    stop(paste("Error: The following nodes have the same colour but different outgoing edge labels:",
               paste(conflicting_nodes, collapse = ", ")))
  }

  # Tag method at node level
  nodes$method[
    is.na(nodes$method) &
      nodes$color != "#FFFFFF"
  ] <- "ahc"


  # Build staged_tree object
  staged_tree <- create_staged_tree(
    nodes      = nodes,
    edges      = edges,
    filtereddf = filtereddf,
    method     = "ahc"
  )

  return(staged_tree)
}
