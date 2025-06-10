# Q-Method Analysis Functions

#' Run Q-Method Analysis
#'
#' This function performs Q-methodology analysis on the provided data.
#'
#' @param statements Dataframe containing the Q statements
#' @param qsort_data Dataframe containing the Q-sort data
#' @param num_factors Number of factors to extract
#' @param rotation Rotation method to use (varimax or none)
#' @return A list containing the results of the Q-method analysis
run_qmethod_analysis <- function(statements, qsort_data, num_factors = 3, rotation = "varimax") {
  # Reshape the Q-sort data into a matrix
  qsort_matrix <- reshape_qsort_data(qsort_data)
  
  # Run the Q-method analysis
  qmethod_result <- try({
    qmethod::qmethod(qsort_matrix, 
                     nfactors = num_factors, 
                     rotation = rotation)
  })
  
  if (inherits(qmethod_result, "try-error")) {
    return(list(
      error = "Error in Q-method analysis. Please check your data and try again."
    ))
  }
  
  # Extract and format the results
  results <- list(
    summary = qmethod_result,
    factor_loadings = extract_factor_loadings(qmethod_result),
    factor_scores = extract_factor_scores(qmethod_result, statements),
    consensus_statements = extract_consensus_statements(qmethod_result, statements),
    distinguishing_statements = extract_distinguishing_statements(qmethod_result, statements)
  )
  
  return(results)
}

#' Reshape Q-Sort Data
#'
#' Reshapes the Q-sort data from long format to a matrix format suitable for Q-method analysis.
#'
#' @param qsort_data Dataframe containing the Q-sort data in long format
#' @return A matrix of Q-sort data with participants as rows and statements as columns
reshape_qsort_data <- function(qsort_data) {
  # Reshape from long to wide format
  qsort_wide <- reshape2::dcast(qsort_data, participant_id ~ statement_id, value.var = "score")
  
  # Convert to matrix and set row names
  qsort_matrix <- as.matrix(qsort_wide[, -1])
  rownames(qsort_matrix) <- qsort_wide$participant_id
  
  return(qsort_matrix)
}

#' Extract Factor Loadings
#'
#' Extracts and formats the factor loadings from the Q-method analysis results.
#'
#' @param qmethod_result The result of the Q-method analysis
#' @return A dataframe of factor loadings
extract_factor_loadings <- function(qmethod_result) {
  # Extract factor loadings
  loadings <- qmethod_result$loa
  
  # Add a column indicating the defining factor for each participant
  loadings$`Defining Factor` <- apply(loadings, 1, function(row) {
    factor_index <- which.max(abs(row))
    if (abs(row[factor_index]) >= 0.5) {  # Threshold for defining factor
      return(paste0("Factor ", factor_index))
    } else {
      return("None")
    }
  })
  
  return(loadings)
}

#' Extract Factor Scores
#'
#' Extracts and formats the factor scores from the Q-method analysis results.
#'
#' @param qmethod_result The result of the Q-method analysis
#' @param statements Dataframe containing the Q statements
#' @return A dataframe of factor scores with statement text
extract_factor_scores <- function(qmethod_result, statements) {
  # Extract factor scores
  scores <- qmethod_result$zsc
  
  # Add statement text
  scores_with_text <- data.frame(
    Statement = statements$statement[match(rownames(scores), statements$id)],
    scores
  )
  
  return(scores_with_text)
}

#' Extract Consensus Statements
#'
#' Extracts and formats the consensus statements from the Q-method analysis results.
#'
#' @param qmethod_result The result of the Q-method analysis
#' @param statements Dataframe containing the Q statements
#' @return A dataframe of consensus statements
extract_consensus_statements <- function(qmethod_result, statements) {
  # Extract consensus statements
  consensus <- qmethod_result$qdc$consensus
  
  if (length(consensus) == 0 || is.null(consensus)) {
    return(data.frame(
      Statement_ID = character(),
      Statement = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create dataframe with statement text
  consensus_df <- data.frame(
    Statement_ID = consensus,
    Statement = statements$statement[match(consensus, statements$id)],
    stringsAsFactors = FALSE
  )
  
  # Add factor scores for each consensus statement
  factor_scores <- qmethod_result$zsc
  for (i in 1:ncol(factor_scores)) {
    factor_name <- paste0("Factor_", i, "_Score")
    consensus_df[[factor_name]] <- factor_scores[as.character(consensus_df$Statement_ID), i]
  }
  
  return(consensus_df)
}

#' Extract Distinguishing Statements
#'
#' Extracts and formats the distinguishing statements from the Q-method analysis results.
#'
#' @param qmethod_result The result of the Q-method analysis
#' @param statements Dataframe containing the Q statements
#' @return A dataframe of distinguishing statements
extract_distinguishing_statements <- function(qmethod_result, statements) {
  # Extract distinguishing statements
  distinguishing <- qmethod_result$qdc$dist.statements
  
  if (length(distinguishing) == 0 || is.null(distinguishing)) {
    return(data.frame(
      Factor = character(),
      Statement_ID = character(),
      Statement = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create dataframe with statement text
  dist_df <- data.frame()
  
  for (i in 1:length(distinguishing)) {
    if (length(distinguishing[[i]]) > 0) {
      temp_df <- data.frame(
        Factor = paste0("Factor ", i),
        Statement_ID = distinguishing[[i]],
        Statement = statements$statement[match(distinguishing[[i]], statements$id)],
        stringsAsFactors = FALSE
      )
      dist_df <- rbind(dist_df, temp_df)
    }
  }
  
  # Add factor scores for each distinguishing statement
  if (nrow(dist_df) > 0) {
    factor_scores <- qmethod_result$zsc
    for (i in 1:ncol(factor_scores)) {
      factor_name <- paste0("Factor_", i, "_Score")
      dist_df[[factor_name]] <- factor_scores[as.character(dist_df$Statement_ID), i]
    }
  }
  
  return(dist_df)
}

#' Plot Factor Loadings
#'
#' Creates a plot of factor loadings.
#'
#' @param factor_loadings Dataframe of factor loadings
#' @return A ggplot object
plot_factor_loadings <- function(factor_loadings) {
  # Remove the "Defining Factor" column for plotting
  plot_data <- factor_loadings[, !colnames(factor_loadings) %in% "Defining Factor"]
  
  # Reshape data for plotting
  plot_data_long <- reshape2::melt(
    as.matrix(plot_data),
    varnames = c("Participant", "Factor"),
    value.name = "Loading"
  )
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = Factor, y = Loading, fill = Factor)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~ Participant) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = "Factor Loadings by Participant",
      x = "Factor",
      y = "Loading"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set1")
  
  return(p)
}

#' Plot Factor Scores
#'
#' Creates a plot of factor scores.
#'
#' @param factor_scores Dataframe of factor scores
#' @param statements Dataframe containing the Q statements
#' @return A ggplot object
plot_factor_scores <- function(factor_scores, statements) {
  # Reshape data for plotting
  plot_data <- factor_scores
  rownames(plot_data) <- NULL
  
  # Add statement ID
  plot_data$Statement_ID <- statements$id[match(plot_data$Statement, statements$statement)]
  
  # Reshape to long format
  plot_data_long <- reshape2::melt(
    plot_data,
    id.vars = c("Statement", "Statement_ID"),
    variable.name = "Factor",
    value.name = "Score"
  )
  
  # Sort by score within each factor
  plot_data_long <- plot_data_long[order(plot_data_long$Factor, plot_data_long$Score), ]
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = reorder(Statement_ID, Score), y = Score, fill = Factor)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~ Factor, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = "Factor Scores by Statement",
      x = "Statement ID",
      y = "Z-Score"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set1")
  
  return(p)
}
