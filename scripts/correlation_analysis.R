# Correlation Analysis Functions

#' Prepare Correlation Data
#'
#' Prepares data for correlation analysis by combining factor loadings, demographics, and personality data.
#'
#' @param factor_loadings Dataframe of factor loadings
#' @param demographics Dataframe containing demographic data
#' @param personality Dataframe containing personality data
#' @param include_factor_loadings Whether to include factor loadings in the correlation analysis
#' @param include_demographics Whether to include demographics in the correlation analysis
#' @param include_personality Whether to include personality traits in the correlation analysis
#' @return A dataframe of combined data for correlation analysis
prepare_correlation_data <- function(factor_loadings, demographics, personality,
                                    include_factor_loadings = TRUE,
                                    include_demographics = TRUE,
                                    include_personality = TRUE) {
  # Initialize empty dataframe
  correlation_data <- data.frame(participant_id = rownames(factor_loadings))
  
  # Add factor loadings if requested
  if (include_factor_loadings) {
    # Remove the "Defining Factor" column
    factor_data <- factor_loadings[, !colnames(factor_loadings) %in% "Defining Factor"]
    correlation_data <- cbind(correlation_data, factor_data)
  }
  
  # Add demographics if requested and available
  if (include_demographics && !is.null(demographics)) {
    # Select numeric demographic variables
    numeric_demographics <- demographics[, sapply(demographics, is.numeric)]
    
    # Add participant_id if not already included
    if (!"participant_id" %in% colnames(numeric_demographics)) {
      numeric_demographics$participant_id <- demographics$participant_id
    }
    
    # Merge with correlation data
    correlation_data <- merge(correlation_data, numeric_demographics, by = "participant_id")
  }
  
  # Add personality traits if requested and available
  if (include_personality && !is.null(personality)) {
    # Merge with correlation data
    correlation_data <- merge(correlation_data, personality, by = "participant_id")
  }
  
  # Remove participant_id column for correlation analysis
  correlation_data$participant_id <- NULL
  
  return(correlation_data)
}

#' Run Correlation Analysis
#'
#' Performs correlation analysis on the provided data.
#'
#' @param correlation_data Dataframe of data for correlation analysis
#' @param method Correlation method to use (pearson or spearman)
#' @return A list containing the correlation matrix and table
run_correlation_analysis <- function(correlation_data, method = "spearman") {
  # Calculate correlation matrix
  cor_matrix <- cor(correlation_data, method = method, use = "pairwise.complete.obs")
  
  # Calculate p-values
  cor_test <- psych::corr.test(correlation_data, method = method, adjust = "fdr")
  p_values <- cor_test$p
  
  # Create correlation table with p-values
  cor_table <- data.frame(
    Variable1 = character(),
    Variable2 = character(),
    Correlation = numeric(),
    P_Value = numeric(),
    Significance = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      var1 <- colnames(cor_matrix)[i]
      var2 <- colnames(cor_matrix)[j]
      cor_val <- cor_matrix[i, j]
      p_val <- p_values[i, j]
      
      # Determine significance
      if (p_val < 0.001) {
        sig <- "***"
      } else if (p_val < 0.01) {
        sig <- "**"
      } else if (p_val < 0.05) {
        sig <- "*"
      } else {
        sig <- ""
      }
      
      # Add to table
      cor_table <- rbind(cor_table, data.frame(
        Variable1 = var1,
        Variable2 = var2,
        Correlation = cor_val,
        P_Value = p_val,
        Significance = sig,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by absolute correlation value
  cor_table <- cor_table[order(-abs(cor_table$Correlation)), ]
  
  return(list(
    correlation_matrix = cor_matrix,
    correlation_table = cor_table
  ))
}

#' Plot Correlation Matrix
#'
#' Creates a correlation plot of the correlation matrix.
#'
#' @param cor_matrix Correlation matrix
#' @return A corrplot object
plot_correlation_matrix <- function(cor_matrix) {
  # Create nice variable labels
  var_labels <- colnames(cor_matrix)
  
  # Shorten factor names
  var_labels <- gsub("Factor", "F", var_labels)
  
  # Shorten demographic variable names
  var_labels <- gsub("years_experience", "Exp", var_labels)
  var_labels <- gsub("age", "Age", var_labels)
  
  # Shorten personality trait names
  var_labels <- gsub("openness", "Open", var_labels)
  var_labels <- gsub("conscientiousness", "Consc", var_labels)
  var_labels <- gsub("extraversion", "Extra", var_labels)
  var_labels <- gsub("agreeableness", "Agree", var_labels)
  var_labels <- gsub("neuroticism", "Neuro", var_labels)
  var_labels <- gsub("risk_tolerance", "Risk", var_labels)
  var_labels <- gsub("tech_aptitude", "Tech", var_labels)
  var_labels <- gsub("security_awareness", "SecAw", var_labels)
  
  # Plot correlation matrix
  corrplot::corrplot(
    cor_matrix,
    method = "circle",
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex = 0.6,
    col = corrplot::COL2("RdBu", 10),
    diag = FALSE,
    title = "Correlation Matrix",
    mar = c(0, 0, 1, 0)
  )
}

#' Plot Correlation Network
#'
#' Creates a network plot of correlations.
#'
#' @param cor_matrix Correlation matrix
#' @param threshold Correlation threshold for including edges
#' @return A ggplot object
plot_correlation_network <- function(cor_matrix, threshold = 0.3) {
  # Convert correlation matrix to edge list
  edges <- data.frame(
    from = character(),
    to = character(),
    weight = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) >= threshold) {
        edges <- rbind(edges, data.frame(
          from = colnames(cor_matrix)[i],
          to = colnames(cor_matrix)[j],
          weight = cor_matrix[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Create node list
  nodes <- data.frame(
    id = colnames(cor_matrix),
    label = colnames(cor_matrix),
    stringsAsFactors = FALSE
  )
  
  # Determine node types
  nodes$type <- "Other"
  nodes$type[grep("^Factor", nodes$id)] <- "Factor"
  nodes$type[nodes$id %in% c("age", "years_experience")] <- "Demographic"
  nodes$type[nodes$id %in% c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism")] <- "Big5"
  nodes$type[nodes$id %in% c("risk_tolerance", "tech_aptitude", "security_awareness")] <- "Security"
  
  # Create network plot
  if (requireNamespace("igraph", quietly = TRUE) && requireNamespace("ggraph", quietly = TRUE)) {
    # Create graph
    graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
    
    # Set edge weights
    igraph::E(graph)$width <- abs(igraph::E(graph)$weight) * 5
    igraph::E(graph)$color <- ifelse(igraph::E(graph)$weight > 0, "blue", "red")
    
    # Set node colors
    node_colors <- c("Factor" = "orange", "Demographic" = "green", "Big5" = "purple", "Security" = "red", "Other" = "gray")
    
    # Create plot
    p <- ggraph::ggraph(graph, layout = "fr") +
      ggraph::geom_edge_link(ggplot2::aes(width = abs(weight), color = weight)) +
      ggraph::geom_node_point(ggplot2::aes(color = type), size = 8) +
      ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE) +
      ggplot2::scale_edge_width(range = c(0.5, 3)) +
      ggplot2::scale_edge_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
      ggplot2::scale_color_manual(values = node_colors) +
      ggraph::theme_graph() +
      ggplot2::labs(
        title = "Correlation Network",
        subtitle = paste("Correlations with absolute value >=", threshold)
      )
    
    return(p)
  } else {
    # If igraph and ggraph are not available, return a message
    message("Packages 'igraph' and 'ggraph' are required for network plots.")
    return(NULL)
  }
}
