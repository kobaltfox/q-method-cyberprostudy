# Visualization Functions for Q-Method Cybersecurity Study
# This script contains functions for visualizing the results of the analyses

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(reshape2)
library(gridExtra)

#' Create a factor loadings plot
#'
#' @param qmethod_results List containing the results of the Q-method analysis
#' @param factors Vector of factor numbers to include in the plot
#' @return A ggplot object containing the factor loadings plot
plot_factor_loadings <- function(qmethod_results, factors = c(1, 2)) {
  # Get factor loadings
  loadings <- qmethod_results$loadings
  
  # Get factor columns
  factor_cols <- paste0("RC", factors)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Participant = loadings$Participant,
    Factor1 = loadings[[factor_cols[1]]],
    Factor2 = loadings[[factor_cols[2]]],
    HighestLoading = loadings$HighestLoading,
    SignificantLoading = loadings$SignificantLoading,
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Factor1, y = Factor2, color = as.factor(HighestLoading), 
                           shape = SignificantLoading, label = Participant)) +
    geom_point(size = 3) +
    geom_text(hjust = -0.3, vjust = 0.3, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Factor Loadings Plot (Factors", factors[1], "and", factors[2], ")"),
      x = paste("Factor", factors[1]),
      y = paste("Factor", factors[2]),
      color = "Highest Loading",
      shape = "Significant Loading"
    ) +
    scale_shape_manual(values = c(1, 16)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Create a factor scores plot
#'
#' @param qmethod_results List containing the results of the Q-method analysis
#' @param factors Vector of factor numbers to include in the plot
#' @param top_n Number of top and bottom statements to include in the plot
#' @return A ggplot object containing the factor scores plot
plot_factor_scores <- function(qmethod_results, factors = c(1, 2), top_n = 5) {
  # Get factor scores
  scores <- qmethod_results$scores
  
  # Get factor columns
  factor_cols <- paste0("Factor", factors)
  
  # Create data frame for plotting
  plot_data <- scores[, c("Statement", "StatementText", factor_cols)]
  
  # Reshape data for plotting
  plot_data_long <- pivot_longer(plot_data, cols = factor_cols, 
                               names_to = "Factor", values_to = "Score")
  
  # Get top and bottom statements for each factor
  top_statements <- plot_data_long %>%
    group_by(Factor) %>%
    arrange(desc(Score)) %>%
    slice_head(n = top_n) %>%
    ungroup()
  
  bottom_statements <- plot_data_long %>%
    group_by(Factor) %>%
    arrange(Score) %>%
    slice_head(n = top_n) %>%
    ungroup()
  
  # Combine top and bottom statements
  plot_data_subset <- rbind(top_statements, bottom_statements)
  
  # Create plot
  p <- ggplot(plot_data_subset, aes(x = reorder(Statement, Score), y = Score, fill = Factor)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Factor Scores for Top and Bottom", top_n, "Statements"),
      x = "Statement",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  return(p)
}

#' Create a consensus statements plot
#'
#' @param qmethod_results List containing the results of the Q-method analysis
#' @param top_n Number of consensus statements to include in the plot
#' @return A ggplot object containing the consensus statements plot
plot_consensus_statements <- function(qmethod_results, top_n = 10) {
  # Get consensus statements
  consensus <- qmethod_results$consensus
  
  # Sort by standard deviation (ascending)
  consensus <- consensus[order(consensus$ScoreSD), ]
  
  # Get top consensus statements
  consensus_top <- head(consensus, top_n)
  
  # Get factor columns
  factor_cols <- grep("^Factor", colnames(consensus), value = TRUE)
  
  # Create data frame for plotting
  plot_data <- consensus_top[, c("Statement", "StatementText", factor_cols)]
  
  # Reshape data for plotting
  plot_data_long <- pivot_longer(plot_data, cols = factor_cols, 
                               names_to = "Factor", values_to = "Score")
  
  # Create plot
  p <- ggplot(plot_data_long, aes(x = reorder(Statement, -Score), y = Score, fill = Factor)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Top", top_n, "Consensus Statements"),
      x = "Statement",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  return(p)
}

#' Create a distinguishing statements plot
#'
#' @param qmethod_results List containing the results of the Q-method analysis
#' @param factor Factor number to include in the plot
#' @param top_n Number of distinguishing statements to include in the plot
#' @return A ggplot object containing the distinguishing statements plot
plot_distinguishing_statements <- function(qmethod_results, factor = 1, top_n = 10) {
  # Get distinguishing statements
  distinguishing <- qmethod_results$distinguishing
  
  # Get distinguishing column for this factor
  distinguishing_col <- paste0("DistinguishingForFactor", factor)
  
  # Get factor column for this factor
  factor_col <- paste0("Factor", factor)
  
  # Filter for distinguishing statements for this factor
  distinguishing_factor <- distinguishing[distinguishing[[distinguishing_col]], ]
  
  # Sort by absolute score (descending)
  distinguishing_factor <- distinguishing_factor[order(-abs(distinguishing_factor[[factor_col]])), ]
  
  # Get top distinguishing statements
  distinguishing_top <- head(distinguishing_factor, top_n)
  
  # Get all factor columns
  factor_cols <- grep("^Factor", colnames(distinguishing), value = TRUE)
  
  # Create data frame for plotting
  plot_data <- distinguishing_top[, c("Statement", "StatementText", factor_cols)]
  
  # Reshape data for plotting
  plot_data_long <- pivot_longer(plot_data, cols = factor_cols, 
                               names_to = "Factor", values_to = "Score")
  
  # Create plot
  p <- ggplot(plot_data_long, aes(x = reorder(Statement, -Score), y = Score, fill = Factor)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Top", top_n, "Distinguishing Statements for Factor", factor),
      x = "Statement",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  return(p)
}

#' Create a demographic distribution plot
#'
#' @param demographics Data frame containing demographic data
#' @param variable Name of the demographic variable to plot
#' @return A ggplot object containing the demographic distribution plot
plot_demographic_distribution <- function(demographics, variable) {
  # Check if variable exists in demographics
  if (!variable %in% colnames(demographics)) {
    stop(paste("Variable", variable, "not found in demographics data"))
  }
  
  # Extract variable data
  data <- demographics[[variable]]
  
  # Check if variable is numeric or categorical
  if (is.numeric(data)) {
    # Numeric variable
    p <- ggplot(demographics, aes(x = .data[[variable]])) +
      geom_histogram(bins = 10, fill = "steelblue", color = "white") +
      labs(
        title = paste("Distribution of", variable),
        x = variable,
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  } else {
    # Categorical variable
    p <- ggplot(demographics, aes(x = .data[[variable]], fill = .data[[variable]])) +
      geom_bar() +
      labs(
        title = paste("Distribution of", variable),
        x = variable,
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  }
  
  return(p)
}

#' Create a demographic differences plot
#'
#' @param demographic_diff_results List containing the results of the demographic differences analysis
#' @return A ggplot object containing the demographic differences plot
plot_demographic_differences <- function(demographic_diff_results) {
  # Get variable name
  variable <- demographic_diff_results$variable
  
  # Get data
  data <- demographic_diff_results$data
  
  # Check if variable is numeric or categorical
  if (demographic_diff_results$type == "numeric") {
    # Numeric variable
    p <- ggplot(data, aes(x = AssignedFactor, y = .data[[variable]], fill = AssignedFactor)) +
      geom_boxplot() +
      labs(
        title = paste("Differences in", variable, "by Factor"),
        x = "Factor",
        y = variable
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      )
  } else {
    # Categorical variable
    p <- ggplot(data, aes(x = AssignedFactor, fill = .data[[variable]])) +
      geom_bar(position = "fill") +
      labs(
        title = paste("Differences in", variable, "by Factor"),
        x = "Factor",
        y = "Proportion",
        fill = variable
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  }
  
  return(p)
}

#' Create a personality trait distribution plot
#'
#' @param personality Data frame containing personality trait data
#' @param trait Name of the personality trait to plot
#' @return A ggplot object containing the personality trait distribution plot
plot_personality_distribution <- function(personality, trait) {
  # Check if trait exists in personality
  if (!trait %in% colnames(personality)) {
    stop(paste("Trait", trait, "not found in personality data"))
  }
  
  # Create plot
  p <- ggplot(personality, aes(x = .data[[trait]])) +
    geom_histogram(bins = 10, fill = "steelblue", color = "white") +
    labs(
      title = paste("Distribution of", trait),
      x = trait,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
  
  return(p)
}

#' Create a personality trait differences plot
#'
#' @param personality_diff_results List containing the results of the personality trait differences analysis
#' @return A ggplot object containing the personality trait differences plot
plot_personality_differences <- function(personality_diff_results) {
  # Get trait name
  trait <- personality_diff_results$trait
  
  # Get data
  data <- personality_diff_results$data
  
  # Create plot
  p <- ggplot(data, aes(x = AssignedFactor, y = .data[[trait]], fill = AssignedFactor)) +
    geom_boxplot() +
    labs(
      title = paste("Differences in", trait, "by Factor"),
      x = "Factor",
      y = trait
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  return(p)
}

#' Create a personality trait correlations plot
#'
#' @param personality_corr_results List containing the results of the personality trait correlation analysis
#' @return A corrplot object containing the personality trait correlations plot
plot_personality_correlations <- function(personality_corr_results) {
  # Get correlation matrix
  cor_matrix <- personality_corr_results$cor_matrix
  
  # Create plot
  corrplot(cor_matrix, method = "circle", type = "upper", 
          tl.col = "black", tl.srt = 45, 
          title = "Personality Trait Correlations",
          mar = c(0, 0, 1, 0))
  
  return(invisible(NULL))
}

#' Create a correlation matrix plot
#'
#' @param correlation_results List containing the results of the correlation analysis
#' @param title Title of the plot
#' @return A corrplot object containing the correlation matrix plot
plot_correlation_matrix <- function(correlation_results, title = "Correlation Matrix") {
  # Get correlation matrix
  cor_matrix <- correlation_results$matrix
  
  # Get p-values
  p_values <- correlation_results$p_values
  
  # Create plot
  corrplot(cor_matrix, method = "circle", type = "upper", 
          p.mat = p_values, sig.level = 0.05, insig = "blank",
          tl.col = "black", tl.srt = 45, 
          title = title,
          mar = c(0, 0, 1, 0))
  
  return(invisible(NULL))
}

#' Create a factor-specific correlations plot
#'
#' @param correlation_results List containing the results of the correlation analysis
#' @param factor Factor name to include in the plot
#' @param top_n Number of top correlations to include in the plot
#' @return A ggplot object containing the factor-specific correlations plot
plot_factor_correlations <- function(correlation_results, factor, top_n = 10) {
  # Get significant correlations
  significant_correlations <- correlation_results$significant
  
  # Filter for correlations with this factor
  factor_correlations <- significant_correlations[
    significant_correlations$Variable1 == factor | significant_correlations$Variable2 == factor,
  ]
  
  # If no correlations found, return NULL
  if (nrow(factor_correlations) == 0) {
    return(NULL)
  }
  
  # Ensure the factor is Variable1
  for (i in 1:nrow(factor_correlations)) {
    if (factor_correlations$Variable2[i] == factor) {
      # Swap Variable1 and Variable2
      temp <- factor_correlations$Variable1[i]
      factor_correlations$Variable1[i] <- factor_correlations$Variable2[i]
      factor_correlations$Variable2[i] <- temp
    }
  }
  
  # Sort by absolute correlation (descending)
  factor_correlations <- factor_correlations[order(-abs(factor_correlations$Correlation)), ]
  
  # Get top correlations
  factor_correlations_top <- head(factor_correlations, top_n)
  
  # Create plot
  p <- ggplot(factor_correlations_top, aes(x = reorder(Variable2, Correlation), y = Correlation, 
                                         fill = Correlation > 0)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Top", top_n, "Correlations with", factor),
      x = "Variable",
      y = "Correlation"
    ) +
    scale_fill_manual(values = c("firebrick", "steelblue"), 
                     labels = c("Negative", "Positive"),
                     name = "Correlation") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  return(p)
}

#' Create a dashboard of plots
#'
#' @param plots List of ggplot objects to include in the dashboard
#' @param ncol Number of columns in the dashboard
#' @return A grid.arrange object containing the dashboard of plots
create_dashboard <- function(plots, ncol = 2) {
  # Remove NULL plots
  plots <- plots[!sapply(plots, is.null)]
  
  # Create dashboard
  do.call(grid.arrange, c(plots, ncol = ncol))
  
  return(invisible(NULL))
}
