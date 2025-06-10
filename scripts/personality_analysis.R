# Personality Analysis Functions

#' Plot Personality Summary
#'
#' Creates a boxplot of personality traits across all participants.
#'
#' @param personality Dataframe containing personality data
#' @return A ggplot object
plot_personality_summary <- function(personality) {
  # Reshape data for plotting
  plot_data <- reshape2::melt(
    personality,
    id.vars = "participant_id",
    variable.name = "Trait",
    value.name = "Score"
  )
  
  # Remove participant_id column from plotting
  plot_data <- plot_data[plot_data$Trait != "participant_id", ]
  
  # Order traits
  trait_order <- c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism",
                  "risk_tolerance", "tech_aptitude", "security_awareness")
  plot_data$Trait <- factor(plot_data$Trait, levels = trait_order)
  
  # Create nice labels for traits
  trait_labels <- c(
    "openness" = "Openness",
    "conscientiousness" = "Conscientiousness",
    "extraversion" = "Extraversion",
    "agreeableness" = "Agreeableness",
    "neuroticism" = "Neuroticism",
    "risk_tolerance" = "Risk Tolerance",
    "tech_aptitude" = "Tech Aptitude",
    "security_awareness" = "Security Awareness"
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Trait, y = Score, fill = Trait)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = "Distribution of Personality Traits",
      x = "Personality Trait",
      y = "Score"
    ) +
    ggplot2::scale_x_discrete(labels = trait_labels) +
    ggplot2::scale_fill_brewer(palette = "Set3")
  
  return(p)
}

#' Plot Personality by Factor
#'
#' Creates a plot showing the relationship between a personality trait and Q-method factors.
#'
#' @param personality Dataframe containing personality data
#' @param factor_loadings Dataframe of factor loadings
#' @param personality_var Name of the personality variable to plot
#' @param personality_label Label for the personality variable
#' @return A ggplot object
plot_personality_by_factor <- function(personality, factor_loadings, personality_var, personality_label) {
  # Merge personality with factor loadings
  merged_data <- merge(personality, factor_loadings, by.x = "participant_id", by.y = "row.names")
  
  # Create scatter plot with trend lines
  plot_data <- reshape2::melt(
    merged_data,
    id.vars = c("participant_id", personality_var),
    measure.vars = grep("^Factor", colnames(merged_data), value = TRUE),
    variable.name = "Factor",
    value.name = "Loading"
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = personality_var, y = "Loading", color = "Factor")) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(personality_label, "vs. Factor Loadings"),
      x = personality_label,
      y = "Factor Loading"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1")
  
  return(p)
}

#' Plot Personality Correlation
#'
#' Creates a correlation plot of personality traits.
#'
#' @param personality Dataframe containing personality data
#' @return A corrplot object
plot_personality_correlation <- function(personality) {
  # Select only numeric columns (exclude participant_id)
  personality_data <- personality[, !colnames(personality) %in% "participant_id"]
  
  # Calculate correlation matrix
  cor_matrix <- cor(personality_data, use = "pairwise.complete.obs")
  
  # Create nice labels for traits
  trait_labels <- c(
    "openness" = "Open",
    "conscientiousness" = "Consc",
    "extraversion" = "Extra",
    "agreeableness" = "Agree",
    "neuroticism" = "Neuro",
    "risk_tolerance" = "Risk",
    "tech_aptitude" = "Tech",
    "security_awareness" = "SecAw"
  )
  
  # Plot correlation matrix
  corrplot::corrplot(
    cor_matrix,
    method = "circle",
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.7,
    col = corrplot::COL2("RdBu", 10),
    diag = FALSE,
    title = "Correlation Between Personality Traits",
    mar = c(0, 0, 1, 0)
  )
}

#' Calculate Personality Profiles by Factor
#'
#' Calculates the average personality profile for each factor.
#'
#' @param personality Dataframe containing personality data
#' @param factor_loadings Dataframe of factor loadings
#' @return A dataframe of personality profiles by factor
calculate_personality_profiles <- function(personality, factor_loadings) {
  # Merge personality with factor loadings
  merged_data <- merge(personality, factor_loadings, by.x = "participant_id", by.y = "row.names")
  
  # Determine the defining factor for each participant
  merged_data$defining_factor <- merged_data$`Defining Factor`
  
  # Filter out participants with no defining factor
  merged_data <- merged_data[merged_data$defining_factor != "None", ]
  
  # Select personality traits
  personality_traits <- c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism",
                         "risk_tolerance", "tech_aptitude", "security_awareness")
  
  # Calculate average personality profile for each factor
  profiles <- aggregate(merged_data[, personality_traits], 
                       by = list(Factor = merged_data$defining_factor), 
                       FUN = mean, 
                       na.rm = TRUE)
  
  return(profiles)
}

#' Plot Personality Profiles by Factor
#'
#' Creates a radar chart of personality profiles by factor.
#'
#' @param personality Dataframe containing personality data
#' @param factor_loadings Dataframe of factor loadings
#' @return A ggplot object
plot_personality_profiles <- function(personality, factor_loadings) {
  # Calculate personality profiles by factor
  profiles <- calculate_personality_profiles(personality, factor_loadings)
  
  # Reshape data for plotting
  plot_data <- reshape2::melt(
    profiles,
    id.vars = "Factor",
    variable.name = "Trait",
    value.name = "Score"
  )
  
  # Create nice labels for traits
  trait_labels <- c(
    "openness" = "Openness",
    "conscientiousness" = "Conscientiousness",
    "extraversion" = "Extraversion",
    "agreeableness" = "Agreeableness",
    "neuroticism" = "Neuroticism",
    "risk_tolerance" = "Risk Tolerance",
    "tech_aptitude" = "Tech Aptitude",
    "security_awareness" = "Security Awareness"
  )
  plot_data$Trait <- factor(plot_data$Trait, levels = names(trait_labels))
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Trait, y = Score, color = Factor, group = Factor)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Personality Profiles by Factor",
      x = "Personality Trait",
      y = "Average Score"
    ) +
    ggplot2::scale_x_discrete(labels = trait_labels) +
    ggplot2::scale_color_brewer(palette = "Set1")
  
  return(p)
}
