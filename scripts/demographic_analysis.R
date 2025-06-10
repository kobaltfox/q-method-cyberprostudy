# Demographic Analysis Functions

#' Plot Age Summary
#'
#' Creates a histogram of participant ages.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_age_summary <- function(demographics) {
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Age Distribution of Participants",
      x = "Age",
      y = "Count"
    )
  
  return(p)
}

#' Plot Gender Summary
#'
#' Creates a bar chart of participant genders.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_gender_summary <- function(demographics) {
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = gender, fill = gender)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Gender Distribution of Participants",
      x = "Gender",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")
  
  return(p)
}

#' Plot Education Summary
#'
#' Creates a bar chart of participant education levels.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_education_summary <- function(demographics) {
  # Order education levels
  education_levels <- c("High School", "Bachelors", "Masters", "PhD")
  demographics$education <- factor(demographics$education, levels = education_levels)
  
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = education, fill = education)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Education Distribution of Participants",
      x = "Education Level",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3")
  
  return(p)
}

#' Plot Job Role Summary
#'
#' Creates a bar chart of participant job roles.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_job_role_summary <- function(demographics) {
  # Count job roles
  job_counts <- table(demographics$job_role)
  job_df <- data.frame(
    job_role = names(job_counts),
    count = as.numeric(job_counts)
  )
  
  # Order by count
  job_df <- job_df[order(-job_df$count), ]
  job_df$job_role <- factor(job_df$job_role, levels = job_df$job_role)
  
  p <- ggplot2::ggplot(job_df, ggplot2::aes(x = job_role, y = count, fill = job_role)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Job Role Distribution of Participants",
      x = "Job Role",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3")
  
  return(p)
}

#' Plot Experience Summary
#'
#' Creates a histogram of participant years of experience.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_experience_summary <- function(demographics) {
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = years_experience)) +
    ggplot2::geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Years of Experience Distribution",
      x = "Years of Experience",
      y = "Count"
    )
  
  return(p)
}

#' Plot Industry Summary
#'
#' Creates a bar chart of participant industries.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_industry_summary <- function(demographics) {
  # Count industries
  industry_counts <- table(demographics$industry)
  industry_df <- data.frame(
    industry = names(industry_counts),
    count = as.numeric(industry_counts)
  )
  
  # Order by count
  industry_df <- industry_df[order(-industry_df$count), ]
  industry_df$industry <- factor(industry_df$industry, levels = industry_df$industry)
  
  p <- ggplot2::ggplot(industry_df, ggplot2::aes(x = industry, y = count, fill = industry)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Industry Distribution of Participants",
      x = "Industry",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3")
  
  return(p)
}

#' Plot Organization Size Summary
#'
#' Creates a bar chart of participant organization sizes.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_org_size_summary <- function(demographics) {
  # Order organization sizes
  org_sizes <- c("Small", "Medium", "Large")
  demographics$organization_size <- factor(demographics$organization_size, levels = org_sizes)
  
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = organization_size, fill = organization_size)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Organization Size Distribution of Participants",
      x = "Organization Size",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")
  
  return(p)
}

#' Plot Security Training Summary
#'
#' Creates a bar chart of participant security training levels.
#'
#' @param demographics Dataframe containing demographic data
#' @return A ggplot object
plot_security_training_summary <- function(demographics) {
  # Order training levels
  training_levels <- c("None", "Basic", "Intermediate", "Advanced")
  demographics$security_training <- factor(demographics$security_training, levels = training_levels)
  
  p <- ggplot2::ggplot(demographics, ggplot2::aes(x = security_training, fill = security_training)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Security Training Distribution of Participants",
      x = "Security Training Level",
      y = "Count"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set1")
  
  return(p)
}

#' Plot Demographic by Factor
#'
#' Creates a plot showing the relationship between a demographic variable and Q-method factors.
#'
#' @param demographics Dataframe containing demographic data
#' @param factor_loadings Dataframe of factor loadings
#' @param demographic_var Name of the demographic variable to plot
#' @param demographic_label Label for the demographic variable
#' @return A ggplot object
plot_demographic_by_factor <- function(demographics, factor_loadings, demographic_var, demographic_label) {
  # Merge demographics with factor loadings
  merged_data <- merge(demographics, factor_loadings, by.x = "participant_id", by.y = "row.names")
  
  # Determine the type of demographic variable
  if (is.numeric(merged_data[[demographic_var]])) {
    # For numeric variables (age, years_experience), create a scatter plot
    plot_data <- reshape2::melt(
      merged_data,
      id.vars = c("participant_id", demographic_var),
      measure.vars = grep("^Factor", colnames(merged_data), value = TRUE),
      variable.name = "Factor",
      value.name = "Loading"
    )
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = demographic_var, y = "Loading", color = "Factor")) +
      ggplot2::geom_point(size = 3, alpha = 0.7) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste(demographic_label, "vs. Factor Loadings"),
        x = demographic_label,
        y = "Factor Loading"
      ) +
      ggplot2::scale_color_brewer(palette = "Set1")
    
  } else {
    # For categorical variables, create a boxplot
    plot_data <- reshape2::melt(
      merged_data,
      id.vars = c("participant_id", demographic_var),
      measure.vars = grep("^Factor", colnames(merged_data), value = TRUE),
      variable.name = "Factor",
      value.name = "Loading"
    )
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = demographic_var, y = "Loading", fill = "Factor")) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::labs(
        title = paste(demographic_label, "vs. Factor Loadings"),
        x = demographic_label,
        y = "Factor Loading"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set1")
  }
  
  return(p)
}
