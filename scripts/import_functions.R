# Import Functions for Q-Method Cybersecurity Study
# This script contains functions for importing data from CSV files

library(readr)
library(dplyr)
library(tidyr)

#' Import Q statements
#'
#' @param file_path Path to the CSV file containing Q statements
#' @return A data frame containing Q statements
import_statements <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Import statements
  statements <- read_csv(file_path, col_types = cols(
    id = col_integer(),
    statement = col_character()
  ))
  
  # Check if required columns exist
  required_cols <- c("id", "statement")
  missing_cols <- setdiff(required_cols, colnames(statements))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(statements)
}

#' Import Q-sort data
#'
#' @param file_path Path to the CSV file containing Q-sort data
#' @return A data frame containing Q-sort data
import_qsort_data <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Import Q-sort data
  qsort_data <- read_csv(file_path, col_types = cols(
    participant_id = col_character(),
    statement_id = col_integer(),
    score = col_integer()
  ))
  
  # Check if required columns exist
  required_cols <- c("participant_id", "statement_id", "score")
  missing_cols <- setdiff(required_cols, colnames(qsort_data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(qsort_data)
}

#' Import demographic data
#'
#' @param file_path Path to the CSV file containing demographic data
#' @return A data frame containing demographic data
import_demographics <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Import demographic data
  demographics <- read_csv(file_path)
  
  # Check if required columns exist
  required_cols <- c("participant_id")
  missing_cols <- setdiff(required_cols, colnames(demographics))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(demographics)
}

#' Import personality trait data
#'
#' @param file_path Path to the CSV file containing personality trait data
#' @return A data frame containing personality trait data
import_personality <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Import personality trait data
  personality <- read_csv(file_path)
  
  # Check if required columns exist
  required_cols <- c("participant_id")
  missing_cols <- setdiff(required_cols, colnames(personality))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  return(personality)
}

#' Import all data
#'
#' @param statements_path Path to the CSV file containing Q statements
#' @param qsort_data_path Path to the CSV file containing Q-sort data
#' @param demographics_path Path to the CSV file containing demographic data (optional)
#' @param personality_path Path to the CSV file containing personality trait data (optional)
#' @return A list containing all imported data
import_all_data <- function(statements_path, qsort_data_path, 
                          demographics_path = NULL, personality_path = NULL) {
  # Import statements
  statements <- import_statements(statements_path)
  
  # Import Q-sort data
  qsort_data <- import_qsort_data(qsort_data_path)
  
  # Create result list
  result <- list(
    statements = statements,
    qsort_data = qsort_data
  )
  
  # Import demographic data if path is provided
  if (!is.null(demographics_path)) {
    demographics <- import_demographics(demographics_path)
    result$demographics <- demographics
  }
  
  # Import personality trait data if path is provided
  if (!is.null(personality_path)) {
    personality <- import_personality(personality_path)
    result$personality <- personality
  }
  
  return(result)
}

#' Check data consistency
#'
#' @param data List containing all imported data
#' @return A list containing the results of the data consistency check
check_data_consistency <- function(data) {
  # Initialize results
  results <- list(
    consistent = TRUE,
    messages = character()
  )
  
  # Check if statements and qsort_data are available
  if (is.null(data$statements) || is.null(data$qsort_data)) {
    results$consistent <- FALSE
    results$messages <- c(results$messages, "Statements or Q-sort data missing")
    return(results)
  }
  
  # Check if all statement IDs in qsort_data exist in statements
  statement_ids_in_qsort <- unique(data$qsort_data$statement_id)
  statement_ids_in_statements <- data$statements$id
  
  missing_statement_ids <- setdiff(statement_ids_in_qsort, statement_ids_in_statements)
  
  if (length(missing_statement_ids) > 0) {
    results$consistent <- FALSE
    results$messages <- c(results$messages, 
                         paste("Statement IDs in Q-sort data not found in statements:", 
                              paste(missing_statement_ids, collapse = ", ")))
  }
  
  # Check if demographics is available
  if (!is.null(data$demographics)) {
    # Check if all participant IDs in qsort_data exist in demographics
    participant_ids_in_qsort <- unique(data$qsort_data$participant_id)
    participant_ids_in_demographics <- data$demographics$participant_id
    
    missing_participant_ids <- setdiff(participant_ids_in_qsort, participant_ids_in_demographics)
    
    if (length(missing_participant_ids) > 0) {
      results$consistent <- FALSE
      results$messages <- c(results$messages, 
                           paste("Participant IDs in Q-sort data not found in demographics:", 
                                paste(missing_participant_ids, collapse = ", ")))
    }
  }
  
  # Check if personality is available
  if (!is.null(data$personality)) {
    # Check if all participant IDs in qsort_data exist in personality
    participant_ids_in_qsort <- unique(data$qsort_data$participant_id)
    participant_ids_in_personality <- data$personality$participant_id
    
    missing_participant_ids <- setdiff(participant_ids_in_qsort, participant_ids_in_personality)
    
    if (length(missing_participant_ids) > 0) {
      results$consistent <- FALSE
      results$messages <- c(results$messages, 
                           paste("Participant IDs in Q-sort data not found in personality:", 
                                paste(missing_participant_ids, collapse = ", ")))
    }
  }
  
  # Check if all participants have the same number of statements
  participant_statement_counts <- data$qsort_data %>%
    group_by(participant_id) %>%
    summarize(count = n(), .groups = "drop")
  
  if (length(unique(participant_statement_counts$count)) > 1) {
    results$consistent <- FALSE
    results$messages <- c(results$messages, 
                         "Not all participants have the same number of statements")
    
    # Add details about participant statement counts
    for (i in 1:nrow(participant_statement_counts)) {
      participant_id <- participant_statement_counts$participant_id[i]
      count <- participant_statement_counts$count[i]
      
      results$messages <- c(results$messages, 
                           paste(participant_id, "has", count, "statements"))
    }
  }
  
  # Check if all participants have valid scores
  score_range <- range(data$qsort_data$score)
  
  if (score_range[1] < -4 || score_range[2] > 4) {
    results$consistent <- FALSE
    results$messages <- c(results$messages, 
                         paste("Score range is outside expected range (-4 to 4):", 
                              score_range[1], "to", score_range[2]))
  }
  
  return(results)
}

#' Prepare data for analysis
#'
#' @param data List containing all imported data
#' @return A list containing the prepared data for analysis
prepare_data_for_analysis <- function(data) {
  # Check data consistency
  consistency_check <- check_data_consistency(data)
  
  if (!consistency_check$consistent) {
    warning(paste("Data consistency issues detected:", 
                 paste(consistency_check$messages, collapse = "; ")))
  }
  
  # Create a list of prepared data
  prepared_data <- list(
    statements = data$statements,
    qsort_data = data$qsort_data,
    demographics = data$demographics,
    personality = data$personality,
    consistency_check = consistency_check
  )
  
  return(prepared_data)
}
