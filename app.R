library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(psych)
library(qmethod)
library(reshape2)
library(gridExtra)
library(markdown)

# Source helper scripts
source("scripts/qmethod_analysis.R")
source("scripts/demographic_analysis.R")
source("scripts/personality_analysis.R")
source("scripts/correlation_analysis.R")
source("scripts/report_generation.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Q-Method"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Import", tabName = "data_import", icon = icon("file-import")),
      menuItem("Q-Method Analysis", tabName = "qmethod_analysis", icon = icon("chart-bar")),
      menuItem("Demographic Analysis", tabName = "demographic_analysis", icon = icon("users")),
      menuItem("Personality Analysis", tabName = "personality_analysis", icon = icon("brain")),
      menuItem("Correlation Analysis", tabName = "correlation_analysis", icon = icon("link")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Welcome to the Q-Method Cybersecurity Study Analysis Application",
                  width = 12,
                  includeMarkdown("README.md")
                )
              )
      ),
      
      # Data Import tab
      tabItem(tabName = "data_import",
              fluidRow(
                box(
                  title = "Import Data",
                  width = 12,
                  p("Import your data files here. You can either use the sample data files or upload your own files."),
                  checkboxInput("use_sample_data", "Use sample data", value = TRUE),
                  conditionalPanel(
                    condition = "!input.use_sample_data",
                    fileInput("statements_file", "Q Statements CSV File", accept = c("text/csv", ".csv")),
                    fileInput("qsort_file", "Q-Sort Data CSV File", accept = c("text/csv", ".csv")),
                    fileInput("demographics_file", "Demographics CSV File (optional)", accept = c("text/csv", ".csv")),
                    fileInput("personality_file", "Personality Traits CSV File (optional)", accept = c("text/csv", ".csv"))
                  ),
                  actionButton("import_data", "Import Data", class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Data Preview",
                  width = 12,
                  tabsetPanel(
                    id = "data_preview_tabs",
                    tabPanel("Q Statements", DTOutput("statements_table")),
                    tabPanel("Q-Sort Data", DTOutput("qsort_table")),
                    tabPanel("Demographics", DTOutput("demographics_table")),
                    tabPanel("Personality Traits", DTOutput("personality_table"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Data Consistency Check",
                  width = 12,
                  verbatimTextOutput("data_consistency_check")
                )
              )
      ),
      
      # Q-Method Analysis tab
      tabItem(tabName = "qmethod_analysis",
              fluidRow(
                box(
                  title = "Q-Method Analysis Settings",
                  width = 4,
                  numericInput("num_factors", "Number of Factors to Extract", value = 3, min = 1, max = 8),
                  selectInput("rotation_method", "Rotation Method", 
                              choices = c("Varimax" = "varimax", "None" = "none"), 
                              selected = "varimax"),
                  actionButton("run_qmethod_analysis", "Run Analysis", class = "btn-primary")
                ),
                box(
                  title = "Factor Extraction Summary",
                  width = 8,
                  verbatimTextOutput("factor_extraction_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Factor Loadings",
                  width = 6,
                  DTOutput("factor_loadings_table")
                ),
                box(
                  title = "Factor Loadings Plot",
                  width = 6,
                  plotOutput("factor_loadings_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Factor Scores",
                  width = 12,
                  tabsetPanel(
                    id = "factor_scores_tabs",
                    tabPanel("Table", DTOutput("factor_scores_table")),
                    tabPanel("Plot", plotOutput("factor_scores_plot"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Consensus and Distinguishing Statements",
                  width = 12,
                  tabsetPanel(
                    id = "consensus_tabs",
                    tabPanel("Consensus Statements", DTOutput("consensus_statements_table")),
                    tabPanel("Distinguishing Statements", DTOutput("distinguishing_statements_table"))
                  )
                )
              )
      ),
      
      # Demographic Analysis tab
      tabItem(tabName = "demographic_analysis",
              fluidRow(
                box(
                  title = "Demographic Summary",
                  width = 12,
                  tabsetPanel(
                    id = "demographic_summary_tabs",
                    tabPanel("Age", plotOutput("age_summary_plot")),
                    tabPanel("Gender", plotOutput("gender_summary_plot")),
                    tabPanel("Education", plotOutput("education_summary_plot")),
                    tabPanel("Job Role", plotOutput("job_role_summary_plot")),
                    tabPanel("Experience", plotOutput("experience_summary_plot")),
                    tabPanel("Industry", plotOutput("industry_summary_plot")),
                    tabPanel("Organization Size", plotOutput("org_size_summary_plot")),
                    tabPanel("Security Training", plotOutput("security_training_summary_plot"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Demographics by Factor",
                  width = 12,
                  selectInput("demographic_factor_var", "Demographic Variable",
                              choices = c("Age", "Gender", "Education", "Job Role", "Experience", 
                                          "Industry", "Organization Size", "Security Training"),
                              selected = "Job Role"),
                  plotOutput("demographic_by_factor_plot")
                )
              )
      ),
      
      # Personality Analysis tab
      tabItem(tabName = "personality_analysis",
              fluidRow(
                box(
                  title = "Personality Traits Summary",
                  width = 12,
                  plotOutput("personality_summary_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Personality Traits by Factor",
                  width = 12,
                  selectInput("personality_factor_var", "Personality Trait",
                              choices = c("Openness", "Conscientiousness", "Extraversion", 
                                          "Agreeableness", "Neuroticism", "Risk Tolerance",
                                          "Tech Aptitude", "Security Awareness"),
                              selected = "Security Awareness"),
                  plotOutput("personality_by_factor_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Personality Traits Correlation",
                  width = 12,
                  plotOutput("personality_correlation_plot")
                )
              )
      ),
      
      # Correlation Analysis tab
      tabItem(tabName = "correlation_analysis",
              fluidRow(
                box(
                  title = "Correlation Analysis Settings",
                  width = 4,
                  selectInput("correlation_type", "Correlation Type",
                              choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                              selected = "spearman"),
                  checkboxGroupInput("correlation_variables", "Variables to Include",
                                     choices = c("Factor Loadings", "Demographics", "Personality Traits"),
                                     selected = c("Factor Loadings", "Demographics", "Personality Traits")),
                  actionButton("run_correlation_analysis", "Run Analysis", class = "btn-primary")
                ),
                box(
                  title = "Correlation Matrix",
                  width = 8,
                  plotOutput("correlation_matrix_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Detailed Correlations",
                  width = 12,
                  DTOutput("correlation_table")
                )
              )
      ),
      
      # Reports tab
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Generate Reports",
                  width = 12,
                  checkboxGroupInput("report_sections", "Sections to Include",
                                     choices = c("Q-Method Analysis", "Demographic Analysis", 
                                                "Personality Analysis", "Correlation Analysis"),
                                     selected = c("Q-Method Analysis", "Demographic Analysis", 
                                                 "Personality Analysis", "Correlation Analysis")),
                  downloadButton("download_report", "Generate and Download Report")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  data_store <- reactiveValues(
    statements = NULL,
    qsort_data = NULL,
    demographics = NULL,
    personality = NULL,
    qmethod_results = NULL,
    data_loaded = FALSE
  )
  
  # Import data
  observeEvent(input$import_data, {
    if (input$use_sample_data) {
      # Load sample data
      data_store$statements <- read.csv("data/sample/statements.csv")
      data_store$qsort_data <- read.csv("data/sample/qsort_data.csv")
      data_store$demographics <- read.csv("data/sample/demographics.csv")
      data_store$personality <- read.csv("data/sample/personality.csv")
    } else {
      # Load user-provided data
      req(input$statements_file)
      req(input$qsort_file)
      
      data_store$statements <- read.csv(input$statements_file$datapath)
      data_store$qsort_data <- read.csv(input$qsort_file$datapath)
      
      if (!is.null(input$demographics_file)) {
        data_store$demographics <- read.csv(input$demographics_file$datapath)
      }
      
      if (!is.null(input$personality_file)) {
        data_store$personality <- read.csv(input$personality_file$datapath)
      }
    }
    
    data_store$data_loaded <- TRUE
  })
  
  # Data preview tables
  output$statements_table <- renderDT({
    req(data_store$statements)
    datatable(data_store$statements, options = list(pageLength = 10))
  })
  
  output$qsort_table <- renderDT({
    req(data_store$qsort_data)
    datatable(data_store$qsort_data, options = list(pageLength = 10))
  })
  
  output$demographics_table <- renderDT({
    req(data_store$demographics)
    datatable(data_store$demographics, options = list(pageLength = 10))
  })
  
  output$personality_table <- renderDT({
    req(data_store$personality)
    datatable(data_store$personality, options = list(pageLength = 10))
  })
  
  # Data consistency check
  output$data_consistency_check <- renderText({
    req(data_store$data_loaded)
    
    # Check if all required data is present
    if (is.null(data_store$statements) || is.null(data_store$qsort_data)) {
      return("Error: Missing required data files (statements or Q-sort data).")
    }
    
    # Check if statements data has required columns
    if (!all(c("id", "statement") %in% colnames(data_store$statements))) {
      return("Error: Statements data must have 'id' and 'statement' columns.")
    }
    
    # Check if qsort data has required columns
    if (!all(c("participant_id", "statement_id", "score") %in% colnames(data_store$qsort_data))) {
      return("Error: Q-sort data must have 'participant_id', 'statement_id', and 'score' columns.")
    }
    
    # Check if demographics data has required columns (if provided)
    if (!is.null(data_store$demographics) && 
        !("participant_id" %in% colnames(data_store$demographics))) {
      return("Error: Demographics data must have a 'participant_id' column.")
    }
    
    # Check if personality data has required columns (if provided)
    if (!is.null(data_store$personality) && 
        !("participant_id" %in% colnames(data_store$personality))) {
      return("Error: Personality data must have a 'participant_id' column.")
    }
    
    # Check if all statement IDs in qsort data exist in statements data
    qsort_statement_ids <- unique(data_store$qsort_data$statement_id)
    statement_ids <- data_store$statements$id
    missing_ids <- setdiff(qsort_statement_ids, statement_ids)
    
    if (length(missing_ids) > 0) {
      return(paste("Error: The following statement IDs in Q-sort data do not exist in statements data:",
                   paste(missing_ids, collapse = ", ")))
    }
    
    # Check if all participant IDs in demographics and personality data exist in qsort data
    qsort_participant_ids <- unique(data_store$qsort_data$participant_id)
    
    if (!is.null(data_store$demographics)) {
      demo_participant_ids <- data_store$demographics$participant_id
      missing_demo_ids <- setdiff(demo_participant_ids, qsort_participant_ids)
      
      if (length(missing_demo_ids) > 0) {
        return(paste("Error: The following participant IDs in demographics data do not exist in Q-sort data:",
                     paste(missing_demo_ids, collapse = ", ")))
      }
    }
    
    if (!is.null(data_store$personality)) {
      pers_participant_ids <- data_store$personality$participant_id
      missing_pers_ids <- setdiff(pers_participant_ids, qsort_participant_ids)
      
      if (length(missing_pers_ids) > 0) {
        return(paste("Error: The following participant IDs in personality data do not exist in Q-sort data:",
                     paste(missing_pers_ids, collapse = ", ")))
      }
    }
    
    return("Data consistency check passed.")
  })
  
  # Q-Method Analysis
  observeEvent(input$run_qmethod_analysis, {
    req(data_store$data_loaded)
    
    # Run Q-method analysis
    data_store$qmethod_results <- run_qmethod_analysis(
      data_store$statements,
      data_store$qsort_data,
      num_factors = input$num_factors,
      rotation = input$rotation_method
    )
  })
  
  # Factor extraction summary
  output$factor_extraction_summary <- renderPrint({
    req(data_store$qmethod_results)
    data_store$qmethod_results$summary
  })
  
  # Factor loadings table
  output$factor_loadings_table <- renderDT({
    req(data_store$qmethod_results)
    datatable(data_store$qmethod_results$factor_loadings, 
              options = list(pageLength = 10),
              rownames = TRUE)
  })
  
  # Factor loadings plot
  output$factor_loadings_plot <- renderPlot({
    req(data_store$qmethod_results)
    plot_factor_loadings(data_store$qmethod_results$factor_loadings)
  })
  
  # Factor scores table
  output$factor_scores_table <- renderDT({
    req(data_store$qmethod_results)
    datatable(data_store$qmethod_results$factor_scores, 
              options = list(pageLength = 10),
              rownames = TRUE)
  })
  
  # Factor scores plot
  output$factor_scores_plot <- renderPlot({
    req(data_store$qmethod_results)
    plot_factor_scores(data_store$qmethod_results$factor_scores, data_store$statements)
  })
  
  # Consensus statements table
  output$consensus_statements_table <- renderDT({
    req(data_store$qmethod_results)
    datatable(data_store$qmethod_results$consensus_statements, 
              options = list(pageLength = 10),
              rownames = TRUE)
  })
  
  # Distinguishing statements table
  output$distinguishing_statements_table <- renderDT({
    req(data_store$qmethod_results)
    datatable(data_store$qmethod_results$distinguishing_statements, 
              options = list(pageLength = 10),
              rownames = TRUE)
  })
  
  # Demographic Analysis
  
  # Age summary plot
  output$age_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_age_summary(data_store$demographics)
  })
  
  # Gender summary plot
  output$gender_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_gender_summary(data_store$demographics)
  })
  
  # Education summary plot
  output$education_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_education_summary(data_store$demographics)
  })
  
  # Job role summary plot
  output$job_role_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_job_role_summary(data_store$demographics)
  })
  
  # Experience summary plot
  output$experience_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_experience_summary(data_store$demographics)
  })
  
  # Industry summary plot
  output$industry_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_industry_summary(data_store$demographics)
  })
  
  # Organization size summary plot
  output$org_size_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_org_size_summary(data_store$demographics)
  })
  
  # Security training summary plot
  output$security_training_summary_plot <- renderPlot({
    req(data_store$demographics)
    plot_security_training_summary(data_store$demographics)
  })
  
  # Demographics by factor plot
  output$demographic_by_factor_plot <- renderPlot({
    req(data_store$demographics, data_store$qmethod_results)
    
    # Get the selected demographic variable
    demographic_var <- switch(input$demographic_factor_var,
                             "Age" = "age",
                             "Gender" = "gender",
                             "Education" = "education",
                             "Job Role" = "job_role",
                             "Experience" = "years_experience",
                             "Industry" = "industry",
                             "Organization Size" = "organization_size",
                             "Security Training" = "security_training")
    
    plot_demographic_by_factor(data_store$demographics, 
                              data_store$qmethod_results$factor_loadings,
                              demographic_var,
                              input$demographic_factor_var)
  })
  
  # Personality Analysis
  
  # Personality summary plot
  output$personality_summary_plot <- renderPlot({
    req(data_store$personality)
    plot_personality_summary(data_store$personality)
  })
  
  # Personality by factor plot
  output$personality_by_factor_plot <- renderPlot({
    req(data_store$personality, data_store$qmethod_results)
    
    # Get the selected personality trait
    personality_var <- switch(input$personality_factor_var,
                             "Openness" = "openness",
                             "Conscientiousness" = "conscientiousness",
                             "Extraversion" = "extraversion",
                             "Agreeableness" = "agreeableness",
                             "Neuroticism" = "neuroticism",
                             "Risk Tolerance" = "risk_tolerance",
                             "Tech Aptitude" = "tech_aptitude",
                             "Security Awareness" = "security_awareness")
    
    plot_personality_by_factor(data_store$personality, 
                              data_store$qmethod_results$factor_loadings,
                              personality_var,
                              input$personality_factor_var)
  })
  
  # Personality correlation plot
  output$personality_correlation_plot <- renderPlot({
    req(data_store$personality)
    plot_personality_correlation(data_store$personality)
  })
  
  # Correlation Analysis
  
  # Run correlation analysis
  observeEvent(input$run_correlation_analysis, {
    req(data_store$qmethod_results)
    
    # Prepare data for correlation analysis
    correlation_data <- prepare_correlation_data(
      data_store$qmethod_results$factor_loadings,
      data_store$demographics,
      data_store$personality,
      include_factor_loadings = "Factor Loadings" %in% input$correlation_variables,
      include_demographics = "Demographics" %in% input$correlation_variables,
      include_personality = "Personality Traits" %in% input$correlation_variables
    )
    
    # Run correlation analysis
    correlation_results <- run_correlation_analysis(
      correlation_data,
      method = input$correlation_type
    )
    
    # Store results
    data_store$correlation_results <- correlation_results
  })
  
  # Correlation matrix plot
  output$correlation_matrix_plot <- renderPlot({
    req(data_store$correlation_results)
    plot_correlation_matrix(data_store$correlation_results$correlation_matrix)
  })
  
  # Correlation table
  output$correlation_table <- renderDT({
    req(data_store$correlation_results)
    datatable(data_store$correlation_results$correlation_table,
              options = list(pageLength = 10),
              rownames = TRUE)
  })
  
  # Report generation
  output$download_report <- downloadHandler(
    filename = function() {
      paste("qmethod-cybersecurity-report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Generate report
      generate_report(
        file,
        data_store$statements,
        data_store$qsort_data,
        data_store$demographics,
        data_store$personality,
        data_store$qmethod_results,
        data_store$correlation_results,
        include_qmethod = "Q-Method Analysis" %in% input$report_sections,
        include_demographics = "Demographic Analysis" %in% input$report_sections,
        include_personality = "Personality Analysis" %in% input$report_sections,
        include_correlation = "Correlation Analysis" %in% input$report_sections
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
