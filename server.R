library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(waiter)
library(DT)
library(plotly)
library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
library(rmarkdown)
library(dplyr)
library(logger)

# Source helper scripts if not already done
source("global.R")
source("R/weighting_params.R")
source("R/ui_helpers.R")
source("R/utils.R")
source("R/error_handling.R")
source("R/visualizations.R")
source("R/export.R")
source("R/secrets.R")
source("R/computation_engine.R")

# Ensure required directories exist
ensure_files_exist()

# Application configuration (may be augmented by config.yml values)
config <- list(
  app_title = "Survey Weighting Suite",
  version = "2.14.0",
  max_upload_size = 100 * 1024^2, # 100MB
  supported_formats = c("csv", "xlsx", "sav", "rds"),
  theme = "sunset",
  debug_mode = FALSE
)

ui <- dashboardPage(
  dashboardHeader(title = "Weighting Suite"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    tabItems(
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Data Import",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fileInput("data_file", "Choose Data File"),
            DTOutput("data_preview")
          )
        )
      ),
      
      tabItem(tabName = "weighting",
        fluidRow(
          box(
            title = "Weight Configuration",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("weight_controls")
          )
        )
      ),
      
      tabItem(tabName = "diagnostics",
        fluidRow(
          box(
            title = "Weight Diagnostics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("diagnostics_plot")
          )
        )
      ),
      
      tabItem(tabName = "settings",
        fluidRow(
          box(
            title = "Data Preview",
            status = "primary",
            width = 12,
            DTOutput("data_preview")
          )
        )
      ),
      
      tabItem("quality",
        fluidRow(
          box(
            title = "Data Quality Checks",
            status = "primary",
            width = 12,
            verbatimTextOutput("quality_summary")
          )
        )
      ),
      
      tabItem("variables",
        fluidRow(
          box(
            title = "Variable Selection",
            status = "primary",
            width = 12,
            selectizeInput("selected_vars", "Select Variables",
                         choices = NULL, multiple = TRUE)
          )
        )
      ),
      
      # Weighting tabs
      tabItem("method",
        fluidRow(
          box(
            title = "Weighting Method",
            status = "primary",
            width = 12,
            selectInput("weight_method", "Select Method",
              choices = c(
                "Post-stratification" = "post_stratification",
                "IPW" = "ipw",
                "Calibration" = "calibration",
                "RAKE" = "rake"
              )
            )
          )
        )
      ),
      
      tabItem("parameters",
        fluidRow(
          box(
            title = "Method Parameters",
            status = "primary",
            width = 12,
            uiOutput("method_parameters")
          )
        )
      ),
      
      tabItem("execute",
        fluidRow(
          box(
            title = "Execution Controls",
            status = "primary",
            width = 12,
            actionButton("compute_weights", "Compute Weights",
                        class = "btn-primary"),
            verbatimTextOutput("computation_status")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize computation engine
  engine <- ComputationEngine$new()
  
  # Unified application state
  state <- reactiveValues(
    data = NULL,
    weights = NULL,
    diagnostics = NULL,
    error_log = character(),
    session_info = list(),
    computation_log = list(),
    computation_status = "idle"
  )
  
  # Data Loading Observer
  observeEvent(input$data_file, {
    # Wrap waiter calls in tryCatch
    tryCatch({
      show_waiter(
        html = spin_flower(),
        color = getOption("waiter.color", "rgba(0,0,0,0.7)")
      )
      
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
      
      data <- load_data(file_path, file_ext,
                       header = input$header,
                       encoding = input$encoding)
      
      # Update state
      state$data <- data
      state$data_summary <- generate_data_summary(data)
      
      # Update UI elements
      updateSelectInput(session, "variables",
                       choices = names(data))
      
      showNotification("Data loaded successfully",
                      type = "success")
      
    }, error = function(e) {
      log_error_enhanced(e, "Data loading failed")
      showNotification(
        paste("Error:", e$message),
        type = "error"
      )
    }, finally = {
      tryCatch(hide_waiter(), error = function(e) NULL)
    })
  })
  
  # Weight Computation Observer
  observeEvent(input$compute_weights, {
    req(state$data)
    
    # Collect parameters
    params <- collect_parameters(input, session)
    
    # Execute weighting using engine
    results <- engine$execute_weighting(
      data = state$data,
      method = input$weight_method,
      params = params
    )
    
    if (!is.null(results)) {
      state$weights <- results$weights
      state$diagnostics <- results$diagnostics
      state$computation_status <- "complete"
      
      # Update visualizations
      update_visualizations()
    }
  })
  
  # Dynamic UI Updates
  observe({
    req(input$weight_method)
    output$method_parameters <- renderUI({
      generate_parameter_ui(input$weight_method)
    })
  })
  
  # Theme switching observers
  observeEvent(input$theme_select, {
    updateSelectInput(session, "theme", selected = input$theme_select)
    theme_class <- paste0(input$theme_select, "-theme")
    runjs(sprintf("document.getElementById('main-content').className = 'fruit-theme %s';", theme_class))
  })
  
  observeEvent(input$theme, {
    updateSelectInput(session, "theme_select", selected = input$theme)
    theme_class <- paste0(input$theme, "-theme")
    runjs(sprintf("document.getElementById('main-content').className = 'fruit-theme %s';", theme_class))
  })

  # Render Outputs
  output$data_preview <- renderDT({
    req(state$data)
    datatable(state$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_summary <- renderPrint({
    req(state$data_summary)
    state$data_summary
  })
  
  output$data_status_box <- renderInfoBox({
    infoBox(
      "Data Status",
      if(is.null(state$data)) "No data" else "Data loaded",
      icon = icon("database"),
      color = if(is.null(state$data)) "red" else "green"
    )
  })
  
  output$method_status_box <- renderInfoBox({
    infoBox(
      "Method",
      input$weight_method %||% "None selected",
      icon = icon("cogs"),
      color = if(is.null(input$weight_method)) "yellow" else "blue"
    )
  })
  
  # Diagnostic Plots
  output$weight_distribution <- renderPlotly({
    req(state$weights)
    create_weight_distribution_plot(state$weights)
  })
  
  output$convergence_plot <- renderPlotly({
    req(state$diagnostics)
    create_convergence_plot(state$diagnostics)
  })
  
  # Export Handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weighted_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format)
    },
    content = function(file) {
      export_results(
        data = state$data,
        weights = state$weights,
        diagnostics = state$diagnostics,
        format = input$export_format,
        include_diagnostics = input$include_diagnostics,
        filename = file
      )
    }
  )
  
  # Data Quality Output
  output$quality_summary <- renderPrint({
    req(state$data)
    list(
      "Missing Values" = colSums(is.na(state$data)),
      "Variable Types" = sapply(state$data, class),
      "Unique Values" = sapply(state$data, function(x) length(unique(x))),
      "Sample Values" = sapply(state$data, function(x) head(unique(x), 3))
    )
  })
  
  # Update variable selection choices when data changes
  observe({
    req(state$data)
    updateSelectizeInput(session, "selected_vars",
                        choices = names(state$data))
  })
  
  # Method parameter UI updates
  observe({
    req(input$weight_method, state$data)
    output$method_parameters <- renderUI({
      generate_parameter_ui(input$weight_method)
    })
    updateMethodUI(session, state$data, input$weight_method)
  })
  
  # Computation status
  output$computation_status <- renderPrint({
    req(state$computation_status)
    paste("Status:", state$computation_status)
  })
  
  # Session cleanup
  session$onSessionEnded(function() {
    cleanup_session(state)
  })
}

library(R6)
ComputationEngine <- R6Class("ComputationEngine",
  public = list(
    initialize = function() {
      # Initialize your computation engine
    },
    execute_weighting = function(data, method, params) {
      # Implement weighting computation logic
    }
  )
)