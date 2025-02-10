# app.R
# Survey Weighting Suite - Unified Solution
# Version 1.0.0

# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
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
})

# Source component files
source("R/utils.R")
source("R/error_handling.R")
source("R/visualizations.R")
source("R/export.R")

# Configuration
config <- list(
  app_title = "Survey Weighting Suite",
  version = "1.0.0",
  max_upload_size = 100 * 1024^2, # 100MB
  supported_formats = c("csv", "xlsx", "sav", "rds"),
  theme = "sunset",
  debug_mode = FALSE
)

# Initialize application state
app_state <- reactiveValues(
  data = NULL,
  weights = NULL,
  diagnostics = NULL,
  error_log = character(),
  session_info = list(),
  computation_log = list()
)

# Custom CSS injection
css_path <- "www/styles.css"
if (file.exists(css_path)) {
  css_content <- includeCSS(css_path)
} else {
  warning("CSS file not found. Using default styling.")
}

# UI Definition
ui <- dashboardPage(
  header = dashboardHeader(
    title = config$app_title,
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      actionButton(
        "help_btn",
        label = "Help",
        icon = icon("question-circle"),
        class = "btn-info navbar-btn"
      )
    )
  ),
  
  sidebar = dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Data Management",
        tabName = "data",
        icon = icon("database"),
        startExpanded = TRUE,
        menuSubItem("Upload & Preview", tabName = "upload"),
        menuSubItem("Data Quality", tabName = "quality"),
        menuSubItem("Variable Selection", tabName = "variables")
      ),
      menuItem(
        "Weighting",
        tabName = "weighting",
        icon = icon("balance-scale"),
        startExpanded = FALSE,
        menuSubItem("Method Selection", tabName = "method"),
        menuSubItem("Parameters", tabName = "parameters"),
        menuSubItem("Execution", tabName = "execute")
      ),
      menuItem(
        "Diagnostics",
        tabName = "diagnostics",
        icon = icon("chart-line")
      ),
      menuItem(
        "Export",
        tabName = "export",
        icon = icon("download")
      ),
      menuItem(
        "Settings",
        tabName = "settings",
        icon = icon("cog")
      )
    ),
    
    # Conditional inputs based on selected tab
    conditionalPanel(
      condition = "input.sidebar_menu === 'upload'",
      fileInput(
        "data_file",
        "Upload Data File",
        accept = c(
          ".csv", ".xlsx", ".sav", ".rds",
          "text/csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/x-spss-sav"
        ),
        multiple = FALSE
      ),
      checkboxInput("header", "First Row as Header", TRUE),
      selectInput(
        "encoding",
        "File Encoding",
        choices = c("UTF-8", "ISO-8859-1", "UTF-16"),
        selected = "UTF-8"
      )
    )
  ),
  
  body = dashboardBody(
    useShinyjs(),
    use_waiter(),
    
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "custom.js")
    ),
    
    # Main content area
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          infoBox(
            "Data Status",
            textOutput("data_status"),
            icon = icon("database"),
            color = "purple",
            width = 4
          ),
          infoBox(
            "Selected Method",
            textOutput("method_status"),
            icon = icon("cogs"),
            color = "yellow",
            width = 4
          ),
          infoBox(
            "Analysis Status",
            textOutput("analysis_status"),
            icon = icon("chart-line"),
            color = "green",
            width = 4
          )
        ),
        fluidRow(
          box(
            title = "Quick Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("quick_summary")
          )
        )
      ),
      
      # Data Upload tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("data_summary")
          )
        )
      )
      
      # Additional tabs will be added in the next part...
    )
  )
)

# Server Logic and Integration
server <- function(input, output, session) {
  # Initialize session state
  session_state <- reactiveValues(
    computation_status = "idle",
    last_error = NULL,
    progress = 0,
    method_params = list(),
    validation_results = list()
  )

  # Computational Engine
  ComputationEngine <- R6Class("ComputationEngine",
    public = list(
      initialize = function() {
        private$reset_state()
      },
      
      execute_weighting = function(data, method, params) {
        private$validate_inputs(data, method, params)
        private$set_status("running")
        
        result <- tryCatch({
          switch(method,
            "post_stratification" = private$run_post_stratification(data, params),
            "raking" = private$run_raking(data, params),
            "calibration" = private$run_calibration(data, params),
            "ipw" = private$run_ipw(data, params),
            stop("Unknown weighting method")
          )
        },
        error = function(e) {
          private$handle_error(e)
          return(NULL)
        })
        
        if (!is.null(result)) {
          private$set_status("completed")
          private$log_completion(result)
        }
        
        result
      }
    ),
    
    private = list(
      status = "idle",
      error_log = list(),
      computation_log = list(),
      
      reset_state = function() {
        self$status <- "idle"
        self$error_log <- list()
        self$computation_log <- list()
      },
      
      validate_inputs = function(data, method, params) {
        validation <- validate_inputs(data, params)
        if (!validation$valid) {
          stop(paste("Validation failed:", paste(validation$errors, collapse = "; ")))
        }
      },
      
      run_post_stratification = function(data, params) {
        # Post-stratification implementation
        withProgress(message = 'Performing post-stratification', value = 0, {
          result <- list(
            weights = compute_post_strat_weights(data, params),
            diagnostics = compute_diagnostics(data, weights),
            convergence = check_convergence(weights, params)
          )
          
          incProgress(1)
          result
        })
      },
      
      run_raking = function(data, params) {
        # Raking implementation
        withProgress(message = 'Performing raking', value = 0, {
          weights <- numeric(nrow(data))
          max_iter <- params$max_iterations
          
          for (i in 1:max_iter) {
            incProgress(i/max_iter)
            weights_new <- update_raking_weights(data, weights, params)
            
            if (check_convergence(weights_new, weights, params$tolerance)) {
              break
            }
            weights <- weights_new
          }
          
          list(
            weights = weights,
            diagnostics = compute_diagnostics(data, weights),
            convergence = list(iterations = i, achieved = i < max_iter)
          )
        })
      },
      
      run_calibration = function(data, params) {
        # Calibration implementation
        withProgress(message = 'Performing calibration', value = 0, {
          result <- calibrate_weights(data, params)
          incProgress(1)
          result
        })
      },
      
      run_ipw = function(data, params) {
        # Inverse probability weighting implementation
        withProgress(message = 'Computing IPW', value = 0, {
          result <- compute_ipw_weights(data, params)
          incProgress(1)
          result
        })
      }
    )
  )
  
  # Initialize computation engine
  engine <- ComputationEngine$new()
  
  # Data Loading and Validation
  observeEvent(input$data_file, {
    show_waiter(
      html = spin_flower(),
      color = "#1a1a2e"
    )
    
    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
      
      data <- switch(file_ext,
        "csv" = read_csv(file_path),
        "xlsx" = read_xlsx(file_path),
        "sav" = read_sav(file_path),
        "rds" = readRDS(file_path),
        stop("Unsupported file format")
      )
      
      # Validate and clean data
      data <- clean_dataset(data)
      
      # Update app state
      app_state$data <- data
      app_state$data_summary <- generate_data_summary(data)
      
      # Update UI elements
      updateSelectInput(session, "weight_vars",
                       choices = names(data),
                       selected = NULL)
      
      # Show success notification
      showNotification("Data loaded successfully", type = "success")
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
    }, finally = {
      hide_waiter()
    })
  })
  
  # Method Selection and Parameter Updates
  observeEvent(input$weighting_method, {
    req(input$weighting_method)
    
    # Update parameter UI based on selected method
    method_params <- get_method_parameters(input$weighting_method)
    session_state$method_params <- method_params
    
    output$parameter_ui <- renderUI({
      generate_parameter_ui(method_params)
    })
  })
  
  # Weight Computation
  observeEvent(input$compute_weights, {
    req(app_state$data)
    
    # Collect parameters
    params <- collect_parameters(input, session_state$method_params)
    
    # Execute weighting
    results <- engine$execute_weighting(
      data = app_state$data,
      method = input$weighting_method,
      params = params
    )
    
    if (!is.null(results)) {
      app_state$weights <- results$weights
      app_state$diagnostics <- results$diagnostics
      
      # Update visualizations
      update_visualizations()
      
      # Enable export options
      shinyjs::enable("export_buttons")
    }
  })
  
  # Visualization Updates
  update_visualizations <- function() {
    req(app_state$data, app_state$weights)
    
    output$weight_distribution <- renderPlotly({
      create_weight_distribution_plot(app_state$weights)
    })
    
    output$diagnostic_plots <- renderPlotly({
      create_diagnostic_plots(app_state$diagnostics)
    })
    
    output$summary_stats <- renderTable({
      generate_summary_statistics(
        app_state$data,
        app_state$weights
      )
    })
  }
  
  # Export Handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weighted_data_", format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".", input$export_format)
    },
    content = function(file) {
      export_results(
        data = app_state$data,
        weights = app_state$weights,
        diagnostics = app_state$diagnostics,
        format = input$export_format,
        filename = file
      )
    }
  )
  
  # Error Handling
  observe({
    if (!is.null(session_state$last_error)) {
      showNotification(
        session_state$last_error$message,
        type = "error",
        duration = NULL
      )
    }
  })
  
  # Session Cleanup
  session$onSessionEnded(function() {
    # Clean up temporary files and reset state
    cleanup_session(app_state)
  })
}

# Additional UI Components
diagnosticsTab <- tabItem(
  tabName = "diagnostics",
  fluidRow(
    box(
      title = "Weight Distribution",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("weight_distribution")
    ),
    box(
      title = "Convergence Plot",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("convergence_plot")
    )
  ),
  fluidRow(
    box(
      title = "Summary Statistics",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      tableOutput("summary_stats")
    )
  )
)

exportTab <- tabItem(
  tabName = "export",
  fluidRow(
    box(
      title = "Export Options",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("export_format",
                 "Select Format",
                 choices = c("CSV" = "csv",
                           "Excel" = "xlsx",
                           "SPSS" = "sav",
                           "Text" = "txt")),
      checkboxInput("include_diagnostics",
                   "Include Diagnostics",
                   value = TRUE),
      downloadButton("download_data",
                    "Download Results",
                    class = "btn-primary"),
      downloadButton("download_report",
                    "Download Report",
                    class = "btn-info")
    )
  )
)

settingsTab <- tabItem(
  tabName = "settings",
  fluidRow(
    box(
      title = "Application Settings",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("theme",
                 "Select Theme",
                 choices = c("Sunset Dark" = "sunset",
                           "Ocean Dark" = "ocean",
                           "Forest Dark" = "forest")),
      numericInput("max_iterations",
                  "Maximum Iterations",
                  value = 100,
                  min = 10,
                  max = 1000),
      numericInput("convergence_threshold",
                  "Convergence Threshold",
                  value = 1e-6,
                  min = 1e-10,
                  max = 1e-2)
    )
  )
)

# install.R
# Installation and Dependency Management

# Function to check and install required packages
install_dependencies <- function(silent = FALSE) {
  # List of required packages with versions
  required_packages <- list(
    shiny = "1.7.4",
    shinydashboard = "0.7.2",
    shinydashboardPlus = "2.0.3",
    shinyjs = "2.1.0",
    shinyWidgets = "0.7.6",
    waiter = "0.2.5",
    DT = "0.27",
    plotly = "4.10.1",
    tidyverse = "2.0.0",
    haven = "2.5.1",
    openxlsx = "4.2.5",
    readxl = "1.4.2",
    rmarkdown = "2.20",
    testthat = "3.1.8",
    roxygen2 = "7.2.3",
    devtools = "2.4.5"
  )
  
  # Check for missing packages
  installed_packages <- rownames(installed.packages())
  missing_packages <- setdiff(names(required_packages), installed_packages)
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    if (!silent) {
      message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    }
    for (pkg in missing_packages) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
  
  # Verify versions and update if necessary
  for (pkg in names(required_packages)) {
    pkg_version <- packageVersion(pkg)
    required_version <- package_version(required_packages[[pkg]])
    
    if (pkg_version < required_version) {
      if (!silent) {
        message(sprintf("Updating %s from %s to %s", pkg, pkg_version, required_version))
      }
      install.packages(pkg, dependencies = TRUE)
    }
  }
  
  # Load all packages
  invisible(lapply(names(required_packages), library, character.only = TRUE))
}

# Function to check system requirements
check_system_requirements <- function() {
  requirements <- list(
    R_version = "4.1.0",
    memory = 4, # GB
    cores = 2
  )
  
  # Check R version
  if (getRversion() < package_version(requirements$R_version)) {
    stop("R version ", requirements$R_version, " or higher is required")
  }
  
  # Check available memory
  available_memory <- memory.limit() / 1024 # Convert to GB
  if (available_memory < requirements$memory) {
    warning("Less than recommended memory available")
  }
  
  # Check available cores
  available_cores <- parallel::detectCores()
  if (available_cores < requirements$cores) {
    warning("Less than recommended CPU cores available")
  }
  
  TRUE
}

# Initialize application environment
initialize_environment <- function() {
  # Create necessary directories
  dirs <- c("data", "logs", "output", "temp", "www")
  invisible(lapply(dirs, dir.create, showWarnings = FALSE))
  
  # Set up logging
  log_file <- file.path("logs", format(Sys.time(), "log_%Y%m%d_%H%M%S.txt"))
  setup_logging(log_file)
  
  # Initialize configuration
  config <- yaml::read_yaml("config.yml")
  options(survey.weights.config = config)
  
  TRUE
}

# tests/testthat/test-main.R
library(testthat)
library(shiny)

# Test data loading and validation
test_that("data loading works correctly", {
  # Test CSV loading
  test_data <- data.frame(
    id = 1:100,
    var1 = rnorm(100),
    var2 = sample(letters[1:3], 100, replace = TRUE)
  )
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)
  
  loaded_data <- load_data(temp_file)
  expect_equal(nrow(loaded_data), 100)
  expect_equal(ncol(loaded_data), 3)
  expect_true(all(names(loaded_data) == c("id", "var1", "var2")))
})

# Test weight computation
test_that("weight computation produces valid weights", {
  test_data <- data.frame(
    stratum = rep(1:2, each = 50),
    value = rnorm(100)
  )
  
  params <- list(
    method = "post_stratification",
    strata = "stratum",
    target_props = c(0.4, 0.6)
  )
  
  weights <- compute_weights(test_data, params)
  
  expect_equal(length(weights), nrow(test_data))
  expect_true(all(weights > 0))
  expect_true(abs(sum(weights) - nrow(test_data)) < 1e-10)
})

# Test diagnostics
test_that("diagnostic functions work correctly", {
  weights <- runif(100, 0.5, 1.5)
  diagnostics <- compute_diagnostics(weights)
  
  expect_true(is.numeric(diagnostics$deff))
  expect_true(is.numeric(diagnostics$cv))
  expect_true(all(diagnostics$deff > 0))
})

# Test error handling
test_that("error handling works correctly", {
  expect_error(
    compute_weights(NULL),
    "Input data is missing"
  )
  
  expect_error(
    compute_weights(data.frame(x = 1), list(method = "invalid")),
    "Invalid weighting method"
  )
})

# Test UI reactivity
test_that("UI updates correctly", {
  testServer(server, {
    session$setInputs(weighting_method = "post_stratification")
    expect_true(!is.null(output$parameter_ui))
    
    session$setInputs(data_file = NULL)
    expect_null(output$data_preview)
  })
})
# Survey Weighting Suite - Documentation

## Quick Start

```r
# Install and load the package
source("install.R")
install_dependencies()

# Launch the application
runApp()