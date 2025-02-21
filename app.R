# app.R
# Survey Weighting Suite - Main Application
# Version 2.1.0

# 1. Package Loading -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
library(pacman)

# Use pacman to handle all package dependencies
pacman::p_load(
  shiny,
  shinydashboard,
  shinydashboardPlus,
  shinyjs,
  shinyWidgets,
  waiter,
  DT,
  plotly,
  tidyverse,
  haven,
  openxlsx,
  readxl,
  rmarkdown,
  logger,
  yaml,
  survey,
  srvyr,
  sampling,
  data.table,
  parallel,
  future,
  future.apply,
  progressr,
  shinyvalidate,
  rmarkdown,
  knitr,
  character.only = FALSE
)

# Initialize logging (simpler version)
log_setup <- function() {
  tryCatch({
    log_dir <- "logs"
    if (!dir.exists(log_dir)) {
      dir.create(log_dir)
    }
    log_file <- file.path(log_dir, paste0("app_", format(Sys.time(), "%Y%m%d"), ".log"))
    log_appender(appender_file(log_file))
    log_threshold(INFO)
  }, error = function(e) {
    warning("Could not set up logging: ", e$message)
    # Fallback to console logging
    log_appender(appender_console())
  })
}

# Initialize logging
log_setup()

# Ensure max file size is set correctly
options(shiny.maxRequestSize = 30*1024^2) # Sets max size to 30MB

# 2. Ensure Required Files Exist -----------------------------------------------
ensure_files_exist <- function() {
  # Create www directory if it doesn't exist
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Create minimal CSS if it doesn't exist
  if (!file.exists("www/styles.css")) {
    writeLines("/* Custom styles */", "www/styles.css")
  }
  
  # Create minimal JS if it doesn't exist
  if (!file.exists("www/custom.js")) {
    writeLines("// Custom JavaScript", "www/custom.js")
  }
  
  # Create minimal config if it doesn't exist
  if (!file.exists("config.yml")) {
    writeLines(
      "app:\n  name: 'Survey Weighting Suite'\n  debug: false\nsettings:\n  max_upload_size: 31457280\n  theme:\n    waiter_color: '#ffffff'",
      "config.yml"
    )
  }
}

# Call ensure_files_exist
ensure_files_exist()

# 3. Source Component Files ----------------------------------------------------
source_files <- function(files) {
  for (file in files) {
    tryCatch({
      source(file)
    }, error = function(e) {
      warning(sprintf("Error loading %s: %s", file, e$message))
    })
  }
}

source_files(c(
  "R/utils.R",
  "R/error_handling.R",
  "R/visualizations.R",
  "R/export.R"
))

# 4. Load Configuration -------------------------------------------------------
config <- tryCatch({
  yaml::read_yaml("config.yml")
}, error = function(e) {
  # Fallback configuration
  list(
    app = list(
      name = "Survey Weighting Suite",
      debug = FALSE
    ),
    settings = list(
      max_upload_size = 30 * 1024^2,
      theme = list(
        waiter_color = "#ffffff"
      )
    )
  )
})

# Ensure max file size is set from config
options(shiny.maxRequestSize = config$settings$max_upload_size %||% (30 * 1024^2))

# 5. Initialize Application State --------------------------------------------
app_state <- reactiveValues(
  data = NULL,
  weights = NULL,
  diagnostics = NULL,
  error_log = character(),
  session_info = list(),
  computation_log = list()
)

# 6. UI Components ---------------------------------------------------------

# 6.1 Dashboard Tab
dashboardTab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(
      title = "Data Overview",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      DTOutput("data_summary_table")
    ),
    box(
      title = "Weight Distribution",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("weight_dist_plot")
    ),
    box(
      title = "Key Metrics",
      status = "warning",
      solidHeader = TRUE,
      width = 6,
      tableOutput("key_metrics_table")
    )
  )
)

# 6.2 Data Management Tab
dataTab <- tabItem(
  tabName = "data",
  fluidRow(
    box(
      title = "Data Import",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fileInput("data_file", 
                "Choose Data File",
                multiple = FALSE,
                accept = c(".csv", ".xlsx", ".xls", ".sav", ".rds"),
                width = "100%",
                buttonLabel = "Browse...",
                placeholder = "No file selected"
               # size = 0.5 -- no parameter accepted in MB for progress bar only
      ),  # Changed from `) ),` to just `),`
      checkboxInput("header", "First Row as Header", TRUE),
      selectInput("encoding", "File Encoding",
                  choices = c("UTF-8", "ISO-8859-1", "UTF-16"),
                  selected = "UTF-8")
    )
  ),
  fluidRow(
    box(
      title = "Data Preview",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput("data_preview")
    )
  )
)

# 6.3 Weighting Tab
weightingTab <- tabItem(
  tabName = "weighting",
  fluidRow(
    box(
      title = "Weighting Method",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      selectInput("weighting_method",
                  "Select Weighting Method",
                  choices = c(
                    "Rake Weighting (Iterative Proportional Fitting)" = "raking",
                    "Post-stratification (Full Cross-classification)" = "post_strat",
                    "Calibration (Linear Relationship)" = "calibration",
                    "Inverse Probability Weighting (Selection Model)" = "ipw"
                  )
      ),
      helpText(HTML("
        <strong>Method Guide:</strong><br/>
        • <u>Rake Weighting</u>: Best for matching marginal distributions<br/>
        • <u>Post-stratification</u>: For complete population cross-tabs<br/>
        • <u>Calibration</u>: Uses auxiliary variables linearly<br/>
        • <u>IPW</u>: Models selection/participation probability
      "))
    ),
    box(
      title = "Variable Selection",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      uiOutput("variable_selection"),
      uiOutput("target_inputs")
    )
  ),
  fluidRow(
    box(
      title = "Execution",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      actionButton("compute_weights",
                   "Compute Weights",
                   class = "btn-primary"),
      downloadButton("download_weights",
                     "Download Weights",
                     class = "btn-success")
    )
  )
)

# 6.4 Diagnostics Tab
diagnosticsTab <- tabItem(
  tabName = "diagnostics",
  fluidRow(
    box(
      title = "Variable Distribution Comparison",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("diagnostic_var", "Select Variable for Comparison",
                  choices = NULL),
      plotlyOutput("weight_comparison")
    ),
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
    ),
    box(
      title = "Weight Quality Indicators",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      verbatimTextOutput("weight_warnings")
    )
  )
)

# 6.5 Settings Tab
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
                  choices = c(
                    "Sunset Dark" = "sunset",
                    "Ocean Dark" = "ocean",
                    "Forest Dark" = "forest"
                  )),
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

# 7. Main UI Definition -----------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Survey Weighting Suite",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      dashboardTab,
      dataTab,
      weightingTab,
      diagnosticsTab,
      settingsTab
    )
  )
)

# 8. Server Logic ---------------------------------------------------------
server <- function(input, output, session) {
  # Initialize input validator
  iv <- InputValidator$new()
  
  # Add validation rules for target inputs
  iv$add_rule("target_inputs", function(value) {
    if (is.null(value)) return("Target proportions are required")
    targets <- as.numeric(strsplit(value, ",")[[1]])
    if (abs(sum(targets) - 1) > 0.01) {
      sprintf("Sum of proportions (%.3f) should be close to 1", sum(targets))
    }
  })
  
  # Enable validation
  iv$enable()
  
  # Initialize reactive values
  values <- reactiveValues(
    data = NULL,
    weights = NULL,
    diagnostics = NULL
  )
  
  # Data Loading and Validation
  observeEvent(input$data_file, {
    show_waiter(
      html = spin_flower(),
      color = config$theme$waiter_color
    )
    
    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
      
      # Read data based on file extension
      values$data <- switch(file_ext,
                            "csv" = read.csv(file_path, header = input$header, 
                                             encoding = input$encoding),
                            "xlsx" = readxl::read_excel(file_path),
                            "sav" = haven::read_sav(file_path),
                            "rds" = readRDS(file_path),
                            stop("Unsupported file format")
      )
      
      # Update variable selection UI
      updateSelectInput(session, "weight_vars",
                        choices = names(values$data))
      
      hide_waiter()
      showNotification("Data loaded successfully", type = "success")
      
    }, error = function(e) {
      hide_waiter()
      showNotification(paste("Error:", e$message), type = "error")
      log_error(paste("Data loading error:", e$message))
    })
    
    if (nrow(values$data) > 1e6) {
      showModal(modalDialog(
        title = "Large Dataset Detected",
        "Would you like to use sampling for initial testing?",
        footer = tagList(
          actionButton("use_sample", "Use 10% Sample"),
          actionButton("use_full", "Use Full Dataset")
        )
      ))
    }
  })
  
  # Handle sampling choice
  observeEvent(input$use_sample, {
    sample_size <- ceiling(nrow(values$data) * 0.1)
    values$data <- values$data[sample(nrow(values$data), sample_size), ]
    removeModal()
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    req(values$data)
    datatable(head(values$data, 100),
              options = list(scrollX = TRUE))
  })
  
  # Dynamic UI for variable selection
  output$variable_selection <- renderUI({
    req(values$data)
    tagList(
      selectizeInput("weight_vars", "Weighting Variables",
                     choices = names(values$data),
                     multiple = TRUE),
      checkboxInput("use_strata", "Use Stratification", FALSE),
      conditionalPanel(
        condition = "input.use_strata == true",
        selectInput("strata_var", "Stratification Variable",
                   choices = c("None", names(values$data)))
      )
    )
  })

  # Enhanced target input validation with real-time feedback
  observe({
    req(input$weight_vars)
    for (var in input$weight_vars) {
      if (!is.null(input[[paste0("target_", var)]])) {
        tryCatch({
          targets <- as.numeric(strsplit(input[[paste0("target_", var)]], ",")[[1]])
          levels <- unique(values$data[[var]])
          validate_targets(var, targets, levels)
          updateTextInput(session, 
                         paste0("target_", var),
                         value = input[[paste0("target_", var)]],
                         status = "success")
        }, error = function(e) {
          updateTextInput(session,
                         paste0("target_", var),
                         value = input[[paste0("target_", var)]],
                         status = "error")
        })
      }
    }
  })

  # Enhanced target inputs with default values
  output$target_inputs <- renderUI({
    req(input$weight_vars)
    tagList(
      lapply(input$weight_vars, function(var) {
        levels <- unique(values$data[[var]])
        default_props <- rep(1/length(levels), length(levels))
        div(
          textInput(
            paste0("target_", var),
            paste("Target proportions for", var),
            placeholder = paste(default_props, collapse = ", ")
          ),
          helpText(sprintf(
            "Enter %d comma-separated values (one per level) summing to 1", 
            length(levels)
          ))
        )
      })
    )
  })

  # Enhanced weight computation with parallel processing
  observeEvent(input$compute_weights, {
    req(values$data, input$weight_vars)
    show_waiter()
    
    # Enable parallel processing for large datasets
    if (nrow(values$data) > 10000) {
      plan(multisession)
    }
    
    withProgress(message = "Computing weights...", value = 0, {
      tryCatch({
        # Validate all target inputs first
        for (var in input$weight_vars) {
          targets <- as.numeric(strsplit(input[[paste0("target_", var)]], ",")[[1]])
          levels <- unique(values$data[[var]])
          validate_targets(var, targets, levels)
        }
        
        # Create survey design
        design <- svydesign(
          ids = ~1,
          strata = if(input$use_strata) as.formula(paste("~", input$strata_var)) else ~1,
          data = as.data.frame(values$data),
          weights = NULL
        )
        
        # Convert to data.table for better performance
        dt <- as.data.table(values$data)
        
        # Compute weights based on method
        values$weights <- switch(input$weighting_method,
          "raking" = {
            # Prepare margins
            margins <- lapply(input$weight_vars, function(var) {
              targets <- as.numeric(strsplit(input[[paste0("target_", var)]], ",")[[1]])
              setNames(targets, unique(values$data[[var]]))
            })
            
            # Create formulas
            formulas <- lapply(input$weight_vars, function(var) {
              as.formula(paste("~", var))
            })
            
            # Perform raking
            rake_result <- rake(
              design,
              sample.margins = formulas,
              population.margins = margins,
              control = list(
                maxit = input$max_iterations,
                epsilon = input$convergence_threshold
              )
            )
            
            values$diagnostics$convergence <- data.frame(
              iteration = seq_along(rake_result$iterations),
              error = rake_result$iterations
            )
            
            weights(rake_result)
          },
          "post_strat" = {
            # Improved post-stratification with progress
            post_vars <- paste(input$weight_vars, collapse = "+")
            pop_table <- future_lapply(input$weight_vars, function(var) {
              targets <- as.numeric(strsplit(input[[paste0("target_", var)]], ",")[[1]])
              data.table(
                var = unique(dt[[var]]),
                freq = targets * nrow(dt)
              )
            })
            
            post_result <- postStratify(
              design,
              strata = as.formula(paste("~", post_vars)),
              population = Reduce(merge, pop_table)
            )
            weights(post_result)
          },
          "calibration" = {
            # Enhanced calibration with auxiliary variables
            aux_vars <- paste(input$weight_vars, collapse = "+")
            pop_totals <- future_lapply(input$weight_vars, function(var) {
              targets <- as.numeric(strsplit(input[[paste0("target_", var)]], ",")[[1]])
              setNames(targets * nrow(dt), unique(dt[[var]]))
            })
            
            cal_result <- calibrate(
              design,
              formula = as.formula(paste("~", aux_vars)),
              population = unlist(pop_totals),
              bounds = c(0.2, 5)  # Prevent extreme weights
            )
            weights(cal_result)
          },
          "ipw" = {
            # Advanced IPW with model diagnostics
            model_vars <- paste(input$weight_vars, collapse = "+")
            dt[, selection := 1]  # Add selection indicator
            
            model <- glm(
              as.formula(paste("selection ~", model_vars)),
              family = binomial(),
              data = dt
            )
            
            # Store model diagnostics
            values$diagnostics$model <- summary(model)
            
            1 / predict(model, type = "response")
          }
        )
        
        # Compute diagnostics
        values$diagnostics$summary <- data.frame(
          metric = c(
            "Effective Sample Size",
            "Design Effect",
            "CV of Weights",
            "Maximum Weight",
            "Minimum Weight",
            "Proportion Extreme (>5 or <0.2)"
          ),
          value = c(
            sum(values$weights)^2 / sum(values$weights^2),
            mean(values$weights^2),
            sd(values$weights) / mean(values$weights),
            max(values$weights),
            min(values$weights),
            mean(values$weights > 5 | values$weights < 0.2)
          )
        )
        
        # Check for extreme weights
        extreme_weights <- sum(values$weights > 5 | values$weights < 0.2)
        if (extreme_weights > 0) {
          warning(sprintf("%d weights are extreme (>5 or <0.2)", extreme_weights))
        }
        
        hide_waiter()
        showNotification("Weights computed successfully", type = "success")
        
      }, error = function(e) {
        hide_waiter()
        showNotification(paste("Error:", e$message), type = "error")
        log_error(paste("Weight computation error:", e$message))
      })
    })
  })

  # Diagnostics Plots
  output$weight_distribution <- renderPlotly({
    req(values$weights)
    plot_ly(x = values$weights, type = "histogram",
            nbinsx = 30) %>%
      layout(title = "Weight Distribution")
  })
  
  output$convergence_plot <- renderPlotly({
    req(values$diagnostics)
    plot_ly(data = values$diagnostics$convergence,
            x = ~iteration, y = ~error, type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Convergence Plot")
  })
  
  # Enhanced diagnostics outputs
  output$weight_comparison <- renderPlotly({
    req(values$weights, input$diagnostic_var)
    df <- data.frame(
      value = values$data[[input$diagnostic_var]],
      weight = values$weights
    )
    
    plot_ly() %>%
      add_histogram(data = df, x = ~value, name = "Unweighted",
                   histnorm = "probability") %>%
      add_histogram(data = df, x = ~value, weights = ~weight,
                   name = "Weighted", histnorm = "probability") %>%
      layout(barmode = "overlay",
             title = paste("Distribution of", input$diagnostic_var),
             xaxis = list(title = input$diagnostic_var),
             yaxis = list(title = "Density"))
  })

  output$weight_warnings <- renderText({
    req(values$weights)
    warnings <- character()
    
    # Check for extreme weights
    extreme_high <- sum(values$weights > 5)
    extreme_low <- sum(values$weights < 0.2)
    if (extreme_high > 0) {
      warnings <- c(warnings, 
                   sprintf("%d weights > 5 (%.1f%%)", 
                          extreme_high, 100 * extreme_high/length(values$weights)))
    }
    if (extreme_low > 0) {
      warnings <- c(warnings,
                   sprintf("%d weights < 0.2 (%.1f%%)",
                          extreme_low, 100 * extreme_low/length(values$weights)))
    }
    
    # Check effective sample size
    ess <- sum(values$weights)^2 / sum(values$weights^2)
    if (ess < 0.5 * length(values$weights)) {
      warnings <- c(warnings,
                   sprintf("Low effective sample size: %.1f (%.1f%% of original)",
                          ess, 100 * ess/length(values$weights)))
    }
    
    if (length(warnings) == 0) {
      "No weight quality issues detected."
    } else {
      paste("Warning:", paste(warnings, collapse = "\n"))
    }
  })

  # Enhanced download handler
  output$download_weights <- downloadHandler(
    filename = function() {
      paste0("weighted_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      write.csv(
        data.frame(
          values$data,
          weight = values$weights
        ),
        file,
        row.names = FALSE
      )
    }
  )

  # Theme Updates
  observeEvent(input$theme, {
    theme_settings <- switch(input$theme,
                             "sunset" = list(bg = "#1a1a2e", text = "#e94560"),
                             "ocean" = list(bg = "#1b262c", text = "#3282b8"),
                             "forest" = list(bg = "#2d3436", text = "#6ab04c")
    )
    
    shinyjs::runjs(sprintf(
      "document.documentElement.style.setProperty('--bg-color', '%s');
       document.documentElement.style.setProperty('--text-color', '%s');",
      theme_settings$bg,
      theme_settings$text
    ))
  })
  
  # Session Cleanup
  session$onSessionEnded(function() {
    log_info("Session ended")
  })
}

# 9. Initialize and Run Application --------------------------------------
initialize_environment <- function() {
  tryCatch({
    # Create necessary directories
    dirs <- c("logs", "www", "data")
    sapply(dirs, function(d) {
      if (!dir.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
    })
    
    # Set options
    options(
      shiny.maxRequestSize = config$settings$max_upload_size %||% (30 * 1024^2),
      shiny.fullstacktrace = config$app$debug %||% FALSE
    )
    
    TRUE
  }, error = function(e) {
    warning("Initialization error: ", e$message)
    TRUE
  })
}

# Run the application
if (initialize_environment()) {
  log_info("Starting application")
  shinyApp(ui = ui, server = server)
} else {
  stop("Failed to initialize application environment")
}
