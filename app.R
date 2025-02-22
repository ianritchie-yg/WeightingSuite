# Survey Weighting Suite - Main Application
# Version 2.2.0

# Source global settings and package management
source("global.R")

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

# Load secrets
source("R/secrets.R")

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
options(shiny.maxRequestSize = config$settings$max_upload_size %||% (300 * 1024^2))

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
                    "Post-stratification" = "post_strat",
                    "Raking" = "raking",
                    "Calibration" = "calibration",
                    "IPW" = "ipw"
                  )
      ),
      uiOutput("method_parameters")
    ),
    box(
      title = "Variable Selection",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      uiOutput("variable_selection")
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
                  choices = list(
                    `Grapefruit (Warm Red)` = "grapefruit",
                    `Plum (2582 C)` = "plum",
                    `Blueberry (306 C)` = "blueberry",
                    `Avocado (339 C)` = "avocado",
                    `Pomegranate (213 C)` = "pomegranate"
                  ),
                  selected = "grapefruit"),
      
      numericInput("max_iterations",
                   "Maximum Iterations",
                   value = 100,
                   min = 10,
                   max = 1000),
      numericInput("convergence_threshold",
                   "Convergence Threshold",
                   value = 1e-6,
                   min = 1e-10,
                   max = 1e-2),
      
      tags$div(
        class = "theme-switcher",
        tags$div(
          class = "theme-options",
          # Define themes once
          local({
            themes <- list(
              grapefruit = "#FF412C",
              plum = "#9F29FF",
              blueberry = "#0684EE",
              avocado = "#41CA4B",
              pomegranate = "#FF2A80"
            )
            
            # Create buttons for each theme
            lapply(names(themes), function(themeName) {
              tags$button(
                class = paste0("theme-option ", if(themeName == "grapefruit") "active" else ""),
                `data-theme` = themeName,
                `data-toggle` = "tooltip",
                title = paste("Switch to", tools::toTitleCase(themeName), "theme"),
                onclick = sprintf("switchTheme('%s')", themeName),
                style = sprintf("background-color: %s;", themes[[themeName]])
              )
            })
          })
        )
      )
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
      log_error_enhanced(e, "data_loading")
    })
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    req(values$data)
    datatable(head(values$data, 100),
              options = list(scrollX = TRUE))
  })
  
  # Weight Computation
  observeEvent(input$compute_weights, {
    req(values$data)
    show_waiter()
    
    tryCatch({
      # Implement your weighting logic here
      # This is a placeholder
      values$weights <- rep(1, nrow(values$data))
      values$diagnostics <- list(
        convergence = data.frame(
          iteration = 1:10,
          error = runif(10, 0, 1)
        )
      )
      
      hide_waiter()
      showNotification("Weights computed successfully", type = "success")
      
    }, error = function(e) {
      hide_waiter()
      showNotification(paste("Error:", e$message), type = "error")
      log_error_enhanced(e, "weight_computation")
      
      # Attempt recovery
      recovery <- recover_weight_computation(e, values$data, input)
      if (recovery$action == "retry") {
        # Retry with new parameters
        tryCatch({
          # Implement retry logic here
          # This is a placeholder
          values$weights <- rep(1, nrow(values$data))
          values$diagnostics <- list(
            convergence = data.frame(
              iteration = 1:10,
              error = runif(10, 0, 1)
            )
          )
          
          hide_waiter()
          showNotification("Weights computed successfully after recovery", type = "success")
          
        }, error = function(e) {
          hide_waiter()
          showNotification(paste("Error after recovery attempt:", e$message), type = "error")
          log_error_enhanced(e, "weight_computation_recovery")
        })
      } else {
        showNotification(recovery$message, type = "error")
      }
    })
  })
  
  # Example usage of Crunch.io API
  observeEvent(input$fetch_crunch_data, {
    tryCatch({
      data <- fetch_crunch_data("some/endpoint")
      # Process and display data
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      log_error_enhanced(e, "crunch_api")
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
  
  # Theme Updates
  observeEvent(input$theme, {
    js <- sprintf("switchTheme('%s');", input$theme)
    shinyjs::runjs(js)
  })
  
  observeEvent(input$theme_changed, {
    # Update the theme select input to match manual theme switches
    updateSelectInput(session, "theme", selected = input$theme_changed)
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
