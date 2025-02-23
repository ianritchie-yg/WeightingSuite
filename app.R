# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)

# Source dependencies
source("global.R")
source("R/weighting_params.R")
source("R/ui_helpers.R")
source("R/utils.R")
source("R/error_handling.R")
source("R/visualizations.R")
source("R/export.R")
source("R/secrets.R")

# Ensure required directories exist
ensure_files_exist()

# UI Definition
ui <- dashboardPage(
    dashboardHeader(title = "WeightingSuite"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data"),
            menuItem("Weighting", tabName = "weighting"),
            menuItem("Diagnostics", tabName = "diagnostics"),
            menuItem("Settings", tabName = "settings")
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem("weighting",
                fluidRow(
                    box(
                        selectInput("weight_method", "Weighting Method",
                            choices = c("Post-Stratification", "IPW", "Calibration", "RAKE")
                        ),
                        conditionalPanel(
                            condition = "input.weight_method == 'Post-Stratification'",
                            generate_post_strat_ui("ps")
                        ),
                        conditionalPanel(
                            condition = "input.weight_method == 'IPW'",
                            generate_ipw_ui("ipw")
                        ),
                        conditionalPanel(
                            condition = "input.weight_method == 'Calibration'",
                            generate_calibration_ui("cal")
                        ),
                        conditionalPanel(
                            condition = "input.weight_method == 'RAKE'",
                            generate_rake_ui("rake")
                        )
                    ),
                    box(
                        generate_export_ui("export")
                    )
                )
            )
        )
    )
)

# Server Definition
server <- function(input, output, session) {
    values <- reactiveValues(
        data = NULL,
        weights = NULL,
        diagnostics = NULL
    )

    # Data loading observer
    observeEvent(input$dataFile, {
        tryCatch({
            values$data <- read.csv(input$dataFile$datapath)
            update_method_ui(session, values$data, input$weight_method)
            log_info("Data loaded successfully")
        }, error = function(e) {
            log_error_enhanced("Data loading failed", e)
        })
    })

    # Weight computation observer
    observeEvent(input$compute_weights, {
        req(values$data)
        
        params <- switch(input$weight_method,
            "Post-Stratification" = collect_post_strat_params(input, values$data),
            "IPW" = collect_ipw_params(input, values$data),
            "Calibration" = collect_calibration_params(input, values$data),
            "RAKE" = collect_rake_params(input, values$data)
        )

        tryCatch({
            # Validate parameters
            validate_params(input$weight_method, params)
            
            # Compute weights
            result <- compute_weights(input$weight_method, params, values$data)
            values$weights <- result$weights
            values$diagnostics <- result$diagnostics
      # Update diagnostics plots
            update_diagnostics_plots()
    }, error = function(e) {
            log_error_enhanced("Weight computation failed", e)
        })
    })
}

shinyApp(ui = ui, server = server)
