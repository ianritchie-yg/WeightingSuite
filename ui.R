library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(waiter)

source("global.R")

# Read configuration
config <- yaml::read_yaml("config.yml")

# Custom CSS injection
css_path <- "www/styles.css"
if (file.exists(css_path)) {
  css_content <- includeCSS(css_path)
} else {
  warning("CSS file not found. Using default styling.")
}

ui <- dashboardPage(
  header = dashboardHeader(
    title = config$app$name,
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      actionButton("help_btn", "Help", icon = icon("question-circle"),
                  class = "btn-info navbar-btn")
    ),
    tags$li(
      class = "dropdown",
      div(
        class = "theme-switcher",
        selectInput("theme_select", "Theme",
          choices = c(
            "Grapefruit" = "grapefruit",
            "Plum" = "plum",
            "Blueberry" = "blueberry",
            "Avocado" = "avocado",
            "Pomegranate" = "pomegranate"
          ),
          selected = "blueberry"
        )
      )
    )
  ),
  
  sidebar = dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Management", tabName = "data", icon = icon("database"),
        startExpanded = TRUE,
        menuSubItem("Upload & Preview", tabName = "upload"),
        menuSubItem("Data Quality", tabName = "quality"),
        menuSubItem("Variable Selection", tabName = "variables")
      ),
      menuItem("Weighting", tabName = "weighting", icon = icon("balance-scale"),
        menuSubItem("Method Selection", tabName = "method"),
        menuSubItem("Parameters", tabName = "parameters"),
        menuSubItem("Execution", tabName = "execute")
      ),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  body = dashboardBody(
    useShinyjs(),
    use_waiter(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$style(HTML("
        .fruit-theme {
          background-color: #f0f0f0;
        }
        .navbar {
          background-color: #337ab7;
          color: white;
        }
      "))
    ),
    
    tags$div(
      class = "fruit-theme",
      id = "main-content",
      
      tabItems(
        # Dashboard tab
        tabItem("dashboard",
          fluidRow(
            infoBoxOutput("data_status_box"),
            infoBoxOutput("method_status_box"),
            infoBoxOutput("analysis_status_box")
          ),
          fluidRow(
            box(
              title = "Quick Summary",
              status = "primary",
              width = 12,
              verbatimTextOutput("quick_summary")
            )
          )
        ),
        
        # Data Upload tab
        tabItem("upload",
          fluidRow(
            box(
              title = "Data Import",
              status = "primary",
              width = 12,
              fileInput("data_file", "Upload Data File",
                accept = c(".csv", ".xlsx", ".sav", ".rds")
              ),
              helpText("Please upload your survey data file in one of the supported formats."),
              checkboxInput("header", "First Row as Header", TRUE),
              selectInput("encoding", "File Encoding",
                choices = c("UTF-8", "ISO-8859-1", "UTF-16")
              )
            )
          ),
          fluidRow(
            box(
              title = "Data Preview",
              status = "primary",
              width = 12,
              DTOutput("data_preview")
            ),
            box(
              title = "Data Summary",
              status = "info",
              width = 12,
              verbatimTextOutput("data_summary")
            )
          )
        ),
        
        # Weighting tab
        tabItem("weighting",
          fluidRow(
            box(
              title = "Method Selection",
              status = "primary",
              width = 6,
              selectInput("weight_method", "Weighting Method",
                choices = c(
                  "Post-stratification" = "post_stratification",
                  "IPW" = "ipw",
                  "Calibration" = "calibration",
                  "RAKE" = "rake"
                ),
                label = "Select the weighting method to apply to your survey data."
              ),
              uiOutput("method_parameters")
            ),
            box(
              title = "Weight Controls",
              status = "info",
              width = 6,
              uiOutput("weight_controls")
            )
          ),
          fluidRow(
            box(
              title = "Computation Status",
              status = "warning",
              width = 12,
              actionButton("compute_weights", "Compute Weights",
                          class = "btn-primary",
                          {{ aria-label = "Compute weights for the survey data" }}
              ),
              verbatimTextOutput("computation_status")
            )
          )
        ),
        
        # Diagnostics tab
        tabItem("diagnostics",
          fluidRow(
            box(
              title = "Weight Distribution",
              status = "primary",
              width = 6,
              plotOutput("weight_distribution")
            ),
            box(
              title = "Convergence Plot",
              status = "info",
              width = 6,
              plotOutput("convergence_plot")
            )
          ),
          fluidRow(
            box(
              title = "Summary Statistics",
              status = "warning",
              width = 12,
              tableOutput("summary_stats")
            )
          )
        ),
        
        # Export tab
        tabItem("export",
          fluidRow(
            box(
              title = "Export Options",
              status = "primary",
              width = 12,
              selectInput("export_format", "Format",
                         choices = c("CSV", "Excel", "SPSS", "R")),
              checkboxInput("include_diagnostics", "Include Diagnostics",
                           value = TRUE),
              downloadButton("download_data", "Download Data"),
              downloadButton("download_report", "Download Report")
            )
          )
        ),
        
        # Settings tab
        tabItem("settings",
          fluidRow(
            box(
              title = "Application Settings",
              status = "primary",
              width = 12,
              selectInput("theme", "Theme",
                choices = c(
                  "Grapefruit" = "grapefruit",
                  "Plum" = "plum", 
                  "Blueberry" = "blueberry",
                  "Avocado" = "avocado",
                  "Pomegranate" = "pomegranate"
                ),
                selected = "blueberry"),
              numericInput("max_iterations", "Max Iterations",
                          value = config$settings$max_iterations),
              numericInput("convergence_threshold", "Convergence Threshold",
                          value = config$settings$default_convergence_threshold)
            )
          )
        )
      )
    )
  ),
  theme = shinythemes::shinytheme("flatly")
)
