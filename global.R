# global.R
# Initialize package management

if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
library(pacman)
library(yaml)

# Define list of required packages
packages <- c(
  # UI Framework and Core Functionality
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyjs",
  "shinyWidgets",
  "waiter",
  
  # Data Display and Visualization
  "DT",
  "plotly",
  "ggplot2",
  
  # Data Manipulation and Analysis
  "tidyverse",
  
  # File Input/Output
  "haven",
  "openxlsx",
  "readxl",
  "writexl",
  
  # Documentation and Logging
  "rmarkdown",
  "logger",
  "yaml",
  
  # Additional Utilities
  "scales",
  "stringr",
  "digest",
  "jsonlite",
  "shinyvalidate"
)

# Load all required packages using pacman
pacman::p_load(char = packages)

# Suppress package loading messages
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

# Initialize logging system
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

# Set global options
options(shiny.maxRequestSize = 300 * 1024^2)  # Sets max size to 300MB

# Create required directories if they don't exist
dir.create("www/css", recursive = TRUE, showWarnings = FALSE)
dir.create("www/js", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)

# Load configuration
config <- yaml::read_yaml("config.yml")

# Source all R files
for(file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

# Source helper files
source("R/utils.R")
source("R/error_handling.R")
source("R/visualizations.R")
source("R/export.R")
source("R/statistics.R")

# Initialize environment
initialize_environment()

# Ensure required directories exist
ensure_files_exist()