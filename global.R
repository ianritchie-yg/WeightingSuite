# global.R
# Initialize package management
if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
library(pacman)

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

# Use pacman to load all packages
pacman::p_load(char = packages)

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
options(shiny.maxRequestSize = 300*1024^2) # Sets max size to 300MB