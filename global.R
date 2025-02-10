# global.R
packages <- c(
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyjs",
  "shinyWidgets",
  "waiter",
  "DT",
  "plotly",
  "tidyverse",
  "haven",
  "openxlsx",
  "readxl",
  "rmarkdown",
  "logger",
  "yaml"
)

# Install missing packages
installed_packages <- rownames(installed.packages())
for (package in packages) {
  if (!package %in% installed_packages) {
    install.packages(package)
  }
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# Initialize logging
log_appender(appender_file("logs/app.log"))
log_threshold(INFO)

# Ensure max file size is set correctly
options(shiny.maxRequestSize = 30*1024^2) # Sets max size to 30MB