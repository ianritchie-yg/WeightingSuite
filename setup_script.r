# setup.R
setup_app <- function() {
  # Create directory structure
  dirs <- c("R", "tests/testthat", "www", "docs")
  sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # Install required packages
  required_packages <- c(
    "shiny",
    "shinydashboard",
    "shinyjs",
    "waiter",
    "DT",
    "plotly",
    "readxl",
    "haven",
    "yaml",
    "logger",
    "R6",
    "tidyverse",
    "openxlsx",
    "rmarkdown"
  )
  
  # Bug fix: Extract package names from installed.packages()
  installed_pkgs <- installed.packages()[, "Package"]
  new_packages <- required_packages[!required_packages %in% installed_pkgs]
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  }
  
  # Create empty files (you'll need to fill these with the necessary content)
  file.create(c(
    "app.R",
    "config.yml",
    "R/utils.R",
    "R/error_handling.R",
    "R/visualizations.R",
    "R/export.R",
    "www/styles.css",
    "www/custom.js",
    "tests/testthat/test-main.R",
    "docs/usage.md"
  ))
  
  # Create DESCRIPTION file
  writeLines(
    c(
      "Package: SurveyWeightingSuite",
      "Title: Survey Weighting Suite",
      "Version: 1.0.0",
      "Authors@R: ",
      "    person(\"Your\", \"Name\", email = \"your.email@example.com\", role = c(\"aut\", \"cre\"))",
      "Description: A comprehensive tool for survey weighting.",
      "License: MIT + file LICENSE",
      "Encoding: UTF-8",
      paste("Imports:", paste(required_packages, collapse = ", "))
    ),
    "DESCRIPTION"
  )
  
  message("Setup complete! Next steps:")
  message("1. Add the code to each of the created files")
  message("2. Run the app using shiny::runApp()")
}

# Run setup
setup_app()