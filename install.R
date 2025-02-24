# install.R
# Survey Weighting Suite - Installation and Setup
# Version 1.0.0

#' Check and install required packages
#' @param silent Logical; if TRUE, suppresses status messages
#' @return Invisible TRUE if successful
install_dependencies <- function(silent = FALSE) {
  # List of required packages with versions
  required_packages <- list(
    # Core Shiny packages
    shiny = "1.7.4",
    shinydashboard = "0.7.2",
    shinydashboardPlus = "2.0.3",
    shinyjs = "2.1.0",
    shinyWidgets = "0.7.6",
    waiter = "0.2.5",
    
    # Data handling packages
    tidyverse = "2.0.0",
    DT = "0.27",
    haven = "2.5.1",
    openxlsx = "4.2.5",
    readxl = "1.4.2",
    
    # Visualization packages
    plotly = "4.10.1",
    ggplot2 = "3.4.0",
    
    # Documentation and testing
    rmarkdown = "2.20",
    testthat = "3.1.8",
    roxygen2 = "7.2.3",
    devtools = "2.4.5",
    
    # Additional utilities
    yaml = "2.3.7",
    logger = "0.2.2",
    future = "1.32.0",
    promises = "1.2.0.1"
  )
  
  # Function to check package version
  check_version <- function(pkg, required_version) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(FALSE)
    }
    package_version(packageVersion(pkg)) >= package_version(required_version)
  }
  
  # Install or update packages as needed
  for (pkg in names(required_packages)) {
    if (!check_version(pkg, required_packages[[pkg]])) {
      if (!silent) {
        message(sprintf("Installing/updating %s to version %s", pkg, required_packages[[pkg]]))
      }
      tryCatch({
        install.packages(pkg, dependencies = TRUE, quiet = silent)
      }, error = function(e) {
        warning(sprintf("Failed to install %s: %s", pkg, e$message))
      })
    }
  }
  
  # Load all packages
  invisible(lapply(names(required_packages), library, character.only = TRUE))
  
  if (!silent) {
    message("Package installation and loading completed.")
  }
  
  invisible(TRUE)
}

#' Check system requirements
#' @return List of system check results
check_system_requirements <- function() {
  requirements <- list(
    R_version = "4.1.0",
    memory = 4, # GB
    cores = 2,
    disk_space = 1 # GB
  )
  
  results <- list()
  
  # Check R version
  results$R_version <- list(
    status = getRversion() >= package_version(requirements$R_version),
    current = as.character(getRversion()),
    required = requirements$R_version
  )
  
  # Check available memory
  mem_info <- try({
    available_memory <- memory.limit() / 1024 # Convert to GB
    list(
      status = available_memory >= requirements$memory,
      current = round(available_memory, 1),
      required = requirements$memory
    )
  }, silent = TRUE)
  
  if (!inherits(mem_info, "try-error")) {
    results$memory <- mem_info
  }
  
  # Check available cores
  results$cores <- list(
    status = parallel::detectCores() >= requirements$cores,
    current = parallel::detectCores(),
    required = requirements$cores
  )
  
  # Check available disk space
  disk_info <- try({
    free_space <- fs::fs_bytes(fs::fs_path_free()) / (1024^3) # Convert to GB
    list(
      status = free_space >= requirements$disk_space,
      current = round(free_space, 1),
      required = requirements$disk_space
    )
  }, silent = TRUE)
  
  if (!inherits(disk_info, "try-error")) {
    results$disk_space <- disk_info
  }
  
  results
}

#' Initialize application environment
#' @param force Logical; if TRUE, overwrites existing directories
#' @param silent Logical; if TRUE, suppresses status messages
#' @return Invisible TRUE if successful
initialize_environment <- function(force = FALSE, silent = FALSE) {
  # Create directory structure
  dirs <- c(
    "data",         # For data storage
    "logs",         # For application logs
    "output",       # For generated outputs
    "temp",         # For temporary files
    "www",          # For web assets
    "R",            # For R scripts
    "tests",        # For test files
    "docs"          # For documentation
  )
  
  # Create directories if they don't exist
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      if (!silent) message(sprintf("Created directory: %s", dir))
    }
  }
  
  # Set up logging
  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  log_file <- file.path(log_dir, sprintf("install_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S")))
  
  # Initialize configuration
  if (!file.exists("config.yml") || force) {
    config <- list(
      app = list(
        name = "Survey Weighting Suite",
        version = "1.0.0",
        debug = FALSE
      ),
      paths = list(
        data = "data",
        logs = "logs",
        output = "output",
        temp = "temp"
      ),
      settings = list(
        max_upload_size = 300 * 1024^2,  # 300MB
        theme = "sunset",
        locale = "en_US.UTF-8"
      )
    )
    yaml::write_yaml(config, "config.yml")
  }
  
  # Create or update .gitignore
  gitignore_content <- c(
    "*.Rproj.user",
    "*.Rhistory",
    "*.RData",
    "*.Ruserdata",
    "/temp/*",
    "/logs/*",
    "!/logs/.gitkeep",
    "/output/*",
    "!/output/.gitkeep",
    ".DS_Store"
  )
  writeLines(gitignore_content, ".gitignore")
  
  invisible(TRUE)
}

#' Main setup function
#' @param silent Logical; if TRUE, suppresses status messages
#' @param force Logical; if TRUE, forces reinstallation
#' @export
setup <- function(silent = FALSE, force = FALSE) {
  tryCatch({
    # Check system requirements
    requirements <- check_system_requirements()
    requirements_met <- all(sapply(requirements, function(x) x$status))
    
    if (!requirements_met) {
      warning("Some system requirements not met. Check the results for details.")
      print(requirements)
    }
    
    # Install dependencies
    install_dependencies(silent = silent)
    
    # Initialize environment
    initialize_environment(force = force, silent = silent)
    
    if (!silent) {
      message("Setup completed successfully.")
      message("You can now run the application using:\n",
              "source('app.R')\n")
    }
    
    invisible(TRUE)
    
  }, error = function(e) {
    stop("Setup failed: ", e$message)
  })
}

# Run setup if this script is run directly
if (!interactive()) {
  setup()
}