# Utility functions for Survey Weighting Suite
#' @importFrom logger log_threshold INFO log_appender file_appender
library(shiny)
library(logger)
library(readxl)
library(readr)
library(haven)

utils::globalVariables(c(
  "treatment_var", "margin_vars", "strata_vars",
  "calibration_vars", "target_vars"
))

# Define required parameters for each weighting method
post_strat_params <- list(required = c("strata_vars", "target_props"))
ipw_params <- list(required = c("treatment_var", "covariates", "ps_method"))
calibration_params <- list(required = c("calibration_vars", "target_totals", "bounds"))
rake_params <- list(required = c("margin_vars", "target_margins"))

#' Load data from various file formats
#' @param file_path Path to the data file
#' @param file_type File extension
#' @param header Boolean indicating if first row contains headers
#' @param encoding File encoding
#' @return data.frame
load_data <- function(file_path, file_type, header = TRUE, encoding = "UTF-8") {
  tryCatch({
    data <- switch(tolower(file_type),
      "csv" = read_csv(file_path, col_names = header, locale = locale(encoding = encoding)),
      "xlsx" = read_xlsx(file_path, col_names = header),
      "sav" = read_sav(file_path),
      "rds" = readRDS(file_path),
      stop("Unsupported file format")
    )

    # Convert to data.frame
    data <- as.data.frame(data)

    # Basic cleaning
    data <- clean_dataset(data)

    return(data)
  }, error = function(e) {
    log_error(paste("Error loading data:", e$message))
    stop(e$message)
  })
}

#' Clean and validate dataset
#' @param data Input dataframe
#' @return Cleaned dataframe
clean_dataset <- function(data) {
  # Remove completely empty rows and columns
  data <- data[rowSums(!is.na(data)) > 0, colSums(!is.na(data)) > 0]
  
  # Convert character columns containing only numbers to numeric
  for(col in names(data)) {
    if(is.character(data[[col]]) && all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", na.omit(data[[col]])))) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  return(data)
}

#' Identify potential date columns
#' @param data Input dataset
#' @return Character vector of column names
identify_date_columns <- function(data) {
  date_patterns <- c(
    "^\\d{4}-\\d{2}-\\d{2}$",
    "^\\d{2}/\\d{2}/\\d{4}$",
    "^\\d{2}-\\d{2}-\\d{4}$"
  )

  date_cols <- character(0)
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      for (pattern in date_patterns) {
        if (all(grepl(pattern, na.omit(data[[col]])))) {
          date_cols <- c(date_cols, col)
          break
        }
      }
    }
  }

  return(date_cols)
}

#' Generate summary statistics for a dataset
#' @param data Input dataframe
#' @return List of summary statistics
generate_data_summary <- function(data) {
  list(
    dimensions = dim(data),
    types = sapply(data, class),
    missing = colSums(is.na(data)),
    summary = summary(data)
  )
}

#' Collect parameters from input values
#' @param input Shiny input object
#' @param method_params List of method parameters
#' @return List of parameters
collect_parameters <- function(input, method_params) {
  params <- list()

  # Common parameters
  params$method <- input$weighting_method
  params$max_iterations <- input$max_iterations
  params$convergence_threshold <- input$convergence_threshold

  # Method-specific parameters
  switch(input$weighting_method,
    "post_strat" = {
      params$strata_vars <- input$strata_vars
      params$target_props <- input$target_props
    },
    "raking" = {
      params$margin_vars <- input$margin_vars
      params$target_margins <- input$target_margins
    },
    "calibration" = {
      params$calibration_vars <- input$calibration_vars
      params$target_totals <- input$target_totals
      params$bounds <- c(input$lower_bound, input$upper_bound)
    },
    "ipw" = {
      params$treatment_var <- input$treatment_var
      params$covariates <- input$covariates
      params$ps_method <- input$ps_method
    }
  )

  return(params)
}

#' Generate method-specific parameter UI
#' @param method Selected weighting method
#' @return UI elements
generate_method_parameters_ui <- function(method) {
  switch(method,
    "post_strat" = {
      tagList(
        selectizeInput("strata_vars", "Stratification Variables",
                       choices = NULL, multiple = TRUE),
        textAreaInput("target_props", "Target Proportions",
                      placeholder = "Enter as CSV: strata,proportion")
      )
    },
    "raking" = {
      tagList(
        selectizeInput("margin_vars", "Margin Variables",
                     choices = NULL, multiple = TRUE),
        textAreaInput("target_margins", "Target Margins",
                      placeholder = "Enter as CSV: variable,category,proportion")
      )
    },
    "calibration" = {
      tagList(
        selectizeInput("calibration_vars", "Calibration Variables",
                       choices = NULL, multiple = TRUE),
        textAreaInput("target_totals", "Target Totals",
                      placeholder = "Enter as CSV: variable,total"),
        numericInput("lower_bound", "Lower Bound", 0),
        numericInput("upper_bound", "Upper Bound", Inf)
      )
    },
    "ipw" = {
      tagList(
        selectInput("treatment_var", "Treatment Variable",
                    choices = NULL),
        selectizeInput("covariates", "Covariates",
                       choices = NULL, multiple = TRUE),
        selectInput("ps_method", "Propensity Score Method",
                    choices = c("logistic", "random_forest", "gbm"))
      )
    }
  )
}

#' Setup logging configuration
#' @param log_file Path to log file
setup_logging <- function(log_file) {
  log_threshold(INFO)
  log_appender(file_appender(log_file))
}

#' Load secrets from configuration file
#' @return List containing API credentials
load_secrets <- function() {
  secrets_file <- "config/secrets.yml"
  if (!file.exists(secrets_file)) {
    stop("Secrets file not found. Please create config/secrets.yml")
  }
  yaml::yaml.load_file(secrets_file)
}

#' Fetch data from Crunch.io API
#' @param endpoint API endpoint to fetch data from
#' @param params List of query parameters
#' @return Data from API
fetch_crunch_data <- function(endpoint, params = list()) {
  secrets <- load_secrets()
  api_url <- secrets$crunch$api_url
  api_key <- secrets$crunch$api_key

  url <- paste0(api_url, endpoint)
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key)), query = params)

  if (response$status_code != 200) {
    stop("Failed to fetch data from Crunch.io API")
  }

  content <- httr::content(response, as = "parsed")
  return(content)
}

#' Validate weighting parameters based on method
#' @param params List of parameters
#' @param method Weighting method
#' @return Validated parameters or error
validate_weighting_parameters <- function(params, method) {
  required_params <- switch(method,
    "Post-Stratification" = post_strat_params$required,
    "IPW" = ipw_params$required,
    "Calibration" = calibration_params$required,
    "RAKE" = rake_params$required
  )

  missing_params <- required_params[!required_params %in% names(params)]
  if (length(missing_params) > 0) {
    stop(paste("Missing required parameters:", paste(missing_params, collapse = ", ")))
  }

  TRUE
}

#' Format parameters for display
#' @param params List of parameters
#' @param method Weighting method
#' @return Formatted parameter string
format_parameters <- function(params, method) {
  params <- lapply(params, function(x) {
    if (is.numeric(x)) format(x, scientific = FALSE)
    else if (is.character(x)) paste(x, collapse = ", ")
    else x
  })
  return(params)
}

#' Update UI elements based on data
#' @param session Shiny session object
#' @param data Dataset
#' @param method Weighting method
update_method_ui <- function(session, data, method) {
  if (is.null(data)) return()

  col_names <- names(data)
  numeric_cols <- names(which(sapply(data, is.numeric)))
  factor_cols <- names(which(sapply(data, is.factor)))

  switch(method,
    "post_strat" = {
      updateSelectizeInput(session, "strata_vars",
        choices = factor_cols,
        selected = NULL
      )
    },
    "ipw" = {
      updateSelectInput(session, "treatment_var",
        choices = factor_cols,
        selected = NULL
      )
      updateSelectizeInput(session, "covariates",
        choices = setdiff(col_names, "treatment_var"),
        selected = NULL
      )
    },
    "calibration" = {
      updateSelectizeInput(session, "calibration_vars",
        choices = numeric_cols,
        selected = NULL
      )
    },
    "rake" = {
      updateSelectizeInput(session, "margin_vars",
        choices = factor_cols,
        selected = NULL
      )
    }
  )
}

#' Check if parameters are compatible
#' @param params List of parameters
#' @param method Weighting method
#' @param data Dataset
#' @return TRUE if compatible, error message if not
check_parameter_compatibility <- function(params, method, data) {
  tryCatch({
    switch(method,
      "Post-Stratification" = {
        check_cols_exist(data, params$strata_vars)
      },
      "IPW" = {
        check_cols_exist(data, c(params$treatment_var, params$covariates))
      },
      "Calibration" = {
        # Bug fixed: use 'calibration_vars' instead of incorrect 'target_vars'
        check_cols_exist(data, params$calibration_vars)
      },
      "RAKE" = {
        check_cols_exist(data, params$margin_vars)
      }
    )
    return(TRUE)
  }, error = function(e) {
    log_error(sprintf("Parameter compatibility check failed: %s", e$message))
    return(FALSE)
  })
}

check_cols_exist <- function(data, cols) {
  missing <- cols[!cols %in% names(data)]
  if (length(missing) > 0) {
    stop(sprintf("Columns not found in dataset: %s",
         paste(missing, collapse=", ")))
  }
  return(TRUE)
}

#' Ensure required directories exist
#' @return Invisible TRUE
ensure_files_exist <- function() {
  dirs <- c("data", "logs", "output", "temp", "www", "R")
  invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
  TRUE
}

#' Null coalesce operator
#' @param x First value
#' @param y Default value
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Clean up session resources
#' @param state Reactive values object containing session state
#' @return Invisible NULL
cleanup_session <- function(state) {
  tryCatch({
    # Clean up temporary files
    if (!is.null(getwd())) {
      temp_files <- list.files("temp", full.names = TRUE)
      unlink(temp_files)
    }
    
    # Clear reactive values
    if (!is.null(state)) {
      state$data <- NULL
      state$weights <- NULL
      state$diagnostics <- NULL
      state$computation_status <- "idle"
      state$data_summary <- NULL
    }
    
  }, error = function(e) {
    warning("Error in cleanup: ", e$message)
  })
  
  invisible(NULL)
}

#' Ensure Required Files and Directories Exist
#'
#' @return NULL
#' @export
ensure_files_exist <- function() {
  dirs <- c("www", "logs", "data", "R")
  sapply(dirs, dir.create, showWarnings = FALSE)
  
  # Create minimal CSS if missing
  if (!file.exists("www/styles.css")) {
    css_content <- "
    .fruit-theme { background-color: #ffffff; }
    .grapefruit-theme { background-color: #ff6b6b; }
    .plum-theme { background-color: #a06cd5; }
    .blueberry-theme { background-color: #4a90e2; }
    .avocado-theme { background-color: #7ed321; }
    .pomegranate-theme { background-color: #d0021b; }
    "
    writeLines(css_content, "www/styles.css")
  }
  
  # Create minimal JS if missing
  if (!file.exists("www/custom.js")) {
    js_content <- "
    $(document).ready(function() {
      // Theme switching logic
      $('.theme-switcher select').change(function() {
        var theme = $(this).val();
        $('#main-content').attr('class', 'fruit-theme ' + theme + '-theme');
      });
    });
    "
    writeLines(js_content, "www/custom.js")
  }
  
  # Create config if missing
  if (!file.exists("config.yml")) {
    config_content <- "
    app:
      name: 'Survey Weighting Suite'
      version: '2.14.0'
    settings:
      max_upload_size: 104857600  # 100MB
      max_iterations: 100
      default_convergence_threshold: 0.001
    "
    writeLines(config_content, "config.yml")
  }
}

#' Source Multiple Files with Error Handling
#'
#' @param files Character vector of file paths to source
#' @return NULL
#' @export
source_files <- function(files) {
  for (file in files) {
    tryCatch({
      source(file, local = TRUE)
    }, error = function(e) {
      warning(sprintf("Failed to source %s: %s", file, e$message))
    })
  }
}

#' Initialize Environment Settings
#'
#' @return NULL
#' @export
initialize_environment <- function() {
  options(
    shiny.maxRequestSize = 100 * 1024^2,
    shiny.fullstacktrace = TRUE,
    shiny.trace = TRUE
  )
  
  ensure_files_exist()
}