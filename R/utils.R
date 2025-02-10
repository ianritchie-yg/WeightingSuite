# R/utils.R
# Utility functions for Survey Weighting Suite

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

#' Clean and prepare dataset
#' @param data Input dataset
#' @return Cleaned dataset
clean_dataset <- function(data) {
  # Remove completely empty rows and columns
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  data <- data[, colSums(is.na(data)) != nrow(data)]
  
  # Convert character columns to factors where appropriate
  char_cols <- sapply(data, is.character)
  unique_vals <- sapply(data[char_cols], function(x) length(unique(x)))
  factor_cols <- names(unique_vals[unique_vals < nrow(data) * 0.5])
  
  data[factor_cols] <- lapply(data[factor_cols], factor)
  
  # Handle date columns
  date_cols <- identify_date_columns(data)
  for (col in date_cols) {
    data[[col]] <- as.Date(data[[col]])
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

#' Generate summary statistics for dataset
#' @param data Input dataset
#' @return List of summary statistics
generate_data_summary <- function(data) {
  list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    col_types = sapply(data, class),
    missing_summary = colSums(is.na(data)),
    numeric_summary = lapply(data[sapply(data, is.numeric)], summary),
    factor_summary = lapply(data[sapply(data, is.factor)], table)
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