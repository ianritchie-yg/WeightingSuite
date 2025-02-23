library(R6)
library(logger)

# Error Code Constants
ERROR_CODES <- list(
  # Validation Errors
  DATA_VALIDATION = list(
    EMPTY_DATA = "ED001",
    INVALID_FORMAT = "ED002",
    MISSING_COLUMNS = "ED003",
    INVALID_VALUES = "ED004"
  ),
  
  # Computation Errors
  WEIGHT_COMPUTATION = list(
    CONVERGENCE_FAILED = "WC001",
    NEGATIVE_WEIGHTS = "WC002",
    EXTREME_WEIGHTS = "WC003",
    ITERATION_LIMIT = "WC004"
  ),
  
  # File Operations
  FILE_OPERATIONS = list(
    READ_ERROR = "FO001",
    WRITE_ERROR = "FO002",
    INVALID_PATH = "FO003",
    PERMISSION_DENIED = "FO004"
  ),
  
  # UI Validation Errors
  UI_VALIDATION = list(
    INVALID_INPUT = "UI001",
    MISSING_REQUIRED = "UI002",
    INVALID_RANGE = "UI003",
    INCOMPATIBLE_PARAMS = "UI004"
  ),
  
  # Parameter Validation Errors
  PARAMETER_VALIDATION = list(
    MISSING_REQUIRED = "PV001",
    INVALID_TYPE = "PV002",
    INVALID_RANGE = "PV003",
    INCOMPATIBLE_COMBO = "PV004"
  )
)

# Custom Error Classes
ValidationError <- R6::R6Class("ValidationError",
  public = list(
    message = NULL,
    code = NULL,
    details = NULL,
    
    initialize = function(message, code = "VAL_ERROR", details = list()) {
      self$message <- message
      self$code <- code
      self$details <- details
    }
  )
)

ComputationError <- R6::R6Class("ComputationError",
  public = list(
    message = NULL,
    code = NULL,
    stack_trace = NULL,
    
    initialize = function(message, code = "COMP_ERROR", stack_trace = NULL) {
      self$message <- message
      self$code <- code
      self$stack_trace <- stack_trace %||% sys.calls()
    }
  )
)

FileOperationError <- R6::R6Class("FileOperationError",
  public = list(
    message = NULL,
    code = NULL,
    file_path = NULL,
    
    initialize = function(message, code = "FILE_ERROR", file_path = NULL) {
      self$message <- message
      self$code <- code
      self$file_path <- file_path
    }
  )
)

WeightComputationError <- R6::R6Class("WeightComputationError",
  public = list(
    message = NULL,
    code = NULL,
    method = NULL,
    stats = NULL,
    
    initialize = function(message, code = "WEIGHT_ERROR", method = NULL, stats = list()) {
      self$message <- message
      self$code <- code
      self$method <- method
      self$stats <- stats
    }
  )
)

ParameterValidationError <- R6::R6Class("ParameterValidationError",
    public = list(
        message = NULL,
        code = NULL,
        param_name = NULL,
        expected = NULL,
        received = NULL,
        
        initialize = function(message, code = "PARAM_ERROR", param_name = NULL, 
                            expected = NULL, received = NULL) {
            self$message <- message
            self$code <- code
            self$param_name <- param_name
            self$expected <- expected
            self$received <- received
        }
    )
)

# Enhanced Validation Functions
#' Validate input data
#' @param data Input dataset
#' @return List with validation results
validate_data <- function(data) {
  tryCatch({
    # Check if data is empty
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      throw_error("Dataset is empty", ERROR_CODES$DATA_VALIDATION$EMPTY_DATA)
    }
    
    # Check for minimum required rows
    if (nrow(data) < 10) {
      throw_error("Dataset must have at least 10 rows", 
                 ERROR_CODES$DATA_VALIDATION$INSUFFICIENT_ROWS)
    }
    
    # Check for duplicate column names
    if (any(duplicated(names(data)))) {
      throw_error("Duplicate column names detected", 
                 ERROR_CODES$DATA_VALIDATION$INVALID_FORMAT)
    }
    
    # Check for excessive missing values
    missing_prop <- colMeans(is.na(data))
    if (any(missing_prop > 0.5)) {
      problem_cols <- names(which(missing_prop > 0.5))
      throw_error(sprintf("Columns with >50%% missing values: %s",
                         paste(problem_cols, collapse = ", ")),
                 ERROR_CODES$DATA_VALIDATION$INVALID_VALUES)
    }
    
    list(valid = TRUE, message = "Validation successful")
    
  }, error = function(e) {
    log_error_enhanced(e, "data_validation")
    list(valid = FALSE, message = e$message, code = e$code)
  })
}

#' Validate computed weights
#' @param weights Numeric vector of computed weights
#' @param params List of weighting parameters
#' @return List with validation results
validate_weights <- function(weights, params) {
  tryCatch({
    # Basic checks
    if (!is.numeric(weights)) {
      throw_error("Weights must be numeric", 
                 ERROR_CODES$WEIGHT_COMPUTATION$INVALID_VALUES)
    }
    
    # Statistical checks
    weight_stats <- list(
      mean = mean(weights),
      sd = sd(weights),
      min = min(weights),
      max = max(weights),
      cv = sd(weights) / mean(weights)
    )
    
    # Validation rules
    if (any(is.na(weights))) {
      throw_error("Missing values in weights", 
                 ERROR_CODES$WEIGHT_COMPUTATION$INVALID_VALUES,
                 weight_stats)
    }
    
    if (any(weights <= 0)) {
      throw_error("Negative or zero weights detected", 
                 ERROR_CODES$WEIGHT_COMPUTATION$NEGATIVE_WEIGHTS,
                 weight_stats)
    }
    
    if (weight_stats$cv > params$max_cv) {
      throw_error("Weight variation exceeds threshold", 
                 ERROR_CODES$WEIGHT_COMPUTATION$EXTREME_WEIGHTS,
                 weight_stats)
    }
    
    # Add method-specific validation
    if (!is.null(params$method)) {
        switch(params$method,
            "post_stratification" = validate_post_strat_weights(weights, params),
            "ipw" = validate_ipw_weights(weights, params),
            "calibration" = validate_calibration_weights(weights, params),
            "rake" = validate_rake_weights(weights, params)
        )
    }
    
    list(valid = TRUE,
         message = "Weight validation successful",
         stats = weight_stats)
         
  }, error = function(e) {
    log_error_enhanced(e, "weight_validation")
    list(valid = FALSE,
         message = e$message,
         code = e$code,
         stats = e$details)
  })
}

#' Enhanced error logging with levels
#' @param error Error object or message
#' @param context Error context
#' @param level Logging level (DEBUG, INFO, WARN, ERROR)
log_error_enhanced <- function(error, context = "", level = "ERROR") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  error_info <- list(
    timestamp = timestamp,
    level = level,
    context = context,
    message = if(is.character(error)) error else error$message,
    code = if(is.character(error)) NA else error$code,
    details = if(is.character(error)) NULL else error$details
  )
  
  # Format message for logging
  log_message <- sprintf("[%s] [%s] %s: %s", 
                        timestamp,
                        level,
                        context,
                        error_info$message)
  
  # Log based on level
  switch(level,
         "DEBUG" = log_debug(log_message),
         "INFO"  = log_info(log_message),
         "WARN"  = log_warn(log_message),
         "ERROR" = log_error(log_message))
  
  error_info
}

#' Attempt to recover from weight computation errors
#' @param error WeightComputationError object
#' @param data Original dataset
#' @param params Original parameters
#' @return List with recovery results
recover_weight_computation <- function(error, data, params) {
  tryCatch({
    recovery_action <- switch(error$code,
      "WC001" = {
        # Convergence failure recovery
        new_params <- params
        new_params$convergence_threshold <- params$convergence_threshold * 1.5
        new_params$max_iterations <- params$max_iterations * 2
        list(action = "retry", params = new_params)
      },
      "WC002" = {
        # Negative weights recovery
        new_params <- params
        new_params$lower_bound <- 0.1
        list(action = "retry", params = new_params)
      },
      "WC003" = {
        # Extreme weights recovery
        new_params <- params
        new_params$trim_threshold <- 0.95
        list(action = "trim", params = new_params)
      },
      list(action = "abort", message = "Unrecoverable error")
    )
    
    log_error_enhanced(sprintf("Recovery attempt for %s: %s", 
                             error$code, 
                             recovery_action$action),
                      "weight_recovery",
                      "INFO")
    
    recovery_action
    
  }, error = function(e) {
    log_error_enhanced(e, "recovery_attempt")
    list(action = "abort", 
         message = "Recovery attempt failed")
  })
}

#' Custom error throwing function
#' @param message Error message
#' @param code Error code
#' @param details Additional error details
throw_error <- function(message, code = NULL, details = NULL) {
  error <- structure(
    list(
      message = message,
      code = code,
      details = details,
      timestamp = Sys.time()
    ),
    class = c("survey_weight_error", "error", "condition")
  )
  stop(error)
}

# Add parameter validation function
validate_weighting_params <- function(method, params) {
    tryCatch({
        # Get required parameters for method
        required_params <- switch(method,
            "post_stratification" = post_strat_params$required,
            "ipw" = ipw_params$required,
            "calibration" = calibration_params$required,
            "rake" = rake_params$required,
            throw_error("Invalid weighting method", 
                       ERROR_CODES$PARAMETER_VALIDATION$INVALID_TYPE)
        )
        
        # Check for missing required parameters
        missing_params <- setdiff(required_params, names(params))
        if (length(missing_params) > 0) {
            throw_error(
                sprintf("Missing required parameters: %s", 
                        paste(missing_params, collapse = ", ")),
                ERROR_CODES$PARAMETER_VALIDATION$MISSING_REQUIRED,
                list(method = method, missing = missing_params)
            )
        }
        
        # Validate parameter types and ranges
        validate_param_types(method, params)
        validate_param_ranges(method, params)
        
        list(valid = TRUE, message = "Parameter validation successful")
        
    }, error = function(e) {
        log_error_enhanced(e, "parameter_validation")
        list(valid = FALSE, message = e$message, code = e$code)
    })
}

# Add helper functions for parameter validation
validate_param_types <- function(method, params) {
    # Implementation depends on method-specific requirements
    # Add type checking logic here
}

validate_param_ranges <- function(method, params) {
    # Implementation depends on method-specific requirements
    # Add range validation logic here
}

# Add method-specific weight validation functions
validate_post_strat_weights <- function(weights, params) {
    # Implementation for post-stratification weights
}

validate_ipw_weights <- function(weights, params) {
    # Implementation for IPW weights
}

validate_calibration_weights <- function(weights, params) {
    # Implementation for calibration weights
}

validate_rake_weights <- function(weights, params) {
    # Implementation for RAKE weights
}