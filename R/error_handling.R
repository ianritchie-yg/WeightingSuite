# R/error_handling.R
# Error handling utilities for Survey Weighting Suite

#' Custom error class for validation errors
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

#' Custom error class for computation errors
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

#' Validate input data
#' @param data Input dataset
#' @return List with validation results
validate_data <- function(data) {
  tryCatch({
    # Check if data is empty
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(list(
        valid = FALSE,
        message = "Dataset is empty",
        code = "EMPTY_DATA"
      ))
    }
    
    # Check for minimum required rows
    if (nrow(data) < 10) {
      return(list(
        valid = FALSE,
        message = "Dataset must have at least 10 rows",
        code = "INSUFFICIENT_ROWS"
      ))
    }
    
    # Check for duplicate column names
    if (any(duplicated(names(data)))) {
      return(list(
        valid = FALSE,
        message = "Duplicate column names detected",
        code = "DUPLICATE_COLS"
      ))
    }
    
    # Check for excessive missing values
    missing_prop <- colMeans(is.na(data))
    if (any(missing_prop > 0.5)) {
      problem_cols <- names(which(missing_prop > 0.5))
      return(list(
        valid = FALSE,
        message = sprintf("Columns with >50%% missing values: %s",
                         paste(problem_cols, collapse = ", ")),
        code = "EXCESSIVE_MISSING"
      ))
    }
    
    # All checks passed
    return(list(
      valid = TRUE,
      message = "Validation successful",
      code = "VALID"
    ))
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = sprintf("Validation error: %s", e$message),
      code = "VALIDATION_ERROR"
    ))
  })
}

#' Validate weighting parameters
#' @param params List of parameters
#' @param method Weighting method
#' @return List with validation results
validate_parameters <- function(params, method) {
  tryCatch({
    # Common parameter validation
    if (params$max_iterations <= 0) {
      return(list(
        valid = FALSE,
        message = "Maximum iterations must be positive",
        code = "INVALID_MAX_ITER"
      ))
    }
    
    if (params$convergence_threshold <= 0) {
      return(list(
        valid = FALSE,
        message = "Convergence threshold must be positive",
        code = "INVALID_THRESHOLD"
      ))
    }
    
    # Method-specific validation
    switch(method,
      "post_strat" = {
        if (length(params$strata_vars) == 0) {
          return(list(
            valid = FALSE,
            message = "No stratification variables selected",
            code = "NO_STRATA_VARS"
          ))
        }
      },
      "raking" = {
        if (length(params$margin_vars) == 0) {
          return(list(
            valid = FALSE,
            message = "No margin variables selected",
            code = "NO_MARGIN_VARS"
          ))
        }
      },
      "calibration" = {
        if (length(params$calibration_vars) == 0) {
          return(list(
            valid = FALSE,
            message = "No calibration variables selected",
            code = "NO_CALIB_VARS"
          ))
        }
        if (params$lower_bound >= params$upper_bound) {
          return(list(
            valid = FALSE,
            message = "Lower bound must be less than upper bound",
            code = "INVALID_BOUNDS"
          ))
        }
      },
      "ipw" = {
        if (is.null(params$treatment_var)) {
          return(list(
            valid = FALSE,
            message = "No treatment variable selected",
            code = "NO_TREAT_VAR"
          ))
        }
        if (length(params$covariates) == 0) {
          return(list(
            valid = FALSE,
            message = "No covariates selected",
            code = "NO_COVARIATES"
          ))
        }
      }
    )
    
    # All checks passed
    return(list(
      valid = TRUE,
      message = "Parameter validation successful",
      code = "VALID"
    ))
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = sprintf("Parameter validation error: %s", e$message),
      code = "PARAM_VALIDATION_ERROR"
    ))
  })
}

#' Handle and log errors
#' @param e Error object
#' @param context Error context
log_error <- function(e, context = "") {
  error_time <- Sys.time()
  error_details <- list(
    timestamp = error_time,
    context = context,
    message = e$message,
    call = deparse(e$call),
    stack_trace = sys.calls()
  )
  
  # Log error
  log_error(sprintf("[%s] %s: %s",
                    context,
                    class(e)[1],
                    e$message))
  
  # Return error details
  return(error_details)
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