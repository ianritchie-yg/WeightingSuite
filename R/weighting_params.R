#' Weighting Parameters Configuration
#' Contains parameter definitions for all weighting methods

# Parameter definitions for each method
post_strat_params <- list(
    required = c("strata_vars", "population_totals"),
    optional = c("missing_handling", "zero_handling")
)

ipw_params <- list(
    required = c("treatment_var", "covariates"),
    optional = c("method", "link", "trimming")
)

calibration_params <- list(
    required = c("target_vars", "target_values"),
    optional = c("method", "bounds", "max_iter")
)

rake_params <- list(
    required = c("margin_vars", "margin_values"),
    optional = c("epsilon", "max_iter")
)

# Parameter collection functions
collect_post_strat_params <- function(input, data) {
    list(
        strata_vars = input$ps_strata_vars,
        population_totals = input$ps_population_totals,
        missing_handling = input$ps_missing_handling,
        zero_handling = input$ps_zero_handling
    )
}

collect_ipw_params <- function(input, data) {
    list(
        treatment_var = input$ipw_treatment_var,
        covariates = input$ipw_covariates,
        method = input$ipw_method,
        link = input$ipw_link,
        trimming = input$ipw_trimming
    )
}

#' Collect Calibration Parameters
collect_calibration_params <- function(input, data) {
    list(
        target_vars = input$cal_target_vars,
        target_values = input$cal_target_values,
        method = input$cal_method,
        bounds = c(input$cal_lower_bound, input$cal_upper_bound),
        max_iter = input$cal_max_iter
    )
}

#' Collect RAKE Parameters  
collect_rake_params <- function(input, data) {
    list(
        margin_vars = input$rake_margin_vars,
        margin_values = input$rake_margin_values,
        epsilon = input$rake_epsilon,
        max_iter = input$rake_max_iter
    )
}

#' Validate parameters based on weighting method
#' @param method Weighting method (e.g., "post_stratification", "ipw", "calibration", "rake")
#' @param params List of parameters
#' @return TRUE if all required parameters are present; otherwise, an error is thrown.
validate_params <- function(method, params) {
  tryCatch({
    # Normalize method to lowercase for consistency
    method <- tolower(method)
    required <- switch(method,
      "post_stratification" = post_strat_params$required,
      "ipw" = ipw_params$required, 
      "calibration" = calibration_params$required,
      "rake" = rake_params$required,
      stop("Invalid method")
    )
    
    missing <- required[!required %in% names(params)]
    if (length(missing) > 0) {
      stop(sprintf("Missing required parameters for %s: %s", 
                   method, paste(missing, collapse = ", ")))
    }
    
    return(TRUE)
  }, error = function(e) {
    log_error(sprintf("Parameter validation failed: %s", e$message))
    stop(e$message)
  })
}

#' Default Parameter Values
default_params <- list(
  post_stratification = list(
    missing_handling = "complete.cases",
    zero_handling = "collapse"
  ),
  ipw = list(
    method = "logistic",
    trim_threshold = 0.01,
    stabilize = TRUE
  ),
  calibration = list(
    method = "linear",
    max_iter = 50,
    epsilon = 1e-7
  ),
  rake = list(
    max_iter = 100,
    epsilon = 1e-6,
    cap_weights = 5,
    force_convergence = FALSE
  )
)
