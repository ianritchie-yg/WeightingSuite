utils::globalVariables(c(
  "weights", "covariates", "constraints"
))

#' Calculate weight efficiency
#' @param weights Numeric vector of weights
#' @return Numeric value representing weight efficiency
calculate_weight_efficiency <- function(weights) {
  n <- length(weights)
  sum(weights)^2 / (n * sum(weights^2))
}

#' Calculate design effect
#' @param weights Numeric vector of weights
#' @return Numeric value representing design effect
calculate_design_effect <- function(weights) {
  n <- length(weights)
  (n * sum(weights^2)) / (sum(weights)^2)
}

#' Evaluate convergence of iterative procedures
#' @param current Current values
#' @param previous Previous values
#' @param tolerance Convergence tolerance
#' @return List containing convergence status and difference
evaluate_convergence <- function(current, previous, tolerance = 1e-6) {
  diff <- max(abs(current - previous))
  list(
    converged = diff < tolerance,
    difference = diff
  )
}

#' Calculate balance score for propensity score weighting
#' @param weights Numeric vector of weights
#' @param covariates Matrix or data frame of covariates
#' @return Numeric value representing balance score
calculate_balance_score <- function(weights, covariates) {
  # Implement balance score calculation
  # Example: Calculate standardized mean differences
  weighted_means <- colMeans(sweep(covariates, 1, weights, "*"))
  unweighted_means <- colMeans(covariates)
  mean(abs(weighted_means - unweighted_means))
}

#' Calculate method-specific metrics
#' @param weights Numeric vector of weights
#' @param method Character string specifying the weighting method
#' @param additional_params Additional parameters specific to the method
#' @return List of method-specific quality metrics
calculate_method_specific_metrics <- function(weights, method, additional_params = list()) {
  base_metrics <- list(
    efficiency = calculate_weight_efficiency(weights),
    deff = calculate_design_effect(weights),
    cv = sd(weights) / mean(weights),
    range = range(weights)
  )
  
  # Normalize method string to lowercase for matching
  method_lower <- tolower(method)
  
  method_metrics <- switch(method_lower,
    "raking" = {
      list(
        margin_deviation = calculate_margin_deviation(weights,
                                                      additional_params$target_margins)
      )
    },
    "calibration" = {
      list(
        constraint_satisfaction = check_constraints(weights,
                                                    additional_params$constraints)
      )
    },
    "ipw" = {
      list(
        balance_score = calculate_balance_score(weights,
                                                  additional_params$covariates)
      )
    },
    list()  # Default empty list for other methods
  )
  c(base_metrics, method_metrics)
}
