utils::globalVariables(c(
  "iteration", "error", "difference", "variable", 
  "adjustment", "std_diff_before", "std_diff_after"
))

#' Plot the distribution of cases across strata
#' @param data Data frame containing strata information
#' @param strata_col Name of the strata column
#' @return ggplot object
plot_strata_distribution <- function(data, strata_col) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = strata_col)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Distribution of Cases Across Strata",
                  x = "Strata",
                  y = "Count")
}

#' Plot weights by strata
#' @param data Data frame containing weights and strata
#' @param weight_col Name of the weight column
#' @param strata_col Name of the strata column
#' @return ggplot object
plot_weight_by_strata <- function(data, weight_col, strata_col) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = strata_col, y = weight_col)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Weight Distribution by Strata",
                  x = "Strata",
                  y = "Weight")
}

#' Plot propensity score distribution
#' @param data Data frame containing propensity scores
#' @param ps_col Name of the propensity score column
#' @param group_col Name of the group column (optional)
#' @return ggplot object
plot_propensity_distribution <- function(data, ps_col, group_col = NULL) {
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = ps_col))
  if (!is.null(group_col)) {
    p <- p + ggplot2::geom_density(ggplot2::aes_string(fill = group_col), alpha = 0.5)
  } else {
    p <- p + ggplot2::geom_density()
  }
  p + ggplot2::theme_minimal() +
    ggplot2::labs(title = "Propensity Score Distribution",
                  x = "Propensity Score",
                  y = "Density")
}

#' Plot covariate balance
#' @param balance_stats Data frame containing balance statistics
#' @return ggplot object
plot_covariate_balance <- function(balance_stats) {
  ggplot2::ggplot(balance_stats,
                  ggplot2::aes_string(x = "std_diff_before", y = "std_diff_after")) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Covariate Balance Plot",
                  x = "Standardized Difference Before Weighting",
                  y = "Standardized Difference After Weighting")
}

#' Plot calibration convergence
#' @param convergence_data Data frame containing convergence information
#' @return ggplot object
plot_calibration_convergence <- function(convergence_data) {
  ggplot2::ggplot(convergence_data,
                  ggplot2::aes_string(x = "iteration", y = "error")) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Calibration Convergence",
                  x = "Iteration",
                  y = "Error")
}

#' Plot margin adjustments
#' @param margins Data frame containing margin adjustments
#' @return ggplot object
plot_margin_adjustments <- function(margins) {
  ggplot2::ggplot(margins,
                  ggplot2::aes_string(x = "variable", y = "adjustment")) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Margin Adjustments",
                  x = "Variable",
                  y = "Adjustment")
}

#' Plot raking convergence
#' @param rake_data Data frame containing raking convergence information
#' @return ggplot object
plot_rake_convergence <- function(rake_data) {
  ggplot2::ggplot(rake_data,
                  ggplot2::aes_string(x = "iteration", y = "difference")) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Raking Convergence",
                  x = "Iteration",
                  y = "Difference from Target")
}

#' Plot raking margins
#' @param margins Data frame containing raking margins
#' @return ggplot object
plot_rake_margins <- function(margins) {
  ggplot2::ggplot(margins,
                  ggplot2::aes_string(x = "variable", y = "difference")) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Raking Margins Comparison",
                  x = "Variable",
                  y = "Difference from Target")
}
