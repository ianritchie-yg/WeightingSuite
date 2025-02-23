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
    geom_bar() +
    theme_minimal() +
    labs(title = "Distribution of Cases Across Strata",
         x = "Strata",
         y = "Count")
}

#' Plot weights by strata
#' @param data Data frame containing weights and strata
#' @param weight_col Name of the weight column
#' @param strata_col Name of the strata column
#' @return ggplot object
plot_weight_by_strata <- function(data, weight_col, strata_col) {
  ggplot2::ggplot(data, aes_string(x = strata_col, y = weight_col)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Weight Distribution by Strata",
         x = "Strata",
         y = "Weight")
}

#' Plot propensity score distribution
#' @param data Data frame containing propensity scores
#' @param ps_col Name of the propensity score column
#' @param group_col Name of the group column (optional)
#' @return ggplot object
plot_propensity_distribution <- function(data, ps_col, group_col = NULL) {
  p <- ggplot2::ggplot(data, aes_string(x = ps_col))
  if (!is.null(group_col)) {
    p <- p + geom_density(aes_string(fill = group_col), alpha = 0.5)
  } else {
    p <- p + geom_density()
  }
  p + theme_minimal() +
    labs(title = "Propensity Score Distribution",
         x = "Propensity Score",
         y = "Density")
}

#' Plot covariate balance
#' @param balance_stats Data frame containing balance statistics
#' @return ggplot object
plot_covariate_balance <- function(balance_stats) {
  ggplot2::ggplot(balance_stats,
                  aes(x = std_diff_before, y = std_diff_after)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Covariate Balance Plot",
         x = "Standardized Difference Before Weighting",
         y = "Standardized Difference After Weighting")
}

#' Plot calibration convergence
#' @param convergence_data Data frame containing convergence information
#' @return ggplot object
plot_calibration_convergence <- function(convergence_data) {
  ggplot2::ggplot(convergence_data,
                  aes(x = iteration, y = error)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Calibration Convergence",
         x = "Iteration",
         y = "Error")
}

#' Plot margin adjustments
#' @param margins Data frame containing margin adjustments
#' @return ggplot object
plot_margin_adjustments <- function(margins) {
  ggplot2::ggplot(margins,
                  aes(x = variable, y = adjustment)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Margin Adjustments",
         x = "Variable",
         y = "Adjustment")
}

#' Plot raking convergence
#' @param rake_data Data frame containing raking convergence information
#' @return ggplot object
plot_rake_convergence <- function(rake_data) {
  ggplot2::ggplot(rake_data,
                  aes(x = iteration, y = difference)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Raking Convergence",
         x = "Iteration",
         y = "Difference from Target")
}

#' Plot raking margins
#' @param margins Data frame containing raking margins
#' @return ggplot object
plot_rake_margins <- function(margins) {
  ggplot2::ggplot(margins,
                  aes_string(x = "variable", y = "difference")) +
    geom_col() +
    theme_minimal() +
    labs(title = "Raking Margins Comparison",
         x = "Variable",
         y = "Difference from Target")
}
