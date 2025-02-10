# R/visualizations.R
# Visualization functions for Survey Weighting Suite

#' Create weight distribution plot
#' @param weights Vector of weights
#' @return plotly object
create_weight_distribution_plot <- function(weights) {
  plot_data <- data.frame(weight = weights)
  
  p <- plot_ly(plot_data, x = ~weight, type = "histogram",
               nbinsx = 30, name = "Weight Distribution") %>%
    layout(
      title = list(
        text = "Distribution of Weights",
        font = list(size = 16)
      ),
      xaxis = list(title = "Weight Value"),
      yaxis = list(title = "Frequency"),
      showlegend = FALSE,
      margin = list(t = 50)
    ) %>%
    add_annotations(
      text = sprintf("Mean: %.2f<br>SD: %.2f",
                    mean(weights, na.rm = TRUE),
                    sd(weights, na.rm = TRUE)),
      x = 0.95,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      align = "right"
    )
  
  return(p)
}

#' Create convergence plot
#' @param convergence_data Convergence metrics data
#' @return plotly object
create_convergence_plot <- function(convergence_data) {
  p <- plot_ly(convergence_data, x = ~iteration, y = ~error,
               type = "scatter", mode = "lines+markers",
               name = "Convergence") %>%
    layout(
      title = list(
        text = "Convergence History",
        font = list(size = 16)
      ),
      xaxis = list(title = "Iteration"),
      yaxis = list(
        title = "Error",
        type = "log"
      ),
      showlegend = FALSE,
      margin = list(t = 50)
    )
  
  return(p)
}

#' Create diagnostic plots
#' @param data Original dataset
#' @param weights Computed weights
#' @param vars Variables to plot
#' @return List of plotly objects
create_diagnostic_plots <- function(data, weights, vars) {
  plots <- list()
  
  for (var in vars) {
    if (is.numeric(data[[var]])) {
      plots[[var]] <- create_numeric_diagnostic_plot(data[[var]], weights, var)
    } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
      plots[[var]] <- create_categorical_diagnostic_plot(data[[var]], weights, var)
    }
  }
  
  return(plots)
}

#' Create numeric variable diagnostic plot
#' @param var_data Variable data
#' @param weights Weights
#' @param var_name Variable name
#' @return plotly object
create_numeric_diagnostic_plot <- function(var_data, weights, var_name) {
  plot_data <- data.frame(
    value = var_data,
    weight = weights
  )
  
  p <- plot_ly() %>%
    add_histogram(data = plot_data, x = ~value, name = "Unweighted",
                 opacity = 0.5) %>%
    add_histogram(data = plot_data, x = ~value, weights = ~weight,
                 name = "Weighted", opacity = 0.5) %>%
    layout(
      title = list(
        text = sprintf("Distribution of %s", var_name),
        font = list(size = 16)
      ),
      xaxis = list(title = var_name),
      yaxis = list(title = "Frequency"),
      barmode = "overlay",
      margin = list(t = 50)
    )
  
  return(p)
}

#' Create categorical variable diagnostic plot
#' @param var_data Variable data
#' @param weights Weights
#' @param var_name Variable name
#' @return plotly object
create_categorical_diagnostic_plot <- function(var_data, weights, var_name) {
  unweighted_props <- prop.table(table(var_data))
  weighted_props <- prop.table(tapply(weights, var_data, sum))
  
  plot_data <- data.frame(
    category = names(unweighted_props),
    unweighted = as.numeric(unweighted_props),
    weighted = as.numeric(weighted_props)
  )
  
  p <- plot_ly() %>%
    add_trace(data = plot_data, x = ~category, y = ~unweighted,
              type = "bar", name = "Unweighted") %>%
    add_trace(data = plot_data, x = ~category, y = ~weighted,
              type = "bar", name = "Weighted") %>%
    layout(
      title = list(
        text = sprintf("Distribution of %s", var_name),
        font = list(size = 16)
      ),
      xaxis = list(title = var_name),
      yaxis = list(
        title = "Proportion",
        tickformat = ".1%"
      ),
      barmode = "group",
      margin = list(t = 50)
    )
  
  return(p)
}

#' Create summary statistics table
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @return data.frame
generate_summary_statistics <- function(weights, diagnostics) {
  stats <- data.frame(
    Metric = c(
      "Sample Size",
      "Mean Weight",
      "Median Weight",
      "SD of Weights",
      "Min Weight",
      "Max Weight",
      "CV of Weights",
      "Design Effect",
      "Effective Sample Size",
      "Number of Iterations",
      "Final Convergence Error"
    ),
    Value = c(
      length(weights),
      mean(weights),
      median(weights),
      sd(weights),
      min(weights),
      max(weights),
      sd(weights) / mean(weights),
      1 + var(weights) / (mean(weights)^2),
      length(weights) / (1 + var(weights) / (mean(weights)^2)),
      diagnostics$iterations,
      diagnostics$final_error
    )
  )
  
  # Format numeric values
  stats$Value <- format(stats$Value, digits = 3, scientific = FALSE)
  
  return(stats)
}