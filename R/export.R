# R/export.R
# Export functions for Survey Weighting Suite

#' Export results to various formats
#' @param data Original dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @param method Weighting method used
#' @param params Parameters used for weighting
#' @param format Export format
#' @param include_diagnostics Include diagnostics in export
#' @param filename Output filename
#' @return Invisible TRUE if successful
export_results <- function(data, weights, diagnostics, method, params, 
                           format = "CSV", include_diagnostics = TRUE, filename) {
    
    result_data <- data
    result_data$weight <- weights
    
    # Add method-specific columns
    result_data <- add_method_specific_columns(result_data, method, params)
    
    if (include_diagnostics) {
        diag_cols <- add_diagnostic_columns(data.frame(), diagnostics)
        result_data <- cbind(result_data, diag_cols)
    }
    
    switch(format,
           "CSV" = write.csv(result_data, filename, row.names = FALSE),
           "RDS" = saveRDS(result_data, filename),
           "STATA" = haven::write_dta(result_data, filename)
    )
    
    filename
}

#' Add diagnostic columns to dataset
#' @param data Dataset
#' @param diagnostics Diagnostic information
#' @return Updated dataset
add_diagnostic_columns <- function(data, diagnostics) {
  # Add relevant diagnostic information as columns
  if (!is.null(diagnostics$individual_error)) {
    data$convergence_error <- diagnostics$individual_error
  }
  
  if (!is.null(diagnostics$influence)) {
    data$influence_score <- diagnostics$influence
  }
  
  if (!is.null(diagnostics$leverage)) {
    data$leverage_score <- diagnostics$leverage
  }
  
  return(data)
}

#' Add method-specific columns to dataset
#' @param data Dataset
#' @param method Weighting method
#' @param params Parameters used
#' @return Updated dataset
add_method_specific_columns <- function(data, method, params) {
  switch(method,
    "Post-Stratification" = {
      data$strata <- do.call(paste, data[params$strata_vars])
    },
    "IPW" = {
      data$ps_score <- params$propensity_scores
    },
    "Calibration" = {
      data$g_weights <- params$g_weights
    },
    "RAKE" = {
      data$iteration_count <- params$iterations
    }
  )
  return(data)
}

#' Export results to Excel
#' @param data Dataset with weights
#' @param diagnostics Diagnostic information
#' @param filename Output filename
export_to_excel <- function(data, diagnostics, filename) {
  wb <- createWorkbook()
  
  # Add main data sheet
  addWorksheet(wb, "Weighted_Data")
  writeData(wb, "Weighted_Data", data)
  
  # Add summary statistics
  addWorksheet(wb, "Summary_Statistics")
  writeData(wb, "Summary_Statistics",
           generate_summary_statistics(data$weight, diagnostics))
  
  # Add convergence information
  if (!is.null(diagnostics$convergence)) {
    addWorksheet(wb, "Convergence_History")
    writeData(wb, "Convergence_History", diagnostics$convergence)
  }
  
  # Add metadata
  addWorksheet(wb, "Metadata")
  metadata <- data.frame(
    Item = c("Creation Date",
             "Software Version",
             "Method Used",
             "Number of Cases",
             "Number of Variables"),
    Value = c(as.character(Sys.time()),
              "1.0.0",
              diagnostics$method,
              nrow(data),
              ncol(data))
  )
  writeData(wb, "Metadata", metadata)
  
  # Save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
}

#' Export results to SPSS
#' @param data Dataset with weights
#' @param filename Output filename
export_to_spss <- function(data, filename) {
  # Add variable labels
  var_labels <- sapply(names(data), function(x) x)
  names(var_labels) <- names(data)
  
  # Add value labels for factors
  val_labels <- list()
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      val_labels[[col]] <- setNames(
        as.numeric(levels(data[[col]])),
        levels(data[[col]])
      )
    }
  }
  
  # Write to SPSS format
  write_sav(data,
            filename,
            variable.labels = var_labels,
            value.labels = val_labels)
}

#' Generate R markdown report
#' @param data Dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @param output_file Output filename
#' @param template Template file
generate_report <- function(data, weights, diagnostics,
                            output_file, template = "report_template.Rmd") {
  # Prepare report parameters
  params <- list(
    date = Sys.Date(),
    data = data,
    weights = weights,
    diagnostics = diagnostics,
    plots = list(
      weight_dist = create_weight_distribution_plot(weights),
      convergence = create_convergence_plot(diagnostics$convergence)
    ),
    summary_stats = generate_summary_statistics(weights, diagnostics)
  )
  
  # Render report
  rmarkdown::render(
    input = template,
    output_file = output_file,
    params = params,
    quiet = TRUE
  )
}

#' Generate weighted summary report
#' @param data Original dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @param method Weighting method used
#' @param params Parameters used
#' @param output_format Output format ("pdf" or "html")
#' @return Path to generated report
generate_weight_report <- function(data, weights, diagnostics, method, params,
                                   output_format = "html") {
  report_template <- create_report_template(method, params)
  
  # Generate plots
  weight_dist_plot <- create_weight_distribution_plot(weights)
  conv_plot <- create_convergence_plot(diagnostics$convergence)
  method_plots <- generate_method_specific_plots(method, weights, params)
  
  # Calculate metrics
  quality_metrics <- calculate_quality_metrics(weights, diagnostics, method)
  
  # Render report
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(report_template, temp_rmd)
  
  rmarkdown::render(temp_rmd,
                    output_format = paste0("html_document"),
                    output_file = file.path(tempdir(), "weight_report.html"),
                    params = list(
                      data = data,
                      weights = weights,
                      diagnostics = diagnostics,
                      plots = list(
                        weight_dist = weight_dist_plot,
                        convergence = conv_plot,
                        method_specific = method_plots
                      ),
                      metrics = quality_metrics
                    ))
}

#' Create method-specific visualizations
#' @param method Weighting method
#' @param data Dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
create_method_plots <- function(method, data, weights, diagnostics) {
  base_plots <- list(
    weight_dist = create_weight_distribution_plot(weights),
    diagnostic_plots = create_diagnostic_plots(weights, diagnostics)
  )
  
  method_plots <- switch(method,
    "post_stratification" = list(
      strata_dist = create_strata_distribution_plot(data, weights),
      coverage_plot = create_coverage_plot(diagnostics$strata_coverage)
    ),
    "ipw" = list(
      propensity_plot = create_propensity_score_plot(diagnostics$propensity_scores),
      balance_plot = create_balance_plot(diagnostics$balance_statistics)
    ),
    "calibration" = list(
      convergence_plot = create_convergence_plot(diagnostics$convergence),
      margin_plot = create_margin_plot(diagnostics$margins)
    ),
    "rake" = list(
      iteration_plot = create_iteration_plot(diagnostics$iterations),
      margin_adjustment = create_margin_adjustment_plot(diagnostics$adjustments)
    )
  )
  
  return(c(base_plots, method_plots))
}

generate_method_specific_plots <- function(method, weights, params) {
  switch(method,
    "Post-Stratification" = plot_strata_distribution(weights, params),
    "IPW" = plot_propensity_distribution(params$propensity_scores),
    "Calibration" = plot_calibration_convergence(params$convergence),
    "RAKE" = plot_rake_convergence(params$iterations)
  )
}

#' Generate method-specific summary statistics
#' @param method Weighting method
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
generate_method_summary <- function(method, weights, diagnostics) {
  base_stats <- generate_basic_weight_stats(weights)
  
  method_stats <- switch(method,
    "post_stratification" = list(
      strata_counts = diagnostics$strata_counts,
      coverage_stats = diagnostics$strata_coverage
    ),
    "ipw" = list(
      propensity_stats = diagnostics$propensity_summary,
      balance_measures = diagnostics$balance_measures
    ),
    "calibration" = list(
      convergence_stats = diagnostics$convergence_summary,
      margin_errors = diagnostics$margin_errors
    ),
    "rake" = list(
      iteration_stats = diagnostics$iteration_summary,
      final_margins = diagnostics$final_margins
    )
  )
  
  return(list(
    base_statistics = base_stats,
    method_statistics = method_stats
  ))
}

calculate_quality_metrics <- function(weights, diagnostics, method) {
  base_metrics <- list(
    efficiency = calculate_weight_efficiency(weights),
    design_effect = calculate_design_effect(weights)
  )
  
  method_metrics <- switch(method,
    "Post-Stratification" = list(
      strata_balance = evaluate_strata_balance(diagnostics)
    ),
    "IPW" = list(
      covariate_balance = evaluate_covariate_balance(diagnostics)
    ),
    "Calibration" = list(
      margin_error = evaluate_margin_error(diagnostics)
    ),
    "RAKE" = list(
      convergence = evaluate_convergence(diagnostics)
    )
  )
  
  return(c(base_metrics, method_metrics))
}

#' Enhanced report generation with method-specific sections
#' @param data Dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @param method Weighting method
#' @param params Method parameters
#' @param output_format Output format
generate_enhanced_report <- function(data, weights, diagnostics, 
                                     method, params, output_format = "html") {
  # Generate visualizations and statistics
  plots <- create_method_plots(method, data, weights, diagnostics)
  summary_stats <- generate_method_summary(method, weights, diagnostics)
  
  # Create method-specific RMarkdown content
  method_section <- switch(method,
    "post_stratification" = create_post_strat_section(),
    "ipw" = create_ipw_section(),
    "calibration" = create_calibration_section(),
    "rake" = create_rake_section()
  )
  
  # Combine with base template
  rmd_content <- create_report_template(
    method = method,
    method_section = method_section,
    plots = plots,
    summary_stats = summary_stats,
    params = params
  )
  
  # Generate report
  output_file <- generate_report_file(
    rmd_content = rmd_content,
    output_format = output_format,
    plots = plots,
    summary_stats = summary_stats,
    params = params
  )
  
  return(output_file)
}