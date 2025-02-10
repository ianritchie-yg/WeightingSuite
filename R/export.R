# R/export.R
# Export functions for Survey Weighting Suite

#' Export results to various formats
#' @param data Original dataset
#' @param weights Computed weights
#' @param diagnostics Diagnostic information
#' @param format Export format
#' @param include_diagnostics Include diagnostics in export
#' @param filename Output filename
#' @return Invisible TRUE if successful
export_results <- function(data, weights, diagnostics, format,
                          include_diagnostics = TRUE, filename) {
  tryCatch({
    # Add weights to dataset
    result_data <- data
    result_data$weight <- weights
    
    # Add diagnostic information if requested
    if (include_diagnostics) {
      result_data <- add_diagnostic_columns(result_data, diagnostics)
    }
    
    # Export based on format
    switch(format,
      "csv" = {
        write_csv(result_data, filename)
      },
      "xlsx" = {
        export_to_excel(result_data, diagnostics, filename)
      },
      "sav" = {
        export_to_spss(result_data, filename)
      },
      "rds" = {
        saveRDS(list(
          data = result_data,
          diagnostics = diagnostics
        ), filename)
      },
      stop("Unsupported export format")
    )
    
    invisible(TRUE)
    
  }, error = function(e) {
    log_error(e, "export_results")
    stop(sprintf("Export failed: %s", e$message))
  })
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