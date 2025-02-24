#' Generate Input UI Elements Based on Parameter Type
#' @param id The input ID
#' @param label The label for the input
#' @param param_type The type of parameter
#' @param choices Optional choices for select inputs
generate_param_input <- function(id, label, param_type, choices = NULL) {
  if (param_type == "logical") {
    switchInput(
      inputId = id,
      label = label,
      value = FALSE,
      onLabel = "Yes",
      offLabel = "No"
    )
  } else if (param_type == "select" && !is.null(choices)) {
    selectInput(
      inputId = id,
      label = label,
      choices = choices
    )
  } else if (param_type == "numeric") {
    numericInput(
      inputId = id,
      label = label,
      value = 0
    )
  } else {
    textInput(
      inputId = id,
      label = label,
      value = ""
    )
  }
}

#' Generate Post-Stratification UI Elements
generate_post_strat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("strata_vars"), "Stratification Variables", 
                   choices = NULL, multiple = TRUE),
    textAreaInput(ns("population_totals"), "Population Totals"),
    selectInput(ns("missing_handling"), "Missing Value Handling",
                choices = c("remove", "separate_category")),
    selectInput(ns("zero_handling"), "Zero Cell Handling",
                choices = c("collapse", "ignore"))
  )
}

#' Generate IPW UI Elements
generate_ipw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("treatment_var"), "Treatment Variable", choices = NULL),
    selectizeInput(ns("covariates"), "Covariates", choices = NULL, multiple = TRUE),
    selectInput(ns("method"), "Method", 
                choices = c("logistic", "probit")),
    numericInput(ns("trimming"), "Weight Trimming", value = 0.01)
  )
}

#' Generate Calibration UI Elements
generate_calibration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("target_vars"), "Target Variables", choices = NULL, multiple = TRUE),
    textAreaInput(ns("target_values"), "Target Values"),
    selectInput(ns("method"), "Calibration Method",
                choices = c("linear", "raking", "logit")),
    numericInput(ns("lower_bound"), "Lower Bound", value = 0),
    numericInput(ns("upper_bound"), "Upper Bound", value = Inf),
    numericInput(ns("max_iter"), "Maximum Iterations", value = 50)
  )
}

#' Generate RAKE UI Elements
generate_rake_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("margin_vars"), "Margin Variables", choices = NULL, multiple = TRUE),
    textAreaInput(ns("margin_values"), "Margin Values"),
    numericInput(ns("epsilon"), "Convergence Threshold", value = 1e-6),
    numericInput(ns("max_iter"), "Maximum Iterations", value = 100)
  )
}

#' Generate Export Options UI
generate_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("format"), "Export Format",
                choices = c("CSV", "RDS", "STATA")),
    checkboxInput(ns("include_diagnostics"), "Include Diagnostics", value = TRUE),
    downloadButton(ns("download"), "Export Results"),
    downloadButton(ns("report"), "Generate Report")
  )
}

#' Generate General Settings UI
generate_settings_ui <- function(id) {
  tagList(
    selectInput(
      inputId = paste0(id, "_theme"),
      label = "Color Theme",
      choices = c("Blueberry", "Grapefruit", "Plum", "Avocado", "Pomegranate")
    ),
    numericInput(
      inputId = paste0(id, "_decimal_places"),
      label = "Decimal Places",
      value = 2,
      min = 0,
      max = 10
    ),
    switchInput(
      inputId = paste0(id, "_show_progress"),
      label = "Show Progress Bars",
      value = TRUE
    )
  )
}

#' Update UI Element Choices Based on Available Data
#' @param session The current session
#' @param data The dataset
#' @param prefix The prefix for input IDs
update_variable_choices <- function(session, data, prefix) {
  choices <- names(data)
  updateSelectInput(session, paste0(prefix, "_strata_vars"), choices = choices)
  updateSelectInput(session, paste0(prefix, "_treatment_var"), choices = choices)
  updateSelectInput(session, paste0(prefix, "_covariates"), choices = choices)
  updateSelectInput(session, paste0(prefix, "_calibration_vars"), choices = choices)
  updateSelectInput(session, paste0(prefix, "_margin_vars"), choices = choices)
}

#' Update UI Choices for Calibration and RAKE
update_ui_choices <- function(session, data) {
  numeric_vars <- names(which(sapply(data, is.numeric)))
  factor_vars <- names(which(sapply(data, is.factor)))
  
  # Corrected IDs to match those defined in the UI functions
  updateSelectizeInput(session, "target_vars", choices = numeric_vars)
  updateSelectizeInput(session, "margin_vars", choices = factor_vars)
}

#' Generate spinner for waiter
#' @return HTML for spinner
spin_flower <- function() {
  HTML('<div class="flower-spinner">
         <div class="dots-container">
           <div class="bigger-dot">
             <div class="smaller-dot"></div>
           </div>
         </div>
       </div>')
}

#' Generate parameter UI based on method
#' @param method Weighting method
#' @return UI elements
generate_parameter_ui <- function(method) {
  switch(method,
    "post_stratification" = generate_post_strat_ui(),
    "ipw" = generate_ipw_ui(),
    "calibration" = generate_calibration_ui(),
    "rake" = generate_rake_ui(),
    NULL
  )
}

# Individual UI generation functions
generate_post_strat_ui <- function() {
  tagList(
    selectInput("strata_vars", "Stratification Variables", 
                choices = NULL, multiple = TRUE),
    numericInput("max_strata", "Maximum Strata", 
                 value = 100, min = 1),
    actionButton("compute_post_strat", "Compute Weights")
  )
}
