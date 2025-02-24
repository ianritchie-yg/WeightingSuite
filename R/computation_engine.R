ComputationEngine <- R6::R6Class("ComputationEngine",
  public = list(
    initialize = function() {
      private$reset_state()
    },
    
    execute_weighting = function(data, method, params) {
      private$validate_inputs(data, method, params)
      private$set_status("running")
      
      result <- tryCatch({
        switch(method,
          "post_stratification" = private$run_post_stratification(data, params),
          "raking" = private$run_raking(data, params),
          "calibration" = private$run_calibration(data, params),
          "ipw" = private$run_ipw(data, params),
          stop("Unknown weighting method")
        )
      }, error = function(e) {
        private$handle_error(e)
        return(NULL)
      })
      
      if (!is.null(result)) {
        private$set_status("completed")
        private$log_completion(result)
      }
      
      result
    }
  ),
  
  private = list(
    status = "idle",
    error_log = list(),
    computation_log = list(),
    
    reset_state = function() {
      self$status <- "idle"
      self$error_log <- list()
      self$computation_log <- list()
    },
    
    validate_inputs = function(data, method, params) {
      validation <- validate_inputs(data, params)
      if (!validation$valid) {
        stop(paste("Validation failed:", paste(validation$errors, collapse = "; ")))
      }
    },
    
    run_post_stratification = function(data, params) {
      shiny::withProgress(message = 'Performing post-stratification', value = 0, {
        result <- list(
          weights = compute_post_strat_weights(data, params),
          diagnostics = compute_diagnostics(data, weights),
          convergence = check_convergence(weights, params)
        )
        shiny::incProgress(1)
        result
      })
    },
    
    run_raking = function(data, params) {
      shiny::withProgress(message = 'Performing raking', value = 0, {
        weights <- numeric(nrow(data))
        max_iter <- params$max_iterations
        
        for (i in 1:max_iter) {
          shiny::incProgress(i/max_iter)
          weights_new <- update_raking_weights(data, weights, params)
          
          if (check_convergence(weights_new, weights, params$tolerance)) {
            break
          }
          weights <- weights_new
        }
        
        list(
          weights = weights,
          diagnostics = compute_diagnostics(data, weights),
          convergence = list(iterations = i, achieved = i < max_iter)
        )
      })
    },
    
    run_calibration = function(data, params) {
      shiny::withProgress(message = 'Performing calibration', value = 0, {
        result <- calibrate_weights(data, params)
        shiny::incProgress(1)
        result
      })
    },
    
    run_ipw = function(data, params) {
      shiny::withProgress(message = 'Computing IPW', value = 0, {
        result <- compute_ipw_weights(data, params)
        shiny::incProgress(1)
        result
      })
    },
    
    handle_error = function(e) {
      error_info <- list(
        timestamp = Sys.time(),
        message = e$message,
        call = e$call,
        status = self$status
      )
      self$error_log <- c(self$error_log, list(error_info))
      self$set_status("error")
      log_error_enhanced(e, "Computation error")
    },
    
    set_status = function(new_status) {
      self$status <- new_status
      self$computation_log <- c(self$computation_log, 
        list(list(timestamp = Sys.time(), status = new_status)))
    },
    
    log_completion = function(result) {
      completion_info <- list(
        timestamp = Sys.time(),
        method = result$method,
        iterations = result$convergence$iterations,
        success = result$convergence$achieved
      )
      self$computation_log <- c(self$computation_log, list(completion_info))
    }
  )
)
