#' Validate target proportions
#' @param var Variable name
#' @param targets Vector of target proportions
#' @param levels Vector of variable levels
#' @return List with validation status and message
validate_targets <- function(var, targets, levels) {
  result <- list(valid = TRUE, message = NULL)
  
  tryCatch({
    targets <- as.numeric(targets)
    
    # Check for non-numeric values resulting in NA
    if (any(is.na(targets))) {
      result$valid <- FALSE
      result$message <- sprintf("Non-numeric target value(s) provided for %s", var)
      return(result)
    }
    
    # Check number of targets matches levels
    if (length(targets) != length(levels)) {
      result$valid <- FALSE
      result$message <- sprintf(
        "Number of proportions (%d) doesn't match levels (%d) for %s",
        length(targets), length(levels), var
      )
      return(result)
    }
    
    # Check sum to 1 (within a tolerance)
    if (abs(sum(targets) - 1) > 0.01) {
      result$valid <- FALSE
      result$message <- sprintf(
        "Proportions for %s sum to %.3f (should be close to 1)",
        var, sum(targets)
      )
      return(result)
    }
    
    # Check that all targets are within the range [0, 1]
    if (any(targets < 0) || any(targets > 1)) {
      result$valid <- FALSE
      result$message <- sprintf(
        "All proportions for %s must be between 0 and 1",
        var
      )
      return(result)
    }
    
  }, error = function(e) {
    result$valid <- FALSE
    result$message <- sprintf("Error validating %s: %s", var, e$message)
  })
  
  return(result)
}
