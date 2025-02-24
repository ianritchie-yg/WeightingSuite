#' Show loading screen
#' @param html HTML content for spinner
#' @param color Background color
show_waiter <- function(html = spin_flower(), color = NULL) {
  if(!requireNamespace("waiter", quietly = TRUE)) {
    warning("Waiter package not available")
    return(invisible(NULL))
  }
  
  waiter_color <- if(is.null(color)) {
    getOption("waiter.color", "rgba(0,0,0,0.7)")
  } else {
    color
  }
  
  waiter::show_waiter(html = html, color = waiter_color)
}

#' Hide loading screen
hide_waiter <- function() {
  if(!requireNamespace("waiter", quietly = TRUE)) {
    warning("Waiter package not available")
    return(invisible(NULL))
  }
  
  waiter::hide_waiter()
}

#' Generate spinner for waiter
#' @return HTML for spinner
spin_flower <- function() {
  if(!requireNamespace("waiter", quietly = TRUE)) {
    return(NULL)
  }
  
  waiter::spin_flower()
}
