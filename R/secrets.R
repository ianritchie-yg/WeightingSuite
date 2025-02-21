library(RcppTOML)

#' Load secret credentials from TOML file
#' @param path Path to secrets file (default: ".secrets.toml")
#' @return List of credentials
#' @export
load_secrets <- function(path = ".secrets.toml") {
  if (!file.exists(path)) {
    stop("Secrets file not found. Please copy .secrets.example.toml to .secrets.toml and fill in your credentials.")
  }
  
  tryCatch({
    secrets <- parseTOML(path)
    validate_secrets(secrets)
    secrets
  }, error = function(e) {
    stop(sprintf("Error loading secrets: %s", e$message))
  })
}

#' Validate required secrets are present
#' @param secrets List of secrets to validate
#' @return TRUE if valid, throws error if invalid
validate_secrets <- function(secrets) {
  required <- list(
    crunch = c("api_key", "api_url", "username", "password")
  )
  
  for (section in names(required)) {
    if (is.null(secrets[[section]])) {
      stop(sprintf("Missing required section: %s", section))
    }
    
    for (key in required[[section]]) {
      if (is.null(secrets[[section]][[key]])) {
        stop(sprintf("Missing required credential: %s.%s", section, key))
      }
    }
  }
  
  TRUE
}
