
# Create directory structure
dirs <- c(
  "www/css",
  "www/js",
  "R",
  "logs"
)

# Create directories
sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# Move files to correct locations
file.rename("styles.css", "www/css/styles.css")
file.rename("custom.js", "www/js/custom.js")

# Create empty log file
file.create("logs/app.log")
