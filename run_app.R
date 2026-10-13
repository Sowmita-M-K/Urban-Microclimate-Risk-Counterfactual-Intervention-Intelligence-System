# ==============================================================================
# TerraPulse: One-Click Application Launcher
# Run this script directly in R or RStudio:
# source("run_app.R")
# ==============================================================================

message("-------------------------------------------------------------------")
message("  Starting TerraPulse: Urban Climate & Resilience Intelligence    ")
message("-------------------------------------------------------------------")

# Set working directory to project root
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Run setup check
source("setup_dependencies.R")

# Launch Shiny application
port <- 3838
host <- "127.0.0.1"
message(paste0("\nLaunching Web Application on http://", host, ":", port))
message("Press Ctrl+C or Esc to stop the server.\n")

shiny::runApp(
  appDir = ".",
  host = host,
  port = port,
  launch.browser = TRUE
)
