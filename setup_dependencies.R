# TerraPulse Environment & Dependency Setup
# This script checks for required packages and installs any missing ones.

required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "scales")
optional_packages <- c("plotly", "leaflet")

message("Checking TerraPulse core dependencies...")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing required package:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(paste("✓ Required package already installed:", pkg))
  }
}

message("\nChecking optional visualization enhancement packages...")
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Optional package not yet installed (fallback enabled):", pkg))
    # You can run install.packages('", pkg, "') to enable additional interactive animations.
  } else {
    message(paste("✓ Optional package active:", pkg))
  }
}

message("\nAll core dependencies ready. You can now run the app via: shiny::runApp()")
