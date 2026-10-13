# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# High-Performance R Shiny Application
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Source modules and analytical engines
source("R/data_generator.R")
source("R/simulation_engine.R")
source("R/mod_overview.R")
source("R/mod_spatial.R")
source("R/mod_simulator.R")
source("R/mod_roi.R")
source("R/mod_data.R")

# Ensure dataset exists
if (!file.exists("data/urban_heat_sample.csv")) {
  dir.create("data", showWarnings = FALSE)
  save_sample_data("data/urban_heat_sample.csv")
}

# Define modern theme with bslib (Bootstrap 5)
terrapulse_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#10b981",
  secondary = "#64748b",
  success = "#10b981",
  info = "#06b6d4",
  warning = "#f59e0b",
  danger = "#ef4444",
  bg = "#0a0e17",
  fg = "#f3f4f6",
  base_font = font_google("Plus Jakarta Sans"),
  heading_font = font_google("Plus Jakarta Sans"),
  code_font = font_google("JetBrains Mono")
)

# UI Definition
ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$span(style = "font-size: 1.35rem; font-weight: 800; color: #ffffff; letter-spacing: -0.03em;", "TERRAPULSE"),
    tags$span(class = "brand-badge", "CLIMATE AI")
  ),
  theme = terrapulse_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # Tab 1: Executive Overview
  nav_panel(
    title = "Executive Overview",
    div(class = "container-fluid py-3",
        mod_overview_ui("overview")
    )
  ),
  
  # Tab 2: Spatial Cartography & Risk
  nav_panel(
    title = "Spatial Cartography",
    div(class = "container-fluid py-3",
        mod_spatial_ui("spatial")
    )
  ),
  
  # Tab 3: Counterfactual Sandbox
  nav_panel(
    title = "Intervention Sandbox",
    div(class = "container-fluid py-3",
        mod_simulator_ui("simulator")
    )
  ),
  
  # Tab 4: Economic ROI & Capital Optimizer
  nav_panel(
    title = "ROI & Capital Optimizer",
    div(class = "container-fluid py-3",
        mod_roi_ui("roi")
    )
  ),
  
  # Tab 5: Data Explorer
  nav_panel(
    title = "Data Explorer",
    div(class = "container-fluid py-3",
        mod_data_ui("data_exp")
    )
  ),
  
  # Navigation Bar Spacer
  nav_spacer(),
  
  # Right Menu Help / Status
  nav_item(
    tags$a(
      href = "https://github.com",
      target = "_blank",
      class = "nav-link text-white-50 d-flex align-items-center gap-1",
      tags$span("v2.4.0 Engine")
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Master reactive dataset
  data_r <- reactive({
    if (file.exists("data/urban_heat_sample.csv")) {
      read.csv("data/urban_heat_sample.csv", stringsAsFactors = FALSE)
    } else {
      generate_urban_dataset()
    }
  })
  
  # Initialize Modules
  mod_overview_server("overview", data_r)
  mod_spatial_server("spatial", data_r)
  mod_simulator_server("simulator", data_r)
  mod_roi_server("roi", data_r)
  mod_data_server("data_exp", data_r)
}

# Shiny App Entry Point
shinyApp(ui = ui, server = server)
