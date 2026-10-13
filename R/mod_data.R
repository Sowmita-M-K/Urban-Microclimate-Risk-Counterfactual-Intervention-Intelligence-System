# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Data Explorer & Report Export (mod_data)
# ==============================================================================

mod_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-header",
        span("Metropolitan Telemetry Dataset Explorer"),
        div(
          class = "d-flex gap-2 align-items-center",
          selectInput(ns("tier_filter"), label = NULL, 
                      choices = c("All Risk Tiers", "Severe Emergency", "High Risk", "Moderate", "Low Risk"), 
                      width = "180px"),
          downloadButton(ns("download_csv"), "📥 Export CSV", class = "btn btn-sm btn-primary")
        )
      ),
      div(
        class = "card-body",
        p(class = "help-label mb-3", "Inspect and audit the comprehensive microclimate, demographic, and grid indicators across all metropolitan municipal districts."),
        div(class = "table-responsive",
            tableOutput(ns("raw_data_table"))
        )
      )
    )
  )
}

mod_data_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    filtered_data <- reactive({
      df <- data_r()
      if (!is.null(input$tier_filter) && input$tier_filter != "All Risk Tiers") {
        df <- df %>% dplyr::filter(risk_tier == input$tier_filter)
      }
      df
    })
    
    output$raw_data_table <- renderTable({
      df <- filtered_data()
      
      df %>%
        dplyr::select(
          `ID` = district_id,
          `District` = district_name,
          `Zone` = zone_type,
          `Population` = population,
          `Surface (°C)` = surface_temp_c,
          `Anomaly (°C)` = heat_anomaly_c,
          `Canopy %` = tree_canopy_pct,
          `Poverty %` = poverty_rate_pct,
          `Hospitalizations` = annual_heat_hospitalizations,
          `Peak MW` = grid_peak_load_mw,
          `MHVI Score` = mhvi_score,
          `Risk Classification` = risk_tier
        ) %>%
        dplyr::mutate(
          Population = scales::comma(Population),
          `Surface (°C)` = sprintf("%.1f", `Surface (°C)`),
          `Anomaly (°C)` = paste0("+", sprintf("%.1f", `Anomaly (°C)`)),
          `Canopy %` = paste0(`Canopy %`, "%"),
          `Poverty %` = paste0(`Poverty %`, "%"),
          `MHVI Score` = sprintf("%.1f", `MHVI Score`)
        )
    }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("terrapulse_urban_climate_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(data_r(), file, row.names = FALSE)
      }
    )
    
  })
}
