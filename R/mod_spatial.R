# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Geospatial Risk & Canopy Deficit Analytics (mod_spatial)
# ==============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "row g-4",
      div(
        class = "col-lg-7",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("Geographic District Microclimate Heat Map"),
            span(class = "badge bg-dark border border-secondary", "Thermal Spatial Distribution")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Spatial mapping of metropolitan districts by coordinate centroids. Bubble radius reflects population exposed; color indicates peak surface temperature."),
            plotOutput(ns("spatial_map_plot"), height = "440px")
          )
        )
      ),
      div(
        class = "col-lg-5",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("Vegetative Canopy vs. Thermal Suppression"),
            span(class = "badge bg-dark border border-secondary", "NDVI Cooling Effect")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Strong inverse correlation between urban tree canopy density and peak land surface temperature. Districts with <10% canopy suffer up to 12°C higher temperatures."),
            plotOutput(ns("canopy_scatter_plot"), height = "440px")
          )
        )
      )
    ),
    
    # District Profiles Breakdown Cards
    div(
      class = "card mt-4",
      div(
        class = "card-header",
        span("District Thermal & Infrastructure Matrix"),
        div(
          class = "d-flex align-items-center gap-2",
          selectInput(ns("district_inspect"), label = NULL, choices = NULL, width = "220px")
        )
      ),
      div(
        class = "card-body",
        uiOutput(ns("district_detail_cards"))
      )
    )
  )
}

mod_spatial_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Populate district dropdown
    observe({
      df <- data_r()
      choices <- c("All Districts" = "ALL", setNames(df$district_id, df$district_name))
      updateSelectInput(session, "district_inspect", choices = choices)
    })
    
    # Spatial Map Plot (Geospatial Grid / Centroids)
    output$spatial_map_plot <- renderPlot({
      df <- data_r()
      
      ggplot2::ggplot(df, ggplot2::aes(x = centroid_lon, y = centroid_lat)) +
        # Simulated urban zoning background tiles
        ggplot2::stat_density_2d(ggplot2::aes(fill = ..level..), geom = "polygon", alpha = 0.18, contour = TRUE) +
        ggplot2::scale_fill_gradient(low = "#1e3a8a", high = "#ef4444", guide = "none") +
        # District centroids
        ggplot2::geom_point(ggplot2::aes(size = population, color = surface_temp_c), alpha = 0.88) +
        ggplot2::scale_size_continuous(range = c(6, 17), labels = scales::comma_format()) +
        ggplot2::scale_color_gradient2(
          low = "#10b981", mid = "#f59e0b", high = "#ef4444",
          midpoint = 36.5,
          name = "Surface Temp (°C)"
        ) +
        ggplot2::geom_text(ggplot2::aes(label = district_name), vjust = -1.5, color = "#f3f4f6", size = 3.3, fontface = "bold") +
        ggplot2::labs(
          x = "Longitude Coordinate",
          y = "Latitude Coordinate",
          size = "Population"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "rgba(15, 23, 42, 0.4)", color = "rgba(255, 255, 255, 0.1)"),
          panel.grid.major = ggplot2::element_line(color = "rgba(255, 255, 255, 0.06)", linetype = "dotted"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(color = "#e5e7eb", family = "sans"),
          axis.text = ggplot2::element_text(color = "#9ca3af"),
          axis.title = ggplot2::element_text(color = "#ffffff", face = "bold"),
          legend.position = "right",
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.text = ggplot2::element_text(color = "#d1d5db")
        )
    }, bg = "transparent")
    
    # Tree Canopy vs Temperature Scatter Plot
    output$canopy_scatter_plot <- renderPlot({
      df <- data_r()
      
      ggplot2::ggplot(df, ggplot2::aes(x = tree_canopy_pct, y = surface_temp_c)) +
        ggplot2::geom_smooth(method = "loess", color = "#10b981", fill = "#10b981", alpha = 0.2) +
        ggplot2::geom_point(ggplot2::aes(color = zone_type), size = 5.5, alpha = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = district_id), vjust = -1.2, color = "#9ca3af", size = 3.2, fontface = "bold") +
        ggplot2::labs(
          x = "Tree Canopy Coverage (%)",
          y = "Surface Temperature (°C)",
          color = "Urban Zone Type"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_line(color = "rgba(255, 255, 255, 0.08)"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(color = "#e5e7eb", family = "sans"),
          axis.text = ggplot2::element_text(color = "#9ca3af"),
          axis.title = ggplot2::element_text(color = "#ffffff", face = "bold"),
          legend.position = "bottom",
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.text = ggplot2::element_text(color = "#d1d5db", size = 9)
        )
    }, bg = "transparent")
    
    # Detailed card renderer for inspect view
    output$district_detail_cards <- renderUI({
      df <- data_r()
      sel <- input$district_inspect
      
      if (is.null(sel) || sel == "ALL") {
        # Show summary grid of top 4 highest risk districts
        top_risk <- df %>% dplyr::arrange(desc(mhvi_score)) %>% head(4)
        
        cards <- lapply(1:nrow(top_risk), function(i) {
          row <- top_risk[i, ]
          div(
            class = "col-md-3 mb-3",
            div(
              class = "p-3 rounded-3",
              style = "background: rgba(255, 255, 255, 0.03); border: 1px solid rgba(255, 255, 255, 0.08);",
              div(class = "d-flex justify-content-between align-items-center mb-2",
                  span(class = "badge bg-danger", row$risk_tier),
                  span(style = "color: #9ca3af; font-size: 0.8rem; font-weight: 700;", row$district_id)
              ),
              h6(row$district_name, style = "color: #ffffff; font-weight: 700; margin-bottom: 0.5rem;"),
              div(style = "font-size: 0.82rem; color: #9ca3af;",
                  div("Surface Temp: ", strong(style = "color: #f87171;", paste0(row$surface_temp_c, "°C"))),
                  div("Canopy: ", strong(style = "color: #34d399;", paste0(row$tree_canopy_pct, "%"))),
                  div("Elderly: ", strong(style = "color: #e5e7eb;", paste0(row$elderly_pct, "%"))),
                  div("Grid Peak: ", strong(style = "color: #38bdf8;", paste0(row$grid_peak_load_mw, " MW")))
              )
            )
          )
        })
        return(div(class = "row", cards))
      } else {
        target <- df %>% dplyr::filter(district_id == sel)
        if (nrow(target) == 0) return(NULL)
        
        div(
          class = "row g-3",
          div(
            class = "col-md-3",
            div(class = "metric-box",
                div(class = "metric-title", "District Name"),
                h5(target$district_name, style = "color: #ffffff; font-weight: 700; margin: 0;"),
                div(class = "metric-subtitle", paste("Zone:", target$zone_type))
            )
          ),
          div(
            class = "col-md-3",
            div(class = "metric-box danger",
                div(class = "metric-title", "Surface & Ambient Temp"),
                div(class = "metric-value", style = "font-size: 1.6rem;", paste0(target$surface_temp_c, "°C / ", target$ambient_temp_c, "°C")),
                div(class = "metric-subtitle", paste0("Anomaly: +", target$heat_anomaly_c, "°C vs baseline"))
            )
          ),
          div(
            class = "col-md-3",
            div(class = "metric-box cyan",
                div(class = "metric-title", "Tree Canopy & Impervious"),
                div(class = "metric-value", style = "font-size: 1.6rem;", paste0(target$tree_canopy_pct, "% / ", target$impervious_surface_pct, "%")),
                div(class = "metric-subtitle", "Vegetative vs Impervious Pavement")
            )
          ),
          div(
            class = "col-md-3",
            div(class = "metric-box purple",
                div(class = "metric-title", "Grid Stress & Hospitalizations"),
                div(class = "metric-value", style = "font-size: 1.6rem;", paste0(target$grid_utilization_pct, "% | ", target$annual_heat_hospitalizations)),
                div(class = "metric-subtitle", "Substation Load | Annual Admissions")
            )
          )
        )
      }
    })
    
  })
}
