# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Overview & Citywide Vulnerability Intelligence (mod_overview)
# ==============================================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Top Headline Banner
    div(
      class = "mb-4 p-4 rounded-3",
      style = "background: linear-gradient(135deg, rgba(16, 185, 129, 0.08) 0%, rgba(6, 182, 212, 0.04) 100%); border: 1px solid rgba(16, 185, 129, 0.2); border-radius: 14px;",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
        div(
          h3("Metropolitan Heat Island & Climate Inequity Intelligence", style = "font-weight: 800; margin-bottom: 0.35rem; color: #ffffff; letter-spacing: -0.02em;"),
          p("Real-time synthesized microclimate telemetry across 12 metropolitan sectors, quantifying environmental justice disparities and emergency healthcare load.", 
            style = "color: #9ca3af; margin: 0; font-size: 0.95rem;")
        ),
        div(
          class = "status-indicator",
          span(class = "pulse-dot"),
          span("LIVE TELEMETRY MODEL ACTIVE")
        )
      )
    ),
    
    # 4 Key Value Metric Cards
    div(
      class = "row g-3 mb-4",
      div(
        class = "col-xl-3 col-md-6",
        div(
          class = "metric-box danger",
          div(class = "metric-title", "Peak Surface Anomaly"),
          div(class = "metric-value", textOutput(ns("peak_anomaly_val"))),
          div(class = "metric-subtitle", span(style = "color: #ef4444; font-weight: 700;", "▲ +16.1°C"), " above rural baseline")
        )
      ),
      div(
        class = "col-xl-3 col-md-6",
        div(
          class = "metric-box",
          div(class = "metric-title", "At-Risk Population"),
          div(class = "metric-value", textOutput(ns("vulnerable_pop_val"))),
          div(class = "metric-subtitle", "Residents in High/Severe risk zones")
        )
      ),
      div(
        class = "col-xl-3 col-md-6",
        div(
          class = "metric-box purple",
          div(class = "metric-title", "Heat Hospitalizations"),
          div(class = "metric-value", textOutput(ns("hospitalizations_val"))),
          div(class = "metric-subtitle", "Annual heat-induced admissions")
        )
      ),
      div(
        class = "col-xl-3 col-md-6",
        div(
          class = "metric-box cyan",
          div(class = "metric-title", "Grid Cooling Demand"),
          div(class = "metric-value", textOutput(ns("cooling_burden_val"))),
          div(class = "metric-subtitle", "Peak seasonal cooling load")
        )
      )
    ),
    
    # Main Analytical Visualizations
    div(
      class = "row g-4",
      div(
        class = "col-lg-7",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("Climate Inequity & Environmental Justice Analysis"),
            span(class = "badge bg-dark border border-secondary", "Socio-Thermal Disparity")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Cross-analysis of neighborhood poverty rates against satellite surface temperatures. Note the severe cluster of lower-income districts bearing disproportionate heat exposure."),
            plotOutput(ns("inequity_scatter_plot"), height = "380px")
          )
        )
      ),
      div(
        class = "col-lg-5",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("District Heat Vulnerability Index (MHVI)"),
            span(class = "badge bg-dark border border-secondary", "0-100 Multi-Vector Score")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Composite metric weighting surface heat (45%), demographic vulnerability (35%), and vegetative canopy deficit (20%)."),
            plotOutput(ns("vulnerability_ranking_plot"), height = "380px")
          )
        )
      )
    ),
    
    # Mathematical & Methodological Callout
    div(
      class = "mt-4 p-3 rounded-3",
      style = "background: rgba(17, 24, 39, 0.6); border: 1px dashed rgba(255, 255, 255, 0.12);",
      div(
        class = "d-flex align-items-center gap-3",
        div(
          style = "font-size: 1.5rem; color: #10b981;", "💡"
        ),
        div(
          h6("Why TerraPulse is Different:", style = "color: #ffffff; font-weight: 700; margin-bottom: 0.2rem;"),
          p("Unlike traditional static weather monitors, TerraPulse solves the urban heat crisis by modeling the causal links between surface albedo, micro-canopy transpiration, demographic vulnerability, and healthcare capacity. Head to the ", 
            strong("Intervention Sandbox"), " to simulate real-time green infrastructure investments.",
            style = "color: #9ca3af; margin: 0; font-size: 0.85rem;")
        )
      )
    )
  )
}

mod_overview_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive calculations for KPIs
    output$peak_anomaly_val <- renderText({
      df <- data_r()
      paste0("+", max(df$heat_anomaly_c), "°C")
    })
    
    output$vulnerable_pop_val <- renderText({
      df <- data_r()
      vulnerable <- df %>% 
        dplyr::filter(risk_tier %in% c("High Risk", "Severe Emergency")) %>%
        dplyr::pull(population) %>% 
        sum()
      scales::comma(vulnerable)
    })
    
    output$hospitalizations_val <- renderText({
      df <- data_r()
      paste0(sum(df$annual_heat_hospitalizations), " cases")
    })
    
    output$cooling_burden_val <- renderText({
      df <- data_r()
      paste0(round(sum(df$grid_peak_load_mw), 0), " MW")
    })
    
    # Plot 1: Climate Inequity Scatter Plot
    output$inequity_scatter_plot <- renderPlot({
      df <- data_r()
      
      ggplot2::ggplot(df, ggplot2::aes(x = poverty_rate_pct, y = surface_temp_c, size = population, color = risk_tier)) +
        ggplot2::geom_point(alpha = 0.85) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#60a5fa", linetype = "dashed", fill = "#3b82f6", alpha = 0.15) +
        ggplot2::scale_size_continuous(range = c(4, 13), labels = scales::comma_format()) +
        ggplot2::scale_color_manual(values = c(
          "Low Risk" = "#10b981", 
          "Moderate" = "#f59e0b", 
          "High Risk" = "#f97316", 
          "Severe Emergency" = "#ef4444"
        )) +
        ggplot2::geom_text(ggplot2::aes(label = district_name), vjust = -1.2, size = 3.2, color = "#e5e7eb", fontface = "bold", check_overlap = TRUE) +
        ggplot2::labs(
          x = "Poverty Rate (% of District Population)",
          y = "Peak Surface Temperature (°C)",
          size = "Population",
          color = "Risk Classification"
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
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.position = "bottom",
          legend.text = ggplot2::element_text(color = "#d1d5db")
        )
    }, bg = "transparent")
    
    # Plot 2: Horizontal Bar Chart of Vulnerability Rankings
    output$vulnerability_ranking_plot <- renderPlot({
      df <- data_r() %>%
        dplyr::arrange(mhvi_score) %>%
        dplyr::mutate(district_name = factor(district_name, levels = district_name))
      
      ggplot2::ggplot(df, ggplot2::aes(x = mhvi_score, y = district_name, fill = mhvi_score)) +
        ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", mhvi_score)), hjust = -0.2, color = "#ffffff", size = 3.4, fontface = "bold") +
        ggplot2::scale_fill_gradientn(colors = c("#10b981", "#f59e0b", "#f97316", "#ef4444")) +
        ggplot2::xlim(0, 105) +
        ggplot2::labs(x = "Composite Heat Vulnerability Index (0-100)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_line(color = "rgba(255, 255, 255, 0.08)"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(color = "#e5e7eb", family = "sans"),
          axis.text = ggplot2::element_text(color = "#9ca3af"),
          axis.title = ggplot2::element_text(color = "#ffffff", face = "bold")
        )
    }, bg = "transparent")
    
  })
}
