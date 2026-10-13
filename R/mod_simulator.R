# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Counterfactual Intervention Sandbox (mod_simulator)
# ==============================================================================

mod_simulator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "row g-4",
      # Left Sidebar Controls
      div(
        class = "col-lg-4",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("Intervention Design Controls"),
            span(class = "badge bg-success", "Simulation Engine")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Adjust green infrastructure levers to simulate physical thermodynamics, averted heatstroke hospitalizations, and power grid peak load reduction."),
            
            # Scope Selector
            div(
              class = "mb-3",
              tags$label("Target Municipal Scope", class = "form-label fw-bold text-white small"),
              selectInput(
                ns("sim_target"), 
                label = NULL,
                choices = c("Metropolitan Citywide (All 12 Districts)" = "ALL"),
                width = "100%"
              )
            ),
            
            # Policy Presets
            div(
              class = "mb-4",
              tags$label("Quick Policy Presets", class = "form-label fw-bold text-white small"),
              div(
                class = "d-flex flex-wrap gap-2",
                actionButton(ns("preset_emergency"), "🚨 Cool Roof Sprint", class = "btn btn-sm btn-outline-light"),
                actionButton(ns("preset_forest"), "🌳 Urban Canopy Plan", class = "btn btn-sm btn-outline-light"),
                actionButton(ns("preset_balanced"), "⚖️ Balanced Resilient", class = "btn btn-sm btn-outline-light"),
                actionButton(ns("preset_reset"), "↺ Reset", class = "btn btn-sm btn-outline-secondary")
              )
            ),
            
            hr(style = "border-color: rgba(255, 255, 255, 0.1);"),
            
            # Lever 1: Cool Roofs
            div(
              class = "mb-3",
              div(class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label("Cool Roof Retrofit (%)", class = "form-label fw-bold text-white small mb-0"),
                  span(class = "badge bg-dark text-info border border-info", "Albedo +0.40")
              ),
              sliderInput(ns("cool_roof_pct"), label = NULL, min = 0, max = 100, value = 25, step = 5, post = "%"),
              p(class = "help-label", "High-reflectance rooftop coatings that bounce solar radiation before thermal absorption.")
            ),
            
            # Lever 2: Urban Canopy
            div(
              class = "mb-3",
              div(class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label("Tree Canopy Expansion (+%)", class = "form-label fw-bold text-white small mb-0"),
                  span(class = "badge bg-dark text-success border border-success", "Evapotranspiration")
              ),
              sliderInput(ns("canopy_add_pct"), label = NULL, min = 0, max = 25, value = 6, step = 1, post = "%"),
              p(class = "help-label", "Pockets of native dense trees providing vegetative shade and atmospheric cooling.")
            ),
            
            # Lever 3: Cool Pavements
            div(
              class = "mb-3",
              div(class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label("Reflective Cool Pavement (%)", class = "form-label fw-bold text-white small mb-0"),
                  span(class = "badge bg-dark text-warning border border-warning", "Albedo +0.25")
              ),
              sliderInput(ns("cool_pavement_pct"), label = NULL, min = 0, max = 80, value = 15, step = 5, post = "%"),
              p(class = "help-label", "Solar-reflective slurry applied to asphalt roads, parking structures, and pedestrian alleys.")
            )
          )
        )
      ),
      
      # Right Results Sandbox
      div(
        class = "col-lg-8",
        # Real-time counterfactual impact indicators
        div(
          class = "row g-3 mb-4",
          div(
            class = "col-md-3",
            div(
              class = "metric-box",
              div(class = "metric-title", "Max Temp Reduction"),
              div(class = "metric-value", style = "color: #10b981;", textOutput(ns("metric_temp_drop"))),
              div(class = "metric-subtitle", "Surface cooling achieved")
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "metric-box purple",
              div(class = "metric-title", "Lives / Cases Saved"),
              div(class = "metric-value", style = "color: #c084fc;", textOutput(ns("metric_cases_saved"))),
              div(class = "metric-subtitle", "Averted hospital admissions")
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "metric-box cyan",
              div(class = "metric-title", "Peak Grid Shaved"),
              div(class = "metric-value", style = "color: #38bdf8;", textOutput(ns("metric_mw_shaved"))),
              div(class = "metric-subtitle", "Avoided blackouts & surge")
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "metric-box",
              div(class = "metric-title", "Annual Cost Savings"),
              div(class = "metric-value", style = "color: #34d399;", textOutput(ns("metric_annual_savings"))),
              div(class = "metric-subtitle", "Health + Energy savings/yr")
            )
          )
        ),
        
        # Comparative Charts
        div(
          class = "row g-4",
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-header",
                span("Counterfactual Microclimate Impact: Baseline vs. Simulated Temperatures"),
                span(class = "badge bg-dark border border-secondary", "Thermal Shift")
              ),
              div(
                class = "card-body",
                plotOutput(ns("temp_comparison_plot"), height = "320px")
              )
            )
          ),
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-header",
                span("Public Health Burden Relief: Averted Heat Hospitalizations"),
                span(class = "badge bg-dark border border-secondary", "Epidemiological Risk Model")
              ),
              div(
                class = "card-body",
                plotOutput(ns("health_impact_plot"), height = "280px")
              )
            )
          )
        )
      )
    )
  )
}

mod_simulator_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Update target selector
    observe({
      df <- data_r()
      choices <- c("Metropolitan Citywide (All 12 Districts)" = "ALL", setNames(df$district_id, df$district_name))
      updateSelectInput(session, "sim_target", choices = choices)
    })
    
    # Presets logic
    observeEvent(input$preset_emergency, {
      updateSliderInput(session, "cool_roof_pct", value = 65)
      updateSliderInput(session, "canopy_add_pct", value = 3)
      updateSliderInput(session, "cool_pavement_pct", value = 40)
    })
    
    observeEvent(input$preset_forest, {
      updateSliderInput(session, "cool_roof_pct", value = 15)
      updateSliderInput(session, "canopy_add_pct", value = 18)
      updateSliderInput(session, "cool_pavement_pct", value = 10)
    })
    
    observeEvent(input$preset_balanced, {
      updateSliderInput(session, "cool_roof_pct", value = 40)
      updateSliderInput(session, "canopy_add_pct", value = 8)
      updateSliderInput(session, "cool_pavement_pct", value = 30)
    })
    
    observeEvent(input$preset_reset, {
      updateSliderInput(session, "cool_roof_pct", value = 0)
      updateSliderInput(session, "canopy_add_pct", value = 0)
      updateSliderInput(session, "cool_pavement_pct", value = 0)
    })
    
    # Run simulation reactively
    sim_data_r <- reactive({
      df <- data_r()
      simulate_intervention(
        baseline_df = df,
        target_district_id = input$sim_target %||% "ALL",
        cool_roof_pct = input$cool_roof_pct,
        canopy_add_pct = input$canopy_add_pct,
        cool_pavement_pct = input$cool_pavement_pct
      )
    })
    
    # Output metrics
    output$metric_temp_drop <- renderText({
      sim <- sim_data_r()
      max_drop <- max(sim$surface_temp_c - sim$sim_surface_temp_c)
      paste0("-", sprintf("%.2f", max_drop), "°C")
    })
    
    output$metric_cases_saved <- renderText({
      sim <- sim_data_r()
      saved <- sum(sim$averted_hospitalizations)
      paste0(scales::comma(saved), " pts")
    })
    
    output$metric_mw_shaved <- renderText({
      sim <- sim_data_r()
      mw <- sum(sim$grid_mw_shaved)
      paste0(sprintf("%.1f", mw), " MW")
    })
    
    output$metric_annual_savings <- renderText({
      sim <- sim_data_r()
      tot <- sum(sim$annual_benefits_usd)
      paste0("$", scales::comma(round(tot / 1000)), "K")
    })
    
    # Plot: Before vs After Temperature
    output$temp_comparison_plot <- renderPlot({
      sim <- sim_data_r()
      
      # Prepare long format
      plot_df <- sim %>%
        dplyr::select(district_name, Baseline = surface_temp_c, Simulated = sim_surface_temp_c) %>%
        tidyr::pivot_longer(cols = c("Baseline", "Simulated"), names_to = "Scenario", values_to = "Temperature") %>%
        dplyr::mutate(district_name = factor(district_name, levels = rev(sim$district_name)))
      
      ggplot2::ggplot(plot_df, ggplot2::aes(x = Temperature, y = district_name, fill = Scenario)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
        ggplot2::scale_fill_manual(values = c("Baseline" = "#ef4444", "Simulated" = "#10b981")) +
        ggplot2::labs(x = "Surface Temperature (°C)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_line(color = "rgba(255, 255, 255, 0.08)"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(color = "#e5e7eb", family = "sans"),
          axis.text = ggplot2::element_text(color = "#9ca3af"),
          axis.title = ggplot2::element_text(color = "#ffffff", face = "bold"),
          legend.position = "top",
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.text = ggplot2::element_text(color = "#d1d5db")
        )
    }, bg = "transparent")
    
    # Plot: Public Health Burden Relief
    output$health_impact_plot <- renderPlot({
      sim <- sim_data_r()
      
      plot_df <- sim %>%
        dplyr::arrange(desc(annual_heat_hospitalizations)) %>%
        dplyr::slice(1:8) %>%
        dplyr::mutate(district_name = factor(district_name, levels = rev(district_name)))
      
      long_health <- plot_df %>%
        dplyr::select(district_name, Remaining = sim_hospitalizations, Averted = averted_hospitalizations) %>%
        tidyr::pivot_longer(cols = c("Remaining", "Averted"), names_to = "Status", values_to = "Hospitalizations")
      
      ggplot2::ggplot(long_health, ggplot2::aes(x = Hospitalizations, y = district_name, fill = Status)) +
        ggplot2::geom_col(position = "stack", width = 0.65) +
        ggplot2::scale_fill_manual(values = c("Remaining" = "#64748b", "Averted" = "#10b981")) +
        ggplot2::labs(x = "Annual Heat-Induced Hospitalizations (Green = Averted Cases)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_line(color = "rgba(255, 255, 255, 0.08)"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(color = "#e5e7eb", family = "sans"),
          axis.text = ggplot2::element_text(color = "#9ca3af"),
          axis.title = ggplot2::element_text(color = "#ffffff", face = "bold"),
          legend.position = "top",
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.text = ggplot2::element_text(color = "#d1d5db")
        )
    }, bg = "transparent")
    
  })
}
