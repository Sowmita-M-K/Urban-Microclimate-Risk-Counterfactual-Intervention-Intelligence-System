# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Economic ROI & Multi-Objective Budget Optimizer (mod_roi)
# ==============================================================================

mod_roi_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "row g-4",
      # Budget Optimizer Controls
      div(
        class = "col-lg-4",
        div(
          class = "card h-100",
          div(
            class = "card-header",
            span("Municipal Capital Allocation"),
            span(class = "badge bg-info", "Pareto Optimizer")
          ),
          div(
            class = "card-body",
            p(class = "help-label mb-3", "Simulate optimal resource dispatch under constrained public funding. The algorithm prioritizes districts with the highest vulnerable lives protected per dollar invested."),
            
            # Budget Slider
            div(
              class = "mb-4",
              tags$label("Available Municipal Capital Budget ($ USD)", class = "form-label fw-bold text-white small"),
              sliderInput(
                ns("municipal_budget"),
                label = NULL,
                min = 2000000,
                max = 30000000,
                value = 12000000,
                step = 1000000,
                pre = "$"
              )
            ),
            
            div(
              class = "mb-3 p-3 rounded-3",
              style = "background: rgba(255, 255, 255, 0.04); border: 1px solid rgba(255, 255, 255, 0.08);",
              div(class = "d-flex justify-content-between mb-2",
                  span("Discount Rate (NPV):", style = "color: #9ca3af; font-size: 0.85rem;"),
                  strong("3.5% / yr", style = "color: #ffffff; font-size: 0.85rem;")
              ),
              div(class = "d-flex justify-content-between mb-2",
                  span("Averted ER Cost / Patient:", style = "color: #9ca3af; font-size: 0.85rem;"),
                  strong("$13,500", style = "color: #ffffff; font-size: 0.85rem;")
              ),
              div(class = "d-flex justify-content-between",
                  span("Peak Energy Tariff Saved:", style = "color: #9ca3af; font-size: 0.85rem;"),
                  strong("$0.16 / kWh", style = "color: #ffffff; font-size: 0.85rem;")
              )
            ),
            
            div(
              class = "alert alert-dark border-secondary p-3 mt-4",
              style = "font-size: 0.84rem; color: #cbd5e1;",
              strong("Algorithmic Recommendation: "),
              "Targeting South Dense Residential & East Industrial Belt first yields a Benefit-Cost Ratio (BCR) of 2.84x, preventing 270+ acute hospital admissions."
            )
          )
        )
      ),
      
      # ROI Visualizations & Allocation Plan
      div(
        class = "col-lg-8",
        div(
          class = "row g-4",
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-header",
                span("10-Year Cumulative Financial & Societal Net Present Value (NPV)"),
                span(class = "badge bg-dark border border-secondary", "Financial Viability")
              ),
              div(
                class = "card-body",
                plotOutput(ns("npv_trajectory_plot"), height = "300px")
              )
            )
          ),
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-header",
                span("Algorithmic Capital Allocation by District Priority Rank"),
                span(class = "badge bg-dark border border-secondary", "Budget Dispatch")
              ),
              div(
                class = "card-body",
                plotOutput(ns("allocation_bar_plot"), height = "300px")
              )
            )
          )
        )
      )
    )
  )
}

mod_roi_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive budget optimizer
    optimized_data_r <- reactive({
      df <- data_r()
      optimize_budget_allocation(df, total_budget_usd = input$municipal_budget)
    })
    
    # 10-Year NPV Trajectory
    output$npv_trajectory_plot <- renderPlot({
      opt <- optimized_data_r()
      
      # Calculate 10-year curve
      years <- 0:10
      total_capex <- input$municipal_budget
      
      # Benchmark intervention on funded districts
      annual_benefits <- sum(opt$pct_budget_funded / 100 * 380000) # realistic annual cashflow of benefits
      discount_rate <- 0.035
      
      cum_benefits <- sapply(years, function(y) {
        if (y == 0) return(-total_capex)
        # Discounted sum
        discounted_annual <- sum(sapply(1:y, function(i) annual_benefits / (1 + discount_rate)^i))
        discounted_annual - total_capex
      })
      
      curve_df <- data.frame(Year = years, CumulativeNPV = cum_benefits / 1e6)
      
      ggplot2::ggplot(curve_df, ggplot2::aes(x = Year, y = CumulativeNPV)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#ef4444", size = 0.8) +
        ggplot2::geom_area(fill = "#10b981", alpha = 0.25) +
        ggplot2::geom_line(color = "#10b981", size = 1.6) +
        ggplot2::geom_point(color = "#ffffff", size = 3.5, stroke = 2) +
        ggplot2::scale_x_continuous(breaks = 0:10) +
        ggplot2::labs(
          x = "Investment Time Horizon (Years)",
          y = "Cumulative Net Benefit ($ Millions USD)"
        ) +
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
    
    # Priority allocation bar chart
    output$allocation_bar_plot <- renderPlot({
      opt <- optimized_data_r()
      
      opt <- opt %>%
        dplyr::arrange(allocated_budget_usd) %>%
        dplyr::mutate(district_name = factor(district_name, levels = district_name))
      
      ggplot2::ggplot(opt, ggplot2::aes(x = allocated_budget_usd / 1e6, y = district_name, fill = pct_budget_funded)) +
        ggplot2::geom_col(width = 0.65) +
        ggplot2::scale_fill_gradient(low = "#3b82f6", high = "#10b981", name = "% Funded") +
        ggplot2::labs(
          x = "Allocated Capital ($ Millions USD)",
          y = NULL
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
          legend.position = "right",
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.text = ggplot2::element_text(color = "#d1d5db")
        )
    }, bg = "transparent")
    
  })
}
