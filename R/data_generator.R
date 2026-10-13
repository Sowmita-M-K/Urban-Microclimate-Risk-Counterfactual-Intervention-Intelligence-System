# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Data Generator and Baseline Analytics
# ==============================================================================

generate_urban_dataset <- function(seed = 42) {
  set.seed(seed)
  
  districts <- data.frame(
    district_id = paste0("DST-", sprintf("%02d", 1:12)),
    district_name = c(
      "Downtown Metro Core", "East Industrial Belt", "Old Town Heritage", 
      "South Dense Residential", "West Tech Corridor", "North Green Suburbs",
      "Riverside Waterfront", "Port & Freight Logistics", "University Quarter",
      "Highland Hills", "Healthcare Medical Park", "Railway Transit Hub"
    ),
    zone_type = c(
      "Commercial Dense", "Heavy Industrial", "Historical Mixed",
      "High-Density Residential", "Commercial Modern", "Low-Density Residential",
      "Recreational Mixed", "Industrial Logistics", "Institutional",
      "Suburban Green", "Public Institutional", "Transit Core"
    ),
    area_sqkm = c(4.8, 8.2, 3.5, 6.1, 5.4, 9.5, 4.2, 7.6, 3.8, 11.2, 2.9, 3.1),
    population = c(48000, 14000, 31000, 72000, 26000, 34000, 21000, 9500, 29000, 18000, 12000, 39000),
    median_income_usd = c(78000, 34000, 42000, 31000, 96000, 89000, 68000, 36000, 41000, 105000, 64000, 38000),
    tree_canopy_pct = c(6.5, 3.2, 11.0, 5.8, 18.2, 36.4, 28.5, 2.1, 22.0, 44.5, 16.5, 7.2),
    impervious_surface_pct = c(88.0, 94.5, 82.0, 89.2, 74.0, 48.0, 58.0, 96.0, 68.0, 38.0, 76.0, 87.5),
    albedo_baseline = c(0.12, 0.09, 0.14, 0.11, 0.18, 0.22, 0.20, 0.08, 0.17, 0.25, 0.16, 0.11),
    surface_temp_c = c(39.8, 43.2, 38.4, 41.6, 35.1, 31.8, 33.2, 44.1, 34.6, 30.5, 36.2, 40.5),
    ambient_temp_c = c(34.2, 36.8, 33.5, 35.9, 31.4, 29.2, 30.1, 37.4, 31.0, 28.5, 32.1, 35.0),
    elderly_pct = c(12.4, 15.8, 24.5, 21.0, 8.2, 19.5, 14.2, 11.0, 5.5, 22.0, 16.8, 18.4),
    poverty_rate_pct = c(14.2, 29.5, 23.0, 32.8, 7.5, 6.2, 12.0, 27.4, 24.0, 4.8, 11.5, 26.2),
    annual_heat_hospitalizations = c(88, 76, 115, 198, 22, 18, 26, 49, 31, 12, 38, 112),
    grid_peak_load_mw = c(64.5, 88.0, 39.0, 78.5, 45.0, 38.0, 28.5, 92.0, 34.0, 22.0, 29.0, 51.0),
    substation_capacity_mw = c(70.0, 92.0, 42.0, 82.0, 55.0, 50.0, 36.0, 95.0, 42.0, 35.0, 35.0, 54.0),
    centroid_lat = c(37.7749, 37.7650, 37.7880, 37.7420, 37.7950, 37.8200, 37.7550, 37.7300, 37.8050, 37.8350, 37.7680, 37.7580),
    centroid_lon = c(-122.4194, -122.3850, -122.4080, -122.4350, -122.4500, -122.4400, -122.3950, -122.3700, -122.4280, -122.4700, -122.4480, -122.4120)
  )

  # Calculate derived metrics
  baseline_rural_temp <- 28.0 # Standard regional baseline reference
  
  districts$heat_anomaly_c <- round(districts$ambient_temp_c - baseline_rural_temp, 2)
  districts$grid_utilization_pct <- round((districts$grid_peak_load_mw / districts$substation_capacity_mw) * 100, 1)
  
  # Microclimate Heat Vulnerability Index (MHVI: 0 - 100)
  # Multi-factor normalized composite: Exposure (Temp) + Sensitivity (Elderly/Poverty) + Adaptive Capacity (Canopy)
  norm_temp <- (districts$surface_temp_c - min(districts$surface_temp_c)) / (max(districts$surface_temp_c) - min(districts$surface_temp_c))
  norm_social <- ((districts$elderly_pct / 100 * 0.5) + (districts$poverty_rate_pct / 100 * 0.5))
  norm_green_deficit <- 1 - (districts$tree_canopy_pct / 50)
  norm_green_deficit <- pmax(0, pmin(1, norm_green_deficit))
  
  raw_score <- (norm_temp * 0.45) + (norm_social * 0.35) + (norm_green_deficit * 0.20)
  districts$mhvi_score <- round(raw_score * 100, 1)
  
  # Risk category
  districts$risk_tier <- cut(
    districts$mhvi_score,
    breaks = c(-Inf, 35, 60, 80, Inf),
    labels = c("Low Risk", "Moderate", "High Risk", "Severe Emergency")
  )
  
  # Cooling Energy Cost Estimate ($210 per MW-month during summer peak)
  districts$annual_cooling_cost_usd <- round(districts$grid_peak_load_mw * 1000 * 3.8 * 90 * 0.16) # MW to kWh, cooling hours, tariff
  
  return(districts)
}

# Function to save dataset
save_sample_data <- function(filepath = "data/urban_heat_sample.csv") {
  df <- generate_urban_dataset()
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, filepath, row.names = FALSE)
  return(df)
}
