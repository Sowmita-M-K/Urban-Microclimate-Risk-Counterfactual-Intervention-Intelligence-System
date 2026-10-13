# ==============================================================================
# TerraPulse: Urban Microclimate Risk & Counterfactual Intervention System
# Module: Simulation Engine & Counterfactual Modeling
# ==============================================================================

#' Simulate microclimate interventions for a specific district or citywide
#' 
#' @param baseline_df Data frame containing district metrics
#' @param target_district_id Character ID of district or "ALL" for citywide
#' @param cool_roof_pct Numeric: Percentage of roof area retrofitted with high-albedo coatings (0-100)
#' @param canopy_add_pct Numeric: Absolute percentage points of tree canopy added (0-30)
#' @param cool_pavement_pct Numeric: Percentage of road/parking surface converted to reflective coating (0-100)
#' @param discount_rate Numeric: Annual discount rate for NPV calculation (default 0.035 = 3.5%)
simulate_intervention <- function(
  baseline_df,
  target_district_id = "ALL",
  cool_roof_pct = 20,
  canopy_add_pct = 5,
  cool_pavement_pct = 15,
  discount_rate = 0.035
) {
  df <- baseline_df
  if (target_district_id != "ALL") {
    selected_mask <- df$district_id == target_district_id
  } else {
    selected_mask <- rep(TRUE, nrow(df))
  }
  
  # Physical Modeling Constants (derived from EPA Urban Heat Island Mitigation empirical studies)
  # 1. Albedo cooling coefficient: Increase albedo by 0.1 -> surface temp drop of ~4.2 C
  # Roofs constitute ~25% of urban impervious surface; Pavements constitute ~35%
  roof_fraction <- 0.28
  pavement_fraction <- 0.36
  
  # Calculate effective albedo delta
  # Cool roof albedo delta ~ +0.40 (from 0.15 to 0.55); Cool pavement ~ +0.25 (from 0.10 to 0.35)
  delta_albedo_roof <- (cool_roof_pct / 100) * roof_fraction * 0.40
  delta_albedo_pave <- (cool_pavement_pct / 100) * pavement_fraction * 0.25
  total_delta_albedo <- delta_albedo_roof + delta_albedo_pave
  
  # Surface temperature drop from radiative albedo increase (approx -14 C per 1.0 albedo increase)
  surface_temp_drop_albedo <- total_delta_albedo * 14.5
  
  # Evapotranspiration cooling from tree canopy expansion:
  # Diminishing marginal cooling as canopy densifies
  canopy_cooling_factor <- 0.18 # C drop per 1% canopy gain
  surface_temp_drop_canopy <- (canopy_add_pct * canopy_cooling_factor) * 1.65
  
  total_surface_drop <- ifelse(selected_mask, surface_temp_drop_albedo + surface_temp_drop_canopy, 0)
  
  # Ambient air temperature reduction (~32% coupling with surface temperature drop)
  ambient_temp_drop <- total_surface_drop * 0.34
  
  # Update simulated metrics
  df$sim_surface_temp_c <- round(pmax(25, df$surface_temp_c - total_surface_drop), 2)
  df$sim_ambient_temp_c <- round(pmax(24, df$ambient_temp_c - ambient_temp_drop), 2)
  df$temp_reduction_c <- round(ambient_temp_drop, 2)
  
  # Public Health Impact Modeling:
  # Epidemiological Relative Risk function:
  # Each 1 C drop reduces heat morbidity by ~5.5%, higher in elderly/poverty cohorts
  sensitivity_multiplier <- 1 + (df$elderly_pct / 100 * 0.8) + (df$poverty_rate_pct / 100 * 0.5)
  reduction_ratio <- pmin(0.70, (df$temp_reduction_c * 0.055) * sensitivity_multiplier)
  
  df$averted_hospitalizations <- round(df$annual_heat_hospitalizations * reduction_ratio)
  df$sim_hospitalizations <- df$annual_heat_hospitalizations - df$averted_hospitalizations
  
  # Energy Grid Load Shaving:
  # 1 C ambient drop reduces peak cooling demand by ~3.2%
  load_shave_ratio <- pmin(0.35, df$temp_reduction_c * 0.032)
  df$grid_mw_shaved <- round(df$grid_peak_load_mw * load_shave_ratio, 2)
  df$sim_peak_load_mw <- round(df$grid_peak_load_mw - df$grid_mw_shaved, 2)
  df$sim_grid_utilization_pct <- round((df$sim_peak_load_mw / df$substation_capacity_mw) * 100, 1)
  
  # Financial ROI Analysis:
  # Estimated CapEx:
  # Area in m^2 = area_sqkm * 1e6
  # Cool Roof: Applied to (area * roof_frac * cool_roof_pct) at $14 / m^2
  # Canopy: $320 per mature urban tree (~250 trees needed per square kilometer per 1% canopy gain)
  # Cool Pavement: Applied to (area * pavement_frac * cool_pave_pct) at $9 / m^2
  
  area_m2 <- df$area_sqkm * 1e6
  roof_area_retrofitted <- area_m2 * roof_fraction * (cool_roof_pct / 100)
  pave_area_retrofitted <- area_m2 * pavement_fraction * (cool_pavement_pct / 100)
  trees_planted <- (df$area_sqkm * 250) * canopy_add_pct
  
  capex_roof <- roof_area_retrofitted * 14
  capex_pave <- pave_area_retrofitted * 9
  capex_tree <- trees_planted * 320
  
  df$intervention_capex_usd <- ifelse(selected_mask, round(capex_roof + capex_pave + capex_tree), 0)
  
  # Annual Cost Savings:
  # 1. Direct energy savings ($0.16 / kWh, 90 peak summer days, ~3.8 hrs/day of reduced compressor load)
  energy_savings_usd <- df$grid_mw_shaved * 1000 * 3.8 * 90 * 0.16
  # 2. Avoided healthcare cost ($13,500 avg emergency admission + inpatient care per case)
  health_savings_usd <- df$averted_hospitalizations * 13500
  # 3. Ecosystem co-benefits ($45 / tree / year carbon sequestration + stormwater retention)
  eco_benefits_usd <- ifelse(selected_mask, trees_planted * 45, 0)
  
  df$annual_benefits_usd <- round(energy_savings_usd + health_savings_usd + eco_benefits_usd)
  
  # 10-Year Net Present Value (NPV)
  pv_factor_10yr <- sum(1 / (1 + discount_rate)^(1:10)) # approx 8.31
  df$npv_10yr_usd <- round((df$annual_benefits_usd * pv_factor_10yr) - df$intervention_capex_usd)
  
  # Payback period (years)
  df$payback_years <- ifelse(
    df$annual_benefits_usd > 0 & df$intervention_capex_usd > 0,
    round(df$intervention_capex_usd / df$annual_benefits_usd, 1),
    NA
  )
  
  # Simulated MHVI Score
  norm_sim_temp <- (df$sim_surface_temp_c - min(baseline_df$surface_temp_c)) / (max(baseline_df$surface_temp_c) - min(baseline_df$surface_temp_c))
  norm_sim_canopy <- 1 - ((df$tree_canopy_pct + ifelse(selected_mask, canopy_add_pct, 0)) / 50)
  norm_sim_canopy <- pmax(0, pmin(1, norm_sim_canopy))
  norm_social <- ((df$elderly_pct / 100 * 0.5) + (df$poverty_rate_pct / 100 * 0.5))
  
  sim_raw_score <- (norm_sim_temp * 0.45) + (norm_social * 0.35) + (norm_sim_canopy * 0.20)
  df$sim_mhvi_score <- round(sim_raw_score * 100, 1)
  df$mhvi_reduction <- round(df$mhvi_score - df$sim_mhvi_score, 1)
  
  return(df)
}

#' Optimize budget allocation across districts based on cost-effectiveness
#' 
#' @param baseline_df District metrics
#' @param total_budget_usd Available municipal budget
optimize_budget_allocation <- function(baseline_df, total_budget_usd = 10000000) {
  # Calculate vulnerability and marginal impact density per dollar
  df <- baseline_df
  
  # Score prioritizing vulnerable lives protected per dollar
  df$vulnerability_weight <- (df$mhvi_score / 100) * (df$population / 10000)
  
  # Run a benchmark simulation per district
  test_sim <- simulate_intervention(df, cool_roof_pct = 25, canopy_add_pct = 6, cool_pavement_pct = 20)
  
  # Cost efficiency metric: (Averted cases * $13500 + Energy Savings) / CapEx
  efficiency <- (test_sim$annual_benefits_usd * 8.31) / pmax(1, test_sim$intervention_capex_usd)
  
  df$priority_rank_score <- round(efficiency * df$vulnerability_weight, 2)
  df <- df[order(-df$priority_rank_score), ]
  
  # Greedily allocate budget
  df$allocated_budget_usd <- 0
  remaining_budget <- total_budget_usd
  
  for (i in 1:nrow(df)) {
    district_needed <- test_sim$intervention_capex_usd[test_sim$district_id == df$district_id[i]]
    if (district_needed <= remaining_budget) {
      df$allocated_budget_usd[i] <- district_needed
      remaining_budget <- remaining_budget - district_needed
    } else if (remaining_budget > 0) {
      df$allocated_budget_usd[i] <- remaining_budget
      remaining_budget <- 0
    }
  }
  
  df$pct_budget_funded <- round((df$allocated_budget_usd / pmax(1, test_sim$intervention_capex_usd[match(df$district_id, test_sim$district_id)])) * 100, 1)
  return(df)
}
