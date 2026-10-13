# 🌍 TerraPulse: Urban Microclimate Risk & Counterfactual Intervention Intelligence System

> **A Next-Generation R & Shiny Climate-Tech Platform for Urban Heat Island Mitigation, Environmental Justice Analytics, and Municipal Capital Allocation.**

[![R Version](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![Framework](https://img.shields.io/badge/Framework-R%20Shiny%20%7C%20bslib%205-emerald.svg)](https://shiny.posit.co/)
[![Analytics](https://img.shields.io/badge/Analytics-Microclimate%20Physics%20%26%20Public%20Health-orange.svg)](#)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](#)

---

## 🌟 The Real-Life Problem & The Innovative Solution

### 1. The Global Crisis
Metropolitan areas across the globe are turning into lethal thermal traps due to the **Urban Heat Island (UHI)** effect. Dense asphalt, low vegetative cover, and concrete absorb solar radiation, creating local microclimates up to **12°C (22°F)** hotter than surrounding rural baselines. 

Crucially, this creates a severe **Climate Inequity / Environmental Justice Crisis**:
- Low-income neighborhoods frequently have **3x to 5x less tree canopy** and **20% higher impervious surface density**.
- Heatwave mortality and heatstroke emergency hospitalizations disproportionately affect vulnerable elderly and lower-income communities.
- Simultaneous air conditioning spikes cause **electrical substation overloads and rolling blackouts**.

### 2. The Innovative Solution: Counterfactual Intervention Engine
Traditional dashboards are purely descriptive—they merely report historical temperature maps. 

**TerraPulse is fundamentally different:** It provides a **Counterfactual Predictive Simulation Engine**. City planners, climate researchers, and municipal leaders can test "What-If" green infrastructure interventions in real-time:
1. **Cool Roof Retrofits**: High-albedo reflective coatings reflecting solar radiation before absorption.
2. **Urban Micro-Canopy Expansion**: Native dense pocket forests generating active evapotranspirative cooling.
3. **Reflective Cool Pavements**: Solar-reflective asphalt treatments for transit corridors and parking facilities.
4. **Multi-Objective Capital Optimizer**: An algorithmic prioritization model that maximizes lives protected and peak megawatt grid relief per million dollars invested.

---

## 📐 Mathematical & Analytical Formulations

### 1. Microclimate Heat Vulnerability Index (MHVI)
The MHVI is a composite metric combining physical heat exposure, demographic sensitivity, and adaptive deficit:
$$\text{MHVI} = \left( 0.45 \cdot \tilde{T}_{\text{surface}} \right) + \left( 0.35 \cdot \left[ 0.5 \cdot \text{Elderly} + 0.5 \cdot \text{Poverty} \right] \right) + \left( 0.20 \cdot \left[ 1 - \frac{\text{Canopy}}{50} \right] \right) \times 100$$

### 2. Physical Microclimate Cooling Dynamics
Calculates thermal suppression from radiative albedo shifts and vegetative transpiration:
$$\Delta T_{\text{surface}} = \left( \Delta \alpha_{\text{roof}} \cdot 14.5^\circ\text{C} + \Delta \alpha_{\text{pave}} \cdot 14.5^\circ\text{C} \right) + \left( \Delta \text{Canopy}_{\%} \cdot 0.18^\circ\text{C} \times 1.65 \right)$$
$$\Delta T_{\text{ambient}} \approx 0.34 \cdot \Delta T_{\text{surface}}$$

### 3. Public Health Morbidity Reduction
Based on epidemiological relative risk models:
$$\text{Averted Hospitalizations} = \text{Baseline Hospitalizations} \times \min\left(0.70, \; \Delta T_{\text{ambient}} \cdot 0.055 \cdot \left[ 1 + 0.8 \cdot \text{Elderly}_{\%} + 0.5 \cdot \text{Poverty}_{\%} \right] \right)$$

### 4. 10-Year Societal & Financial Net Present Value (NPV)
$$\text{NPV}_{10\text{yr}} = \sum_{t=1}^{10} \frac{\text{Energy Savings}_t + \text{Health Savings}_t + \text{Eco Benefits}_t}{(1 + r)^t} - \text{CapEx}$$
*(where discount rate $r = 3.5\%$, health emergency hospitalization cost = \$13,500/case, peak energy tariff = \$0.16/kWh).*

---

## 📂 Project Architecture & Folder Structure

```
terrapulse-r-analytics/
├── app.R                   # Master Shiny application wiring UI modules & reactive state
├── run_app.R               # One-click startup script (auto-checks deps & launches browser)
├── setup_dependencies.R    # Dependency installer and validator script
├── README.md               # Complete scientific documentation & manual
├── R/
│   ├── data_generator.R    # Synthesizes 12 multi-district urban microclimate sectors
│   ├── simulation_engine.R # Microclimate thermodynamics, epidemiology & budget optimizer
│   ├── mod_overview.R      # Executive Overview & Environmental Justice analytics
│   ├── mod_spatial.R       # Spatial cartography, centroid heat grid & canopy correlation
│   ├── mod_simulator.R     # Interactive Counterfactual Intervention Sandbox
│   ├── mod_roi.R           # 10-Year NPV trajectory & Pareto capital allocation optimizer
│   └── mod_data.R          # Filterable raw telemetry table & CSV export engine
├── data/
│   └── urban_heat_sample.csv # Benchmark dataset with 12 metropolitan districts
└── www/
    └── custom.css          # Glassmorphism dark-tech design system & responsive styling
```

---

## 🚀 How to Run the Project

### Method 1: Using the One-Click R Script (Recommended)
Open an R prompt or RStudio in the project folder and run:
```r
source("run_app.R")
```

### Method 2: From PowerShell / Terminal
You can launch the web application directly using your installed R 4.6.1 executable:
```powershell
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" -e "shiny::runApp('C:/Users/sowmi/.gemini/antigravity/scratch/terrapulse-r-analytics', host='127.0.0.1', port=3838, launch.browser=TRUE)"
```

### Method 3: In Posit RStudio
1. Open RStudio.
2. Open `app.R`.
3. Click the **Run App** button in the top right corner of the editor.

---

## 🖥️ UI Design & Dashboard Features

1. **Executive Overview**:
   - 4 Live KPI Value Cards (*Peak Surface Anomaly, At-Risk Population, Heat Hospitalizations, Grid Cooling Demand*).
   - Socio-Thermal Disparity scatter plot quantifying climate inequity.
   - Microclimate Heat Vulnerability Index (MHVI) horizontal ranking.

2. **Spatial Cartography**:
   - Coordinate centroid thermal distribution map.
   - Evapotranspiration cooling curve (Tree Canopy % vs. Surface Temperature).
   - Detailed per-district infrastructure cards.

3. **Intervention Sandbox**:
   - Real-time reactive sliders (*Cool Roof %, Tree Canopy Expansion %, Reflective Pavement %*).
   - One-click policy presets (*"Cool Roof Sprint", "Urban Canopy Plan", "Balanced Resilient"*).
   - Before-vs-After temperature suppression and hospitalizations averted charts.

4. **ROI & Capital Optimizer**:
   - Municipal Budget allocation slider (\$2M - \$30M).
   - 10-Year Cumulative Cash Flow and Payback projection.
   - Algorithmic district prioritization ranking.

5. **Telemetry Explorer**:
   - Searchable, risk-tier filtered data table.
   - One-click CSV export.

---

## 🔬 Core R Dependencies
- `shiny` (Web application framework)
- `bslib` (Modern Bootstrap 5 UI & Dark theme system)
- `ggplot2` (Statistical data visualization)
- `dplyr` & `tidyr` (Data wrangling & reshaping)
- `scales` (Number and currency formatting)
