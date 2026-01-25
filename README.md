# Eurostat Sectoral and Total Productivity Analysis
### Project Objective
This project constructs a sector-level panel dataset to analyze the relationship between **Export Intensity** (exports as a % of output) and **Total Factor Productivity (TFP) Growth** across five European economies (Austria, Germany, Netherlands, Portugal, Spain).

This project aims to demonstrate an end-to-end **Data analysis & Econometrics workflow** in R, showcasing skills in:
* **Data wrangling:** Cleaning, harmonizing, and merging multi-dimensional Eurostat datasets (National Accounts, Employment, Capital Stock).
* **Macroeconomic Modeling:** Constructing TFP variables using Cobb-Douglas production functions and calculating log-difference growth rates.
* **Advanced Visualization:** creating complex, multi-layer time-series plots with dynamic recession shading and faceted sectoral breakdowns.
* **Econometric Inference:** Implementing Fixed Effects (FE) models with clustered standard errors to control for unobserved heterogeneity.

### Reproducibility
This project is designed for full portability. It uses the `pacman` package for dependency and paths management.
1.  Clone this repository.
2.  Open `Eurostat_macro_analysis.Rproj` in RStudio.
3.  Run `dofile_Eurostat_macro_analysis.R`.
