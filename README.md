# Variant patterns and influence of inter-regional travel during the SARS-CoV-2 expansion in South Africa

## Overview
This repository contains materials related to the research paper titled **"Variant patterns and influence of inter-regional travel during the SARS-CoV-2 expansion in South Africa"** by Weiyu Luo et al. The study investigates the dynamic impacts of human mobility on COVID-19 transmission in South Africa during 2020, focusing on three types of mobility—provincial inflows, cross-district flows, and within-district flows—and their interactions with lockdown policies.

## Research Objectives
The study aims to address the following research questions:
- How do different types of human mobility affect SARS-CoV-2 transmission?
- How does the impact of mobility on transmission vary across different pandemic stages?
- How do lockdown policies influence human mobility over time?

## Methodology
Using **Location-Based Services (LBS) data**, we derived three novel mobility metrics:
1. **Type 1**: Provincial inflow trips (long-distance, inter-provincial).
2. **Type 2**: Cross-district trips within a province (mid-distance).
3. **Type 3**: Within-district trips (short-distance, local).

A **Structural Equation Model (SEM)** was applied to dynamic panel datasets covering January 1 to December 31, 2020, to quantify the relationships between mobility, daily new COVID-19 cases, and lockdown policies. The model incorporates socio-demographic, socio-economic, and policy-related covariates, accounting for lagged effects and temporal autocorrelation.

## Repository Contents
- **Data**: Aggregated mobility metrics and COVID-19 case data (subject to availability and privacy constraints).
- **Scripts**: Python/R codes for data processing, SEM implementation, and visualization (e.g., R scripts using the `piecewiseSEM` package).
- **Results**:
  - **Data Description**: Provides aggregated mobility metrics (Type 1, Type 2, Type 3) and daily new COVID-19 cases at the provincial level for 2020, standardized per million people. Includes descriptive statistics (mean, standard deviation, min/max) for endogenous variables (mobility metrics, cases) and exogenous variables (e.g., Relative Wealth Index, lockdown levels), offering a foundation for understanding mobility and transmission patterns.
  - **SEM Dynamic Coefficients**: Contains time-varying coefficients from the SEM, quantifying the daily impact of each mobility metric on COVID-19 cases across pandemic stages. Includes coefficients for Type 1, Type 2, and Type 3 metrics with p-values and standard errors, enabling analysis of how mobility influences transmission over time.
  - **Random Effects**: Captures random effects of lockdown levels on mobility metrics, showing how policy changes (e.g., Level 5 to Level 4) affected travel behavior across provinces. These results reveal variations in mobility responses, highlighting policy effectiveness and social distancing fatigue.
- **Supporting Information**: Additional tables, figures, and documentation referenced in the paper.

## Citation
If you use this work, please cite:
> Luo, W., Wu, X., Li, R., Fitzpatrick, M., Charurat, M., Blanco, N., Stafford, K., Naranbhai, V., Abimiku, A., Winters, A., & Xiong, C. (2025). Variant patterns and influence of inter-regional travel during the SARS-CoV-2 expansion in South Africa.

## Contact
For inquiries, please contact the corresponding author:
- **Chenfeng Xiong**: chenfeng.xiong@villanova.edu

We welcome contributions, feedback, and collaboration to further advance mobility and epidemic research.