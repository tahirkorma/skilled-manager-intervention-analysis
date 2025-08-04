# Estimating the Impact of Managers' Online Training on Manager-Employee Relationships

## 📌 Overview

This project investigates whether online training programs for managers improve their relationships with employees. Using a randomized controlled trial (RCT), the study assesses the causal impact of managerial training on employee-manager relationship quality and explores how this effect varies by gender (heterogeneity), is influenced by conflict resolution (mediation), and differs across job roles (moderation).

## 🎯 Objectives

- Estimate the **average treatment effect (ATE)** of managerial training on employee perceptions.
- Assess **gender-based heterogeneity** in training effectiveness.
- Examine **mediation effects** via conflict resolution ability.
- Explore **moderation effects** based on job role (front vs. back office).

## 🧪 Methodology

- **Design:** Randomized Controlled Trial (RCT)
- **Data Source:** Skilled Managers – Productive Workplaces (SMPW)
- **Sample:** 24 UK-based organizations across public and private sectors
- **Approach:** Mixed methods (quantitative + qualitative), Difference-in-Differences (DiD), OLS regression
- **Tools:** R (with mediation package), DAGs for causal inference

## 📊 Key Findings

- **No significant improvement** in manager-employee relationship quality post-training.
- Slight **positive effects in males**, but slight **negative impact in females**.
- **Conflict resolution skills (q7_s)** showed a small mediating effect, though not statistically significant.
- **Predictor `q8_s`** (perceived improvement after talking with the manager) was the strongest influence on relationship quality.

## 📁 Project Structure
    📦 project-directory/
    ├── total_effect.R # Average Treatment Effect (ATE)
    ├── heterogeneity_dropped_na.R # Heterogeneity with dropped NAs
    ├── heterogeneity_imputed.R # Heterogeneity with imputation
    ├── mediation.R # Mediation analysis
    ├── moderation.R # Moderation analysis
    ├── requirements.txt # List of required R packages
    ├── output/ # (Optional) Folder for any output files (plots, tables)
    └── README.md # This file


---

## ▶️ How to Run

1. **Clone the repository**:
   ```bash
   git clone https://github.com/tahirkorma/skilled-manager-intervention-analysis.git
   cd skilled-manager-intervention-analysis

2. **Install required packages in R**:
   ```bash
   install.packages(readLines("requirements.txt"))

3. **Run each script independently in R or RStudio**:
   ```bash
   source("total_effect.R")
   source("heterogeneity_dropped_na.R")
   source("heterogeneity_imputed.R")
   source("mediation.R")
   source("moderation.R")

📝 **License**
This research is conducted as part of an academic dissertation and is shared for educational purposes only.

🙏 **Acknowledgements**
Skilled Managers – Productive Workplaces (SMPW)

Supervisors and faculty at Durham University

Bowyer & Urwin (2024) for RCT design and data
