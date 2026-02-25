# 📈 Time Series Econometrics – ECO 402

This repository contains my coursework, coding exercises, and structured R lessons for **Time Series Econometrics (ECO 402)**.

The goal of this repository is to build a clean, reproducible, and well-organized workflow for learning and applying time series econometrics using R.

This project is developed as part of my undergraduate studies in Quantitative Economics at Berea College.

---


## 📂 Repository Structure

```
Time-Series-Econometrics-ECO-402/
├── Assignments/
│   └── Homework, problem sets, and course-related submissions
├── Data/
│   └── Course datasets and raw data used in assignments
├── R for Time Series/
│   ├── data/
│   │   └── Cleaned and processed datasets used across lessons
│   ├── output/
│   │   └── Generated figures and model outputs
│   ├── 00_setup_and_data.R
│   ├── 01_ts_objects_and_indexing.R
│   ├── 02_visualization_and_decomposition.R
│   ├── 03_stationarity_and_transformations.R
│   ├── 04_acf_pacf_and_whitenoise.R
│   ├── 05_simulating_AR_MA_ARMA.R
│   ├── 06_ARIMA_estimation_and_interpretation.R
│   ├── 07_diagnostics_residuals_and_tests.R
│   ├── 08_forecasting_and_accuracy.R
│   ├── 09_unit_root_tests.R
│   ├── 10_ADL_and_dynamic_regression.R
│   ├── 11_VAR_and_granger_causality.R
│   ├── 12_cointegration_VECM.R
│   ├── 13_volatility_ARCH_GARCH.R
│   ├── 14_state_space_and_kalman.R
│   └── 15_case_study_full_workflow.R
├── Time Series Econometrics.Rproj
├── .gitignore
└── README.md


```

## 💻 R for Time Series Lessons

The `R for Time Series` folder contains a structured and progressive series of R scripts designed to build practical time series econometrics skills.

Each lesson file is:

- Self-contained  
- Clearly sectioned  
- Reproducible  
- Based on applied macroeconomic data  
- Written with clean and organized code  

The lessons cover:

- Time series objects and indexing  
- Visualization and classical decomposition  
- Stationarity and transformations  
- White noise processes  
- ACF and PACF  
- AR, MA, and ARMA models  
- ARIMA estimation and interpretation  
- Residual diagnostics and model testing  
- Forecasting and forecast evaluation  
- Unit root tests (ADF, KPSS)  
- Dynamic regression models (ADL)  
- VAR models and Granger causality  
- Cointegration and VECM  
- ARCH and GARCH volatility models  
- State-space models and Kalman filtering  
- A full applied time series case study  

The structure moves from foundational concepts to advanced econometric modeling.

---

## 🗃️ Dataset Used in Lessons

The R lessons build and reuse a shared monthly macroeconomic dataset constructed from FRED data:

- CPI (CPIAUCSL)  
- Unemployment Rate (UNRATE)  
- Federal Funds Rate (FEDFUNDS)  
- Industrial Production (INDPRO)  

The dataset is:

- Downloaded programmatically  
- Cleaned and transformed  
- Saved locally as `.rds`  
- Reused across all lesson files  

This ensures consistency, reproducibility, and a professional workflow.

---

## 🔁 Reproducibility

This repository is structured as an RStudio Project.

To use this repository:

1. Clone the repository  
2. Open `Time Series Econometrics.Rproj` in RStudio  
3. Run scripts inside `R for Time Series`  

Project paths are managed using the `here` package.  
No hard-coded absolute paths are used.


---

## 👤 Author

**Feda Mohammadi**  
Berea College  
Quantitative Economics & Mathematics  
Minor in Computer Science
