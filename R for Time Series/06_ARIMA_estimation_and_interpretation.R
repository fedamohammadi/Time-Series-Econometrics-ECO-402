# ===========================================================
#   Learning objectives:
#   - Estimate AR, MA, and ARMA models and recover known parameters
#   - Understand the "I" in ARIMA (integrated = differencing)
#   - Use AIC and BIC to choose between competing models
#   - Run residual diagnostics (Ljung-Box test, residual ACF)
#   - Fit ARIMA models to real macroeconomic data
#   - Interpret estimated coefficients in economic terms
#   - Use automatic model selection as a sanity check
#
#   Data source:
#     macro_monthly.rds from 00_setup_and_data.R (for real data sections)
#     Simulated series (for parameter recovery sections)
# ===========================================================

rm(list = ls())

library(here)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(patchwork)

DATA_DIR  <- here("R for Time Series", "data")
OUTPUT_DIR <- here("R for Time Series", "output")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(42)
n <- 500


# ==========================================================
# 1) Why start with simulated data?
# ==========================================================

# Before we touch real data, we estimate models on simulated series
# where we KNOW the true parameters. This is how you build trust
# in a method: if it can't recover parameters you chose yourself,
# you have no business applying it to data where you don't know
# the truth.
#
# We simulate, estimate, and compare. If the estimates are close
# to the true values, good. If not, we figure out why.


# ==========================================================
# 2) Parameter recovery: AR(1) with arima()
# ==========================================================

# True model: y_t = 0.7 * y_{t-1} + epsilon_t
# We expect the estimated phi to be close to 0.7.

true_phi <- 0.7
ar1_sim <- arima.sim(model = list(ar = true_phi), n = n)

# Base R arima() takes order = c(p, d, q)
# For AR(1): order = c(1, 0, 0)
# include.mean = TRUE by default (estimates a constant)

fit_ar1 <- arima(ar1_sim, order = c(1, 0, 0))
print(fit_ar1)

# Extract the estimated AR coefficient
cat("\n--- AR(1) Parameter Recovery ---\n")
cat("True phi:      ", true_phi, "\n")
cat("Estimated phi: ", round(coef(fit_ar1)["ar1"], 4), "\n")
cat("Std error:     ", round(sqrt(fit_ar1$var.coef["ar1", "ar1"]), 4), "\n")

# The estimate should be close to 0.7 but not exactly 0.7.
# With n = 500, the standard error is roughly 1/sqrt(n) ≈ 0.045,
# so we expect the estimate within about 0.09 of the true value
# most of the time.


# ==========================================================
# 3) Parameter recovery: MA(1)
# ==========================================================

# True model: y_t = epsilon_t + 0.6 * epsilon_{t-1}
# We expect the estimated theta to be close to 0.6.

true_theta <- 0.6
ma1_sim <- arima.sim(model = list(ma = true_theta), n = n)

fit_ma1 <- arima(ma1_sim, order = c(0, 0, 1))
print(fit_ma1)

cat("\n--- MA(1) Parameter Recovery ---\n")
cat("True theta:      ", true_theta, "\n")
cat("Estimated theta: ", round(coef(fit_ma1)["ma1"], 4), "\n")
cat("Std error:       ", round(sqrt(fit_ma1$var.coef["ma1", "ma1"]), 4), "\n")


# ==========================================================
# 4) Parameter recovery: ARMA(1,1)
# ==========================================================

# True model: y_t = 0.7 * y_{t-1} + epsilon_t + 0.4 * epsilon_{t-1}
# This is harder to estimate than pure AR or pure MA because the
# AR and MA components can trade off against each other.

true_phi_arma   <- 0.7
true_theta_arma <- 0.4

arma11_sim <- arima.sim(
  model = list(ar = true_phi_arma, ma = true_theta_arma),
  n = n
)

fit_arma11 <- arima(arma11_sim, order = c(1, 0, 1))
print(fit_arma11)

cat("\n--- ARMA(1,1) Parameter Recovery ---\n")
cat("True phi:        ", true_phi_arma, "\n")
cat("Estimated phi:   ", round(coef(fit_arma11)["ar1"], 4), "\n")
cat("True theta:      ", true_theta_arma, "\n")
cat("Estimated theta: ", round(coef(fit_arma11)["ma1"], 4), "\n")

# ARMA estimation is less precise than pure AR or MA.
# The AR and MA coefficients are correlated in the likelihood,
# which inflates standard errors. This is normal.


# ==========================================================
# 5) What happens when you fit the wrong model?
# ==========================================================

# The simulated series is AR(1) with phi = 0.7.
# What if we mistakenly fit an MA(1) to it?

ar1_for_misspec <- arima.sim(model = list(ar = 0.7), n = n)

fit_correct <- arima(ar1_for_misspec, order = c(1, 0, 0))
fit_wrong   <- arima(ar1_for_misspec, order = c(0, 0, 1))

cat("\n--- Correct Specification: AR(1) ---\n")
cat("AIC: ", AIC(fit_correct), "\n")
cat("BIC: ", BIC(fit_correct), "\n")

cat("\n--- Wrong Specification: MA(1) ---\n")
cat("AIC: ", AIC(fit_wrong), "\n")
cat("BIC: ", BIC(fit_wrong), "\n")

# The correctly specified model should have lower AIC and BIC.
# If the MA(1) somehow fits better, increase n or try again.
# The point: information criteria penalize bad fits.


# ==========================================================
# 6) Model selection with AIC and BIC
# ==========================================================

# AIC = -2 * loglik + 2 * k
# BIC = -2 * loglik + log(n) * k
#
# where k = number of estimated parameters, n = sample size.
#
# AIC tends to pick slightly larger models (more parameters).
# BIC penalizes complexity more heavily and tends to pick
# more parsimonious models.
#
# Lower is better for both.
#
# Let's fit several models to our AR(1) simulation and compare.

candidates <- list(
  "AR(1)"     = arima(ar1_for_misspec, order = c(1, 0, 0)),
  "AR(2)"     = arima(ar1_for_misspec, order = c(2, 0, 0)),
  "MA(1)"     = arima(ar1_for_misspec, order = c(0, 0, 1)),
  "MA(2)"     = arima(ar1_for_misspec, order = c(0, 0, 2)),
  "ARMA(1,1)" = arima(ar1_for_misspec, order = c(1, 0, 1)),
  "ARMA(2,1)" = arima(ar1_for_misspec, order = c(2, 0, 1))
)

ic_table <- tibble(
  model = names(candidates),
  aic   = map_dbl(candidates, AIC),
  bic   = map_dbl(candidates, BIC)
) %>%
  arrange(aic)

cat("\n--- Information Criteria Comparison ---\n")
print(ic_table)

# The AR(1) should win or be very close to the top.
# If AR(2) is close, check whether the second coefficient
# is statistically insignificant (i.e., the extra parameter
# adds nothing).

write_csv(ic_table, file.path(OUTPUT_DIR, "06_ic_comparison_simulated.csv"))


# ==========================================================
# 7) The "I" in ARIMA: differencing nonstationary series
# ==========================================================

# ARIMA(p, d, q) means:
#   - Difference the series d times to make it stationary
#   - Then fit an ARMA(p, q) to the differenced series
#
# For example, if CPI is nonstationary (trending upward),
# we cannot fit an ARMA directly to CPI levels.
# But the first difference (or log-difference) might be stationary.
#
# ARIMA(1, 1, 0) on CPI is the same as AR(1) on delta(CPI).
#
# When d = 1 in arima(), the function differences internally
# before estimating. You do not need to difference by hand.

# Let's demonstrate with a simulated random walk
rw <- cumsum(rnorm(n))

# Trying to fit AR(1) to the random walk (bad idea)
fit_rw_ar1 <- arima(rw, order = c(1, 0, 0))
cat("\n--- AR(1) on Random Walk (wrong approach) ---\n")
cat("Estimated phi:", round(coef(fit_rw_ar1)["ar1"], 4), "\n")
# phi will be very close to 1.0, confirming nonstationarity.
# The model is not wrong per se, but it tells you nothing useful.

# Correct approach: ARIMA(0, 1, 0) = difference once, fit white noise
fit_rw_arima <- arima(rw, order = c(0, 1, 0))
cat("\n--- ARIMA(0,1,0) on Random Walk (correct approach) ---\n")
print(fit_rw_arima)
# After differencing, the residuals should look like white noise,
# because that's exactly what a random walk's first difference is.


# ==========================================================
# 8) Loading real data
# ==========================================================

macro <- readRDS(file.path(DATA_DIR, "macro_monthly.rds"))

# Build the transformed series (same as file 04)
macro_full <- macro %>%
  arrange(date) %>%
  mutate(
    log_cpi          = log(cpi),
    inflation_pct    = 100 * (log(cpi) - lag(log(cpi))),
    log_indpro       = log(indpro),
    indpro_growth_pct = 100 * (log(indpro) - lag(log(indpro)))
  ) %>%
  drop_na(inflation_pct, indpro_growth_pct)

# We build separate tsibbles for each series below.
# Important: do NOT use fill_gaps() and then filter(!is.na(...)).
# That removes rows from a tsibble, creating implicit gaps in the
# time index, and fable::ARIMA() will refuse to fit.
# Instead, build each tsibble from the already-clean data so
# the time index is continuous with no gaps.


# ==========================================================
# 9) ARIMA on CPI level (the nonstationary case)
# ==========================================================

# CPI is clearly nonstationary (upward trend, file 04 showed
# persistent ACF). We need to difference it.
#
# Strategy:
#   1. Fit ARIMA(p, 1, q) to CPI level (let d = 1 handle the trend)
#   2. Compare to fitting ARMA(p, q) to inflation directly
#   Both should give similar results because ARIMA(p,1,q) on CPI
#   is algebraically equivalent to ARMA(p,q) on delta(log CPI)...
#   approximately, since we're using levels here, not logs.

# Build a CPI-only tsibble from the clean data (no NAs, no gaps)
macro_cpi <- macro_full %>%
  as_tibble() %>%
  transmute(date = yearmonth(date), cpi = cpi) %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

# Let fable::ARIMA() choose the best model automatically
fit_cpi_auto <- macro_cpi %>%
  model(auto = ARIMA(cpi))

cat("\n--- Automatic ARIMA on CPI Level ---\n")
report(fit_cpi_auto)

# The automatic selection will almost certainly pick d = 1 or d = 2
# because CPI is nonstationary. This confirms what we saw in the ACF.


# ==========================================================
# 10) Fitting models to inflation
# ==========================================================

# Inflation = 100 * delta(log CPI) is approximately stationary.
# We can fit ARMA models directly (d = 0).

macro_infl <- macro_full %>%
  as_tibble() %>%
  transmute(date = yearmonth(date), inflation_pct = inflation_pct) %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

# Fit several candidate models using fable
fit_infl <- macro_infl %>%
  model(
    ar1      = ARIMA(inflation_pct ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
    ar2      = ARIMA(inflation_pct ~ pdq(2, 0, 0) + PDQ(0, 0, 0)),
    ma1      = ARIMA(inflation_pct ~ pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11   = ARIMA(inflation_pct ~ pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21   = ARIMA(inflation_pct ~ pdq(2, 0, 1) + PDQ(0, 0, 0)),
    auto     = ARIMA(inflation_pct ~ PDQ(0, 0, 0))
  )

# Compare information criteria
cat("\n--- Model Comparison for Inflation ---\n")
ic_infl <- glance(fit_infl) %>%
  select(.model, AIC, AICc, BIC) %>%
  arrange(AICc)

print(ic_infl)

write_csv(ic_infl, file.path(OUTPUT_DIR, "06_ic_comparison_inflation.csv"))

# Report the best model
cat("\n--- Best Model for Inflation (by AICc) ---\n")
best_infl_name <- ic_infl$.model[1]
report(fit_infl %>% select(all_of(best_infl_name)))

# Also report the auto-selected model
cat("\n--- Auto-Selected Model for Inflation ---\n")
report(fit_infl %>% select(auto))


# ==========================================================
# 11) Fitting models to industrial production growth
# ==========================================================

macro_indpro <- macro_full %>%
  as_tibble() %>%
  transmute(date = yearmonth(date), indpro_growth_pct = indpro_growth_pct) %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

fit_indpro <- macro_indpro %>%
  model(
    ar1      = ARIMA(indpro_growth_pct ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
    ar2      = ARIMA(indpro_growth_pct ~ pdq(2, 0, 0) + PDQ(0, 0, 0)),
    ma1      = ARIMA(indpro_growth_pct ~ pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11   = ARIMA(indpro_growth_pct ~ pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21   = ARIMA(indpro_growth_pct ~ pdq(2, 0, 1) + PDQ(0, 0, 0)),
    auto     = ARIMA(indpro_growth_pct ~ PDQ(0, 0, 0))
  )

cat("\n--- Model Comparison for Industrial Production Growth ---\n")
ic_indpro <- glance(fit_indpro) %>%
  select(.model, AIC, AICc, BIC) %>%
  arrange(AICc)

print(ic_indpro)

write_csv(ic_indpro, file.path(OUTPUT_DIR, "06_ic_comparison_indpro.csv"))

cat("\n--- Best Model for IP Growth (by AICc) ---\n")
best_indpro_name <- ic_indpro$.model[1]
report(fit_indpro %>% select(all_of(best_indpro_name)))

cat("\n--- Auto-Selected Model for IP Growth ---\n")
report(fit_indpro %>% select(auto))


# ==========================================================
# 12) Residual diagnostics
# ==========================================================

# After fitting a model, we check whether the residuals look
# like white noise. If they do, the model has captured the
# time dependence in the data. If they don't, the model is
# missing something.
#
# Two main checks:
#   a) Visual: plot residuals, their ACF, and a histogram
#   b) Formal: Ljung-Box test
#      H0: residuals are white noise (no autocorrelation up to lag h)
#      H1: residuals have autocorrelation at some lag
#      If p-value < 0.05, reject H0 => model is inadequate.

# --- Diagnostics for the best inflation model ---

cat("\n--- Residual Diagnostics for Best Inflation Model ---\n")

# fable provides a convenient gg_tsresiduals() function
p_resid_infl <- fit_infl %>%
  select(all_of(best_infl_name)) %>%
  gg_tsresiduals() +
  ggtitle(paste("Residual Diagnostics: Inflation,", best_infl_name))

print(p_resid_infl)

ggsave(
  filename = file.path(OUTPUT_DIR, "06_residuals_inflation.png"),
  plot = p_resid_infl,
  width = 10,
  height = 7
)

# Ljung-Box test on residuals
# We test at lag 10 (a common choice for monthly data)
resid_infl <- fit_infl %>%
  select(all_of(best_infl_name)) %>%
  augment() %>%
  pull(.innov)

lb_infl <- Box.test(resid_infl, lag = 10, type = "Ljung-Box")

cat("Ljung-Box test (lag 10) for inflation residuals:\n")
cat("  Test statistic:", round(lb_infl$statistic, 4), "\n")
cat("  p-value:       ", round(lb_infl$p.value, 4), "\n")
if (lb_infl$p.value > 0.05) {
  cat("  => Fail to reject H0. Residuals consistent with white noise.\n")
} else {
  cat("  => Reject H0. Residuals still have autocorrelation.\n")
  cat("     The model may need more AR or MA terms.\n")
}

# --- Diagnostics for the best IP growth model ---

cat("\n--- Residual Diagnostics for Best IP Growth Model ---\n")

p_resid_indpro <- fit_indpro %>%
  select(all_of(best_indpro_name)) %>%
  gg_tsresiduals() +
  ggtitle(paste("Residual Diagnostics: IP Growth,", best_indpro_name))

print(p_resid_indpro)

ggsave(
  filename = file.path(OUTPUT_DIR, "06_residuals_indpro_growth.png"),
  plot = p_resid_indpro,
  width = 10,
  height = 7
)

resid_indpro <- fit_indpro %>%
  select(all_of(best_indpro_name)) %>%
  augment() %>%
  pull(.innov)

lb_indpro <- Box.test(resid_indpro, lag = 10, type = "Ljung-Box")

cat("Ljung-Box test (lag 10) for IP growth residuals:\n")
cat("  Test statistic:", round(lb_indpro$statistic, 4), "\n")
cat("  p-value:       ", round(lb_indpro$p.value, 4), "\n")
if (lb_indpro$p.value > 0.05) {
  cat("  => Fail to reject H0. Residuals consistent with white noise.\n")
} else {
  cat("  => Reject H0. Residuals still have autocorrelation.\n")
  cat("     The model may need more AR or MA terms.\n")
}


# ==========================================================
# 13) Interpreting coefficients
# ==========================================================

# This is where most students stop too early. Getting a coefficient
# is not the same as understanding what it means.
#
# For an AR(1) on inflation:
#   inflation_t = c + phi * inflation_{t-1} + epsilon_t
#
#   phi = 0.6 means: if inflation this month is 1 percentage point
#   above its long-run mean, we expect it to be about 0.6 pp above
#   the mean next month. The shock is persistent but fading.
#
#   The half-life of a shock is: log(0.5) / log(|phi|)
#   For phi = 0.6: half-life = log(0.5)/log(0.6) ≈ 1.36 months
#   For phi = 0.9: half-life = log(0.5)/log(0.9) ≈ 6.58 months
#
# For an MA(1) on inflation:
#   inflation_t = c + epsilon_t + theta * epsilon_{t-1}
#
#   theta = 0.4 means: an unexpected shock today carries over with
#   40% of its original impact into next month, then vanishes.
#   MA shocks are temporary by construction.
#
# The long-run mean (unconditional mean) of an AR(1):
#   E[y] = c / (1 - phi)
#   This is the level the series gravitates toward.

# Let's compute half-lives for the best inflation model
cat("\n--- Interpreting the Best Inflation Model ---\n")
best_infl_report <- fit_infl %>%
  select(all_of(best_infl_name)) %>%
  tidy()

print(best_infl_report)

# Extract AR coefficients if they exist
ar_terms <- best_infl_report %>%
  filter(str_detect(term, "^ar"))

if (nrow(ar_terms) > 0) {
  # For AR(1), the half-life formula is straightforward
  phi_hat <- ar_terms$estimate[1]
  if (abs(phi_hat) < 1 & abs(phi_hat) > 0) {
    half_life <- log(0.5) / log(abs(phi_hat))
    cat("Estimated AR(1) coefficient: ", round(phi_hat, 4), "\n")
    cat("Half-life of a shock:        ", round(half_life, 2), " months\n")
    cat("This means an inflation shock loses half its effect in about",
        round(half_life, 1), "months.\n")
  }
}


# ==========================================================
# 14) Comparing base arima() and fable::ARIMA()
# ==========================================================

# Students should know both interfaces.
# Base arima() is transparent and works everywhere.
# fable::ARIMA() integrates with the tsibble/tidyverse workflow.
#
# They should give you the same estimates (up to numerical precision).

# Fit AR(2) to inflation using base arima()
infl_vec <- macro_infl %>%
  as_tibble() %>%
  filter(!is.na(inflation_pct)) %>%
  pull(inflation_pct)

fit_base_ar2 <- arima(infl_vec, order = c(2, 0, 0))
cat("\n--- Base arima() AR(2) on Inflation ---\n")
print(fit_base_ar2)

# Same thing with fable
cat("\n--- fable::ARIMA() AR(2) on Inflation ---\n")
report(fit_infl %>% select(ar2))

# Compare the coefficients side by side
cat("\n--- Coefficient Comparison ---\n")
cat("Base arima ar1: ", round(coef(fit_base_ar2)["ar1"], 5), "\n")
cat("fable     ar1: ",
    round((fit_infl %>% select(ar2) %>% tidy() %>%
             filter(term == "ar1") %>% pull(estimate)), 5), "\n")


# ==========================================================
# 15) A note on seasonal ARIMA
# ==========================================================

# You may have noticed PDQ(0, 0, 0) in our model specifications.
# That turns off the seasonal component.
#
# Monthly macro data often has seasonal patterns (e.g., retail sales
# spike in December). A seasonal ARIMA or SARIMA adds seasonal
# AR and MA terms at the seasonal lag (lag 12 for monthly data).
#
# The full notation is ARIMA(p, d, q)(P, D, Q)[s] where:
#   p, d, q = non-seasonal AR order, differencing, MA order
#   P, D, Q = seasonal AR order, differencing, MA order
#   s = seasonal period (12 for monthly, 4 for quarterly)
#
# We will cover this in a later file. For now, we suppress it
# with PDQ(0, 0, 0) to focus on the non-seasonal structure.

# But here's a preview of what auto-selection picks if we
# let the seasonal component be free:

fit_infl_seasonal <- macro_infl %>%
  model(auto_seasonal = ARIMA(inflation_pct))

cat("\n--- Auto ARIMA with Seasonal Terms Allowed ---\n")
report(fit_infl_seasonal)

# Compare AICc with and without seasonal terms
cat("\nAICc without seasonal:", glance(fit_infl %>% select(auto))$AICc, "\n")
cat("AICc with seasonal:   ", glance(fit_infl_seasonal)$AICc, "\n")


# ==========================================================
# 16) Summary of the estimation workflow
# ==========================================================

# The standard workflow for fitting ARIMA models:
#
# Step 1: Plot the series. Is it stationary?
#         If not, difference it (or log-difference it).
#
# Step 2: Plot ACF and PACF of the (differenced) series.
#         Use the identification table from file 05:
#           ACF decays, PACF cuts off at p  => try AR(p)
#           ACF cuts off at q, PACF decays  => try MA(q)
#           Both decay                      => try ARMA(p,q)
#
# Step 3: Estimate candidate models. Compare AIC/BIC.
#
# Step 4: Check residuals of the best model.
#         - Are they white noise? (Ljung-Box test)
#         - Is the ACF of residuals clean?
#         - Does the histogram look roughly normal?
#
# Step 5: If residuals are not white noise, go back to step 2
#         and try a different specification.
#
# Step 6: Interpret the coefficients.
#         - What is the persistence (half-life)?
#         - What is the long-run mean?
#         - Does the model make economic sense?
#
# Step 7: Use the model for forecasting (next file).
#
# This is sometimes called the Box-Jenkins methodology.
# It's old (1970s) but still the backbone of time series practice.

workflow_summary <- tibble(
  step = 1:7,
  action = c(
    "Plot the series and assess stationarity",
    "Plot ACF/PACF and identify candidate models (AR, MA, ARMA)",
    "Estimate candidates and compare AIC/BIC",
    "Check residual diagnostics (Ljung-Box, ACF, histogram)",
    "If residuals fail, revise the model and re-estimate",
    "Interpret coefficients (persistence, half-life, long-run mean)",
    "Forecast (covered in the next file)"
  )
)

print(workflow_summary)

write_csv(workflow_summary, file.path(OUTPUT_DIR, "06_box_jenkins_workflow.csv"))

