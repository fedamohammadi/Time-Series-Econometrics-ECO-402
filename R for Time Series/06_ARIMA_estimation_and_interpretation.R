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


