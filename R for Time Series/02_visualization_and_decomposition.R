############################################################
# Purpose:
#   - Visualize time series clearly and consistently
#   - Identify trend, seasonality, and outliers
#   - Perform decomposition:
#       (1) Classical decomposition (trend + seasonal + remainder)
#       (2) STL decomposition (more flexible and preferred)
#
############################################################

# ==========================================================
# 0) Setup
# ==========================================================

install.packages("ggtime")
library(ggtime)

rm(list = ls())

library(here)
library(tidyverse)
library(tsibble)
library(feasts)

DATA_DIR <- here("R for Time Series", "data")
OUTPUT_DIR <- here("R for Time Series", "output")

macro <- readRDS(file.path(DATA_DIR, "macro_monthly.rds"))

glimpse(macro)

# ==========================================================
# 1) Basic Time Series Plots (Clean and readable)
# ==========================================================

# CPI level (nonstationary, strong trend)
p_cpi <- macro %>%
  ggplot(aes(x = date, y = cpi)) +
  geom_line() +
  labs(
    title = "CPI (Level)",
    x = "Date",
    y = "CPI"
  )

print(p_cpi)

# Inflation (already computed in setup file)
p_infl <- macro %>%
  ggplot(aes(x = date, y = infl_cpi)) +
  geom_line() +
  labs(
    title = "Inflation (Approx: 100 × Δlog(CPI))",
    x = "Date",
    y = "Percent"
  )

print(p_infl)

# Unemployment rate (more cyclical)
p_unrate <- macro %>%
  ggplot(aes(x = date, y = unrate)) +
  geom_line() +
  labs(
    title = "Unemployment Rate",
    x = "Date",
    y = "Percent"
  )

print(p_unrate)

# Optional: Save plots
ggsave(file.path(OUTPUT_DIR, "plot_cpi_level.png"), p_cpi, width = 9, height = 4.5)
ggsave(file.path(OUTPUT_DIR, "plot_inflation.png"), p_infl, width = 9, height = 4.5)
ggsave(file.path(OUTPUT_DIR, "plot_unrate.png"), p_unrate, width = 9, height = 4.5)

# ==========================================================
# 2) Seasonal Plots (Do we see systematic monthly patterns?)
# ==========================================================

# Season plot: CPI inflation by month
macro <- macro %>% fill_gaps()

macro %>%
  gg_season(infl_cpi) +
  labs(
    title = "Seasonal Plot: Inflation by Month",
    y = "Inflation (Approx %)"
  )

macro %>%
  ggtime::gg_subseries(infl_cpi) +
  labs(
    title = "Subseries Plot: Inflation by Month",
    y = "Inflation (Approx %)"
  )

# ==========================================================
# 3) Classical Decomposition (Trend + Seasonal + Remainder)
# ==========================================================

# Classical decomposition needs a ts object with frequency
# We'll decompose CPI (levels) and also inflation (if it makes sense)

cpi_ts <- ts(
  macro$cpi,
  start = c(year(min(macro$date)), month(min(macro$date))),
  frequency = 12
)

# Additive decomposition:
# y_t = trend_t + seasonal_t + remainder_t
cpi_decomp_add <- decompose(cpi_ts, type = "additive")

plot(cpi_decomp_add, main = "Classical Additive Decomposition: CPI")

# Note:
# Multiplicative decomposition:
# y_t = trend_t × seasonal_t × remainder_t
# This is only reasonable if seasonal variation grows with the level.
cpi_decomp_mult <- decompose(cpi_ts, type = "multiplicative")
plot(cpi_decomp_mult, main = "Classical Multiplicative Decomposition: CPI")

# ==========================================================
# 4) STL Decomposition (Better in practice)
# ==========================================================

# STL is more flexible than classical decomposition.
# It handles changing seasonality better.

cpi_tsbl <- macro %>% select(date, cpi)

cpi_stl <- cpi_tsbl %>%
  model(STL(cpi))

cpi_stl_components <- components(cpi_stl)

autoplot(cpi_stl_components) +
  labs(title = "STL Decomposition Components: CPI")

# ==========================================================
# 5) Decomposition for a More Stationary Series
# ==========================================================

# Inflation is closer to stationary, but may still have seasonality.
infl_tsbl <- macro %>% select(date, infl_cpi)

infl_stl <- infl_tsbl %>%
  model(STL(infl_cpi))

infl_components <- components(infl_stl)

autoplot(infl_components) +
  labs(title = "STL Decomposition Components: Inflation")

# ==========================================================
# 6) What You Should Take Away (Short, real summary)
# ==========================================================

# - Levels (like CPI) almost always show trend (nonstationary)
# - Inflation (difference of logs) reduces trend and is closer to stationary
# - Classical decomposition is fine for learning, but is rigid
# - STL is what you should use when seasonality changes over time
# - Seasonal plots help you detect stable seasonal patterns quickly

############################################################
# End of file
############################################################