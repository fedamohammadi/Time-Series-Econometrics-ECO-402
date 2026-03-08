############################################################
#
# Purpose:
#   - Understand stationarity in practice
#   - Compare raw levels, logs, differences, and log differences
#   - Visualize how transformations can make a series more stable
#   - Prepare for ACF/PACF, ARIMA, and unit root testing
#
# Data source:
#   Uses macro_monthly.rds created in 00_setup_and_data.R
############################################################

# ==========================================================
# 0) Setup
# ==========================================================

rm(list = ls())

library(here)
library(tidyverse)
library(tsibble)
library(feasts)
library(lubridate)

DATA_DIR <- here("R for Time Series", "data")
OUTPUT_DIR <- here("R for Time Series", "output")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

macro <- readRDS(file.path(DATA_DIR, "macro_monthly.rds"))

glimpse(macro)

# ==========================================================
# 1) Building transformed series
# ==========================================================

macro_trans <- macro %>%
  arrange(date) %>%
  mutate(
    log_cpi = log(cpi),
    d_cpi = cpi - lag(cpi),
    d_log_cpi = log(cpi) - lag(log(cpi)),
    inflation_pct = 100 * d_log_cpi,
    
    log_indpro = log(indpro),
    d_indpro = indpro - lag(indpro),
    d_log_indpro = log(indpro) - lag(log(indpro)),
    indpro_growth_pct = 100 * d_log_indpro
  )

glimpse(macro_trans)


# ==========================================================
# 2) Visual comparison: CPI in different forms
# ==========================================================

p_cpi_level <- macro_trans %>%
  filter(!is.na(cpi)) %>%
  ggplot(aes(x = date, y = cpi)) +
  geom_line() +
  labs(
    title = "CPI in Levels",
    x = "Date",
    y = "CPI"
  )

print(p_cpi_level)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_cpi_level.png"),
  plot = p_cpi_level,
  width = 9,
  height = 4.5
)

p_log_cpi <- macro_trans %>%
  filter(!is.na(log_cpi)) %>%
  ggplot(aes(x = date, y = log_cpi)) +
  geom_line() +
  labs(
    title = "Log CPI",
    x = "Date",
    y = "log(CPI)"
  )

print(p_log_cpi)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_log_cpi.png"),
  plot = p_log_cpi,
  width = 9,
  height = 4.5
)

p_d_cpi <- macro_trans %>%
  filter(!is.na(d_cpi)) %>%
  ggplot(aes(x = date, y = d_cpi)) +
  geom_line() +
  labs(
    title = "First Difference of CPI",
    x = "Date",
    y = expression(Delta * CPI)
  )

print(p_d_cpi)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_d_cpi.png"),
  plot = p_d_cpi,
  width = 9,
  height = 4.5
)

p_inflation <- macro_trans %>%
  filter(!is.na(inflation_pct)) %>%
  ggplot(aes(x = date, y = inflation_pct)) +
  geom_line() +
  labs(
    title = "Inflation Rate: 100 × Δlog(CPI)",
    x = "Date",
    y = "Percent"
  )

print(p_inflation)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_inflation_pct.png"),
  plot = p_inflation,
  width = 9,
  height = 4.5
)


# ==========================================================
# 3) Visual comparison: Industrial Production in different forms
# ==========================================================

p_indpro_level <- macro_trans %>%
  filter(!is.na(indpro)) %>%
  ggplot(aes(x = date, y = indpro)) +
  geom_line() +
  labs(
    title = "Industrial Production in Levels",
    x = "Date",
    y = "INDPRO"
  )

print(p_indpro_level)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_indpro_level.png"),
  plot = p_indpro_level,
  width = 9,
  height = 4.5
)

p_indpro_growth <- macro_trans %>%
  filter(!is.na(indpro_growth_pct)) %>%
  ggplot(aes(x = date, y = indpro_growth_pct)) +
  geom_line() +
  labs(
    title = "Industrial Production Growth: 100 × Δlog(INDPRO)",
    x = "Date",
    y = "Percent"
  )

print(p_indpro_growth)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_indpro_growth_pct.png"),
  plot = p_indpro_growth,
  width = 9,
  height = 4.5
)


# ==========================================================
# 4) Rolling mean and variance intuition
# ==========================================================

# A stationary series should have a roughly stable mean and variance over time.
# We use simple rolling windows for visual intuition only.

roll_mean_12 <- function(x) {
  stats::filter(x, rep(1 / 12, 12), sides = 1)
}

roll_var_12 <- function(x) {
  n <- length(x)
  out <- rep(NA, n)
  for (i in 12:n) {
    out[i] <- var(x[(i - 11):i], na.rm = TRUE)
  }
  out
}

macro_roll <- macro_trans %>%
  mutate(
    roll_mean_cpi = as.numeric(roll_mean_12(cpi)),
    roll_mean_infl = as.numeric(roll_mean_12(inflation_pct)),
    roll_var_cpi = roll_var_12(cpi),
    roll_var_infl = roll_var_12(inflation_pct)
  )

p_roll_mean_cpi <- macro_roll %>%
  filter(!is.na(roll_mean_cpi)) %>%
  ggplot(aes(x = date, y = roll_mean_cpi)) +
  geom_line() +
  labs(
    title = "12-Month Rolling Mean of CPI",
    x = "Date",
    y = "Rolling Mean"
  )

print(p_roll_mean_cpi)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_roll_mean_cpi.png"),
  plot = p_roll_mean_cpi,
  width = 9,
  height = 4.5
)

p_roll_mean_infl <- macro_roll %>%
  filter(!is.na(roll_mean_infl)) %>%
  ggplot(aes(x = date, y = roll_mean_infl)) +
  geom_line() +
  labs(
    title = "12-Month Rolling Mean of Inflation",
    x = "Date",
    y = "Rolling Mean"
  )

print(p_roll_mean_infl)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_roll_mean_inflation.png"),
  plot = p_roll_mean_infl,
  width = 9,
  height = 4.5
)

p_roll_var_cpi <- macro_roll %>%
  filter(!is.na(roll_var_cpi)) %>%
  ggplot(aes(x = date, y = roll_var_cpi)) +
  geom_line() +
  labs(
    title = "12-Month Rolling Variance of CPI",
    x = "Date",
    y = "Rolling Variance"
  )

print(p_roll_var_cpi)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_roll_var_cpi.png"),
  plot = p_roll_var_cpi,
  width = 9,
  height = 4.5
)

p_roll_var_infl <- macro_roll %>%
  filter(!is.na(roll_var_infl)) %>%
  ggplot(aes(x = date, y = roll_var_infl)) +
  geom_line() +
  labs(
    title = "12-Month Rolling Variance of Inflation",
    x = "Date",
    y = "Rolling Variance"
  )

print(p_roll_var_infl)

ggsave(
  filename = file.path(OUTPUT_DIR, "03_roll_var_inflation.png"),
  plot = p_roll_var_infl,
  width = 9,
  height = 4.5
)


# ==========================================================
# 5) Compare level vs transformed series side by side
# ==========================================================

# CPI level vs inflation
macro_trans %>%
  filter(!is.na(cpi), !is.na(inflation_pct)) %>%
  select(date, cpi, inflation_pct) %>%
  pivot_longer(cols = c(cpi, inflation_pct),
               names_to = "series",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ series, scales = "free_y", ncol = 1) +
  labs(
    title = "CPI Level vs Inflation",
    x = "Date",
    y = NULL
  )

# INDPRO level vs growth
macro_trans %>%
  filter(!is.na(indpro), !is.na(indpro_growth_pct)) %>%
  select(date, indpro, indpro_growth_pct) %>%
  pivot_longer(cols = c(indpro, indpro_growth_pct),
               names_to = "series",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ series, scales = "free_y", ncol = 1) +
  labs(
    title = "Industrial Production Level vs Growth",
    x = "Date",
    y = NULL
  )

# ==========================================================
# 6) Simple numerical summaries
# ==========================================================

summary_table <- macro_trans %>%
  summarise(
    mean_cpi = mean(cpi, na.rm = TRUE),
    var_cpi = var(cpi, na.rm = TRUE),
    mean_infl = mean(inflation_pct, na.rm = TRUE),
    var_infl = var(inflation_pct, na.rm = TRUE),
    mean_indpro = mean(indpro, na.rm = TRUE),
    var_indpro = var(indpro, na.rm = TRUE),
    mean_indpro_growth = mean(indpro_growth_pct, na.rm = TRUE),
    var_indpro_growth = var(indpro_growth_pct, na.rm = TRUE)
  )

print(summary_table)

# ==========================================================
# 7) Takeaways
# ==========================================================

# 1) Series in levels often have trend and changing variance.
#    That usually means they are not stationary.

# 2) Taking logs helps stabilize scale, especially when growth is proportional.

# 3) First differences remove trend in many economic time series.

# 4) Log differences are especially useful because:
#       Δlog(y_t) ≈ growth rate
#    and 100 × Δlog(y_t) is approximately percent growth.

# 5) In macroeconomics:
#    - CPI level is usually nonstationary
#    - Inflation is more stable
#    - Industrial production level is often nonstationary
#    - Growth rates are more stable than levels

# 6) Visual evidence is not a formal test.
#    But it gives strong intuition before we move to:
#    - ACF and PACF
#    - Unit root tests
#    - ARIMA modeling










