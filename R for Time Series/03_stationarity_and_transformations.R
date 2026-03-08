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


