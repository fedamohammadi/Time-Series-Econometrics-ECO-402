############################################################
# Purpose:
#   - Visualize time series clearly and consistently
#   - Identify trend, seasonality, and outliers
#   - Perform decomposition:
#       (1) Classical decomposition (trend + seasonal + remainder)
#       (2) STL decomposition (more flexible and preferred)
############################################################

# ==========================================================
# 0) Setup
# ==========================================================

rm(list = ls())

library(here)
library(tidyverse)  
library(lubridate)
library(tsibble)
library(feasts)
library(ggtime)

DATA_DIR <- here("R for Time Series", "data")
OUTPUT_DIR <- here("R for Time Series", "output")

macro <- readRDS(file.path(DATA_DIR, "macro_monthly.rds"))

# Make sure folders exist (safe)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

glimpse(macro)

# ==========================================================
# 1) Handle implicit gaps (required for gg_season / gg_subseries)
# ==========================================================

# Some tsibble tools require explicit time points.
# This makes missing months explicit with NA values.
macro <- macro %>% fill_gaps()

# Optional quick check
# print(count_gaps(macro))

# ==========================================================
# 2) Basic Time Series Plots
# ==========================================================

p_cpi <- macro %>%
  filter(!is.na(cpi)) %>%
  ggplot(aes(x = date, y = cpi)) +
  geom_line() +
  labs(title = "CPI (Level)", x = "Date", y = "CPI")

print(p_cpi)

p_infl <- macro %>%
  filter(!is.na(infl_cpi)) %>%
  ggplot(aes(x = date, y = infl_cpi)) +
  geom_line() +
  labs(title = "Inflation (Approx: 100 × Δlog(CPI))", x = "Date", y = "Percent")

print(p_infl)

p_unrate <- macro %>%
  filter(!is.na(unrate)) %>%
  ggplot(aes(x = date, y = unrate)) +
  geom_line() +
  labs(title = "Unemployment Rate", x = "Date", y = "Percent")

print(p_unrate)

# Save plots (optional)
ggsave(file.path(OUTPUT_DIR, "plot_cpi_level.png"), p_cpi, width = 9, height = 4.5)
ggsave(file.path(OUTPUT_DIR, "plot_inflation.png"), p_infl, width = 9, height = 4.5)
ggsave(file.path(OUTPUT_DIR, "plot_unrate.png"), p_unrate, width = 9, height = 4.5)

# ==========================================================
# 3) Seasonal Plots (monthly seasonality checks)
# ==========================================================

macro_season <- macro %>% fill_gaps()

macro_season %>%
  gg_season(infl_cpi) +
  labs(
    title = "Seasonal Plot: Inflation by Month",
    y = "Inflation (Approx %)"
  )

macro_season %>%
  ggtime::gg_subseries(infl_cpi) +
  labs(
    title = "Subseries Plot: Inflation by Month",
    y = "Inflation (Approx %)"
  )

# ==========================================================
# 4) Classical Decomposition (Trend + Seasonal + Remainder)
# ==========================================================

# Classical decomposition needs a ts object with frequency = 12 (monthly)
# We'll decompose CPI levels.

macro_no_na_cpi <- macro %>%
  filter(!is.na(cpi))

cpi_ts <- ts(
  macro_no_na_cpi$cpi,
  start = c(year(min(macro_no_na_cpi$date)),
            month(min(macro_no_na_cpi$date))),
  frequency = 12
)

# Additive
cpi_decomp_add <- decompose(cpi_ts, type = "additive")
plot(cpi_decomp_add)
title("Classical Additive Decomposition: CPI")

# Multiplicative
cpi_decomp_mult <- decompose(cpi_ts, type = "multiplicative")
plot(cpi_decomp_mult)
title("Classical Multiplicative Decomposition: CPI")


# we save this figure for the sake of our practice (optional)
plot(cpi_decomp_add)
title("Classical Additive Decomposition: CPI")

ggsave(
  filename = file.path(OUTPUT_DIR, "cpi_classical_additive.png"),
  width = 9,
  height = 6
)


# ==========================================================
# 5) STL Decomposition
# ==========================================================

cpi_start <- macro %>% filter(!is.na(cpi)) %>% summarise(min(date)) %>% pull()
cpi_end   <- macro %>% filter(!is.na(cpi)) %>% summarise(max(date)) %>% pull()

macro_cpi <- macro %>%
  filter(date >= cpi_start, date <= cpi_end) %>%
  arrange(date) %>%
  as_tsibble(index = date) %>%
  fill_gaps() %>%
  filter(!is.na(cpi))  # should now keep full months with no missing CPI

# Safety check: STL will fail if any NA remains
stopifnot(sum(is.na(macro_cpi$cpi)) == 0)

cpi_stl <- macro_cpi %>% model(STL(cpi))
cpi_stl_components <- components(cpi_stl)

p_cpi_stl <- autoplot(cpi_stl_components) +
  labs(title = "STL Decomposition Components: CPI")

print(p_cpi_stl)

ggsave(
  filename = file.path(OUTPUT_DIR, "cpi_stl_decomposition.png"),
  plot = p_cpi_stl,
  width = 9,
  height = 6
)

# ---- Inflation: choose complete contiguous window ----
infl_start <- macro %>% filter(!is.na(infl_cpi)) %>% summarise(min(date)) %>% pull()
infl_end   <- macro %>% filter(!is.na(infl_cpi)) %>% summarise(max(date)) %>% pull()

macro_infl <- macro %>%
  filter(date >= infl_start, date <= infl_end) %>%
  arrange(date) %>%
  as_tsibble(index = date) %>%
  fill_gaps() %>%
  filter(!is.na(infl_cpi))

stopifnot(sum(is.na(macro_infl$infl_cpi)) == 0)

infl_stl <- macro_infl %>% model(STL(infl_cpi))
infl_components <- components(infl_stl)

p_infl_stl <- autoplot(infl_components) +
  labs(title = "STL Decomposition Components: Inflation")

print(p_infl_stl)

ggsave(
  filename = file.path(OUTPUT_DIR, "inflation_stl_decomposition.png"),
  plot = p_infl_stl,
  width = 9,
  height = 6
)
