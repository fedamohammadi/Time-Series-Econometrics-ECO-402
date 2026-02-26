############################################################
# Purpose:
#   - Understand time series object types in R
#   - Compare ts, xts, and tsibble
#   - Learn indexing and subsetting
#   - Understand frequency and lag behavior
#
############################################################

# ==========================================================
# 0) Setup
# ==========================================================

rm(list = ls())

library(here)
library(tidyverse)
library(tsibble)
library(xts)
library(forecast)

DATA_DIR <- here("R for Time Series", "data")

macro <- readRDS(file.path(DATA_DIR, "macro_monthly.rds"))

glimpse(macro)

# ==========================================================
# 1) The ts Object (Base R Time Series)
# ==========================================================

# Convert CPI to a base R ts object
cpi_ts <- ts(
  macro$cpi,
  start = c(year(min(macro$date)), month(min(macro$date))),
  frequency = 12
)

cpi_ts
class(cpi_ts)

# Basic plotting
plot(cpi_ts, main = "CPI as ts Object")

# Extract time information
start(cpi_ts)
end(cpi_ts)
frequency(cpi_ts)

# Subset by time
window(cpi_ts, start = c(2008, 1), end = c(2012, 12))

# ==========================================================
# 2) The xts Object (Financial Style Indexing)
# ==========================================================

cpi_xts <- xts(
  macro$cpi,
  order.by = macro$date
)

cpi_xts
class(cpi_xts)

plot(cpi_xts, main = "CPI as xts Object")

# Subsetting by date
cpi_xts["2008/2012"]

# Single month
cpi_xts["2010-01"]

# ==========================================================
# 3) The tsibble Object (Modern Tidy Time Series)
# ==========================================================

class(macro)

macro

# Subset using dplyr
macro %>%
  filter(date >= as.Date("2008-01-01"),
         date <= as.Date("2012-12-01"))

# ==========================================================
# 4) Understanding Lags (Critical for Econometrics)
# ==========================================================

# Base R lag for ts
lag(cpi_ts, k = 1)[1:5]

# Tidy lag
macro %>%
  mutate(cpi_lag1 = lag(cpi)) %>%
  select(date, cpi, cpi_lag1) %>%
  head()

# Important:
# lag() in dplyr shifts DOWN (previous observation)

# ==========================================================
# 5) Frequency and Seasonality
# ==========================================================

frequency(cpi_ts)

# Monthly = 12
# Quarterly would be 4
# Annual would be 1

# Convert to quarterly example
cpi_quarterly <- aggregate(cpi_ts, nfrequency = 4)

plot(cpi_quarterly, main = "Quarterly CPI")

