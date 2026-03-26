############################################################
#   - What is white noise
#   - Learn the meaning of ACF and PACF
#   - Compute and visualize autocorrelation patterns
#   - Compare stationary-looking series and nonstationary series
#   - Build intuition for AR and MA behavior
#
# Data source:
#   Uses macro_monthly.rds created in 00_setup_and_data.R
############################################################

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

macro_acf <- macro %>%
  arrange(date) %>%
  mutate(
    log_cpi = log(cpi),
    d_log_cpi = log(cpi) - lag(log(cpi)),
    inflation_pct = 100 * d_log_cpi,
    
    log_indpro = log(indpro),
    d_log_indpro = log(indpro) - lag(log(indpro)),
    indpro_growth_pct = 100 * d_log_indpro
  )

glimpse(macro_acf)

# ==========================================================
# 2) White Noise -> definition and simulation
# ==========================================================

# White noise is a series with:
#   - mean = 0
#   - constant variance
#   - no autocorrelation across time
# In practice, white noise looks patternless.

set.seed(123)

white_noise_df <- tibble(
  t = 1:300,
  wn = rnorm(300, mean = 0, sd = 1)
)

p_wn <- white_noise_df %>%
  ggplot(aes(x = t, y = wn)) +
  geom_line() +
  labs(
    title = "Simulated White Noise",
    x = "Time",
    y = "Value"
  )

print(p_wn)

ggsave(
  filename = file.path(OUTPUT_DIR, "04_white_noise_series.png"),
  plot = p_wn,
  width = 9,
  height = 4.5
)


# ==========================================================
# 3) ACF and PACF of white noise
# ==========================================================

# For white noise:
#   - ACF should be near 0 at all nonzero lags
#   - PACF should also be near 0 at all nonzero lags

wn_tsibble <- tibble(
  date = yearmonth(seq.Date(from = as.Date("2000-01-01"),
                            by = "month",
                            length.out = 300)),
  wn = white_noise_df$wn
) %>%
  as_tsibble(index = date)

p_wn_acf <- wn_tsibble %>%
  ACF(wn) %>%
  autoplot() +
  labs(title = "ACF of Simulated White Noise")

print(p_wn_acf)

ggsave(
  filename = file.path(OUTPUT_DIR, "04_white_noise_acf.png"),
  plot = p_wn_acf,
  width = 9,
  height = 4.5
)

p_wn_pacf <- wn_tsibble %>%
  PACF(wn) %>%
  autoplot() +
  labs(title = "PACF of Simulated White Noise")

print(p_wn_pacf)

ggsave(
  filename = file.path(OUTPUT_DIR, "04_white_noise_pacf.png"),
  plot = p_wn_pacf,
  width = 9,
  height = 4.5
)















