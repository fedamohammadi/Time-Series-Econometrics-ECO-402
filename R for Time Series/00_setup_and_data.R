############################################################
# Purpose:
#   - Install/load required packages
#   - Create reproducible folders (R for Time Series/data, output)
#   - Build ONE shared monthly macro dataset from FRED
#   - Save cleaned dataset to: data/macro_monthly.rds
############################################################

# ==========================================================
# 0) Housekeeping + Project paths
# ==========================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
set.seed(123)

library(here)

print(here())

DATA_DIR   <- here("R for Time Series", "data")
OUTPUT_DIR <- here("R for Time Series", "output")

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==========================================================
# 1) Packages
# ==========================================================

required_pkgs <- c(
  "tidyverse",
  "lubridate",
  "tsibble",
  "fable",
  "feasts",
  "forecast",
  "tseries",
  "urca",
  "vars",
  "quantmod"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
}

install_if_missing(required_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ==========================================================
# 2) Helper functions
# ==========================================================

xts_to_tbl <- function(x, value_name) {
  tibble(
    date = as.Date(zoo::index(x)),
    !!value_name := as.numeric(x[, 1])
  )
}

to_month_start <- function(d) {
  as.Date(lubridate::floor_date(d, unit = "month"))
}

# ==========================================================
# 3) Build (or load) the shared dataset
# ==========================================================

rds_path <- file.path(DATA_DIR, "macro_monthly.rds")
csv_path <- file.path(DATA_DIR, "macro_monthly.csv")

if (file.exists(rds_path)) {
  
  macro <- readRDS(rds_path)
  message("Loaded cached dataset: ", rds_path)
  print(dplyr::glimpse(macro))
  
} else {
  
  message("Downloading FRED series (one-time)...")
  
  cpi_xts      <- quantmod::getSymbols("CPIAUCSL", src = "FRED", auto.assign = FALSE)
  unrate_xts   <- quantmod::getSymbols("UNRATE",   src = "FRED", auto.assign = FALSE)
  fedfunds_xts <- quantmod::getSymbols("FEDFUNDS", src = "FRED", auto.assign = FALSE)
  indpro_xts   <- quantmod::getSymbols("INDPRO",   src = "FRED", auto.assign = FALSE)
  
  cpi      <- xts_to_tbl(cpi_xts,      "cpi")
  unrate   <- xts_to_tbl(unrate_xts,   "unrate")
  fedfunds <- xts_to_tbl(fedfunds_xts, "fedfunds")
  indpro   <- xts_to_tbl(indpro_xts,   "indpro")
  
  macro <- cpi %>%
    full_join(unrate,   by = "date") %>%
    full_join(fedfunds, by = "date") %>%
    full_join(indpro,   by = "date") %>%
    mutate(date = to_month_start(date)) %>%
    arrange(date) %>%
    filter(date >= as.Date("1990-01-01")) %>%
    mutate(
      infl_cpi = 100 * (log(cpi) - dplyr::lag(log(cpi))),
      g_indpro = 100 * (log(indpro) - dplyr::lag(log(indpro)))
    ) %>%
    as_tsibble(index = date)
  
  saveRDS(macro, rds_path)
  readr::write_csv(as_tibble(macro), csv_path)
  
  message("Saved cleaned dataset to:")
  message("  - ", rds_path)
  message("  - ", csv_path)
  
  print(dplyr::glimpse(macro))
}

# ==========================================================
# 4) Quick sanity checks
# ==========================================================

missing_summary <- macro %>%
  as_tibble() %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "num_NA") %>%
  arrange(desc(num_NA))

print(missing_summary)

p1 <- macro %>%
  as_tibble() %>%
  ggplot(aes(x = date, y = cpi)) +
  geom_line() +
  labs(title = "CPI (Level)", x = "Date", y = "CPI")

p2 <- macro %>%
  as_tibble() %>%
  ggplot(aes(x = date, y = infl_cpi)) +
  geom_line() +
  labs(title = "Inflation (Approx, %)", x = "Date", y = "Inflation")

print(p1)
print(p2)
