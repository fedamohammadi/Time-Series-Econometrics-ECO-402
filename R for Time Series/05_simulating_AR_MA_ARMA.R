# ===========================================================
#   Learning objectives:
#   - Simulate AR(1) processes and understand how phi controls persistence
#   - Simulate MA(1) processes and understand how theta controls memory
#   - Simulate ARMA(1,1) processes and see how AR and MA interact
#   - Learn to read ACF/PACF signatures to identify AR vs MA vs ARMA
#   - See what happens when AR processes are nonstationary (|phi| >= 1)
#
#   Some takeaways:
#     AR(1): ACF decays gradually, PACF cuts off after lag 1
#     MA(1): ACF cuts off after lag 1, PACF decays gradually
#     ARMA(1,1): both ACF and PACF decay gradually
#
# ===========================================================

rm(list = ls())

library(here)
library(tidyverse)
library(tsibble)
library(feasts)
library(patchwork)

OUTPUT_DIR <- here("R for Time Series", "output")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(42)

n <- 500  # number of observations for all simulations


# ==========================================================
# 1) Quick review: the AR(1) model
# ==========================================================

# The AR(1) process is:
#   y_t = c + phi * y_{t-1} + epsilon_t
#
# where epsilon_t ~ WN(0, sigma^2).
#
# phi controls everything:
#   |phi| < 1  => stationary (shocks fade over time)
#   phi close to 1 => very persistent (shocks fade slowly)
#   phi close to 0 => almost white noise (shocks fade instantly)
#   phi < 0 => alternating pattern (oscillation around the mean)
#   |phi| >= 1 => nonstationary (shocks never fade, or series explodes)
#
# We will simulate three cases: phi = 0.9, phi = 0.5, phi = -0.7


# ==========================================================
# 2) Simulating AR(1) with different phi values
# ==========================================================

# arima.sim() uses the parameterization:
#   y_t = phi * y_{t-1} + epsilon_t   (mean zero)
# The "order" argument is c(p, d, q) = c(1, 0, 0) for AR(1).

ar1_high <- arima.sim(model = list(ar = 0.9), n = n)
ar1_mid  <- arima.sim(model = list(ar = 0.5), n = n)
ar1_neg  <- arima.sim(model = list(ar = -0.7), n = n)

# Combine into one data frame for plotting
ar1_df <- tibble(
  t = rep(1:n, 3),
  value = c(as.numeric(ar1_high), as.numeric(ar1_mid), as.numeric(ar1_neg)),
  phi = rep(c("phi = 0.9", "phi = 0.5", "phi = -0.7"), each = n)
)

# Reorder factor levels so plots go from high to low persistence
ar1_df$phi <- factor(ar1_df$phi, levels = c("phi = 0.9", "phi = 0.5", "phi = -0.7"))

p_ar1_series <- ar1_df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~ phi, ncol = 1, scales = "free_y") +
  labs(
    title = "Simulated AR(1) Processes",
    subtitle = "Higher |phi| = more persistence; negative phi = oscillation",
    x = "Time",
    y = "Value"
  )

print(p_ar1_series)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_ar1_series_comparison.png"),
  plot = p_ar1_series,
  width = 10,
  height = 7
)


# ==========================================================
# 3) ACF and PACF of AR(1) processes
# ==========================================================

# The theoretical signatures of AR(1):
#   ACF:  decays exponentially at rate phi^k for lag k
#         (if phi > 0, all positive and decaying)
#         (if phi < 0, alternating sign and decaying)
#   PACF: single significant spike at lag 1, then cuts off to zero
#
# This is the fingerprint of an AR(1). If you see it in real data,
# you know an AR(1) model might be a good fit.

# Helper function: convert a numeric vector into a tsibble
# so we can use feasts::ACF() and feasts::PACF()
make_tsibble <- function(x) {
  tibble(
    date = yearmonth(seq.Date(
      from = as.Date("2000-01-01"),
      by = "month",
      length.out = length(x)
    )),
    value = as.numeric(x)
  ) %>%
    as_tsibble(index = date)
}

# --- AR(1) with phi = 0.9 ---

ts_ar1_high <- make_tsibble(ar1_high)

p_ar1_high_acf <- ts_ar1_high %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: AR(1) with phi = 0.9")

p_ar1_high_pacf <- ts_ar1_high %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: AR(1) with phi = 0.9")

# --- AR(1) with phi = 0.5 ---

ts_ar1_mid <- make_tsibble(ar1_mid)

p_ar1_mid_acf <- ts_ar1_mid %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: AR(1) with phi = 0.5")

p_ar1_mid_pacf <- ts_ar1_mid %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: AR(1) with phi = 0.5")

# --- AR(1) with phi = -0.7 ---

ts_ar1_neg <- make_tsibble(ar1_neg)

p_ar1_neg_acf <- ts_ar1_neg %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: AR(1) with phi = -0.7")

p_ar1_neg_pacf <- ts_ar1_neg %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: AR(1) with phi = -0.7")

# Combine all six plots into one figure
p_ar1_acf_pacf <- (p_ar1_high_acf | p_ar1_high_pacf) /
  (p_ar1_mid_acf | p_ar1_mid_pacf) /
  (p_ar1_neg_acf | p_ar1_neg_pacf) +
  plot_annotation(
    title = "ACF and PACF Signatures of AR(1) Processes",
    subtitle = "Left: ACF decays gradually | Right: PACF cuts off after lag 1"
  )

print(p_ar1_acf_pacf)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_ar1_acf_pacf_comparison.png"),
  plot = p_ar1_acf_pacf,
  width = 12,
  height = 10
)


# ==========================================================
# 4) What happens when phi = 1? (Random walk, nonstationary)
# ==========================================================

# When phi = 1, the AR(1) becomes a random walk:
#   y_t = y_{t-1} + epsilon_t
#
# This is nonstationary because:
#   - Variance grows linearly with time: Var(y_t) = t * sigma^2
#   - There is no fixed mean to revert to
#   - Shocks are permanent (they never die out)
#
# arima.sim() won't let us set ar = 1 directly (it checks stationarity),
# so we build it manually with cumsum().

epsilon <- rnorm(n, mean = 0, sd = 1)
random_walk <- cumsum(epsilon)

rw_df <- tibble(t = 1:n, value = random_walk)

p_rw <- rw_df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line() +
  labs(
    title = "Random Walk (AR(1) with phi = 1)",
    subtitle = "Nonstationary: no mean reversion, variance grows over time",
    x = "Time",
    y = "Value"
  )

print(p_rw)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_random_walk.png"),
  plot = p_rw,
  width = 9,
  height = 4.5
)

# ACF of the random walk: extremely slow decay, stays near 1
# This is the classic sign of nonstationarity.

ts_rw <- make_tsibble(random_walk)

p_rw_acf <- ts_rw %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF of Random Walk (phi = 1): Slow Decay = Nonstationary")

print(p_rw_acf)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_random_walk_acf.png"),
  plot = p_rw_acf,
  width = 9,
  height = 4.5
)


# ==========================================================
# 5) The MA(1) model
# ==========================================================

# The MA(1) process is:
#   y_t = mu + epsilon_t + theta * epsilon_{t-1}
#
# where epsilon_t ~ WN(0, sigma^2).
#
# Key idea: an MA(1) has a memory of exactly one period.
# The current value depends on today's shock plus yesterday's shock.
# After one lag, the direct effect is gone.
#
# Theoretical ACF/PACF signatures of MA(1):
#   ACF:  single significant spike at lag 1, then cuts off to zero
#   PACF: decays gradually (exponentially or in alternating pattern)
#
# This is the mirror image of AR(1).


# ==========================================================
# 6) Simulating MA(1) with different theta values
# ==========================================================

ma1_high <- arima.sim(model = list(ma = 0.8), n = n)
ma1_mid  <- arima.sim(model = list(ma = 0.4), n = n)
ma1_neg  <- arima.sim(model = list(ma = -0.6), n = n)

ma1_df <- tibble(
  t = rep(1:n, 3),
  value = c(as.numeric(ma1_high), as.numeric(ma1_mid), as.numeric(ma1_neg)),
  theta = rep(c("theta = 0.8", "theta = 0.4", "theta = -0.6"), each = n)
)

ma1_df$theta <- factor(ma1_df$theta,
                       levels = c("theta = 0.8", "theta = 0.4", "theta = -0.6"))

p_ma1_series <- ma1_df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~ theta, ncol = 1, scales = "free_y") +
  labs(
    title = "Simulated MA(1) Processes",
    subtitle = "MA processes look more like white noise than AR processes do",
    x = "Time",
    y = "Value"
  )

print(p_ma1_series)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_ma1_series_comparison.png"),
  plot = p_ma1_series,
  width = 10,
  height = 7
)


# ==========================================================
# 7) ACF and PACF of MA(1) processes
# ==========================================================

# --- MA(1) with theta = 0.8 ---

ts_ma1_high <- make_tsibble(ma1_high)

p_ma1_high_acf <- ts_ma1_high %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: MA(1) with theta = 0.8")

p_ma1_high_pacf <- ts_ma1_high %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: MA(1) with theta = 0.8")

# --- MA(1) with theta = 0.4 ---

ts_ma1_mid <- make_tsibble(ma1_mid)

p_ma1_mid_acf <- ts_ma1_mid %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: MA(1) with theta = 0.4")

p_ma1_mid_pacf <- ts_ma1_mid %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: MA(1) with theta = 0.4")

# --- MA(1) with theta = -0.6 ---

ts_ma1_neg <- make_tsibble(ma1_neg)

p_ma1_neg_acf <- ts_ma1_neg %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: MA(1) with theta = -0.6")

p_ma1_neg_pacf <- ts_ma1_neg %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: MA(1) with theta = -0.6")

# Combine all six plots
p_ma1_acf_pacf <- (p_ma1_high_acf | p_ma1_high_pacf) /
  (p_ma1_mid_acf | p_ma1_mid_pacf) /
  (p_ma1_neg_acf | p_ma1_neg_pacf) +
  plot_annotation(
    title = "ACF and PACF Signatures of MA(1) Processes",
    subtitle = "Left: ACF cuts off after lag 1 | Right: PACF decays gradually"
  )

print(p_ma1_acf_pacf)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_ma1_acf_pacf_comparison.png"),
  plot = p_ma1_acf_pacf,
  width = 12,
  height = 10
)


# ==========================================================
# 8) The ARMA(1,1) model
# ==========================================================

# The ARMA(1,1) process combines AR and MA:
#   y_t = c + phi * y_{t-1} + epsilon_t + theta * epsilon_{t-1}
#
# The AR part creates persistence (memory through past values).
# The MA part creates a short burst of extra correlation (memory through past shocks).
#
# Theoretical ACF/PACF signatures of ARMA(1,1):
#   ACF:  decays gradually (no clean cutoff)
#   PACF: decays gradually (no clean cutoff)
#
# This is why ARMA is harder to identify from ACF/PACF alone.
# Neither one cuts off cleanly, so you can't just eyeball p and q
# the way you can for pure AR or pure MA.


# ==========================================================
# 9) Simulating ARMA(1,1) processes
# ==========================================================

arma11_a <- arima.sim(model = list(ar = 0.7, ma = 0.4), n = n)
arma11_b <- arima.sim(model = list(ar = 0.3, ma = 0.8), n = n)
arma11_c <- arima.sim(model = list(ar = 0.8, ma = -0.5), n = n)

arma_df <- tibble(
  t = rep(1:n, 3),
  value = c(as.numeric(arma11_a), as.numeric(arma11_b), as.numeric(arma11_c)),
  params = rep(
    c("phi=0.7, theta=0.4", "phi=0.3, theta=0.8", "phi=0.8, theta=-0.5"),
    each = n
  )
)

arma_df$params <- factor(arma_df$params, levels = c(
  "phi=0.7, theta=0.4", "phi=0.3, theta=0.8", "phi=0.8, theta=-0.5"
))

p_arma_series <- arma_df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~ params, ncol = 1, scales = "free_y") +
  labs(
    title = "Simulated ARMA(1,1) Processes",
    subtitle = "Combines persistence from AR with short memory from MA",
    x = "Time",
    y = "Value"
  )

print(p_arma_series)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_arma11_series_comparison.png"),
  plot = p_arma_series,
  width = 10,
  height = 7
)


# ==========================================================
# 10) ACF and PACF of ARMA(1,1) processes
# ==========================================================

# --- ARMA(1,1) with phi = 0.7, theta = 0.4 ---

ts_arma_a <- make_tsibble(arma11_a)

p_arma_a_acf <- ts_arma_a %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: ARMA(1,1) phi=0.7, theta=0.4")

p_arma_a_pacf <- ts_arma_a %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: ARMA(1,1) phi=0.7, theta=0.4")

# --- ARMA(1,1) with phi = 0.3, theta = 0.8 ---

ts_arma_b <- make_tsibble(arma11_b)

p_arma_b_acf <- ts_arma_b %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: ARMA(1,1) phi=0.3, theta=0.8")

p_arma_b_pacf <- ts_arma_b %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: ARMA(1,1) phi=0.3, theta=0.8")

# --- ARMA(1,1) with phi = 0.8, theta = -0.5 ---

ts_arma_c <- make_tsibble(arma11_c)

p_arma_c_acf <- ts_arma_c %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: ARMA(1,1) phi=0.8, theta=-0.5")

p_arma_c_pacf <- ts_arma_c %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: ARMA(1,1) phi=0.8, theta=-0.5")

# Combine all six plots
p_arma_acf_pacf <- (p_arma_a_acf | p_arma_a_pacf) /
  (p_arma_b_acf | p_arma_b_pacf) /
  (p_arma_c_acf | p_arma_c_pacf) +
  plot_annotation(
    title = "ACF and PACF Signatures of ARMA(1,1) Processes",
    subtitle = "Both ACF and PACF decay gradually (no clean cutoff in either)"
  )

print(p_arma_acf_pacf)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_arma11_acf_pacf_comparison.png"),
  plot = p_arma_acf_pacf,
  width = 12,
  height = 10
)


# ==========================================================
# 11) Higher-order models: AR(2) and MA(2)
# ==========================================================

# AR(2): y_t = phi_1 * y_{t-1} + phi_2 * y_{t-2} + epsilon_t
#   ACF:  decays gradually (can show damped oscillations if roots are complex)
#   PACF: cuts off after lag 2
#
# MA(2): y_t = epsilon_t + theta_1 * epsilon_{t-1} + theta_2 * epsilon_{t-2}
#   ACF:  cuts off after lag 2
#   PACF: decays gradually

# AR(2) with complex roots (produces damped sine wave in ACF)
ar2_sim <- arima.sim(model = list(ar = c(0.5, -0.3)), n = n)

ts_ar2 <- make_tsibble(ar2_sim)

p_ar2_acf <- ts_ar2 %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: AR(2) with phi1=0.5, phi2=-0.3")

p_ar2_pacf <- ts_ar2 %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: AR(2) with phi1=0.5, phi2=-0.3")

# MA(2)
ma2_sim <- arima.sim(model = list(ma = c(0.6, 0.3)), n = n)

ts_ma2 <- make_tsibble(ma2_sim)

p_ma2_acf <- ts_ma2 %>%
  ACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF: MA(2) with theta1=0.6, theta2=0.3")

p_ma2_pacf <- ts_ma2 %>%
  PACF(value, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF: MA(2) with theta1=0.6, theta2=0.3")

p_higher_order <- (p_ar2_acf | p_ar2_pacf) /
  (p_ma2_acf | p_ma2_pacf) +
  plot_annotation(
    title = "Higher-Order Models: AR(2) and MA(2)",
    subtitle = "AR(2) PACF cuts off at lag 2 | MA(2) ACF cuts off at lag 2"
  )

print(p_higher_order)

ggsave(
  filename = file.path(OUTPUT_DIR, "05_higher_order_acf_pacf.png"),
  plot = p_higher_order,
  width = 12,
  height = 7
)







