# ============================================================
# Chapter 4 concepts Applying to UKgas
# ============================================================

data(UKgas)
class(UKgas)      
frequency(UKgas)  
start(UKgas)
end(UKgas)

# ============================================================
# 4.2 White Noise - the baseline I compare everything against
# ============================================================

# I first simulate white noise for reference
set.seed(1)
w <- rnorm(length(UKgas))

par(mfrow = c(2, 2))
plot(w, type = "l", main = "Simulated White Noise (reference)")
acf(w, main = "ACF: White Noise")

# Now I plot UKgas itself
plot(UKgas, main = "UKgas: Quarterly UK Gas Consumption")
acf(UKgas, main = "ACF: UKgas (raw)")
par(mfrow = c(1, 1))



# ============================================================
# 4.3 Random Walk for UKgas
# ============================================================

# Simulating a random walk for visual comparison
set.seed(1)
rw <- cumsum(rnorm(length(UKgas)))

par(mfrow = c(2, 2))
plot(rw, type = "l", main = "Simulated Random Walk (reference)")
acf(rw, main = "ACF: Random Walk")

plot(UKgas, main = "UKgas (raw)")
acf(UKgas, main = "ACF: UKgas")
par(mfrow = c(1, 1))



# ============================================================
# 4.3 Differencing
# ============================================================

# Step 1: First difference to remove the trend component
dUKgas <- diff(UKgas)

par(mfrow = c(2, 2))
plot(dUKgas, main = "First Difference of UKgas")
acf(dUKgas, main = "ACF: First-Differenced UKgas")



# Step 2: Seasonal difference (lag = 4) to also remove the seasonality
sdUKgas <- diff(UKgas, lag = 4)

plot(sdUKgas, main = "Seasonal Difference of UKgas (lag 4)")
acf(sdUKgas, main = "ACF: Seasonally Differenced UKgas")
par(mfrow = c(1, 1))

# Step 3: Apply both differences — first regular, then seasonal
dsdUKgas <- diff(diff(UKgas), lag = 4)

par(mfrow = c(1, 2))
plot(dsdUKgas, main = "Regular + Seasonal Difference of UKgas")
acf(dsdUKgas, main = "ACF: Both Differences Applied")
par(mfrow = c(1, 1))



# ============================================================
# 4.4 Fitted Models and Residual Diagnostics
# ============================================================

# Following the exchange-rate logic from the notes:
# fit a restricted Holt-Winters model and inspect residuals

UKgas.hw <- HoltWinters(UKgas, alpha = 1)

par(mfrow = c(1, 2))
plot(UKgas.hw, main = "Holt-Winters Fit on UKgas")
acf(resid(UKgas.hw), main = "ACF: Holt-Winters Residuals")
par(mfrow = c(1, 1))


# ============================================================
# 4.5 Autoregressive Models - fitting AR to the stationary series
# ============================================================

# First we inspect ACF and PACF together
par(mfrow = c(1, 2))
acf(dsdUKgas, main = "ACF: Doubly-Differenced UKgas")
pacf(dsdUKgas, main = "PACF: Doubly-Differenced UKgas")
par(mfrow = c(1, 1))


# ============================================================
# 4.6 Fitted AR Models and AIC
# ============================================================

# Fit AR model by MLE; R selects order by AIC automatically
UKgas.ar <- ar(dsdUKgas, method = "mle")

# Inspect what R selected
UKgas.ar$order     # chosen AR order
UKgas.ar$ar        # estimated coefficients
UKgas.ar$aic       

# Approximate 95% confidence interval for each coefficient
UKgas.ar$ar + c(-2, 2) * sqrt(UKgas.ar$asy.var)


# ============================================================
# Residual diagnostics for the fitted AR model
# ============================================================

# Removing the first p residuals
p <- UKgas.ar$order
acf(UKgas.ar$resid[-(1:p)], na.action = na.pass,
    main = paste("ACF: AR(", p, ") Residuals on Doubly-Differenced UKgas"))



