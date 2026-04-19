# ============================================================
#  Time Series Econometrics - Final Project
#  Dataset: UKgas (Quarterly UK Gas Consumption, 1960-1986)
#  Student: Feda Mohammadi
#  Date: April 2026
# ============================================================


# ============================================================
# PART 1: LOAD AND DESCRIBE THE DATASET
# ============================================================

# Load the dataset 
data(UKgas)

# Basic inspection of the series
print(UKgas)       # View the full table of values
summary(UKgas)     # Min, Max, Mean, Quartiles
str(UKgas)         # This confirms if it is a Time-Series object with 108 obs, 1960-1986

# UKgas is quarterly gas consumption in the UK (in millions of therms)
# from Q1 1960 to Q4 1986 = 27 years x 4 quarters = 108 observations

# Plot the raw time series
plot(UKgas,
     main = "UK Quarterly Gas Consumption (1960-1986)",
     ylab = "Gas Consumption (millions of therms)",
     xlab = "Year",
     col  = "steelblue")

# What we can see in this plot:
# - Clear upward trend over time
# - Strong seasonal pattern (spikes every winter, dips every summer)
# - The size of the seasonal swings grows over time --> suggests MULTIPLICATIVE pattern
# - No obvious outliers or structural breaks


# ============================================================
# PART 2: DECOMPOSITION AND SEASONAL ANALYSIS
# ============================================================

# Because the seasonal variation grows with the trend,
# a multiplicative decomposition is more appropriate than additive.

# Multiplicative decomposition: x_t = trend * seasonal * random
UKgas.decomp <- decompose(UKgas, type = "mult")

# Extract and inspect the seasonal component
UKgas.decomp$seasonal   # Shows the seasonal factor for each quarter

# Seasonal boxplot: shows which quarters tend to be high or low
# Q1 (winter) = highest consumption; Q3 (summer) = lowest
boxplot(UKgas ~ cycle(UKgas),
        names = c("Q1", "Q2", "Q3", "Q4"),
        main  = "Seasonal Boxplot of UKgas by Quarter",
        ylab  = "Gas Consumption (millions of therms)",
        xlab  = "Quarter",
        col   = c("lightblue", "lightgreen", "lightyellow", "lightsalmon"))

# The boxplot clearly shows:
# - Q1 (Jan-Mar) is by far the highest consumption quarter (heating in winter)
# - Q3 (Jul-Sep) is the lowest (no heating needed)
# - The pattern is very regular and strong across all years


# ============================================================
# PART 3: AUTOCORRELATION ANALYSIS (ACF and PACF)
# ============================================================

# ACF of the original series
# Slow decay in ACF = non-stationary (trend and/or seasonal structure present)
acf(UKgas,
    lag.max = 40,
    main    = "ACF of UKgas (Original Series)")

# PACF of the original series
pacf(UKgas,
     lag.max = 40,
     main    = "PACF of UKgas (Original Series)")

# What we see here:
# - ACF decays very slowly (strong persistence = non-stationary)
# - Seasonal spikes in ACF at lags 4, 8, 12 (quarterly seasonality)
# - PACF has a large spike at lag 1 and cuts off quickly (consistent with AR structure)
# These patterns confirm the series is NOT stationary yet


# ============================================================
# PART 4: HOLT-WINTERS FORECASTING
# ============================================================

# UKgas has both trend and growing seasonal variation,
# so multiplicative Holt-Winters is appropriate.

UKgas.hw <- HoltWinters(UKgas, seasonal = "mult")

# View the fitted smoothing parameters
UKgas.hw$alpha   # alpha: controls how fast the level updates
UKgas.hw$beta    # beta:  controls how fast the trend updates
UKgas.hw$gamma   # gamma: controls how fast the seasonal factors update

# Plot the fitted Holt-Winters model over the original data
plot(UKgas.hw,
     main = "Holt-Winters Fit for UKgas (Multiplicative)",
     ylab = "Gas Consumption (millions of therms)")

# Generate forecasts 2 years (8 quarters) ahead
UKgas.hw.forecast <- predict(UKgas.hw, n.ahead = 8)

# Plot original series with forecasts
ts.plot(UKgas, UKgas.hw.forecast,
        lty  = c(1, 2),
        col  = c("steelblue", "red"),
        main = "UKgas: Holt-Winters Forecasts (8 Quarters Ahead)",
        ylab = "Gas Consumption (millions of therms)",
        xlab = "Year")
legend("topleft",
       legend = c("Observed", "Forecast"),
       lty    = c(1, 2),
       col    = c("steelblue", "red"))


# ============================================================
# PART 5: REGRESSION MODEL WITH TREND AND SEASONAL DUMMIES
# ============================================================

# Because the variance grows over time, we first log-transform the series.
# This stabilizes the variance and makes the seasonal pattern more additive.

log.UKgas <- log(UKgas)

# Plot the log-transformed series to visually confirm variance stabilization
plot(log.UKgas,
     main = "Log of UKgas",
     ylab = "log(Gas Consumption)",
     xlab = "Year",
     col  = "darkgreen")

# Create time index and seasonal factor for regression
Time <- time(UKgas)          # Numeric time variable (1960.00, 1960.25, ...)
Seas <- factor(cycle(UKgas)) # Quarterly factor (1, 2, 3, 4 repeating)

# Fit regression: log(UKgas) ~ time trend + seasonal dummies
# The "0 +" removes the intercept so all 4 quarterly dummies are estimated
UKgas.lm <- lm(log.UKgas ~ 0 + Time + Seas)
summary(UKgas.lm)

# Each seasonal coefficient gives the average log-consumption for that quarter
# The Time coefficient gives the average quarterly growth rate (on log scale)

# Examine the residuals
plot(resid(UKgas.lm),
     type = "l",
     main = "Residuals from Log-Linear Regression with Seasonal Dummies",
     ylab = "Residuals",
     xlab = "Time Index",
     col  = "purple")
abline(h = 0, lty = 2, col = "red")

# ACF of regression residuals
# If residuals are still autocorrelated, OLS assumptions are violated
# and we need stochastic modeling (AR/MA/ARMA) on the residuals
acf(resid(UKgas.lm),
    lag.max = 40,
    main    = "ACF of Regression Residuals")


# ============================================================
# PART 6: TRANSFORM TOWARD STATIONARITY
# ============================================================

# The original series is non-stationary (trend + seasonality).
# We apply log first (to stabilize variance), then difference.

# Step 1: Log transformation (already done above)
# Step 2: Seasonal differencing (lag 4) to remove quarterly seasonality
log.UKgas.sdiff <- diff(log.UKgas, lag = 4)

plot(log.UKgas.sdiff,
     main = "Seasonally Differenced log(UKgas) [lag 4]",
     ylab = "Seasonal Difference of log(UKgas)",
     xlab = "Year",
     col  = "darkorange")
abline(h = 0, lty = 2)

# Now, I check if trend still remains after seasonal differencing
# If yes, I apply first differencing as well

# Step 3: First difference the seasonally differenced series
log.UKgas.diff2 <- diff(log.UKgas.sdiff, lag = 1)

plot(log.UKgas.diff2,
     main = "Seasonally + First Differenced log(UKgas)",
     ylab = "Differenced Series",
     xlab = "Year",
     col  = "darkred")
abline(h = 0, lty = 2)

# ACF and PACF after transformation
# We want the ACF to decay quickly and have no strong seasonal spikes
acf(log.UKgas.sdiff,
    lag.max = 40,
    main    = "ACF: Seasonally Differenced log(UKgas)")

pacf(log.UKgas.sdiff,
     lag.max = 40,
     main    = "PACF: Seasonally Differenced log(UKgas)")

acf(log.UKgas.diff2,
    lag.max = 40,
    main    = "ACF: Seasonally + First Differenced log(UKgas)")

pacf(log.UKgas.diff2,
     lag.max = 40,
     main    = "PACF: Seasonally + First Differenced log(UKgas)")

# We use the transformed series that looks most stationary for model fitting.
# Let x.stat be our working stationary series.
x.stat <- log.UKgas.sdiff   # We adjust this if double-differencing looks better


# ============================================================
# PART 7: FIT AR, MA, AND ARMA MODELS
# ============================================================

# We fit three competing models to the stationary series
# and compare them using AIC. Lower AIC = better model.

# AR(1) model: current value depends on 1 lagged value
fit.ar1 <- arima(x.stat, order = c(1, 0, 0))
fit.ar1
AIC(fit.ar1)

# MA(1) model: current value depends on 1 lagged error
fit.ma1 <- arima(x.stat, order = c(0, 0, 1))
fit.ma1
AIC(fit.ma1)

# ARMA(1,1) model: combines AR(1) and MA(1)
fit.arma11 <- arima(x.stat, order = c(1, 0, 1))
fit.arma11
AIC(fit.arma11)

# Compare all three AICs side by side
cat("AIC - AR(1):    ", AIC(fit.ar1),    "\n")
cat("AIC - MA(1):    ", AIC(fit.ma1),    "\n")
cat("AIC - ARMA(1,1):", AIC(fit.arma11), "\n")

# AIC comparison results:
# AR(1):     -169.52  <-- LOWEST = preferred model
# MA(1):     -169.46  <-- very close second
# ARMA(1,1): -167.55  <-- worst (adding extra parameters did not help)

# Note: AR(1) and MA(1) are extremely close in AIC (-169.52 vs -169.46),
# which means both fit the data almost equally well.
# We pick AR(1) as preferred since it has the slightly lower AIC.

# ACF of residuals for all three models
acf(resid(fit.ar1),
    main = "ACF of AR(1) Residuals")

acf(resid(fit.ma1),
    main = "ACF of MA(1) Residuals")

acf(resid(fit.arma11),
    main = "ACF of ARMA(1,1) Residuals")

# PACF of residuals from the preferred model (AR(1))
pacf(resid(fit.ar1),
     main = "PACF of AR(1) Residuals")

# Update preferred model to AR(1)
preferred.model <- fit.ar1


# ============================================================
# PART 8: FORECAST USING PREFERRED STOCHASTIC MODEL
# ============================================================

# Based on AIC comparison in Part 7, AR(1) is the preferred model.
# AR(1):     -169.52  <-- lowest AIC = preferred
# MA(1):     -169.46
# ARMA(1,1): -167.55

preferred.model <- fit.ar1

# Generate forecasts 8 steps (2 years) ahead
UKgas.forecast <- predict(preferred.model, n.ahead = 8)

# Print forecast values and standard errors
print(UKgas.forecast$pred)  # Forecasted values (on the stationary/transformed scale)
print(UKgas.forecast$se)    # Standard errors of forecasts

# Plot the stationary series with forecasts appended
ts.plot(x.stat,
        UKgas.forecast$pred,
        lty  = c(1, 2),
        col  = c("steelblue", "red"),
        main = "AR(1) Forecasts on Stationary Series",
        ylab = "Transformed Series",
        xlab = "Year")
legend("topleft",
       legend = c("Observed (stationary)", "Forecast"),
       lty    = c(1, 2),
       col    = c("steelblue", "red"))

# NOTE: These forecasts are on the transformed (seasonally differenced log) scale.
# A full back-transformation to the original scale would require
# reversing the seasonal differencing and the log transformation,
# which will be discussed further in the final paper.

# For comparison: recall the Holt-Winters forecasts from Part 4
# (UKgas.hw.forecast) which were on the original scale.
# In the paper we will compare both approaches.





