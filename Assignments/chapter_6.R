# -------------- Chapter 6 ---------------------

# ==================================================
# PART 1: x (simulated AR series with trend)
# ==================================================

# Recreate the x series from In-Class Assignment 9
set.seed(1)
z <- w <- rnorm(100, sd = 20)
for (t in 2:100) z[t] <- 0.8 * z[t - 1] + w[t]
Time <- 1:100
x <- 50 + 3 * Time + z

# First: making the series more stationary
# we difference x to remove the linear trend
x.diff <- diff(x)
plot(x.diff, type = "l", main = "x series after differencing")

# --------------------------------------------------
# STEP 2: PLOT ACF AND PACF
# ACF cuts off after lag q -> try MA(q)
# PACF cuts off after lag p -> try AR(p)
# both tail off slowly -> try ARMA(p,q)
par(mfrow = c(2, 1))
acf(x.diff, main = "ACF of differenced x")
pacf(x.diff, main = "PACF of differenced x")

# --------------------------------------------------
# STEP 3: FIT CANDIDATE MODELS
# order = c(p, d, q): p = AR, d = differencing, q = MA
fit.ar1.x    <- arima(x.diff, order = c(1, 0, 0))   # AR(1)
fit.ma1.x    <- arima(x.diff, order = c(0, 0, 1))   # MA(1)
fit.arma11.x <- arima(x.diff, order = c(1, 0, 1))   # ARMA(1,1)

# --------------------------------------------------
# STEP 4: COMPARE MODELS USING AIC
# lower AIC suggests a better fit among these models
AIC(fit.ar1.x, fit.ma1.x, fit.arma11.x)

# --------------------------------------------------
# STEP 5: RESIDUAL DIAGNOSTICS
# residuals should look close to white noise
par(mfrow = c(2, 1))
acf(resid(fit.arma11.x), main = "Residual ACF - x")
pacf(resid(fit.arma11.x), main = "Residual PACF - x")

# --------------------------------------------------
# STEP 6: FORECAST
pred.x <- predict(fit.arma11.x, n.ahead = 12)
pred.x$pred
pred.x$se

# ==================================================
# PART 2: temp (temperature series)
# ==================================================

# Load the temp series from the same source used in Assignment 9
setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")
global.dat <- read.table("Data/global.dat", header = TRUE)
temp <- global.dat[[1]]
temp <- ts(temp, start = 1856, frequency = 1)

# First: making the series more stationary
# we difference temp to remove trend
temp.diff <- diff(temp)
plot(temp.diff, type = "l", main = "temp series after differencing")

# --------------------------------------------------
# STEP 2: PLOT ACF AND PACF
# ACF cuts off after lag q -> try MA(q)
# PACF cuts off after lag p -> try AR(p)
# both tail off slowly -> try ARMA(p,q)
par(mfrow = c(2, 1))
acf(temp.diff, main = "ACF of differenced temp")
pacf(temp.diff, main = "PACF of differenced temp")

# --------------------------------------------------
# STEP 3: FIT CANDIDATE MODELS
# order = c(p, d, q): p = AR, d = differencing, q = MA
fit.ar1.t    <- arima(temp.diff, order = c(1, 0, 0))   # AR(1)
fit.ma1.t    <- arima(temp.diff, order = c(0, 0, 1))   # MA(1)
fit.arma11.t <- arima(temp.diff, order = c(1, 0, 1))   # ARMA(1,1)

# --------------------------------------------------
# STEP 4: COMPARE MODELS USING AIC
# lower AIC suggests a better fit among these models
AIC(fit.ar1.t, fit.ma1.t, fit.arma11.t)

# --------------------------------------------------
# STEP 5: RESIDUAL DIAGNOSTICS
# residuals should look close to white noise
par(mfrow = c(2, 1))
acf(resid(fit.arma11.t), main = "Residual ACF - temp")
pacf(resid(fit.arma11.t), main = "Residual PACF - temp")

# --------------------------------------------------
# STEP 6: FORECAST
pred.t <- predict(fit.arma11.t, n.ahead = 12)
pred.t$pred
pred.t$se

# ==================================================
# PART 3: UKgas => my final project dataset
# ==================================================

# load dataset
data(UKgas)

# UKgas has clear seasonal movement, so seasonal differencing is used
# lag = 4 because UKgas is quarterly
UKgas.diff <- diff(UKgas, lag = 4)
plot(UKgas.diff, type = "l", main = "UKgas after seasonal differencing")

# --------------------------------------------------
# STEP 2: PLOT ACF AND PACF
# ACF cuts off after lag q -> try MA(q)
# PACF cuts off after lag p -> try AR(p)
# both tail off slowly -> try ARMA(p,q)
par(mfrow = c(2, 1))
acf(UKgas.diff, main = "ACF of differenced UKgas")
pacf(UKgas.diff, main = "PACF of differenced UKgas")

# --------------------------------------------------
# STEP 3: FIT CANDIDATE MODELS
# order = c(p, d, q): p = AR, d = differencing, q = MA
fit.ar1.u    <- arima(UKgas.diff, order = c(1, 0, 0))   # AR(1)
fit.ma1.u    <- arima(UKgas.diff, order = c(0, 0, 1))   # MA(1)
fit.arma11.u <- arima(UKgas.diff, order = c(1, 0, 1))   # ARMA(1,1)

# --------------------------------------------------
# STEP 4: COMPARE MODELS USING AIC
# lower AIC suggests a better fit among these models
AIC(fit.ar1.u, fit.ma1.u, fit.arma11.u)

# --------------------------------------------------
# STEP 5: RESIDUAL DIAGNOSTICS
# residuals should look close to white noise
par(mfrow = c(2, 1))
acf(resid(fit.arma11.u), main = "Residual ACF - UKgas")
pacf(resid(fit.arma11.u), main = "Residual PACF - UKgas")

# --------------------------------------------------
# STEP 6: FORECAST
pred.u <- predict(fit.arma11.u, n.ahead = 12)
pred.u$pred
pred.u$se





