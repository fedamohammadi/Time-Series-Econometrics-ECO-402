# -------------- Chapter 6 ---------------------


# ==================================================
# PART 1: global.dat
# ==================================================

# load the dataset
setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")
global.dat <- read.table("Data/global.dat", header = TRUE)
attach(global.dat)

# First: making the series more stationary
# we difference the global series to reduce trend
global.series <- global.dat[[1]]
global.diff <- diff(global.series)

plot(global.diff, type = "l", main = "Global series after differencing")
# --------------------------------------------------
# STEP 2: PLOT ACF AND PACF
# ACF cuts off after lag q -> try MA(q)
# PACF cuts off after lag p -> try AR(p)
# both tail off slowly -> try ARMA(p,q)

par(mfrow = c(2, 1))
acf(global.diff, main = "ACF of differenced global series")
pacf(global.diff, main = "PACF of differenced global series")


# --------------------------------------------------
# STEP 3: FIT CANDIDATE MODELS
# order = c(p, d, q): p = AR, d = differencing, q = MA

fit.ar1.g     <- arima(global.diff, order = c(1, 0, 0))   # AR(1)
fit.ma1.g     <- arima(global.diff, order = c(0, 0, 1))   # MA(1)
fit.arma11.g  <- arima(global.diff, order = c(1, 0, 1))   # ARMA(1,1)


# --------------------------------------------------
# STEP 4: COMPARE MODELS USING AIC
# lower AIC suggests a better fit among these models

AIC(fit.ar1.g, fit.ma1.g, fit.arma11.g)


# --------------------------------------------------
# STEP 5: RESIDUAL DIAGNOSTICS
# residuals should look close to white noise

par(mfrow = c(2, 1))
acf(resid(fit.arma11.g), main = "Residual ACF - global")
pacf(resid(fit.arma11.g), main = "Residual PACF - global")


# --------------------------------------------------
# STEP 6: FORECAST

pred.g <- predict(fit.arma11.g, n.ahead = 12)
pred.g$pred
pred.g$se




# ==================================================
# PART 2: UKgas => my final project dataset
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

fit.ar1.u     <- arima(UKgas.diff, order = c(1, 0, 0))   # AR(1)
fit.ma1.u     <- arima(UKgas.diff, order = c(0, 0, 1))   # MA(1)
fit.arma11.u  <- arima(UKgas.diff, order = c(1, 0, 1))   # ARMA(1,1)


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







