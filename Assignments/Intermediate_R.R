#-------------------------------------------------------
# Intermediate R for Time Series Econometrics (ECO 402)
#-------------------------------------------------------

# -------------------------------
# 1. Clean Workspace
# -------------------------------

rm(list = ls())
graphics.off()

# -------------------------------
# 2. Packages
# -------------------------------

# install.packages(c("forecast", "tseries", "lmtest", "urca"))
library(forecast)
library(tseries)
library(lmtest)
library(urca)

# -------------------------------
# 3. Import and Inspect Data
# -------------------------------

data <- read.csv("Data/Maine.dat")

str(data)
summary(data)

# -------------------------------
# 4. Create Time Series Object
# -------------------------------

u <- ts(data$unemployment,
        start = c(1976, 1),
        frequency = 12)

plot(u,
     main = "Monthly Unemployment Rate",
     ylab = "Percent",
     xlab = "Year")

# -------------------------------
# 5. Transformations
# -------------------------------

u_log <- log(u)
u_diff <- diff(u)
u_log_diff <- diff(log(u))

par(mfrow = c(2, 2))
plot(u, main = "Level")
plot(u_log, main = "Log")
plot(u_diff, main = "First Difference")
plot(u_log_diff, main = "Log Difference")
par(mfrow = c(1, 1))

# -------------------------------
# 6. Stationarity Tests
# -------------------------------

adf.test(u)
adf.test(u_diff)

kpss.test(u)
kpss.test(u_diff)

# -------------------------------
# 7. Autocorrelation Analysis
# -------------------------------

acf(u, main = "ACF: Level")
pacf(u, main = "PACF: Level")

acf(u_diff, main = "ACF: First Difference")
pacf(u_diff, main = "PACF: First Difference")

# -------------------------------
# 8. AR, MA, ARMA Models
# -------------------------------

ar1 <- arima(u_diff, order = c(1, 0, 0))
ma1 <- arima(u_diff, order = c(0, 0, 1))
arma11 <- arima(u_diff, order = c(1, 0, 1))

ar1
ma1
arma11

# -------------------------------
# 9. Model Comparison
# -------------------------------

AIC(ar1, ma1, arma11)
BIC(ar1, ma1, arma11)

# -------------------------------
# 10. ARIMA Modeling
# -------------------------------

auto_model <- auto.arima(u)
auto_model

manual_model <- arima(u, order = c(1, 1, 1))
manual_model

# -------------------------------
# 11. Residual Diagnostics
# -------------------------------

res <- residuals(auto_model)

par(mfrow = c(2, 2))
plot(res, main = "Residuals")
acf(res, main = "ACF Residuals")
pacf(res, main = "PACF Residuals")
qqnorm(res)
qqline(res)
par(mfrow = c(1, 1))

Box.test(res, lag = 12, type = "Ljung-Box")

# -------------------------------
# 12. Forecasting
# -------------------------------

fc <- forecast(auto_model, h = 12)

plot(fc,
     main = "12-Month Forecast",
     xlab = "Year",
     ylab = "Unemployment Rate")

accuracy(fc)

# -------------------------------
# 13. Structural Break Check
# -------------------------------

time_index <- time(u)
trend <- as.numeric(time_index)

ols_model <- lm(u ~ trend)
summary(ols_model)

coeftest(ols_model)

# -------------------------------
# 14. Lagged Variables
# -------------------------------

u_lag1 <- stats::lag(u, -1)
u_lag2 <- stats::lag(u, -2)

lag_df <- na.omit(cbind(u, u_lag1, u_lag2))
colnames(lag_df) <- c("u", "u_lag1", "u_lag2")

head(lag_df)

# -------------------------------
# 15. Dynamic Regression
# -------------------------------

dyn_model <- lm(u ~ u_lag1 + u_lag2, data = as.data.frame(lag_df))
summary(dyn_model)

# -------------------------------
# 16. Saving Results
# -------------------------------

save(u, auto_model, fc, file = "your file name")

