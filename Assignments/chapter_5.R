# Chapter_5 - Linear models

set.seed(1)
z <- w <- rnorm(100, sd = 20)
for (t in 2:100) z[t] <- 0.8 * z[t - 1] + w[t]
Time <- 1:100
x <- 50 + 3 * Time + z
plot(x, xlab = "time", type = "l")

# Fitted Models
x.lm <- lm(x ~ Time)
coef(x.lm)
(Intercept) Time
sqrt(diag(vcov(x.lm)))
(Intercept) Time

acf(resid(x.lm))
pacf(resid(x.lm))


# 5.3 
x.lm <- lm(x ~ Time)
summary(x.lm)

coef(x.lm)
acf(resid(x.lm))

#5.4 
library(nlme)
x.gls <- gls(x ~ Time,
             cor = corAR1(0.8))


#5.5
x.lm <- lm(x ~ Time + factor(cycle(x)))
acf(resid(x.lm))



#5.7
lx <- log(x)
lx.lm <- lm(lx ~ Time)


#5.9 Forecasting with Linear Regression
x.lm <- lm(x ~ Time)

future.Time <- data.frame(
  Time = 101:110)

predict(x.lm,
        newdata = future.Time)

plot(x, type = "l")
lines(101:110,
      predict(x.lm,
              newdata = future.Time),
      lty = 2)


x.gls <- gls(x ~ Time,
             cor = corAR1(0.8))

predict(x.gls,
        newdata = future.Time)

plot(x, type = "l")
lines(101:110,
      predict(x.gls,
              newdata = future.Time),
      lty = 2)




# Forecasting Temperature Series

temp.gls <- gls(temp ~ time(temp),
                cor = corAR1(0.7))

future.temp <- data.frame(
  "time(temp)" = 2006:2015)

predict(temp.gls,
        newdata = future.temp)

plot(temp, type = "l")
lines(2006:2015,
      predict(temp.gls,
              newdata = future.temp),
      lty = 2)

lx.lm <- lm(lx ~ Time)

predict(lx.lm,
        newdata = future.Time)

exp(predict(lx.lm,
            newdata = future.Time))


# Plotting Log Forecasts

plot(x, type = "l")
lines(101:110,
      exp(predict(lx.lm,
                  newdata = future.Time)),
      lty = 2)
































