
setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")

# ====================================================
Herald.dat <- read.table("Data/Herald.dat", header = TRUE)
attach(Herald.dat)

x <- CO; y <- Benzoa; n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)

mean((x - mean(x)) * (y - mean(y)))
cov(x, y)

mean((x - mean(x))*(y - mean(y)))

cov(x,y) / (sd(x)*sd(y))
cor(x,y)


# ====================================================
wave.dat <- read.table("Data/wave.dat", header = TRUE)
attach(wave.dat)

plot(ts(waveht)) ; plot(ts(waveht[1:60]))
acf(waveht)$acf[2]
acf(waveht, type = c("covariance"))$acf[2]


data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")

plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])
sd(AP[7:138])

sd(AP[7:138] - AP.decom$trend[7:138])
sd(AP.decom$random[7:138])


# ====================================================
Fontdsdt.dat <- read.table("Data/Fontdsdt.dat", header=T)
attach(Fontdsdt.dat)

plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = 'lag (months)', main="")




# =====================================================
# Chapter 3
# =====================================================

ApprovActiv.dat <- read.table("Data/ApprovActiv.dat", header=T)
attach(ApprovActiv.dat)

App.ts <- ts(Approvals, start = c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, lty = c(1,3))

acf(ts.union(App.ts, Act.ts))


app.ran <- decompose(App.ts)$random
app.ran.ts <- window (app.ran, start = c(1996, 3) )
act.ran <- decompose (Act.ts)$random
act.ran.ts <- window (act.ran, start = c(1996, 3) )
acf (ts.union(app.ran.ts, act.ran.ts))
ccf (app.ran.ts, act.ran.ts)

print(acf(ts.union(app.ran.ts, act.ran.ts)))


# Use na.action = na.pass to ignore the NAs created by decompose
acf(ts.union(app.ran.ts, act.ran.ts), na.action = na.pass)

# Do the same for the CCF
ccf(app.ran.ts, act.ran.ts, na.action = na.pass)

# And for the print statement
print(acf(ts.union(app.ran.ts, act.ran.ts), na.action = na.pass))



# --------------- Bass model -----------
T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
                    (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)


Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979",
       ylab = "Sales per year", type='l')
points(T79, Sales)
Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979",
       ylab = "Cumulative sales", type='l')
points(T79, Cusales)


# ----------------------------
motororg.dat <- read.table("Data/motororg.dat", header=T)
attach(motororg.dat)

Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")


Comp.hw1 <- HoltWinters(complaints, beta = 0, gamma = 0) ; Comp.hw1
plot(Comp.hw1)

# -----------------------correction ---------------------
# Use the time series object, not the raw column name
Comp.hw1 <- HoltWinters(Comp.ts, beta = FALSE, gamma = FALSE)

# Now it will print and plot correctly
Comp.hw1
plot(Comp.hw1)


# ============== Sales of Australian wine ==================
wine.dat <- read.table("Data/wine.dat", header=T)
attach(wine.dat)

sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "mult")
sweetw.hw ; sweetw.hw$coef ; sweetw.hw$SSE


sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)
plot (sweetw.hw$fitted)
plot (sweetw.hw)


AP.hw <- HoltWinters(AP, seasonal = "mult")
plot(AP.hw)
AP.predict <- predict(AP.hw, n.ahead = 4 * 12)
ts.plot(AP, AP.predict, lty = 1:2)


# ---------------- Summary of commands used ------

# nls ==> non-linear least squares fit
# HoltWinters ==> estimates the parameters of the Holt-Winters or exponential smoothing model
# predict ==> forecasts future values
# ts.union ==> create the union of two series
# coef ==> extracts the coefficients of a fitted model




