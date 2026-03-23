
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













