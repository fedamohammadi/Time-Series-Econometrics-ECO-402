# Chapter 4 Snippets 

set.seed(1)
w <- rnorm(100)
plot(w, type = "l")


x <- seq(-3,3, length = 1000)
hist(rnorm(100), prob = T); points(x, dnorm(x), type = "l")

set.seed(2)
acf(rnorm(100))



# Random Walk: The Difference Operator
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t - 1] + w[t]
plot(x, type = "l")


acf(diff(x))

setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")


Z.df <- read.table("Data/pounds_nz.dat", header = TRUE)
Z <- read.table("Data/pounds_nz.dat", header = T)

Z.ts <- ts(Z, st = 1991, fr = 4)

Z.hw <- HoltWinters(Z.ts, alpha = 1, gamma = FALSE)
plot(Z.hw, main = "Holt-Winters Fit")
acf(resid(Z.hw), na.action = na.pass, main = "ACF of Residuals")



# Random Walk with drift

setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")
HP.txt <- read.table("Data/HP.txt", header = TRUE)
attach(HP.txt)

plot (as.ts(Price))
DP <- diff(Price) ; plot (as.ts(DP)) ; acf (DP)
mean(DP) + c(-2, 2) * sd(DP)/sqrt(length(DP))









