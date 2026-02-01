# In class activity number 2.
setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")

# This verifies if you're in the right directory.
getwd()


# Figure 1.1: # This is the plot of the international air passengers in the US from 1949-1960. 
#----------------------------------------------------------------
data(AirPassengers)
AP <- AirPassengers

class (AP)
start(AP); end(AP); frequency(AP)
summary(AP)

windows(width = 8, height = 6)
plot(AP, ylab = "Passengers (1000's)")

# This is trying to create the graph in a saperate window for better visual.
while (!is.null(dev.list())) dev.off()
par(reset = TRUE)
par(mar = c(5, 4, 4, 2) + 0.1)


#Figure 1.2
#----------------------------------------------------------------
layout(matrix(1:2, nrow = 2))

plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

## This is trying to create the graph in a saperate window for better visual.
windows(width = 8, height = 6)
par(mar = c(3, 3, 2, 1))
plot(aggregate(AP))


# Figure 1.3: Unemployment in Maine 
#----------------------------------------------------------------
Maine.month <- read.table("Data/Maine.dat", header = TRUE)
attach(Maine.month)
class(Maine.month)

Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)

Maine.annual.ts <- aggregate(Maine.month.ts)/12

layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")

#A time series of February figures

Maine.Feb <- window(Maine.month.ts, start = c(1996, 2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996, 8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.annual.ts)


# Figure 1.4: Unemployment 
#----------------------------------------------------------------
USUnemp <- read.table("Data/USunemp.dat", header = TRUE)
US.month <- read.table("Data/USunemp.dat", header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996, 1), end=c(2006, 10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")


#Figure 1.5: Multiple time series: Electricity, beer and chocolate data
#----------------------------------------------------------------
CBE <- read.table("Data/cbe.dat", header = TRUE)

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))


# Figure 1.6: This one does not plot the graph.
#----------------------------------------------------------------
AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3, ]


# Figure 1.7: International air passengers and Australian electrictity production (1958-1960)
#----------------------------------------------------------------
AP <- AP.elec[,1]; Elec <- AP.elec[,2]

layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")


# Figure 1.8: Scatter plot of air passengers and Australian electrictity production (1958-1960) 
#----------------------------------------------------------------
plot(as.vector(AP), as.vector(Elec),
       xlab = "Air passengers / 1000's",
       ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
cor(AP, Elec)


# Figure 1.9: Quarterly exchange rates fro the period 1991-2000  
#----------------------------------------------------------------
PoundsNZ <- read.table("Data/pounds_nz.dat", header = TRUE)
Z <- read.table(www, header = T)

Z[1:4, ]
Z.ts <- ts(Z, st = 1991, fr = 4)

plot(Z.ts, xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")


# Figure 1.10: Quarterly exchange rates for two periods  
#----------------------------------------------------------------
Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))
layout (1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
plot(Z.96.98, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )


# Figure 1.11: Time plots of the global temperature series
#----------------------------------------------------------------
Global <- read.table("Data/global.dat", header = TRUE)

Global <- scan("Data/global.dat")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                  fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)


# Figure 1.12: Rising mean global temperatures, January 1970–December 2005
#----------------------------------------------------------------
New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series ~ New.time))




