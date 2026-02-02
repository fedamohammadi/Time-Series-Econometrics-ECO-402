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

# This series shows an upward trend over time, as the overall number of passengers keeps increasing.
# It also has a clear seasonal pattern because the same peaks and dips repeat every year.
# The size of the seasonal swings grows over time, which suggests increasing variability as the level rises.
# ==============================================================================


#Figure 1.2
#----------------------------------------------------------------
layout(matrix(1:2, nrow = 2))

plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

## This is trying to create the graph in a saperate window for better visual.
windows(width = 8, height = 6)
par(mar = c(3, 3, 2, 1))
plot(aggregate(AP))

# The aggregate series shows an upward trend over time, which means total air passengers are increasing consistently.
# There is no visible seasonality in the aggregate plot because seasonal effects are averaged out.
# This shows that the main long-run movement of the series is possibly caused by trend rather than short-term fluctuations.

# The monthly box plot shows strong seasonality because passenger levels are different across months.
# Summer months have higher medians and larger spread, but winter months have lower values.
# This shows that the series has a clear and repeating seasonal pattern.
# ==============================================================================


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

# The series shows that this is a cyclical graph since unemployment is rising and falling over time.
# There is no long-run trend but clear medium-term movements which is linked to the business cycle.
# But the the short-term fluctuations shows some seasonal or irregular variation.

# The aggregate series also shows the business cycle more clearly by smooth short-term noise.
# Unemployment declines in the late 1990s, rises in the early 2000s, then stabilizes.
# This series is cyclical rather than trending or strongly seasonal.
# ==============================================================================


# Figure 1.4: Unemployment 
#----------------------------------------------------------------
USUnemp <- read.table("Data/USunemp.dat", header = TRUE)
US.month <- read.table("Data/USunemp.dat", header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996, 1), end=c(2006, 10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")

# This series shows ups and downs over time which is a cyclical pattern.
# Unemployment falls in the late 1990s, rises in the early 2000s, and then falls again.
# There is no clear long-term upward or downward trend.
# ==============================================================================


#Figure 1.5: Multiple time series: Electricity, beer and chocolate data
#----------------------------------------------------------------
CBE <- read.table("Data/cbe.dat", header = TRUE)

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# Elec.ts: This one has an upward trend because the overall level keeps rising over time.
# It also has clear seasonality because the ups and downs repeat in a regular pattern each year.
# The seasonal swings also get bigger over time, so the variation increases as the level increases.

# Beer.ts: This one has a seasonality because it repeats a similar pattern each year.
# The overall level rises for a while, then becomes more flat later, so the trend is weak or changes over time.
# The series also has more noise, with more short-term random movement than the electricity series.

# Choc.ts: This one has seasonality with repeating peaks and drops each year.
# It also has a general upward trend, especially in the later years where the values rise more.
# There are some sharp drops at regular times, which is a strong seasonal cycle.
# ==============================================================================


# Figure 1.6: This one does not plot the graph.
#----------------------------------------------------------------
AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3, ]


# Figure 1.7: International air passengers and Australian electrictity production (1958-1960)
#----------------------------------------------------------------
AP.elec <- ts.intersect(AP, Elec.ts)
AP <- AP.elec[,1]; Elec <- AP.elec[,2]

layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")

# This graph has clear seasonality because the pattern repeats each year.
# Passenger numbers rise and fall in a regular way within each year.
# Over this short period, there is no strong long-term trend.

# The next plot also shows a seasonality with repeating yearly patterns.
# Electricity production rises and falls at similar times each year.
# The overall level changes slowly, so the main feature is seasonality.
# ==============================================================================


# Figure 1.8: Scatter plot of air passengers and Australian electrictity production (1958-1960) 
#----------------------------------------------------------------
plot(as.vector(AP), as.vector(Elec),
       xlab = "Air passengers / 1000's",
       ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
cor(AP, Elec)

# This plot has a positive relationship between air passengers and electricity production.
# As the number of air passengers increases, electricity production also tends to increase.
# The upward sloping line shows that the two variables move together over time.
# ==============================================================================


# Figure 1.9: Quarterly exchange rates fro the period 1991-2000  
#----------------------------------------------------------------
PoundsNZ <- read.table("Data/pounds_nz.dat", header = TRUE)
PoundsNZ[1:4, ]
PoundsNZ.ts <- ts(PoundsNZ, start = 1991, frequency = 4)

plot(PoundsNZ.ts,
     xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")

# This series shows clear ups and downs over time, which is a cyclical behavior.
# There is no strong seasonal pattern, but the level changes over several years.
# The exchange rate falls in the mid-1990s and then rises again toward 2000.
# ==============================================================================


# Figure 1.10: Quarterly exchange rates for two periods  
#----------------------------------------------------------------
PoundsNZ <- window(PoundsNZ.ts, start = c(1992, 1), end = c(1996, 1))
PoundsNZ <- window(PoundsNZ.ts, start = c(1996, 1), end = c(1998, 1))
layout (1:2)
plot(PoundsNZ, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
plot(PoundsNZ, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )

# This one shows a upward trend over this short period.
# The exchange rate increases from 1996 to 1998.
# There is no visible seasonality because the time span is too short.
# ==============================================================================


# Figure 1.11: Time plots of the global temperature series
#----------------------------------------------------------------
Global <- read.table("Data/global.dat", header = TRUE)

Global <- scan("Data/global.dat")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                  fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)


# This series shows a slow upward trend over time.
# There is a lot of short-term random variation, but no clear seasonal pattern.
# The series appears noisy with gradual long-run movement.

# The annual one smooths out short-term noise and shows the long-term trend more clearly.
# It increases slowly over time, especially after the mid-20th century.
# This shows that the main feature of the series is its long-run trend.
# ==============================================================================


# Figure 1.12: Rising mean global temperatures, January 1970–December 2005
#----------------------------------------------------------------
New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series ~ New.time))

# This graph shows a clear upward trend over time.
# The line fluctuates around the trend, which shows short-term random variation.
# There is no clear seasonal pattern, only gradual long-run movement.
# ==============================================================================





