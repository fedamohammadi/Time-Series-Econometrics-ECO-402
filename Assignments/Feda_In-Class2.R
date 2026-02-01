# In class activity #2.
setwd("C:/Users/mohammadif/Documents/Time Series Econometrics")

# This verifies if you're in the right directory.
getwd()


# Figure 1.1
#----------------------------------------------------------------
data(AirPassengers)
AP <- AirPassengers

class (AP)
start(AP); end(AP); frequency(AP)
summary(AP)

windows(width = 8, height = 6)
plot(AP, ylab = "Passengers (1000's)")

# This is the plot of the international air passengers in the US from 1949-1960. It also shows
# a clear insreasing pattern over time.

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
#-----------------------------------









# Figure 1.3 
Maine.month <- read.table("Data/Maine.dat", header = TRUE)

# Check data
str(Maine.month)

# Check data type
class(Maine.month)

# Create monthly time series explicitly
Maine.month.ts <- ts(Maine.month$unemploy,
                     start = c(1996, 1),
                     frequency = 12)

# Aggregate to annual average
Maine.annual.ts <- aggregate(Maine.month.ts) / 12

# Plot
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

plot(Maine.month.ts, ylab = "Unemployed (%)",
     main = "Monthly Unemployment in Maine")

plot(Maine.annual.ts, ylab = "Unemployed (%)",
     main = "Annual Unemployment in Maine")




# this project will be completed in the class only.  




