#------------------------------------------
# Basics of R for Time Series Econometrics (ECO 402)
#------------------------------------------

# -------------------------------
# 1. RStudio Project & Directory
# -------------------------------

getwd()

# -------------------------------
# 2. Assignment and Objects
# -------------------------------

x <- 5
y <- 2
z <- x + y
z

# -------------------------------
# 3. Vectors
# -------------------------------

v <- c(1, 2, 3, 4, 5)

length(v)
mean(v)
sum(v)

v[1]
v[2:4]

# -------------------------------
# 4. Matrices
# -------------------------------

A <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
A

A[1, 2]
A[, 1]

# -------------------------------
# 5. Data Frames
# -------------------------------

df <- data.frame(
  year = c(2020, 2021, 2022),
  gdp  = c(21000, 22000, 23000)
)

df
df$gdp
df[, "gdp"]
df[1, ]

# -------------------------------
# 6. Importing Data
# -------------------------------

data <- read.csv("Data/Maine.dat")

head(data)
str(data)
summary(data)

# -------------------------------
# 7. Logical Operators & Subsetting
# -------------------------------

data[data$year >= 2000, ]
data[data$unemployment > 5, ]

# Operators:
# ==  !=  >  <  >=  <=
# &   |

# -------------------------------
# 8. Basic Plots
# -------------------------------

plot(data$year,
     data$unemployment,
     type = "l",
     xlab = "Year",
     ylab = "Unemployment Rate",
     main = "Unemployment Over Time")

# -------------------------------
# 9. Time Series Objects
# -------------------------------

u_monthly <- ts(data$unemployment,
                start = 1976,
                frequency = 12)

u_annual <- ts(data$unemployment,
               start = 1976,
               frequency = 1)

plot(u_monthly)

# -------------------------------
# 10. Time Series Summary Tools
# -------------------------------

mean(u_monthly)
var(u_monthly)

acf(u_monthly)
pacf(u_monthly)

# -------------------------------
# 11. Packages
# -------------------------------

# install.packages("forecast")
library(forecast)

# -------------------------------
# 12. ARIMA Models (Preview)
# -------------------------------

fit_arima <- arima(u_monthly, order = c(1, 0, 0))
fit_arima

auto_fit <- auto.arima(u_monthly)
auto_fit

# -------------------------------
# 13. Forecasting
# -------------------------------

fcast <- forecast(auto_fit, h = 12)
plot(fcast)

# -------------------------------
# 14. Saving and Loading Objects
# -------------------------------

save(u_monthly, file = "Data/unemployment_ts.RData")
rm(u_monthly)
load("Data/unemployment_ts.RData")



