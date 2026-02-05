#=================================================================
# Comments ----
#=================================================================

# 2 + 2   # This is also a comment because it starts with #

# The code below adds two numbers:
2 + 2

2 + 2  # addition


#=================================================================
# Sections (RStudio folding + outline) ----
#=================================================================

# In RStudio, a comment that ends with "----" becomes a section header.
# You can fold it, and it shows up in the Outline.


#=================================================================
# R as a calculator ----
#=================================================================

2 + 2
2 - 2
2 * 2
2 / 2
2^2

# Order of operations (multiplication before addition)
2 + 2 * 2

# Parentheses change the order
(2 + 2) * 2 

# square root
sqrt(100)   

# Practice (answer as a comment):
# Q: In this expression, which is evaluated first, the division or the minus?
#    10 - 6 / 2
10 - 6 / 2  # division first: 6/2 = 3, then 10 - 3 = 7


#=================================================================
# Code formatting (spaces + common console issue) ----
#=================================================================

# Spaces usually do not change meaning
2+2
2 + 2
2     +        2

# Same for functions
sqrt(100)
sqrt( 100 )
sqrt(
  100
)

# IMPORTANT:
# If you forget a closing parenthesis and run the line,
# the Console shows a "+" prompt because R is waiting for you to finish.
# Press Esc in the Console to cancel the incomplete command.


#=================================================================
# Creating objects ----
#=================================================================

# Assignment operator: <-
my_object <- 2 + 2
my_object

# Overwriting an object (same name, new content)
my_object <- 20
my_object

# Strings need quotes
first_name <- "Joanna"
first_name

# Practice (answer as a comment):
# result <- 2 + 2 + 2
result <- 2 + 2 + 2
result  # = 6


#=================================================================
# Data sets are objects too ----
#=================================================================

# A data frame is just another object.

# A built-in dataset:
women_data <- women
head(women_data)

# Make a sample data frame similar to the video "Ebola data" example
set.seed(123)

ebola_data <- data.frame(
  id = 1:200,
  age = sample(1:80, 200, replace = TRUE),
  sex = sample(c("female", "male"), 200, replace = TRUE, prob = c(0.62, 0.38)),
  district = sample(c("Western", "Urban", "Rural", "Port Loko", "Bombali"), 200, replace = TRUE)
)

head(ebola_data)
# View(ebola_data)

# Table of patients by sex
table(ebola_data$sex)

# Practice:
# 1) Predict the value of answer before you run it:
my_num <- 10
answer <- my_num^2 + 5
answer

# 2) Make a table of patients across districts:
table(ebola_data$district)


#=================================================================
# Renaming objects ----
#=================================================================

# There is no magical rename command.
# You copy to a new name, then remove the old name.

ebola <- ebola_data   
rm(ebola_data)        

# Check what's in your environment:
ls()

# Now "ebola" exists, "ebola_data" is gone
head(ebola)


#=================================================================
# Overwriting objects ----
#=================================================================

first_name <- "Joanna"
first_name

first_name <- "Luigi"  # overwrite
first_name


#=================================================================
# Working with objects ----
#=================================================================

my_number <- 100
sqrt(my_number)

my_sum <- my_number + my_number
my_sum

# Print an object quickly (sends it to Console if you run the line)
my_sum

# Working with a data object
head(ebola, 3)
table(ebola$sex)


#=================================================================
# Errors with objects ----
#=================================================================

# adding strings causes an error
first_name <- "Luigi"
last_name  <- "Fenway"

# This will error
# first_name + last_name
paste(first_name, last_name)
paste0(first_name, " ", last_name)

# Case sensitivity example
my_number <- 48

my_number + 2


#=================================================================
# Naming objects (rules + conventions) ----
#=================================================================

snake_case_example <- "uses_underscores"
period.case.example <- "uses.periods"
camelCaseExample <- "usesCapitalLetters"

# Rules:
# 1) Cannot start with a number
# 2) Cannot include spaces
# 3) Usually only letters, numbers, underscore, and dot (and must start with a letter or dot)


# Valid:
data_2014 <- 2
data_2014

`data-2014` <- 2
`data-2014`


#=================================================================
# Functions (syntax basics) ----
#=================================================================

# A function call looks like:
# function_name(argument1 = value1, argument2 = value2)

# Example: head()
head(ebola, n = 3)

# You can name arguments in any order
head(n = 3, x = ebola)

# Because x then n is the default order, you can skip names if you follow the order
head(ebola, 3)

# Default values: head(x) defaults to n = 6
head(ebola)


#=================================================================
# Function nesting ----
#=================================================================

tolower("Luigi")

# Nesting: put one function inside another
paste("my name is", tolower("Luigi"))

# Alternative: use an intermediate object
lower_name <- tolower("Luigi")
paste("my name is", lower_name)


#=================================================================
# Packages (install, load, and ::) ----
#=================================================================

# Install once (then comment it out)
# install.packages("janitor")

# Load each new session
# library(janitor)

# Use a function with full qualifier package::function
# janitor::clean_names(some_data)

# Why use package::function?
# 1) It is explicit which package the function comes from
# 2) You can call it without library(package) (most of the time)


#=================================================================
# Some useful packages for Time Series Econometrics
#=================================================================


# --- Install (run once) ---
pkgs <- c(
  "forecast",     # ARIMA/ETS/forecasting toolkit
  "tseries",      # ADF/KPSS/PP tests
  "urca",         # serious unit root + Johansen cointegration
  "vars",         # VAR / IRFs / FEVD
  "tsDyn",        # VECM + nonlinear/threshold models
  "zoo",          # irregular time series objects
  "xts",          # time-indexed series (finance-style)
  "rugarch",      # GARCH volatility models
  "strucchange",  # structural breaks / breakpoints
  "ggplot2",      # plotting
  "tsibble",      # modern tidy time series objects
  "dplyr"         # data wrangling (often needed with ts data)
)


new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

# --- Load (every session) ---
invisible(lapply(pkgs, library, character.only = TRUE))

# Quick check: list loaded packages
sessionInfo()






