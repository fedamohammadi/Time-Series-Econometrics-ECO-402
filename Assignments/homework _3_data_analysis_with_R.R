# ===========================================================
# Ebola Analysis: Data Analysis with R
# Name: Feda Mohammadi
# Date: February 14, 2026
# Assignment: Homework 3 
# Professor: Dr. Nabila Rahman
# ===========================================================


# ===========================================================
# Loading packages ----
# ===========================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# all packages used in the lesson
pacman::p_load(
  tidyverse,   # meta-package: ggplot2, dplyr, readr, etc.
  inspectdf,   # inspect_cat(), inspect_num(), show_plot()
  plotly,      # ggplotly() for interactive plots
  janitor,     # tabyl()
  visdat,      # vis_dat()
  esquisse     # esquisse::esquisser()
)


# ===========================================================
# Getting the dataset ----
# ===========================================================

# this is the data used in the video

ebola_sierra_leone <- readr::read_csv("Ebola_Sierra_Leone.csv")

# View(ebola_sierra_leone)


head(ebola_sierra_leone)
tail(ebola_sierra_leone)


# ===========================================================
# Data exploration ----
# ===========================================================

# Dimensions
ncol(ebola_sierra_leone)
nrow(ebola_sierra_leone)
dim(ebola_sierra_leone)

# Summary of each variable
summary(ebola_sierra_leone)

# Help files (examples)
# ?nrow
# ?summary


# ===========================================================
# visdat (visual dataset overview) ----
# ===========================================================

visdat::vis_dat(ebola_sierra_leone)


# ===========================================================
# inspectdf: categorical overview + numeric overview ----
# ===========================================================

# Categorical/date overview (returns a ggplot object)
cat_summary_plot <- inspectdf::show_plot(
  inspectdf::inspect_cat(ebola_sierra_leone)
)

# Make it interactive
plotly::ggplotly(cat_summary_plot)

# Numeric overview
num_summary_plot <- inspectdf::show_plot(
  inspectdf::inspect_num(ebola_sierra_leone)
)

# Make it interactive
plotly::ggplotly(num_summary_plot)


# ===========================================================
# Analyzing a numeric variable (Age) ----
# ===========================================================

# Pull the numeric variable using $ (vector)
age_vec <- ebola_sierra_leone$age

# Core summary functions (handle missing values with na.rm = TRUE when needed)
mean(age_vec, na.rm = TRUE)
median(age_vec, na.rm = TRUE)
sd(age_vec, na.rm = TRUE)
min(age_vec, na.rm = TRUE)
max(age_vec, na.rm = TRUE)

# Summary gives several stats and also shows NA count
summary(age_vec)

# Length is total entries (includes NA)
length(age_vec)

# Sum (usually not meaningful here, but included in lesson)
sum(age_vec, na.rm = TRUE)


# ===========================================================
# Visualizing a numeric variable (base R) ----
# ===========================================================

hist(age_vec, main = "Age Histogram", xlab = "Age")
boxplot(age_vec, main = "Age Boxplot", horizontal = TRUE)


# ===========================================================
# Visualizing a numeric variable (Esquisse GUI) ----
# ===========================================================

# Example ggplot code you can use immediately (histogram of age):
ggplot(ebola_sierra_leone, aes(x = age)) +
  geom_histogram() +
  labs(title = "Age Distribution")

# Example: age by sex (violin + fill)
ggplot(ebola_sierra_leone, aes(x = sex, y = age, fill = sex)) +
  geom_violin() +
  labs(title = "Age by Sex")

# Optional: interactive
plotly::ggplotly(
  ggplot(ebola_sierra_leone, aes(x = sex, y = age, fill = sex)) +
    geom_violin() +
    labs(title = "Age by Sex (Interactive)")
)


# ===========================================================
# Analyzing a categorical variable (District) ----
# ===========================================================

district_vec <- ebola_sierra_leone$district

# Base R table
table(district_vec)

# janitor tabyl (nicer table + proportions)
janitor::tabyl(ebola_sierra_leone, district)

# Two-way tabulation (district by sex)
janitor::tabyl(ebola_sierra_leone, district, sex)


# ===========================================================
# Visualizing a categorical variable ----
# ===========================================================

# Base R: barplot of table
barplot(table(district_vec), las = 2, main = "Cases by District", ylab = "Count")

# Base R: pie chart (useful sometimes, usually worse than bars)
pie(table(district_vec), main = "Cases by District (Pie)")

# ggplot bar chart (cleaner than base R)
ggplot(ebola_sierra_leone, aes(x = district)) +
  geom_bar() +
  labs(title = "Cases by District") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Optional interactive
plotly::ggplotly(
  ggplot(ebola_sierra_leone, aes(x = district)) +
    geom_bar() +
    labs(title = "Cases by District (Interactive)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)


# ===========================================================
# Question answering (from the lesson) ----
# ===========================================================

# If needed, convert to Date (try common formats)
# ebola_sierra_leone <- ebola_sierra_leone %>%
#   mutate(
#     date_of_sample = as.Date(date_of_sample)  # if it already looks like "2014-05-23"
#   )

min(ebola_sierra_leone$date_of_sample, na.rm = TRUE)

# 2) "What was the median age of those affected?"
median(ebola_sierra_leone$age, na.rm = TRUE)

# 3) "More cases in men or women?"
table(ebola_sierra_leone$sex)
janitor::tabyl(ebola_sierra_leone, sex)

# 4) "Which district had the most reported cases?"
sort(table(ebola_sierra_leone$district), decreasing = TRUE)
janitor::tabyl(ebola_sierra_leone, district) %>% arrange(desc(n))


# ebola_sierra_leone <- ebola_sierra_leone %>%
#   mutate(date_of_onset = as.Date(date_of_onset))

ggplot(ebola_sierra_leone, aes(x = date_of_onset)) +
  geom_bar() +
  labs(title = "Cases Over Time (by Date of Onset)", x = "Date of onset", y = "Number of cases")

# Optional interactive
plotly::ggplotly(
  ggplot(ebola_sierra_leone, aes(x = date_of_onset)) +
    geom_bar() +
    labs(title = "Cases Over Time (Interactive)", x = "Date of onset", y = "Number of cases")
)



