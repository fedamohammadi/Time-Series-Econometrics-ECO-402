# =========================================================
# Chapter 2 Graphs (Base R)
# Applied Statistics Handbook
# =========================================================

# --- 0) Sanity check: does plotting work at all? ---
# Run this FIRST. If you do not see a plot, the issue is your RStudio plotting device.
plot(1:10, 1:10, main = "Sanity check: if you see this, plotting works")

# --- 1) Data (built-in, reproducible) ---
# Old Faithful geyser eruption times (minutes)
x <- faithful$eruptions

# Optional: quick look at the data
summary(x)

# =========================================================
# 2.1 Distribution idea: values + frequencies
# (Round to 0.1 so repeated values appear)
# =========================================================
x_round <- round(x, 1)
freq_tbl <- table(x_round)

barplot(freq_tbl,
        las = 2,
        cex.names = 0.7,
        xlab = "Value (rounded to 0.1)",
        ylab = "Frequency",
        main = "Distribution idea: values and frequencies")

# =========================================================
# 2.2 Dotplot
# =========================================================
stripchart(x,
           method = "stack",
           pch = 16,
           cex = 0.8,
           xlab = "Eruption time (minutes)",
           main = "Dotplot")

# =========================================================
# 2.3 Histogram (frequency)
# =========================================================
hist(x,
     breaks = "Sturges",
     freq = TRUE,
     xlab = "Eruption time (minutes)",
     ylab = "Frequency",
     main = "Histogram (frequency)")

# =========================================================
# 2.4 Histogram (relative frequency)
# Note: In base R, freq = FALSE uses a density scale,
# which corresponds to relative frequency on the y-axis.
# =========================================================
hist(x,
     breaks = "Sturges",
     freq = FALSE,
     xlab = "Eruption time (minutes)",
     ylab = "Relative frequency (density scale)",
     main = "Histogram (relative frequency)")

# =========================================================
# 2.5 Density plot
# =========================================================
d <- density(x)
plot(d,
     lwd = 2,
     xlab = "Eruption time (minutes)",
     ylab = "Density",
     main = "Density plot")
rug(x)

# =========================================================
# 2.6 Same dataset: dotplot, histogram, density side by side
# =========================================================
op <- par(mfrow = c(1, 3), mar = c(5, 4, 4, 1))

stripchart(x,
           method = "stack",
           pch = 16,
           cex = 0.7,
           xlab = "Eruption time (minutes)",
           main = "Dotplot")

hist(x,
     breaks = "Sturges",
     freq = TRUE,
     xlab = "Eruption time (minutes)",
     ylab = "Frequency",
     main = "Histogram")

plot(density(x),
     lwd = 2,
     xlab = "Eruption time (minutes)",
     ylab = "Density",
     main = "Density plot")
rug(x)

par(op)  # reset

# =========================================================
# 2.7 Two histograms with different bin widths
# This shows why bin choice matters.
# =========================================================
breaks_small <- seq(min(x), max(x), by = 0.10)  # narrow bins
breaks_large <- seq(min(x), max(x), by = 0.50)  # wide bins

op <- par(mfrow = c(1, 2), mar = c(5, 4, 4, 1))

hist(x,
     breaks = breaks_small,
     freq = TRUE,
     xlab = "Eruption time (minutes)",
     ylab = "Frequency",
     main = "Smaller bin width")

hist(x,
     breaks = breaks_large,
     freq = TRUE,
     xlab = "Eruption time (minutes)",
     ylab = "Frequency",
     main = "Larger bin width")

par(op)  # reset
