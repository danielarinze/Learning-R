# Input data
clock_speeds <- c(2.5, 1.7, 2.9, 2.7, 1.1, 1.9, 1.3, 1.8, 2.8, 2.3,
                  2.4, 1.7, 1.9, 2.2, 2.1, 1.9, 2.7, 3.3, 2.9, 2.2)

# Population mean
population_mean <- 2.2

# Perform one-sample t-test
t_test_result <- t.test(clock_speeds, mu = population_mean)

# Print the results
print(t_test_result)

# Define parameters
population_mean <- 2.2             
sample_mean <- mean(clock_speeds)
sample_sd <- sd(clock_speeds)
n <- length(clock_speeds)
t_value <- (sample_mean - population_mean) / (sample_sd / sqrt(n))
alpha <- 0.05
df <- n - 1

# Generate data for the t-distribution
x <- seq(-4, 4, length.out = 1000) # t-values
y <- dt(x, df) # density of t-distribution

# Critical t-values
t_critical <- qt(c(alpha / 2, 1 - alpha / 2), df)

# Plot the t-distribution
plot(x, y, type = "l", lwd = 2, col = "blue", 
     main = "One-Sample t-Test Visualization", 
     xlab = "t-Statistic", ylab = "Density")

# Shade the rejection regions
polygon(c(x[x <= t_critical[1]], t_critical[1]), 
        c(y[x <= t_critical[1]], 0), col = "red", border = NA)
polygon(c(x[x >= t_critical[2]], t_critical[2]), 
        c(y[x >= t_critical[2]], 0), col = "red", border = NA)

# Add vertical line for sample t-value
abline(v = t_value, col = "green", lwd = 2, lty = 2)

# Add annotations
legend("topright", legend = c("t-Distribution", "Rejection Region", "Observed t-Value"),
       col = c("blue", "red", "green"), lwd = c(2, 2, 2), lty = c(1, 1, 2))
text(t_value, 0.1, labels = paste("t =", round(t_value, 2)), col = "green", pos = 4)
