library(ggplot2)
library(cowplot)
install.packages("car")
library(car)





data = read.csv("C:/Users/User/Downloads/statistics/boxOffice.csv")
head(data)
str(data)
summary(data)
# Find unique values in country column
unique_values <- unique(data$Country) 
unique_values


# Encode as factors and then convert to numeric 
encoded_Country <- as.numeric(factor(data$Country, levels = unique(data$Country)))
encoded_Country

# Perform logistic regression
logistic_model <- glm(Oscar ~ Budget, data = data, family = binomial)

# Summary of the model
summary(logistic_model)

# Perform logistic regression
logistic_model2 <- glm(Oscar ~ ., data = data, family = binomial)

# Summary of the model
summary(logistic_model2)


# Load necessary libraries
library(ggplot2)

# Ensure 'Oscar' is a binary factor
data$Oscar <- as.factor(data$Oscar)

# Predict probabilities
predicted.data <- data.frame(
  probability.of.oscar = logistic_model$fitted.values,
  Budget = data$Budget,
  Oscar = data$Oscar
)

# Plot the data
ggplot(data = predicted.data, aes(x = Budget, y = probability.of.oscar)) +
  geom_point(aes(color = as.factor(Oscar)), size = 5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  xlab("Budget") +
  ylab("Predicted Probability of Winning an Oscar") +
  labs(title = "Logistic Regression: Probability of Winning an Oscar vs. Budget",
       color = "Oscar Won") +
  theme_minimal()







