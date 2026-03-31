# Set up
options(scipen = 999) #Don't display scientific notation, like 1e+02
library(tidyverse)
library(tidycensus)
library(broom) #Cleans a regression model output into a tibble
library(scales) 

# Fetch PA county data directly from Census API
challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

# Test home value as X
model1 <- lm(home_valueE ~ median_incomeE, data = challenge_data)
summary(model1)

# Visualize the relationship
ggplot(challenge_data, aes(x = median_incomeE, y = home_valueE)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Median Income vs Median Home Value in PA Counties",
    x = "Median Income", 
    y = "Median Home Value"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  theme_minimal()   

# Test median age as X
model2 <- lm(home_valueE ~ median_incomeE + median_ageE, 
             data = challenge_data)
summary(model2)

# Visualize the relationship
ggplot(challenge_data, aes(x = median_incomeE, y = home_valueE)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "steelblue"
  ) +
  labs(
    title = "Median Income vs Home Value (Controlling for Median Age)",
    x = "Median Income",
    y = "Median Home Value"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# 10-fold cross-validation
library(caret)

train_control <- trainControl(method = "cv", number = 10)

cv_model <- train(home_valueE ~ median_incomeE + median_ageE,
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model$results

# Check residual plot
challenge_data$residuals <- residuals(model2)
challenge_data$fitted <- fitted(model2)

ggplot(challenge_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Breusche-Pagan
library(lmtest)
bptest(model2)




