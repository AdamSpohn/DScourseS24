library(mice)
library(modelsummary)
library(purrr)
library(broom)
library(tidyverse)

wages <- read_csv("~/Downloads/wages.csv")

wages = wages[!is.na(wages$hgc) & !is.na(wages$tenure), ]

# Converting into college and married into dummy variables
wages$college <- ifelse(wages$college == "college grad", 1, 0)

wages$married <- ifelse(wages$married == "married", 1, 0)

summary_table <- summary(wages)
summary_table


# Calculating missing rate
missing_rate_logwage <- sum(is.na(wages$logwage)) / nrow(wages)
print(paste("Missing rate for log wages:", missing_rate_logwage * 100))


# Testing for systematic relationship of missing value and other variables
wages$missing_logwage <- as.numeric(is.na(wages$logwage))

# Calculate correlation between the dummy variable and other variables
correlation <- cor(wages[-which(names(wages) %in% c("logwage", "missing_logwage"))], wages$missing_logwage)

# Print the correlation
print(correlation)

# t-test
wages$missing_logwage <- wages$missing_logwage == 1

# Remove logwage and missing_logwage columns temporarily
wages_no_logwage <- wages %>% select(-logwage, -missing_logwage)

# Run t-tests 
test_results <- wages_no_logwage %>%
  map_df(~ tidy(t.test(. ~ missing_logwage, data = wages)), .id = "variable")

# View p-values
test_results %>%
  select(variable, p.value) %>%
  print()

# calculating list deletion 
listwiseDeletion <- wages[!is.na(wages$logwage), ]
listwiseDeletionModel = lm(logwage ~ hgc + college + tenure + (tenure^2) + age
                           + married, data = listwiseDeletion)

modelsummary(listwiseDeletionModel, stars = TRUE)
tidy(listwiseDeletionModel)


# Calculating mean imputation
# New data frame
meanImputation <- wages


logwageMean = mean(wages$logwage, na.rm = TRUE)

# Replace missing values in 'logwage' with the calculated mean
meanImputation$logwage[is.na(meanImputation$logwage)] <- logwageMean

meanImputationnModel = lm(logwage ~ hgc + college + tenure + (tenure^2) + age
                           + married, data = meanImputation)
modelsummary(meanImputationnModel, stars = TRUE)

# calculating using predicted values
coefficients = tidy(listwiseDeletionModel)

wages <- subset(wages, select = -missing_logwage)

coef_vector <- as.vector(coefficients[-1, 2])

coef_vector <- unlist(coef_vector)
coef_vector <- as.numeric(coef_vector)

calculate_logwage <- function(row, coef_vector) {
  logwage_pred <- coefficients[1, 2]  # Intercept value
  
  # Multiply each value in the row by the corresponding coefficient and sum
  logwage_pred <- logwage_pred + sum(row * coef_vector)
  
  return(logwage_pred)
}

predicted_values <- wages

# Replace missing values in logwage column
for (i in 1:nrow(wages)) {
  if (is.na(wages$logwage[i])) {
    predicted_values$logwage[i] <- calculate_logwage(wages[i, -1], coef_vector)
  }
}

predicted_values$logwage <- unlist(predicted_values$logwage)

# Check the class of 'logwage' after conversion
class(predicted_values$logwage)

predictedvaluesModel = lm(logwage ~ hgc + college + tenure + (tenure^2) + age
                           + married, data = predicted_values)

modelsummary(predictedvaluesModel, stars = TRUE)

# Using mice for imputation


vars_to_impute <- c("logwage", "hgc", "college", "tenure", "age", "married")

# Create the imputation model
imputation_model <- mice(wages[, vars_to_impute], m = 5, method = "pmm")

# Impute missing values
mice <- complete(imputation_model)

miceModel = lm(logwage ~ hgc + college + tenure + (tenure^2) + age
               + married, data = mice)

modelsummary(miceModel, stars = TRUE)

modelsummary(list(listwiseDeletionModel, meanImputationnModel,
                  predictedvaluesModel, miceModel), stars = TRUE)


