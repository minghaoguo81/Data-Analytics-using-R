# Assuming you have the Caravan dataset loaded
# If not, you can load it using data("Caravan")

# Set seed for reproducibility
set.seed(123)

# Create a training set with the first 1,000 observations
train_set <- Caravan[1:1000, ]

# Create a test set with the remaining observations
test_set <- Caravan[-(1:1000), ]

# Print the dimensions of the training and test sets
cat("Training set dimensions:", dim(train_set), "\n")
cat("Test set dimensions:", dim(test_set), "\n")

# Assuming you have the Caravan dataset loaded and cleaned
# If not, you can load it using data("Caravan")

# Load the required library
library(gbm)

# Set seed for reproducibility
set.seed(123)

# Create a training set with the first 1,000 observations
train_set <- Caravan[1:1000, ]

# Fit a boosting model
boost_model <- gbm(Purchase ~ ., data = train_set, distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)

# Print a summary of the boosting model
summary(boost_model)

# Plot variable importance
plot(boost_model, i = "relative")

# Identify the most important predictors
top_predictors <- rownames(summary(boost_model)$importance)[1:10]  # Adjust the number based on your preference
cat("Top 10 predictors:", top_predictors, "\n")

#install.packages("gbm")

# Assuming you have the Caravan dataset loaded and cleaned
# If not, you can load it using data("Caravan")

# Load the required libraries
library(gbm)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create a training set with the first 1,000 observations
train_set <- Caravan[1:1000, ]

# Create a test set with the remaining observations
test_set <- Caravan[-(1:1000), ]

# Fit a boosting model
boost_model <- gbm(Purchase ~ ., data = train_set, distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)

# Predict the response on the test data
probabilities <- predict(boost_model, newdata = test_set, type = "response")

# Create binary predictions based on a threshold (e.g., 20%)
predictions <- ifelse(probabilities > 0.2, 1, 0)

# Form a confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_set$Purchase)

# Print the confusion matrix
confusion_matrix

# Calculate the fraction of true positives (sensitivity)
sensitivity <- confusion_matrix$byClass["Sensitivity"]
cat("Fraction of people predicted to make a purchase who do make one:", sensitivity, "\n")

# Assuming you have the Caravan dataset loaded and cleaned
# If not, you can load it using data("Caravan")

# Load the required library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create a training set with the first 1,000 observations
train_set <- Caravan[1:1000, ]

# Create a test set with the remaining observations
test_set <- Caravan[-(1:1000), ]

# Fit a logistic regression model
logistic_model <- glm(Purchase ~ ., data = train_set, family = "binomial")

# Predict the response on the test data
logistic_probabilities <- predict(logistic_model, newdata = test_set, type = "response")

# Create binary predictions based on a threshold (e.g., 20%)
logistic_predictions <- ifelse(logistic_probabilities > 0.2, 1, 0)

# Form a confusion matrix
logistic_confusion_matrix <- confusionMatrix(logistic_predictions, test_set$Purchase)

# Print the confusion matrix
logistic_confusion_matrix

# Calculate the fraction of true positives (sensitivity)
logistic_sensitivity <-

logistic_confusion_matrix$byClass["Sensitivity"]
cat("Fraction of people predicted to make a purchase who do make one (Logistic Regression):", logistic_sensitivity, "\n")


