# Load the data
data <- read.csv("Boston.csv")

# Assume 'response' is your binary response variable
# For each predictor, fit a simple logistic regression model
predictors <- colnames(data)[1:13]  # Exclude the response variable
significant_predictors <- c()

for (predictor in predictors) {
  model <- glm(response ~ data[[predictor]], family = binomial)
  
  # Check the p-value
  p_value <- summary(model)$coefficients[2, 4]  # Index 2 corresponds to the predictor variable
  
  if (p_value < 0.05) {
    significant_predictors <- c(significant_predictors, predictor)
  }
  
  cat(paste(predictor, ": p-value =", p_value, "\n"))
}

# Print predictors with significant association
cat("Predictors with significant association:", paste(significant_predictors, collapse = ", "), "\n")

# Load the data
data <- read.csv("Boston.csv")

# Assume 'response' is your binary response variable
model <- glm(response ~ ., data = data, family = binomial)

# Display the summary of the model
summary(model)

# Load the data
data <- read.csv("Boston.csv")

# Assuming your response variable is 'original_response'
original_response <- data$original_response

# Convert to binary variable based on median
median_response <- median(original_response)
data$response <- ifelse(original_response > median_response, 1, 0)

# Fit a multiple logistic regression model
model <- glm(response ~ ., data = data, family = binomial)

# Display the summary of the model
summary(model)

# Load the data
data <- read.csv("Boston.csv")

# Assume 'response' is your original response variable
original_response <- data$medv

# Convert to binary variable based on median
median_response <- median(original_response)
data$response <- ifelse(original_response > median_response, 1, 0)

# Fit a multiple logistic regression model using predictors from column 2 to 14
model <- glm(response ~ ., data = data[, 2:14], family = binomial)

# Display the summary of the model
summary(model)

# New data for prediction
new_data <- data.frame(
  zn = 18,
  indus = 2.97,
  chas = 0,
  nox = 0.4,
  rm = 6.575,
  age = 63,
  dis = 4.8,
  rad = 3,
  tax = 238,
  ptratio = 15.3,
  black = 376.7,
  lstat = 8.23,
  medv = 45
)

# Predict the probability of crime rate being above the median
predicted_prob <- predict(model, newdata = new_data, type = "response")

# Convert probabilities to binary predictions
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# Print the predicted probability and class
cat("Predicted Probability:", predicted_prob, "\n")
cat("Predicted Class (1: Above Median, 0: Below Median):", predicted_class, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class <- data$response

# Confusion Matrix
conf_matrix <- table(Actual = actual_class, Predicted = predicted_class)
print("Confusion Matrix:")
print(conf_matrix)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Balanced Accuracy
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
balanced_accuracy <- (sensitivity + specificity) / 2
cat("Balanced Accuracy:", balanced_accuracy, "\n")

# False Positives (FP)
fp <- conf_matrix[1, 2]
cat("False Positives (FP):", fp, "\n")

# False Positive Rate (FPR)
fpr <- fp / sum(conf_matrix[1, ])
cat("False Positive Rate (FPR):", fpr, "\n")

# Load the data
data <- read.csv("Boston.csv")

# Assume 'response' is your original response variable
original_response <- data$medv

# Convert to binary variable based on median
median_response <- median(original_response)
data$response <- ifelse(original_response > median_response, 1, 0)

# Fit a multiple logistic regression model using predictors from column 2 to 14
model <- glm(response ~ ., data = data[, 2:14], family = binomial)

# Predict the probability of crime rate being above the median for all data
predicted_prob_all <- predict(model, type = "response")

# Convert probabilities to binary predictions using threshold of 0.5
predicted_class_all <- ifelse(predicted_prob_all > 0.5, 1, 0)

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Confusion Matrix
conf_matrix_all <- table(Actual = actual_class_all, Predicted = predicted_class_all)
print("Confusion Matrix:")
print(conf_matrix_all)

# Accuracy
accuracy_all <- sum(diag(conf_matrix_all)) / sum(conf_matrix_all)
cat("Accuracy:", accuracy_all, "\n")

# Balanced Accuracy
sensitivity_all <- conf_matrix_all[2, 2] / sum(conf_matrix_all[2, ])
specificity_all <- conf_matrix_all[1, 1] / sum(conf_matrix_all[1, ])
balanced_accuracy_all <- (sensitivity_all + specificity_all) / 2
cat("Balanced Accuracy:", balanced_accuracy_all, "\n")

# False Positives (FP)
fp_all <- conf_matrix_all[1, 2]
cat("False Positives (FP):", fp_all, "\n")

# False Positive Rate (FPR)
fpr_all <- fp_all / sum(conf_matrix_all[1, ])
cat("False Positive Rate (FPR):", fpr_all, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Load the ROCR package
library(ROCR)

# Create a prediction object
prediction <- prediction(predicted_prob_all, actual_class_all)

# Create an ROC curve
roc <- performance(prediction, "tpr", "fpr")
plot(roc, col = "blue", main = "ROC Curve", lwd = 2)

# Calculate AUC
auc <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Create a data frame with predicted probabilities and actual classes
result_df <- data.frame(predicted_prob = predicted_prob_all, actual_class = actual_class_all)

# Create a sequence of thresholds
thresholds <- seq(0, 1, by = 0.01)

# Initialize variables to store results
max_sum_rate <- 0
best_threshold <- 0

# Loop through thresholds and calculate TP rate + (1 - FP rate)
for (threshold in thresholds) {
  predicted_class <- ifelse(predicted_prob_all > threshold, 1, 0)
  
  # Confusion Matrix
  conf_matrix <- table(Actual = actual_class_all, Predicted = predicted_class)
  
  # Calculate TP rate and FP rate
  tp_rate <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  fp_rate <- conf_matrix[1, 2] / sum(conf_matrix[1, ])
  
  # Calculate the sum of TP rate and (1 - FP rate)
  sum_rate <- tp_rate + (1 - fp_rate)
  
  # Check if the current sum_rate is higher than the maximum
  if (sum_rate > max_sum_rate) {
    max_sum_rate <- sum_rate
    best_threshold <- threshold
  }
}

cat("Best Threshold:", best_threshold, "\n")

# Apply the best threshold to get the predicted class
predicted_class_best_threshold <- ifelse(predicted_prob_all > best_threshold, 1, 0)

# Confusion Matrix
conf_matrix_best_threshold <- table(Actual = actual_class_all, Predicted = predicted_class_best_threshold)
print("Confusion Matrix:")
print(conf_matrix_best_threshold)

# Accuracy
accuracy_best_threshold <- sum(diag(conf_matrix_best_threshold)) / sum(conf_matrix_best_threshold)
cat("Accuracy:", accuracy_best_threshold, "\n")

# Balanced Accuracy
sensitivity_best_threshold <- conf_matrix_best_threshold[2, 2] / sum(conf_matrix_best_threshold[2, ])
specificity_best_threshold <- conf_matrix_best_threshold[1, 1] / sum(conf_matrix_best_threshold[1, ])
balanced_accuracy_best_threshold <- (sensitivity_best_threshold + specificity_best_threshold) / 2
cat("Balanced Accuracy:", balanced_accuracy_best_threshold, "\n")

# False Positives (FP)
fp_best_threshold <- conf_matrix_best_threshold[1, 2]
cat("False Positives (FP):", fp_best_threshold, "\n")

# False Positive Rate (FPR)
fpr_best_threshold <- fp_best_threshold / sum(conf_matrix_best_threshold[1, ])
cat("False Positive Rate (FPR):", fpr_best_threshold, "\n")

# Calculate distances for each point on the ROC curve
distances <- sqrt((1 - roc@tp[[1]] / sum(roc@table)) ^ 2 + (roc@fp[[1]] / sum(roc@table)) ^ 2)

# Find the threshold corresponding to the minimum distance
min_distance_index <- which.min(distances)
best_threshold <- roc@alpha.values[[1]][min_distance_index]

cat("Best Threshold:", best_threshold, "\n")

# Apply the best threshold to get the predicted class
predicted_class_best_threshold <- ifelse(predicted_prob_all > best_threshold, 1, 0)

# Confusion Matrix
conf_matrix_best_threshold <- table(Actual = actual_class_all, Predicted = predicted_class_best_threshold)
print("Confusion Matrix:")
print(conf_matrix_best_threshold)

# Accuracy
accuracy_best_threshold <- sum(diag(conf_matrix_best_threshold)) / sum(conf_matrix_best_threshold)
cat("Accuracy:", accuracy_best_threshold, "\n")

# Balanced Accuracy
sensitivity_best_threshold <- conf_matrix_best_threshold[2, 2] / sum(conf_matrix_best_threshold[2, ])
specificity_best_threshold <- conf_matrix_best_threshold[1, 1] / sum(conf_matrix_best_threshold[1, ])
balanced_accuracy_best_threshold <- (sensitivity_best_threshold + specificity_best_threshold) / 2
cat("Balanced Accuracy:", balanced_accuracy_best_threshold, "\n")

# False Positives (FP)
fp_best_threshold <- conf_matrix_best_threshold[1, 2]
cat("False Positives (FP):", fp_best_threshold, "\n")

# False Positive Rate (FPR)
fpr_best_threshold <- fp_best_threshold / sum(conf_matrix_best_threshold[1, ])
cat("False Positive Rate (FPR):", fpr_best_threshold, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Create a prediction object
prediction <- prediction(predicted_prob_all, actual_class_all)

# Create an ROC curve
roc <- performance(prediction, "tpr", "fpr")

# Calculate distances for each point on the ROC curve
distances <- sqrt((1 - roc@y.values[[1]])^2 + roc@x.values[[1]]^2)

# Find the threshold corresponding to the minimum distance
min_distance_index <- which.min(distances)
best_threshold <- roc@alpha.values[[1]][min_distance_index]

cat("Best Threshold:", best_threshold, "\n")

# Apply the best threshold to get the predicted class
predicted_class_best_threshold <- ifelse(predicted_prob_all > best_threshold, 1, 0)

# Confusion Matrix
conf_matrix_best_threshold <- table(Actual = actual_class_all, Predicted = predicted_class_best_threshold)
print("Confusion Matrix:")
print(conf_matrix_best_threshold)

# Accuracy
accuracy_best_threshold <- sum(diag(conf_matrix_best_threshold)) / sum(conf_matrix_best_threshold)
cat("Accuracy:", accuracy_best_threshold, "\n")

# Balanced Accuracy
sensitivity_best_threshold <- conf_matrix_best_threshold[2, 2] / sum(conf_matrix_best_threshold[2, ])
specificity_best_threshold <- conf_matrix_best_threshold[1, 1] / sum(conf_matrix_best_threshold[1, ])
balanced_accuracy_best_threshold <- (sensitivity_best_threshold + specificity_best_threshold) / 2
cat("Balanced Accuracy:", balanced_accuracy_best_threshold, "\n")

# False Positives (FP)
fp_best_threshold <- conf_matrix_best_threshold[1, 2]
cat("False Positives (FP):", fp_best_threshold, "\n")

# False Positive Rate (FPR)
fpr_best_threshold <- fp_best_threshold / sum(conf_matrix_best_threshold[1, ])
cat("False Positive Rate (FPR):", fpr_best_threshold, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Fit a multiple logistic regression model using predictors from column 2 to 14
model <- glm(response ~ ., data = data[, 2:14], family = binomial)

# Extract coefficients and variable names
coefficients <- coef(model)
variable_names <- names(coefficients)

# Sort variables by the absolute magnitude of their coefficients
sorted_variables <- variable_names[order(abs(coefficients), decreasing = TRUE)]

# Print the first and second most important variables
cat("Most important variable:", sorted_variables[1], "\n")
cat("Second most important variable:", sorted_variables[2], "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Extract the first and second most important variables
most_important_var1 <- sorted_variables[1]
most_important_var2 <- sorted_variables[2]

# Fit a logistic regression model using only the two most important variables
model_most_important <- glm(response ~ get(most_important_var1) + get(most_important_var2), data = data, family = binomial)

# Print the summary of the model
summary(model_most_important)

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Extract the first and second most important variables
most_important_var1 <- sorted_variables[1]
most_important_var2 <- sorted_variables[2]

# Fit a logistic regression model using only the two most important variables
model_most_important <- glm(response ~ get(most_important_var1) + get(most_important_var2), data = data, family = binomial)

# Print the summary of the model
summary(model_most_important)

# Extract coefficients
coefficients <- coef(model_most_important)

# Print the logistic regression equation
cat("Logistic Regression Equation:\n")
cat("log(p / (1 - p)) =", coefficients[1], "+", coefficients[2], "*", most_important_var1, "+", coefficients[3], "*", most_important_var2, "\n")

# If you want the equation in terms of p, you can use the inverse logit transformation
cat("p =", "1 / (1 + exp(-(", coefficients[1], "+", coefficients[2], "*", most_important_var1, "+", coefficients[3], "*", most_important_var2, ")))\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Fit a logistic regression model
model <- glm(response ~ ., data = data[, 2:14], family = binomial)

# Extract coefficients and variable names
coefficients <- coef(model)
variable_names <- names(coefficients)

# Remove the intercept from consideration
variable_names <- variable_names[-1]

# Sort variables by the absolute magnitude of their coefficients
sorted_variables <- variable_names[order(abs(coefficients[-1]), decreasing = TRUE)]

# Print the first and second most important variables
cat("Most important variable:", sorted_variables[1], "\n")
cat("Second most important variable:", sorted_variables[2], "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Extract the first and second most important variables
most_important_var1 <- sorted_variables[1]
most_important_var2 <- sorted_variables[2]

# Fit a logistic regression model using only the two most important variables
model_most_important <- glm(response ~ get(most_important_var1) + get(most_important_var2), data = data, family = binomial)

# Print the summary of the model
summary(model_most_important)

# Extract coefficients
coefficients <- coef(model_most_important)

# Print the logistic regression equation
cat("Logistic Regression Equation:\n")
cat("log(p / (1 - p)) =", coefficients[1], "+", coefficients[2], "*", most_important_var1, "+", coefficients[3], "*", most_important_var2, "\n")

# If you want the equation in terms of p, you can use the inverse logit transformation
cat("p =", "1 / (1 + exp(-(", coefficients[1], "+", coefficients[2], "*", most_important_var1, "+", coefficients[3], "*", most_important_var2, ")))\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Extract the first and second most important variables
most_important_var1 <- sorted_variables[1]
most_important_var2 <- sorted_variables[2]

# Fit a logistic regression model using only the two most important variables
model_most_important <- glm(response ~ get(most_important_var1) + get(most_important_var2), data = data, family = binomial)

# Predict probabilities for the ROC curve
predicted_prob_most_important <- predict(model_most_important, type = "response")

# Load the pROC package
library(pROC)

# Create an ROC curve
roc_most_important <- roc(actual_class_all, predicted_prob_most_important)

# Plot the ROC curve
plot(roc_most_important, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate and print the AUC
auc_most_important <- auc(roc_most_important)
cat("AUC:", auc_most_important, "\n")

# Assuming 'response' is the actual binary response variable in your dataset
actual_class_all <- data$response

# Extract the first and second most important variables
most_important_var1 <- sorted_variables[1]
most_important_var2 <- sorted_variables[2]

# Fit a logistic regression model using only the two most important variables
model_most_important <- glm(response ~ get(most_important_var1) + get(most_important_var2), data = data, family = binomial)

# Predict probabilities for different thresholds
predicted_prob_most_important <- predict(model_most_important, type = "response")

# Create a sequence of thresholds
thresholds <- seq(0, 1, by = 0.01)

# Initialize variables to store results
max_sum_rate <- 0
best_threshold <- 0

# Loop through thresholds and calculate TP rate + (1 - FP rate)
for (threshold in thresholds) {
  predicted_class <- ifelse(predicted_prob_most_important > threshold, 1, 0)
  
  # Confusion Matrix
  conf_matrix <- table(Actual = actual_class_all, Predicted = predicted_class)
  
  # Calculate TP rate and FP rate
  tp_rate <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  fp_rate <- conf_matrix[1, 2] / sum(conf_matrix[1, ])
  
  # Calculate the sum of TP rate and (1 - FP rate)
  sum_rate <- tp_rate + (1 - fp_rate)
  
  # Check if the current sum_rate is higher than the maximum
  if (sum_rate > max_sum_rate) {
    max_sum_rate <- sum_rate
    best_threshold <- threshold
  }
}

cat("Best Threshold:", best_threshold, "\n")

# Apply the best threshold to get the predicted class
predicted_class_best_threshold <- ifelse(predicted_prob_most_important > best_threshold, 1, 0)

# Confusion Matrix
conf_matrix_best_threshold <- table(Actual = actual_class_all, Predicted = predicted_class_best_threshold)
print("Confusion Matrix:")
print(conf_matrix_best_threshold)

# Accuracy
accuracy_best_threshold <- sum(diag(conf_matrix_best_threshold)) / sum(conf_matrix_best_threshold)
cat("Accuracy:", accuracy_best_threshold, "\n")

# Balanced Accuracy
sensitivity_best_threshold <- conf_matrix_best_threshold[2, 2] / sum(conf_matrix_best_threshold[2, ])
specificity_best_threshold <- conf_matrix_best_threshold[1, 1] / sum(conf_matrix_best_threshold[1, ])
balanced_accuracy_best_threshold <- (sensitivity_best_threshold + specificity_best_threshold) / 2
cat("Balanced Accuracy:", balanced_accuracy_best_threshold, "\n")

# False Positives (FP)
fp_best_threshold <- conf_matrix_best_threshold[1, 2]
cat("False Positives (FP):", fp_best_threshold, "\n")

# False Positive Rate (FPR)
fpr_best_threshold <- fp_best_threshold / sum(conf_matrix_best_threshold[1, ])
cat("False Positive Rate (FPR):", fpr_best_threshold, "\n")


