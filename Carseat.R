library(tree)  # for regression trees
library(ISLR)  # for the Carseats dataset

data("Carseats")
head(Carseats)

set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(Carseats), 0.75 * nrow(Carseats))
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]

tree_model <- tree(Sales ~ ., data = train_data)

plot(tree_model)
text(tree_model, pretty = 0)

predictions <- predict(tree_model, newdata = test_data)
mse <- mean((test_data$Sales - predictions)^2)

#install.packages("tree")
#install.packages("ISLR")

# Load required libraries
library(tree)
library(ISLR)

# Load the Carseats dataset
data("Carseats")

# Set seed for reproducibility
set.seed(123)

# Split the data into training and test sets
train_index <- sample(1:nrow(Carseats), 0.75 * nrow(Carseats))
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]

# Fit a regression tree to the training set
tree_model <- tree(Sales ~ ., data = train_data)

# Use cross-validation to determine optimal tree complexity
cv_tree_model <- cv.tree(tree_model)

# Plot cross-validated errors
plot(cv_tree_model$size, cv_tree_model$dev, type = "b", xlab = "Tree Size", ylab = "CV Deviance")

# Identify the optimal tree size
optimal_size <- which.min(cv_tree_model$dev)
abline(v = cv_tree_model$size[optimal_size], col = "red", lty = 2)

# Prune the tree to the optimal size
pruned_tree_model <- prune.tree(tree_model, best = optimal_size)

# Plot the pruned tree
plot(pruned_tree_model)
text(pruned_tree_model, pretty = 0)

# Make predictions on the test set with the pruned tree
pruned_predictions <- predict(pruned_tree_model, newdata = test_data)

# Calculate test MSE for the pruned tree
pruned_mse <- mean((test_data$Sales - pruned_predictions)^2)

# Print the test MSE for both the original and pruned trees
cat("Original Tree Test MSE:", cv_tree_model$dev[optimal_size], "\n")
cat("Pruned Tree Test MSE:", pruned_mse, "\n")

# Load required library
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Load the Carseats dataset
data("Carseats")

# Split the data into training and test sets
train_index <- sample(1:nrow(Carseats), 0.75 * nrow(Carseats))
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]

# Fit a random forest model
rf_model <- randomForest(Sales ~ ., data = train_data, ntree = 500)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate test MSE
rf_mse <- mean((test_data$Sales - rf_predictions)^2)

# Print the test MSE
cat("Random Forest Test MSE:", rf_mse, "\n")

# Plot variable importance
varImpPlot(rf_model)

#install.packages("randomForest")

# Load required library
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Load the Carseats dataset
data("Carseats")

# Split the data into training and test sets
train_index <- sample(1:nrow(Carseats), 0.75 * nrow(Carseats))
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]

# Fit a random forest model with different values of mtry
mtry_values <- c(2, 4, 6, 8)  # Adjust as needed
mse_values <- numeric(length(mtry_values))

for (i in seq_along(mtry_values)) {
  rf_model <- randomForest(Sales ~ ., data = train_data, ntree = 500, mtry = mtry_values[i])
  rf_predictions <- predict(rf_model, newdata = test_data)
  mse_values[i] <- mean((test_data$Sales - rf_predictions)^2)
}

# Print test MSE for different values of mtry
cat("Test MSE for Different mtry Values:", mse_values, "\n")

# Find the optimal mtry value
optimal_mtry <- mtry_values[which.min(mse_values)]
cat("Optimal mtry Value:", optimal_mtry, "\n")

# Plot variable importance
rf_model <- randomForest(Sales ~ ., data = train_data, ntree = 500, mtry = optimal_mtry)
varImpPlot(rf_model)

# Load required library
library(randomForest)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Load the Carseats dataset
data("Carseats")

# Split the data into training and test sets
train_index <- sample(1:nrow(Carseats), 0.75 * nrow(Carseats))
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]

# Fit a random forest model with different values of mtry
mtry_values <- c(2, 4, 6, 8)  # Adjust as needed
mse_values <- numeric(length(mtry_values))

for (i in seq_along(mtry_values)) {
  rf_model <- randomForest(Sales ~ ., data = train_data, ntree = 500, mtry = mtry_values[i])
  rf_predictions <- predict(rf_model, newdata = test_data)
  mse_values[i] <- mean((test_data$Sales - rf_predictions)^2)
}

# Create a plot of mtry values vs. MSE
plot_data <- data.frame(mtry = mtry_values, mse = mse_values)
ggplot(plot_data, aes(x = mtry, y = mse)) +
  geom_point() +
  geom_line() +
  labs(title = "Effect of mtry on Test MSE",
       x = "mtry (Number of Variables Considered at Each Split)",
       y = "Test MSE")

#install.packages("ggplot2")

# Assuming you have the Caravan dataset loaded
# If not, you can load it using data("Caravan")
