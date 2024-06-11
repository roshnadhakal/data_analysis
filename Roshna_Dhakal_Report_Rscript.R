
#load dataset
train_data <- read.csv("concrete_strength_train.csv")
test_data <- read.csv("concrete_strength_test.csv")

# Summary statistics
str(train_data)
summary(train_data)
str(test_data)
summary(test_data)
missing_data<-is.na(test_data)


#remove missing values
train_missing <- colMeans(is.na(train_data))*100
test_missing <- colMeans(is.na(test_data))*100
train_missing
test_missing 
imputed_train_data <- mice(data =train_data, m = 1, method = "mean", maxit = 10)
imputed_test_data <- mice(data = test_data, m = 1, method = "mean", maxit = 10)
train_data_imputed <- complete(imputed_train_data)
test_data_imputed <- complete(imputed_test_data)

imputed_test_data

#md pattern for datasets
md.pattern(train_data_imputed)
md.pattern(test_data_imputed)
md.pattern(train_data)
md.pattern(test_data)

# Descriptive statistics for Strength variable
mean_strength <- mean(train_data$Strength)
median_strength <- median(train_data$Strength)
sd_strength <- sd(train_data$Strength)
quantiles_strength <- quantile(train_data$Strength)

mean_strength
median_strength
sd_strength
quantiles_strength


# Create density plots for cement and concrete strength
density_plot <- ggplot(train_data, aes(x = Cement, fill = "Cement")) +
  geom_density(alpha = 0.5) +
  xlab("Cement") +
  ylab("Density") +
  ggtitle("Density Plot of Cement")

density_plot <- density_plot +
  geom_density(aes(x = Strength, fill = "Strength"), alpha = 0.5) +
  xlab("Concrete Strength") +
  ylab("Density") +
  ggtitle("Density Plot of Cement and Concrete Strength") +
  scale_fill_manual(values = c("Cement" = "blue", "Strength" = "red"))
print(density_plot)


# train_strength_plot for Strength histogram and Cement line

train_strength_plot <- ggplot(train_data, aes(x = Strength)) +
  geom_histogram(aes(fill = "Strength"), binwidth = 5, color = "black", alpha = 0.7) +  # Map fill aesthetic for Strength histogram
  geom_density(aes(y = ..scaled.., fill = "Strength"), alpha = 0.3) +  # Map fill aesthetic for Strength density
  geom_line(stat = "density", aes(x = Cement*0.1, y = ..scaled..*10, color = "green"), size = 1.5) +  # Map fill aesthetic for Cement line
  scale_fill_manual(values = c("Cement" = "red", "Strength" = "skyblue")) +  # Set manual fill colors
  labs(title = "Histogram and Density Plot of Strength with Cement (Train Dataset)", x = "Strength", y = "Density") +
  theme_minimal()
print(train_strength_plot)


# test_strength_plot for Strength histogram and Cement line
test_strength_plot <- ggplot(test_data, aes(x = Strength)) +
  geom_histogram(aes(fill = "Strength"), binwidth = 5, color = "black", alpha = 0.7) +  # Map fill aesthetic for Strength histogram
  geom_density(aes(y = ..scaled.., fill = "green"), alpha = 0.3) +  # Map fill aesthetic for Strength density
  geom_line(stat = "density", aes(x = Cement*0.1, y = ..scaled..*10, color = "Cement"), size = 1.5) +  # Map fill aesthetic for Cement line
  scale_fill_manual(values = c("Cement" = "pink", "Strength" = "skyblue")) +  # Set manual fill colors
  labs(title = "Histogram and Density Plot of Strength with Cement (Test Dataset)", x = "Strength", y = "Density") +
  theme_minimal()
test_strength_plot



#boxplot with outliers for each dataset

boxplot(train_data_imputed)
boxplot(test_data_imputed)


#remove outliers for train data imputed
for (col in names(train_data_imputed)){
  #calculate quantiles and IQR
  Q1 <- quantile(train_data_imputed[[col]], 0.25)
  Q3 <- quantile(train_data_imputed[[col]], 0.75)
  IQR <- IQR(train_data_imputed[[col]])
  lower_whisker <- Q1 - 1.5 * IQR
  upper_whisker <- Q3 + 1.5 * IQR
  
  #identify outliers
  outliers <- train_data_imputed[[col]] < lower_whisker | train_data_imputed[[col]] > upper_whisker
  
  #remove outliers from each column
  train_data_imputed <- train_data_imputed[!outliers,]
}

#remove outliers for test data imputed

for (col in names(test_data_imputed)){
  #calculate quantiles and IQR
  Q1_test <- quantile(test_data_imputed[[col]], 0.25)
  Q3_test <- quantile(test_data_imputed[[col]], 0.75)
  IQR_test <- IQR(test_data_imputed[[col]])
  lower_whisker <- Q1_test - 1.5 * IQR_test
  upper_whisker <- Q3_test + 1.5 * IQR_test
  
  #identify outliers
  outliers <- test_data_imputed[[col]] < lower_whisker | test_data_imputed[[col]] > upper_whisker
  
  #remove outliers from each column
  test_data_imputed <- test_data_imputed[!outliers,]
}

#Scaling
scaled_train_data <- scale(train_data_imputed)
boxplot(scaled_train_data)
scaled_test_data <- scale(test_data_imputed)
boxplot(scaled_test_data)

# Calculate correlation matrix for train data
install.packages("corrplot")
library(corrplot)
correlation_matrix_train <- cor(train_data_imputed)
# Visualize correlation matrix
corrplot(correlation_matrix_train, method = "color")
# Calculate correlation matrix for test data
correlation_matrix_test<- cor(test_data_imputed)
corrplot(correlation_matrix_test, method = "color")


# LINEAR REGRESSION
# Load necessary libraries
install.packages("caret")
library(caret)
# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(train_data_imputed$Strength, p = 0.7, list = FALSE)
train_set <- train_data_imputed[train_index, ]
test_set <- train_data_imputed[-train_index, ]
# Train the linear regression model
lm_model <- lm(Strength ~ ., data = train_set)
# Evaluate the model's performance
summary(lm_model)
# Make predictions
predictions <- predict(lm_model, newdata = test_set)
# Evaluate predictions
accuracy <- sqrt(mean((predictions - test_set$Strength)^2))
accuracy
plot(lm_model)
# Interpret the model
# Coefficients
coefficients(lm_model)



#DECISION TREE MODEL
# Load necessary libraries
install.packages("rpart")
library(caret)
library(rpart)
# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(train_data_imputed$Strength, p = 0.7, list = FALSE)
train_set <- train_data_imputed[train_index, ]
test_set <- train_data_imputed[-train_index, ]
# Train the Decision Tree Regression model
cart_model <- rpart(Strength ~ ., data = train_set)
# Evaluate the model's performance
summary(cart_model)
# Make predictions
predictions_tree <- predict(cart_model, newdata = test_set)
# Evaluate predictions
accuracy_tree <- sqrt(mean((predictions_tree - test_set$Strength)^2))
accuracy_tree
# Interpret the model
# Visualize the tree
plot(cart_model)
text(cart_model)



#RANDOM FOREST
# Load necessary libraries
install.packages("randomForest")
library(randomForest)
# Train the Random Forest Regression model
rf_model <- randomForest(Strength ~ ., data = train_data_imputed)
# Evaluate the model's performance
summary(rf_model)
# Make predictions
predictions_random <- predict(rf_model, newdata = test_data_imputed)
# Evaluate predictions
accuracy_random <- sqrt(mean((predictions_random - test_data_imputed$Strength)^2))
accuracy_random
# Interpret the model
# Feature importance
importance(rf_model)
plot(rf_model)


#KNN REGRESSION MODEL
library(caret)
# Train k-NN model
model <- train(
  Strength ~ ., 
  data = train_data_imputed, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Predictions for KNN
predictions_KNN <- predictions_KNN[1:length(test_set$Strength)]
# Evaluate predictions
accuracy_knn <- sqrt(mean((predictions_knn - test_data_imputed$Strength)^2))
# Plot model error RMSE vs different values of k
plot(model, main = "Model Error vs. k")
# Best tuning parameter k that minimizes the RMSE
model$bestTune
# Compute the prediction error RMSE
knn_rmse <- RMSE(predictions_KNN, test_data_imputed$Strength)
# Compute the prediction error R2
knn_r2 <- R2(predictions_KNN, test_data_imputed$Strength)
# Compute MAE (Mean Absolute Error)
knn_mae <- MAE(predictions_KNN, test_data_imputed$Strength)
knn_mae





# Set up a grid for plots
par(mfrow = c(4, 2))

# Linear Regression
# Plot residuals
plot(predictions, test_set$Strength - predictions, main = "Linear Regression Residuals", xlab = "Predicted Strength", ylab = "Residuals")
abline(h = 0, col = "red")
# Plot fitted vs original
plot(predictions, test_set$Strength, main = "Linear Regression: Fitted vs Original", xlab = "Predicted Strength", ylab = "Original Strength")
abline(0, 1)

# Decision Tree
# Plot residuals
plot(predictions_tree, test_set$Strength - predictions_tree, main = "Decision Tree Residuals", xlab = "Predicted Strength", ylab = "Residuals")
abline(h = 0, col = "red")
# Plot fitted vs original
plot(predictions_tree, test_set$Strength, main = "Decision Tree: Fitted vs Original", xlab = "Predicted Strength", ylab = "Original Strength")
abline(0, 1)

# Random Forest
# Plot residuals
plot(predictions_random, test_data_imputed$Strength - predictions_random, main = "Random Forest Residuals", xlab = "Predicted Strength", ylab = "Residuals")
abline(h = 0, col = "red")
# Plot fitted vs original
plot(predictions_random, test_data_imputed$Strength, main = "Random Forest: Fitted vs Original", xlab = "Predicted Strength", ylab = "Original Strength")
abline(0, 1)

# KNN Regression
# Plot residuals
plot(predictions_KNN, test_set$Strength - predictions_KNN, main = "KNN Regression Residuals", xlab = "Predicted Strength", ylab = "Residuals")
abline(h = 0, col = "red")
# Plot fitted vs original
plot(predictions_KNN, test_set$Strength, main = "KNN Regression: Fitted vs Original", xlab = "Predicted Strength", ylab = "Original Strength")
abline(0, 1)



# Create a data frame to store the model results
model_results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  R2 = c(summary(lm_model)$r.squared, NA, NA),
  Adjust_R2 = c(summary(lm_model)$adj.r.squared, NA, NA),
  MSE = c(accuracy^2, accuracy_tree^2, accuracy_random^2),
  RMSE = c(accuracy, sqrt(mean((predictions_tree - test_set$Strength)^2)), sqrt(mean((predictions_random - test_data_imputed$Strength)^2))),
  MAE = c(mean(abs(predictions - test_set$Strength)), mean(abs(predictions_tree - test_set$Strength)), mean(abs(predictions_random - test_data_imputed$Strength)))
)
model_results <- rbind(model_results, data.frame(
  Model = "KNN",
  R2 = knn_r2,
  Adjust_R2 = NA,  # Adjusted R2 not applicable for KNN
  MSE = NA,  # MSE not applicable for KNN
  RMSE = knn_rmse,
  MAE = knn_mae
))
# Print the model results
print(model_results)

