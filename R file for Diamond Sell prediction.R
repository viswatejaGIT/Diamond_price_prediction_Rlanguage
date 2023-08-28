library(ggplot2)
library(caret)
library(xgboost)

# Load diamonds dataset
data(diamonds)

diamonds <- read.csv("C:\Users\VISWA TEJA\Downloads\new_diamonds.csv")
# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(diamonds$price, p = 0.7, list = FALSE)
trainData <- diamonds[trainIndex, ]
testData <- diamonds[-trainIndex, ]

# Ensure trainData is properly formatted
trainData <- as.data.frame(trainData)

# Move the price column to the last position
trainData <- trainData[, c(1:6, 8, 7)]

# Check which columns contain non-numeric values
non_numeric_cols <- sapply(trainData, function(x) !is.numeric(x))
non_numeric_cols <- names(non_numeric_cols[non_numeric_cols == TRUE])

# Remove non-numeric columns from trainData
trainData <- trainData[, !names(trainData) %in% non_numeric_cols]

# Build the Gradient Boosting model
set.seed(123)
model <- xgboost(data = as.matrix(trainData[,-7]), label = trainData$price, nrounds = 100)

testData <- testData[, sapply(testData, is.numeric)]



testData_reorder <- testData %>%
  select(carat, depth, table, x, price)

# Make predictions using the xgboost model
predictions <- predict(model, newdata = as.matrix(testData_reorder))

predictions <- factor(ifelse(predictions < median(trainData$price), "Low", "High"), levels = c("Low", "High"))
testLabels <- factor(ifelse(testData_reorder$price < median(trainData$price), "Low", "High"), levels = c("Low", "High"))

# Calculate confusion matrix and other metrics
confusionMatrix(predictions, testLabels)


ggplot(data = data.frame(predictions = as.numeric(predictions), actual = as.numeric(testLabels))) +
  geom_point(aes(x = actual, y = predictions)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Price", y = "Predicted Price")


ggplot() +
  geom_density(aes(x = trainData$price, color = "Train"), alpha = 0.5) +
  geom_density(aes(x = testData$price, color = "Test"), alpha = 0.5) +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
  labs(x = "Price", y = "Density")