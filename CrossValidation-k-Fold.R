##Cross validation to determine reliability of the GLM model. 
require(pacman)
p_load(tidyverse, readr, readxl, car, dplyr, caret)

# get working directory
getwd()

###### load the data ##
data <- read_csv("cleandataMBG.csv")



# Partition data and create index matrix of selected values
index <- createDataPartition(data$Mal_Outcome, p=.7, list=FALSE, times=1)

# Create test and train data frames
train_data <- data[index,]
test_data <- data[-index,]

# Convert data frame object (df) to a conventional data frame object
data <- as.data.frame(data3)

# Create test and train data frames
train_data <- data[index,]
test_data <- data[-index,]
nrow(train_data)

nrow(test_data3)

# Re-label values of outcome variable for train_df
train_data$Mal_Outcome[train_data$Mal_Outcome==1] <- "positive"
train_data$Mal_Outcome[train_data$Mal_Outcome==0] <- "negative"

# Re-label values of outcome variable for test_df
test_data$Mal_Outcome[test_data$Mal_Outcome==1] <- "positive"
test_data$Mal_Outcome[test_data$Mal_Outcome==0] <- "negative"

# Convert outcome variable to factor for each data frame
train_data$Mal_Outcome <- as.factor(train_data$Mal_Outcome)
test_data$Mal_Outcome <- as.factor(test_data$Mal_Outcome)

# Specify type of training method used and the number of folds
ctrlspecs <- trainControl(method="cv", 
                           number=5, 
                           savePredictions="all",
                           classProbs=TRUE)

# Set random seed for subsequent random selection and assignment operations
set.seed(1985)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
model1 <- train(Mal_Outcome ~., data=train_data, 
                         method="glm", 
                         family=binomial, 
                         trControl=ctrlspecs)

# Print information about model
print(model1)

# Print results of final model estimated using training data
summary(model1)

# Estimate the importance of different predictors
varImp(model1)

# Predict outcome using model from training data based on testing data
predictions <- predict(model1, newdata=test_data)

# Create confusion matrix to assess model fit/performance on test data
confusionMatrix(data=predictions, test_data$Mal_Outcome)


