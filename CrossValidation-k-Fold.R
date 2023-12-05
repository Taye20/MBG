#NEW
require(pacman)
p_load(tidyverse, readr, readxl, car, dplyr, caret)

# get working directory
getwd()

###### load the data ##
data3 <- read_csv("cleandataMBG_CrossValidation1.csv")



# Partition data and create index matrix of selected values
index <- createDataPartition(data3$Mal_Outcome, p=.7, list=FALSE, times=1)

# Create test and train data frames
train_data3 <- data3[index,]
test_data3 <- data3[-index,]

# Convert data frame object (df) to a conventional data frame object
data3 <- as.data.frame(data3)

# Create test and train data frames
train_data3 <- data3[index,]
test_data3 <- data3[-index,]
nrow(train_data3)

nrow(test_data3)

# Re-label values of outcome variable for train_df
train_data3$Mal_Outcome[train_data3$Mal_Outcome==1] <- "positive"
train_data3$Mal_Outcome[train_data3$Mal_Outcome==0] <- "negative"

# Re-label values of outcome variable for test_df
test_data3$Mal_Outcome[test_data3$Mal_Outcome==1] <- "positive"
test_data3$Mal_Outcome[test_data3$Mal_Outcome==0] <- "negative"

# Convert outcome variable to factor for each data frame
train_data3$Mal_Outcome <- as.factor(train_data3$Mal_Outcome)
test_data3$Mal_Outcome <- as.factor(test_data3$Mal_Outcome)

# Specify type of training method used and the number of folds
ctrlspecs <- trainControl(method="cv", 
                           number=5, 
                           savePredictions="all",
                           classProbs=TRUE)

# Set random seed for subsequent random selection and assignment operations
set.seed(1985)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
model1 <- train(Mal_Outcome ~., data=train_data3, 
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
predictions <- predict(model1, newdata=test_data3)

# Create confusion matrix to assess model fit/performance on test data
confusionMatrix(data=predictions, test_data3$Mal_Outcome)


