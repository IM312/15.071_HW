## edX MITx 15.071x The Analytics Edge
## Assignment 4-2 Predicting Earnings from Census Data
## Dataset --> census.csv

## Libraries
library(caTools)
library(ROCR)


## Problem 1.1
# Load the dataset
census = read.csv("census.csv")
str(census)
# Split the data
spl = sample.split(census$over50k, SplitRatio = 0.6)
set.seed(2000)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)
# Build a logistic regression model using all of the independent variables to predict "over50k"
mod1 = glm(over50k ~ ., data = train, family = binomial)
# What variables are significant
summary(mod1)

## Problem 1.2
# What is the accuracy of the model on the testing set using a threshold of 0.5
predictTest = predict(mod1, newdata = test, type = "response")
table(test$over50k, predictTest > 0.5)
accuracy = (9051+1888)/nrow(test)
accuracy  # 0.855

## Problem 1.3
# What is the baseline accuracy for the testing set
table(test$over50k)
accuracy = 9713/nrow(test)
accuracy

## Problem 1.4
# What is the AUC for this model on the test set
ROCRpred = prediction(predictTest, test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc  # 0.906
#####################
## Interpretation ##
#####################
# Given a random individual from the dataset who earned over $50k and a random indivdual from the dataset who did not earn
# over $50k, the model will correctly classify which is which 90.6% of the time.

## Problem 2.1





