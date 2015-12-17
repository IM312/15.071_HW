## edX MITx 15.071x The Analytics Edge
## Assignment 4-1 Letter Recognition
## Dataset --> letters_ABPR.csv

## Problem 1.1
# Load the file into R
letters = read.csv("letters_ABPR.csv")
# Create a new variable isB in the dataframe
letters$isB = as.factor(letters$letter == "B")
str(letters)
# Split the data set into a training and testing set
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
# Consider a baseline method that predicts the most frequent outcome, "not B". What is the accuracy of the baseline method
# on the test set
table(test$isB)
accuracy = 1175/(1175+383)
accuracy  # 0.754

## Problem 1.2
# Build a classification tree to predict whether a letter is a B or not, using the training set. Remove the variable "letter"
# from the model
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~. -letter, data = train, method = "class")
# What is the accuracy of the CART model on the test set (use type="class" when making predictions on the test set)
predictCART = predict(CARTb, newdata = test, type = "class")
table(test$isB, predictCART)
accuracy = (1118+340)/(1118+57+43+340) # can also use (1118+340)/nrow(test)
accuracy  # 0.936

## Problem 1.3
# Build a random forest model to predict whether the letter is a B or not
install.packages("randomForest")
library(randomForest)
set.seed(1000)
letterForest = randomForest(isB ~. -letter, data = train)
# What is the accuracy of the model on the test set
predictForest = predict(letterForest, newdata = test)
table(test$isB, predictForest)
accuracy = (1165+374)/nrow(test)
accuracy  # 0.988

## Problem 2.1





