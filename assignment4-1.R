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
# Convert letter in the original data set to a factor
letters$letter = as.factor(letters$letter)
str(letters)
# Generate new training and testing sets
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
# What is the baseline accuracy on the testing set
table(test2$letter)
accuracy = 401/(395+383+401+379)
accuracy  # "P" has the most observations. 0.257

## Problem 2.2
# Build a classification tree to predict the letter, using the training set. Remove "isB" from the model, as this is related
# to what we are trying to predict
CARTletter = rpart(letter ~. -isB, data = train2, method = "class")
# What is the test set accuracy of the CART model
predictLetter = predict(CARTletter, newdata = test2, type = "class")
table(test2$letter, predictLetter)
accuracy = (348+318+363+340)/nrow(test2) 
accuracy  # 0.8787

## Problem 2.3
# Estimate a random forest model on the training data
set.seed(1000)
letterForest2 = randomForest(letter ~. -isB, data = train)
# What is the accuracy of the random forest model
predictForest2 = predict(letterForest2, newdata = test)
table(test$letter, predictForest2)
accuracy = (390+380+393+364)/nrow(test2)
accuracy  # 0.980

##################
#### Summary ####
#################

## B/not B ##
# Baseline = 0.754
# CART = 0.936
# Tree = 0.988

## A,B,P,R ##
# Baseline = 0.257
# CART = 0.8787
# Tress = 0.980

## DONE!!









