## edX MITx 15.071x The Analytics Edge
## Assignment 2-3 Reading Test Scores
## Dataset --> pisa2009train.csv & pisa2009test.csv

## Problem 1.1
# Load training and testing data sets
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
# How many students are there in the training set
str(pisaTrain)


## Problem 1.2
# What is the average reading test score of males
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

## Problem 1.3
# Which variables are missing data in at least one observation in the training set
summary(pisaTrain)

## Problem 1.4
# Remove observations with any missing values from pisaTrain and pisaTest
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
# How many observations are now in the training set
str(pisaTrain)
# How many observations are in the testing set
str(pisaTest)

## Problem 2.1
str(pisaTrain)

## Problem 2.2
# Unordered factors in regression models: define one level as the "reference level" and add a binary variable for each
# of the remaining levels
summary(pisaTrain$raceeth)

## Problem 3.1
# Build the model: set the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
# Build linear regression model using the training set to predict readingScore
lmScore = lm(readingScore ~ ., data = pisaTrain)
# What is the multiple R-squared of the model
summary(lmScore) # 0.3251

## Problem 3.2
# What is the training-set RMSE of lmScore
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE
#or
sqrt(mean(lmScore$residuals^2))

## Problem 3.3















