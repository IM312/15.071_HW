## edX MITx 15.071x The Analytics Edge
## Assignment 3-3 Predicting Parole Violators
## Dataset --> parole.csv

## Problem 1.1
# Load the dataset
parole = read.csv("parole.csv")
# How many parolees are in the dataset
str(parole)

## Problem 1.2
# How many parolees violated the terms of their parole
table(parole$violator)

## Problem 1.3
# Variables male, race, state, crime, and viloator are unordered factors. State and crime have 3 or more levels

## Problem 2.1
# Convert the unordered variables with more than 3 levels to factors
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
# Compare the output for factor variable and numerical variable
summary(parole)
# or
summary(parole$state)
summary(parole$crime)

## Problem 2.2
# Prep the data prior to splitting into test and train sets to save work

## Problem 3.1
# Split with the following commands
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
# What proportion of parolees have been allocated to the training and testing sets
str(train)
str(test)

## Problem 4.1
# Train a logistic regression model on the training set with violator as dependent variable and all other variables as 
# independent variables
model1 = glm(violator ~ ., data = train, family = "binomial")
# What variables are significant in the model
summary(model1)

# Problem 4.2
# What can we say based on the coefficient of the multiple.offenses variable
var = exp(1.6119919)
var  # parolee who committed multiple offenses has 5.01 times higher odds of being a violator
# than a parolee who did not commit multiple offenses but is otherwise identical

## Problem 4.3
# Parolee: male, white, aged 50 years at release, from Maryland, served 3 months, max sentence
# of 12 months, didn't commit multiple offenses, committed larceny
# What are the odds the individual is a violator
logit = -4.2411574+0.3869904*1+0.8867192*1-0.0001756*50+0.4433007*0+0.8349797*0-3.3967878*0-0.1238867*3+0.0802954*12+1.6119919*0+0.6837143*1-0.2781054*0-0.0117627*0
logit
odds = exp(logit)
odds
# What is the probability this individual is a violator
Prob = 1/(1+exp(-logit))
Prob

## Problem 5.1
# Obtain the model's predicted probabilities for parolees in the testing set
predictTest = predict(model1, newdata = test, type = "response")
summary(predictTest)

## Problem 5.2
# Evaluate the model's predictions on the test set using a threshold of 0.5
# What is the model's sensitivity
table(test$violator, predictTest > 0.5)
sensitivity = 12/(11+12)
sensitivity
# What is the model's specificity
specificity = 167/(167+12)
specificity  # this model favors specificity over sensitivity
# What is the model's accuracy
accuracy = (167+12)/(167+12+11+12) # model has 12 false negatives at cutoff 0.5
accuracy  # overall, the classifier is correct 88.6% of the time

## Prblem 5.3
# What is the accuracy of a simple model that predicts every parolee is a non-violator
table(test$violator)
accuracy2 = 179/(179+23) # baseline model has 23 false negatives
accuracy2

## Problem 5.5
# The model is likely of value to the board (fewer false negatives) versus the baseline, and 
# using a lower cutoff will likely increase the model's value

## Problem 5.6
# Using the ROCR package, what is the model's AUC value
library(ROCR)
predictTest = predict(model1, type = "response", newdata = test)
ROCRpredTest = prediction(predictTest, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc  # auc = 0.8945834

## Problem 5.7
# What is the meaning of AUC in this context
# It's the probability the model can correctly differentiate between a randomly selected parole
# violator and a randomly selected parole non-violator

## Problem 6.1
# The dataset suffers from selection bias since it does not contain parolees who neither
# violated their parole nor completed their term in 2004, thus causing non-violators to be 
# underrepresented
# To correct this bias, the dataset should track a group of parolees from the start of their
# parole until either they violated parole or they completed their term

## DONE!! 













