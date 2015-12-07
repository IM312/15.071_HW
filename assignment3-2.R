## edX MITx 15.071x The Analytics Edge
## Assignment 3-2 Predicting Loan Repayment
## Dataset --> loans.csv

## Problem 1.1
# Load the dataset and explore it
loans = read.csv("loans.csv")
str(loans)
summary(loans)
# What proportion of loans were not paid in full
table(loans$not.fully.paid)
## Problem 1.3
# Build a data frame limited to observations with some missing data
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)
## Problem 1.4
# Use multiple imputation to fill in the missing data values
install.packages("mice")
library(mice)
set.seed(144)
# Set vars.for.imputation to all variables in the data frame except not.fully.paid, to impute the values using all of the 
# other independent variables
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans = complete(mice(loans[vars.for.imputation]))
summary(loans)
# Test imputation results against provided imputation results (loans_imputed.csv)
impResults = read.csv("loans_imputed.csv")
summary(impResults)

## Problem 2.1
# Split the dataset into training and testing sets
library(caTools)
# Set the random seed to 144
set.seed(144)
# Split so 70% of obs are for the training set
spl = sample.split(loans$not.fully.paid, 0.7)
Train = subset(loans, spl == TRUE)
Test = subset(loans, spl == FALSE)
# Use logistig regression on the training set to predict the dependent variable
mod1 = glm(not.fully.paid ~ ., data = Train, family = binomial)
summary(mod1)
## Problem 2.3
# Predict the probability of the test set loans not being paid back in full. Store the probabilities in 'predicted.risk' var
Test$predicted.risk = predict(mod1, newdata = Test, type = "response")
# Compute the confusion matrix using a threshold of 0.5
threshold = 0.5
table(Test$not.fully.paid, as.numeric(Test$predicted.risk >= threshold))
# Baseline model (using test set)
table(Test$not.fully.paid)
## Problem 2.4
# Use the ROCR package to compute the test set AUC
library(ROCR)
ROCRpred = prediction(Test$predicted.risk, Test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Problem 3.2











