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
# Set vars.for.imputation to all variables in the data frame except not.fully.paid
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
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
spl = sample.split(impResults$not.fully.paid, 0.7)
train = subset(impResults, spl = TRUE)
test = subset(impResults, spl = FALSE)
# Use logistig regression on the training set to predict the dependent variable
mod1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(mod1)

## Problem 2.2
# What is the value of Logit(A)-Logit(B), given Applicant A has FICO of 700 and Applicant B has FICO of 710
logA = 9.260 + (-9.002e-03 * 700)
logB = 9.260 + (-9.002e-03 * 710)
ans1 = logA - logB
ans1
# What is the value of O(A)/O(B)
ans2 = exp(logA)/exp(logB)
ans2

## Problem 2.3
# Predict the probability of the test set loans not being paid back in full. Store the probabilities in 'predicted.risk' var
test$predicted.risk = predict(mod1, newdata = test, type = "response")
# Compute the confusion matrix using a threshold of 0.5
threshold = 0.5
table(test$not.fully.paid, as.numeric(test$predicted.risk >= threshold))
# Baseline model (using test set)
table(test$not.fully.paid)

## Problem 2.4
# Use the ROCR package to compute the test set AUC
library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Problem 3.1
# Use independent var, int.rate as a "smart baseline"
# Bivariate logistic regression model using the training set to predict the dependent var not.fully.paid using only
# the var int.rate
bivariate = glm(impResults$not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(bivariate)
# Check for correlation
cor(train$int.rate, train$fico)

## Problem 3.2
# Make test set predictions on the bivariate model
pred.bivariate = predict(bivariate, newdata = test, type = "response")
# What is the highest predicted probability of a loan not being paid in full
summary(pred.bivariate)

## Problem 3.3
# What is the test set AUC of the bivariate model
pred.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(pred.bivariate, "auc")@y.values)









