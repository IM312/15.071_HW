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

## Problem 4.1
# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding
Investment = 10
Rate = 0.06
Time = 3
Value = Investment*exp(Rate * Time)
Value

## Problem 4.2
# What is the net value of the investment after accounting for the cost of the investment
# Value = Investment*exp(Rate * Time) - Investment

## Problem 5.1
# A Simple Investment Strategy
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
# What is the max profit of $10 investment in any loan in the testing set
summary(test$profit)

## Problem 6.1
# An investment strategy based on risk
# Use the subset() function to build a data frame called highInterest consisting of test set
# loans with an interest rate of at least 15%
highInterest = subset(test, int.rate >= 0.15)
# What is the average profit of a $1 investment in one of these high-interest loans
summary(highInterest)
# What proportion of the high-interest loans were not paid back in full
table(highInterst$not.fully.paid)

## Problem 6.2
# Determine the 100th smallest predicted probability of not paying in full by sorting the
# predicted risks in increasing order
cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
# Use the subset() function to build a data frame called selectedLoans consisting of 
# high-interest loans with predicted risk not exceeding the cutoff
selectedLoans = subset(highInterst, predicted.risk <= cutoff)
# What is the profit of the investor, who invested $1 in each of these 100 loans
sum(selectedLoans$profit)
# How many of the selected loans were not paid back in full
table(selectedLoans$not.fully.paid)

# Assignment complete














