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



