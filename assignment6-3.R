## edX MITx 15.071x The Analytics Edge
## Assignment 6-3 Predicting Medical Costs with Cluster-then-Predict
## Dataset --> reimbursement.csv

## Load libraries

## Problem 1.1 
# Load the dataset
claims = read.csv("reimbursement.csv")
# How many Medicare beneficiaries are included in the dataset
str(claims)

## Problem 1.2
# What proportion of patients have at least one of the chronic conditions described
has.condition = subset(claims, alzheimers == 1 | arthritis == 1 | cancer == 1 | copd == 1 | 
                           depression == 1 | diabetes == 1 | heart.failure == 1 | ihd == 1 |
                           kidney == 1 | osteoporosis ==1 | stroke == 1)
nrow(has.condition)
proportion = 280427/458005
proportion

## Problem 1.3
# What is the max correlation between independent variables in the dataset
cor(claims)  # Recommended solution doesn't answer the question
cor(claims$ihd, claims$diabetes)

## Problem 1.4
# Plot the histogram of the dependent variable
hist(claims$reimbursement2009, col=2)

## Problem 1.5
# Log transform the two reimbursement variables




