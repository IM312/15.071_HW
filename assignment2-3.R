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


