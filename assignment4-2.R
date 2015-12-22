## edX MITx 15.071x The Analytics Edge
## Assignment 4-2 Predicting Earnings from Census Data
## Dataset --> census.csv

## Libraries
library(caTools)

## Problem 1.1
# Load the dataset
census = read.csv("census.csv")
str(census)
# Split the data
spl = sample.split(census$over50k, SplitRatio = 0.6)
set.seed(2000)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)
# Build a logistic regression model using all of the independent variables to predict "over50k"
mod1 = glm(over50k ~ ., data = train, family = binomial)
# What variables are significant
summary(mod1)

## Problem 1.2
