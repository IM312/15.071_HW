## edX MITx 15.071x The Analytics Edge
## Assignment 2-1 Climate Change
## Dataset --> climate_change.csv

## Problem 1.1
# Read in the dataset
climate = read.csv("climate_change.csv")
summary(climate)
# Split the data into a training set, consisting of observations up to and including 2006, and a 
# testing set of the remaining years
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
# Build a linear regression model using all independent variables, except Year and Month, to 
# predict the dependent variable Temp
nonvars = c("Year", "Month")
train = train[,!(names(train) %in% nonvars)]
test = test[,!(names(test) %in% nonvars)]
mod1 = lm(Temp ~ ., data = train)
summary(mod1)

## Problem 1.2
