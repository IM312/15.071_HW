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

## Problem 2.1
# Explain the apparent contradiction of the neg correlation of CH4 and CFC.11
cor(train$N2O, train$CFC.11)   # Collinearity!

## Problem 2.2
# Compute the correlations between all the variables in the training set
cor(train)

## Problem 3.1
# Build a new model using only MEI, TSI, Aerosols, and N2O using the training set
mod2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)
# What is the coefficient of N2O
summary(mod2)

## Problem 4.1
# Use the step() function 
step(mod1)
mod3 = lm(Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(mod3)

## Problem 5.1
# Using the model produced from the step model, calculate temp predictions for the testing
# data set, using the predict function
tempPredict = predict(mod3, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2

## Done






