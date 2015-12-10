## edX MITx 15.071x The Analytics Edge
## Assignment 2-2 Detecting Flu Epidemics via Search Engine Query Data
## Dataset --> FluTrain.csv & FluTest.csv

## Problem 1.1
# Load FluTrain.csv
FluTrain = read.csv("FluTrain.csv")
# Which week corresponds to the highest percentage of ILI-related physician visits
subset(FluTrain, ILI==max(ILI))
# Which week corresponds to the highest percentage of ILI-related query by fraction
subset(FluTrain, Queries==max(Queries))

## Problem 1.2
# Plot the histogram of the dependent variable, ILI
hist(FluTrain$ILI)

## Problem 1.3
# Plot the natrual log of ILI versus Queries
plot(FluTrain$Queries, log(FluTrain$ILI))

## Problem 2.2
# Run the regression model, FluTrend1 using: log(ILI)=intercept+coefficient*Queries
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
# What is the R-squared value
summary(FluTrend1)

## Problem 2.3
# What is the relationship we can infer between the independent and dependent variable
correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
correlation^2
log(1/correlation)
exp(-0.5*correlation)
# correlation^2 is equal to R-squared

## Problem 3.1







