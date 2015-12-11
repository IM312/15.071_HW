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
# Load FluTest.csv
FluTest = read.csv("FluTest.csv")
# Predict the ILI value using FluTrend1 model. Note: need to convert log(ILI) to ILI
PredTest = exp(predict(FluTrend1, newdata = FluTest))
# What is the estimate of ILI-related physician visits for the week of March 11, 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest[11] # Estimated ILI = 2.187378

## Problem 3.2
# What is the relative error between tbe estimate and the observed for that week
# (Observed ILI - Estimated ILI)/Observed ILI
FluTest$ILI[11] # Observed ILI = 2.293422
(2.293422-2.187378)/2.293422

## Problem 3.3
# What is the RMSE between our estimates and the actual observations for the percentage of ILI-related physician visits
SSE = sum((PredTest - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
# or
RMSE2 = sqrt(mean((PredTest - FluTest$ILI)^2))
RMSE2

## Problem 4.1
# Train a time series model - predict the ILI variable in the current week using the ILI variable from previous weeks
# Install and load the zoo package
install.packages("zoo")
library(zoo)
# Create the ILILag2 variable in the training set --> ARIMA models
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
# -2 passed to lag means to return 2 obs before the current one
# na.pad=TRUE means to add missing values for the first to weeks of our dataset
FluTrain$ILILag2 = coredata(ILILag2)
# How many values are missing in the new ILILag2 variable
summary(ILILag2)

## Problem 4.2
# Plot the lof of ILILag2 vs the log of ILI
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

## Problem 4.3
# Train a lin reg model on the FluTrain dataset to predict the log of the ILI var using Queries and the log of ILILag2 var
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
# Which coefficients are significant at the p=0.5 level
summary(FluTrend2) # Model2 R-squared > Model1 R-squared

## Problem 5.1
# Modify the code from above to add an ILILag2 var to the FluTest data frame
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 = coredata(ILILag2)
# How many missing values are there in the new variable
summary(ILILag2)

## Problem 5.3
# Fill in the missing values for ILILag2 in FluTest
str(FluTrain)
nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
# What is the new value of the ILILag2 variable in the first row of FluTest
FluTest$ILILag2[1]
# What is the new value of the ILILag2 variable in the second row of FluTest
FluTest$ILILag2[2]

## Problem 5.4
# Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 = exp(predict(FluTrend2, newdata = FluTest))
# What is the test-set RMSE of the FluTrend2 model
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

# FluTrend1 RMSE = 0.7490645
# FluTrend2 RMSE = 0.2942029
# FluTrend2 model obtained the best test-set RMSE

# DONE


















