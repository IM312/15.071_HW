## edX MITx 15.071x The Analytics Edge
## Assignment 6-2 Market Segmentation for Airlines
## Dataset --> AirlinesCluster.csv

## Load libraries
library(caret)


## Problem 1.1
# Read in the dataset
airlines = read.csv("AirlinesCluster.csv")
# Which two variables have (on average) the smallest values
# Which two variables have (on  average) the largest values
summary(airlines)

## Problem 1.2
# It is important to normalize the data so that the clustering isn't dominated by the variables that are on a larger scale

## Problem 1.3
# Create a normalized data frame
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
# Is the data normalized
summary(airlinesNorm)  # all variables now have mean = 0.0
# Which variable now has the largest max value
# Which data now has the smallest min value
summary(airlinesNorm)


