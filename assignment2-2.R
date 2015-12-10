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





