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

## Problem 2.1
# Compute the (euclidean) distances between data points
distance = dist(airlinesNorm, method = "euclidean")
# Run the heirarchical clustering algorithm on the normalized data
HeirClust = hclust(distance, method = "ward.D")
# Plot the dendrogram
plot(HeirClust)

# Visualize the cuts
rect.hclust(HeirClust, k=5, border = "red")

## Problem 2.2
# Divide the data into 5 clusters
airlineClusters = cutree(HeirClust, k=5)
# How many data points are in Cluster 1
table(airlineClusters)

## Problem 2.3
# Compare the average values in each of the variables for the 5 clusters (the centroids)
tapply(airlines$Balance, airlineClusters, mean)
tapply(airlines$QualMiles, airlineClusters, mean)
tapply(airlines$BonusMiles, airlineClusters, mean)
tapply(airlines$BonusTrans, airlineClusters, mean)
tapply(airlines$FlightMiles, airlineClusters, mean)
tapply(airlines$FlightTrans, airlineClusters, mean)
tapply(airlines$DaysSinceEnroll, airlineClusters, mean)
# advanced method
lapply(split(airlines, airlineClusters), colMeans)

## Problem 2.4





