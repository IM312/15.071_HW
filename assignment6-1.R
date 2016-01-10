## edX MITx 15.071x The Analytics Edge
## Assignment 6-1 Document Clustering with Daily Kos
## Dataset --> dailykos.csv

## Load libraries

## Problem 1.1
# Read the data into R
dailykos = read.csv("dailykos.csv")
# Compute the distances
kosDist = dist(dailykos [2:1546], method = "euclidean")
# Use hclust to build the model
kosHeirClust = hclust(kosDist, method = "ward.D")

## Problem 1.2
# Plot the dendrogram
plot(kosHeirClust)

## Problem 1.3
# Split the data into 7 clusters using the cutree function
heirGroups = cutree(kosHeirClust, k = 7)
# Use the subset function to subset the data by cluster
HeirClust1 = subset(dailykos, heirGroups == 1)
HeirClust2 = subset(dailykos, heirGroups == 2)
HeirClust3 = subset(dailykos, heirGroups == 3)
HeirClust4 = subset(dailykos, heirGroups == 4)
HeirClust5 = subset(dailykos, heirGroups == 5)
HeirClust6 = subset(dailykos, heirGroups == 6)
HeirClust7 = subset(dailykos, heirGroups == 7)
# How many observations are in cluster 3
# Which cluster has the most observations
# Which cluster has the fewest observations
table(heirGroups)






