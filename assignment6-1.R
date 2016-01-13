## edX MITx 15.071x The Analytics Edge
## Assignment 6-1 Document Clustering with Daily Kos
## Dataset --> dailykos.csv

## Load libraries

## Problem 1.1
# Read the data into R
dailykos = read.csv("dailykos.csv")
# Compute the distances
kosDist = dist(dailykos [2:1546], method = "euclidean")  # Start at variable 2 to exclude "Document" variable
# Use hclust to build the model
kosHeirClust = hclust(kosDist, method = "ward.D")

## Problem 1.2
# Plot the dendrogram
plot(kosHeirClust)

## Problem 1.4
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

## Problem 1.5
# Look at the top 6 words in cluster 1
tail(sort(colMeans(HeirClust1[-1]))) 
# Computes the mean frequency values of each word in cluster 1. The [-1] removes the first column of HeirClust1. colMeans
# computes the column (word) mean. The sort function orders the words in increasing order of the mean values. The tail
# function outputs the last 6 words listed.

## Problem 1.6
# Repeat the function above for the remaining clusters
tail(sort(colMeans(HeirClust2[-1]))) 
tail(sort(colMeans(HeirClust3[-1]))) 
tail(sort(colMeans(HeirClust4[-1]))) 
tail(sort(colMeans(HeirClust5[-1]))) 
tail(sort(colMeans(HeirClust6[-1]))) 
tail(sort(colMeans(HeirClust7[-1]))) 

## Problem 2.1
# Run k-means clustering, setting seed to 1000 with 7 clusters (exclude "Document" variable).
set.seed(1000)
KmeansCluster = kmeans(dailykos[2:1546], centers = 7)
# Subset the data into 7 clusters using the "cluster" variable of the kmeans output
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)
# How many observations are in Cluster 3
# Which cluster has the fewest number of observations
table(KmeansCluster$cluster)

## Problem 2.2








