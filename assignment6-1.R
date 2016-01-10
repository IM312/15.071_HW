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
