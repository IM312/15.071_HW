## edX MITx 15.071x The Analytics Edge
## Assignment 5-1 Detecting Vandalism on Wikipedia
## Dataset --> wiki.csv

## Load libraries

## Problem 1.1
# Load the data
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
# Convert the "Vandal" column to a factor
wiki$Vandal = as.factor(wiki$Vandal)
# How many cases of vandalism were detected in the history of this page
summary(wiki)
# or
table(wiki$Vandal)


