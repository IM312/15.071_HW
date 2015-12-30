## edX MITx 15.071x The Analytics Edge
## Assignment 5-2 Automating 
## Dataset --> clinical_trial.csv

## Load libraries

## Problem 1.1
# Load the data
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
# Investigate the data frame
summary(trials)
str(trials)
# How many characters are there in the longest abstract
summary(nchar(trials$abstract))
# or
max(nchar(trials$abstract))

## Problem 1.2
# How many search results provided no abstract
table(nchar(trials$abstract) == 0)
# or
sum(nchar(trials$abstract)==0)

## Problem 1.3
# What is the shortest title of any article
which.min(nchar(trials$title))
trials$title[1258]








