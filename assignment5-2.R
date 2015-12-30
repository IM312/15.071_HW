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
