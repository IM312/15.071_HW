## edX MITx 15.071x The Analytics Edge
## Assignment 5-3 Separating Spam from Ham
## Dataset --> emails.csv

## Load libraries

## Problem 1.1
# Load the dataset
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
# How many emails are in the dataset
str(emails)

## Problem 1.2
# How many of the emails are spam
table(emails$spam)  # 1 = spam

## Problem 1.3
# Which word appears at the beginning of every email
emails$text[1]
emails$text[1000]

## Problem 1.5
# How many characters are in the longest email
max(nchar(emails$text))

## Problem 1.6
# Which row contains the shortest email
which.min(nchar(emails$text))
emails$text[1992]

## Problem 2.1

