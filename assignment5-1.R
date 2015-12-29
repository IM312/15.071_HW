## edX MITx 15.071x The Analytics Edge
## Assignment 5-1 Detecting Vandalism on Wikipedia
## Dataset --> wiki.csv

## Load libraries
library(tm)

## Assignment goal:
# Develop a wikipedia vandalism detector that uses machine learning to distinguish between valid edits and vandalism

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

## Problem 1.2
# Pre-process the data:
# Create the corpus for the Added column called "corpusAdded"
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded
# Remove the English-language stopwords 
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))
# Stem the words
corpusAdded = tm_map(corpusAdded, stemDocument)
# Build the DocumentTermMatrix and call it dtmAdded
dtmAdded = DocumentTermMatrix(corpusAdded)
# How many terms appear in dtmAdded
dtmAdded

## Problem 1.3
# Filter out sparse terms, keeping only terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
# How many terms appear in sparseAdded
sparseAdded

## Problem 1.4
# Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded)) # A = added
# Repeat all of the pre-process steps to create a Removed bag-of-words dataframe, called wordsRemoved, except prepend the 
# words with R
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, c(stopwords("english")))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))  # R = removed
# How many words are in the wordsRemoved dataframe
sparseRemoved
# or
ncol(wordsRemoved)

## Problem 1.5








       


