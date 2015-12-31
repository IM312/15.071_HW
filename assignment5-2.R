## edX MITx 15.071x The Analytics Edge
## Assignment 5-2 Automating 
## Dataset --> clinical_trial.csv

## Load libraries
library(tm)
library(SnowballC)


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

## Problem 2.1
# Note: because we have both title and abstract info for trials, two corpera need to be built
# Preprocessing:
# Convert the title variable to corpusTitle and the abstract variable to corpusAbstract
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
# Convert the corpera to lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
# Remove punctuation in the corpera
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
# Remove the english language stop words
corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))
# Stem the words in each corpera
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
# Build a document term matrix from each corpera
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
# Limit the dtms to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
# Convert the dtms to data frames
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# How many terms remain in dtmTitle
str(dtmTitle)
# or 
dim(dtmTitle)
# or 
ncol(dtmTitle)
# How many terms remain in dtmAbstract
str(dtmAbstract)

## Problem 2.3
# What is the most frequent word stem across all abstracts
csAbstract = colSums(dtmAbstract)
which.max(csAbstract)

## Problem 3.1
# Fix that some of the variables in the two data frames have the same name
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
# What is the effect of these two functions
# The letter T is added in front of all title variable names and the letter A is added in front of all abstract variable
# names

## Problem 3.2
# Combine dtmTitle and dtmAbstract into a single data frame
dtm = cbind(dtmTitle, dtmAbstract)
# Add the dependent variable "trial" to dtm
dtm$trial = trials$trial
# How many columns are in the combined data frame
str(dtm)

## Problem 3.3













