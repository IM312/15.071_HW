## edX MITx 15.071x The Analytics Edge
## Assignment 5-1 Detecting Vandalism on Wikipedia
## Dataset --> wiki.csv

## Load libraries
library(tm)
library(caTools)
library(rpart)
library(rpart.plot)


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
# Combine the two dataframes into a data frame called wikiWords
wikiWords = cbind(wordsAdded, wordsRemoved)
# Add the Vandal column
wikiWords$Vandal = wiki$Vandal
# Set the random seed to 123 and split the data set putting 70% in the training set
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWiki = subset(wikiWords, split == TRUE)
testWiki = subset(wikiWords, split == FALSE)
# What is the accuracy on the test set of a baseline method always predicting "not vandalism"
table(testWiki$Vandal)  # 0.5314

## Problem 1.6
# Build a CART model to predict Vandal, using the training set and all of the other variables as independent variables
mod1 = rpart(Vandal ~ ., data = trainWiki, method = "class")
# What is the accuracy of the model on the test set, using a threshold of 0.5
predictWiki = predict(mod1, newdata = testWiki, type = "class")
table(testWiki$Vandal, predictWiki)
accuracy = (618+12)/nrow(testWiki)
accuracy  # 0.5417

## Problem 1.7
# Plot the CART tree
prp(mod1)
# How many word stems does the model use
# 2

## Problem 1.8
####################
## Interpretation ##
####################
# Even though the CART model beats the baseline, bag of words is not very predictive for this problem
# Overfitting???
predictWikiFit = predict(mod1, newdata = trainWiki, type = "class")
table(trainWiki$Vandal, predictWikiFit)
accuracy = (1443+33)/nrow(trainWiki)
accuracy  # 0.5440 --> No overfitting

## Problem 2.1
# Hypothesis: the presence of a web address is a sign of vandalism
# Search for the presence of a web address in the words added by searching for "http" in the Added column
# Create a copy of your dataframe 
wikiWords2 = wikiWords
# Make a new column in wikiWords2 that is 1 if "http" was in Added
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
# How many revisions added a link
table(wikiWords2$HTTP)

## Problem 2.2
# Use the previously created split variable make new training and testing sets
trainWiki2 = subset(wikiWords2, split == TRUE)
testWiki2 = subset(wikiWords2, split == FALSE)
# Create a new CART model using this new variable as one of the independent variables
mod2 = rpart(Vandal ~ ., data = trainWiki2, method = "class")
# What is the accuracy of the CART model on the test set, using a threshold of 0.5
predictWiki2 = predict(mod2, newdata = testWiki2, type = "class")
table(testWiki2$Vandal, predictWiki2)
accuracy = (609+57)/nrow(testWiki2)
accuracy  # 0.5727

## Problem 2.3
# Hypothesis 2: The number of words added or removed is predictive, perhaps more so than the actual words themselves
# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in the wikiWords2 data frame
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
# What is the average number of words added
summary(wikiWords2)
# or 
mean(wikiWords2$NumWordsAdded)

## Problem 2.4
# Using the previously created split variable make new training and testing sets
trainWiki3 = subset(wikiWords2, split == TRUE)
testWiki3 = subset(wikiWords2, split == FALSE)
# Creat a new CART model
mod3 = rpart(Vandal ~ ., data = trainWiki3, method = "class")
# What is the accuracy of the CART model on the test set
predictWiki3 = predict(mod3, newdata = testWiki3, type = "class")
table(testWiki3$Vandal, predictWiki3)
accuracy = (514+248)/nrow(testWiki3)
accuracy  # 0.6552

## Problem 3.1
# Make a copy of wikiWords2
wikiWords3 = wikiWords2
# Add the two original variables Minor and LoggedIn to the new data frame
wikiWords3$Minor = wiki$Minor
wikiWords3$LoggedIn = wiki$Loggedin
# Using the previously created split variable make new training and testing sets
trainWiki4 = subset(wikiWords3, split == TRUE)
testWiki4 = subset(wikiWords3, split == FALSE)
# Build a CART model using all of the training data
mod4 = rpart(Vandal ~ ., data = trainWiki4, method = "class")
# What is the accuracy of the model on the test set
predictWiki4 = predict(mod4, newdata = testWiki4, type = "class")
table(testWiki4$Vandal, predictWiki4)
accuracy = (595+241)/nrow(testWiki4)
accuracy  # 0.7188

## Problem 3.2
# Plot the CART tree
prp(mod4)
# How many splits are there in the tree
# 3

####################
## Interpretation ##
####################

# By adding new independent variables, the model's accuracy was improved without making the model more complicated

## DONE!





















       


