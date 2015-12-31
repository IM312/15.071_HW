## edX MITx 15.071x The Analytics Edge
## Assignment 5-3 Separating Spam from Ham
## Dataset --> emails.csv

## Load libraries
library(tm)
library(SnowballC)


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
# Preprocessing
# Build a new corpus
corpus = Corpus(VectorSource(emails$text))
# Convert the text to lowercase
corpus = tm_map(corpus, content_transformer(tolower)) # Wasn't getting the right # of terms w/o content_transformer added
# Remove all punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
# Remove all English stopwords from the corpus
corpus = tm_map(corpus, removeWords, stopwords("english"))
# Stem the words in the corpus
corpus = tm_map(corpus, stemDocument)
# Build a document term matrix
dtm = DocumentTermMatrix(corpus)

# How many terms are in dtm
str(dtm)

## Problem 2.2
# Limit dtm to contain terms appearing in at least 5% of documents
spdtm = removeSparseTerms(dtm, 0.95)
# How many terms are in spdtm
str(spdtm)

## Problem 2.3
# Build a data frame called emailsSparse and use the make.names function to make the variable names of emailsSparse valid
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
# What word stem shows up most frequently across all the emails
which.max(colSums(emailsSparse))








