## edX MITx 15.071x The Analytics Edge
## Assignment 5-3 Separating Spam from Ham
## Dataset --> emails.csv

## Load libraries
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)


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

## Problem 2.4
# Add a variable called "spam" to emailsSparse containing the email spam labels
emailsSparse$spam = emails$spam
# How many stems appear at least 5000 times in the ham emails in the dataset
sort(colSums(subset(emailsSparse, spam == 0)))

## Problem 2.5
# How many stems appear at least 1000 times in the spam emails in the dataset
sort(colSums(subset(emailsSparse, spam == 1)))

## Problem 3.1
# Convert the dependent variable to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)
# Split emailsSparse 70/30 into a training set and testing set
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

## Build machine learning models:
# Logistic regression model
spamLog = glm(spam ~ ., data = train, family = "binomial")
# CART Model
spamCART = rpart(spam ~ ., data = train, method = "class")
# Random forest model
set.seed(123)
spamRF = randomForest(spam ~ ., data = train, method = "class")

# For each model obtain the predicted spam probabilities for the training set
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type = "prob")[,2]
predTrainLog = predict(spamLog, type = "response")

###################
## Error Message ##
###################

# glm.fit: algorithm did not converge
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# These messages often indicate overfitting and the first indicates particularly severe overfitting

# How many of the training set predicted probabilities from spamLog are <0.00001
table(predTrainLog < 0.00001)  # 3046
# How many of the training set predicted probabilities from spamLog are >0.99999
table(predTrainLog > 0.99999)  # 954
# How many of the taining set predicted probabilites from spamLog are 0.00001-0.99999
table(predTrainLog >=0.00001 & predTrainLog <=0.99999)

## Problem 3.2
# How many variables are labeled as significant in the log reg model
summary(spamLog)  # 0 (a symptom of the logistic regression algo not converging)


## Problem 3.3
# How many of the stem words "enron", "hou", "vinc", and "Kaminski" appear in the CART tree
prp(spamCART)  # 2

## Problem 3.4
# What is the training set accuracy of spamLog, using a threshold of 0.5
table(train$spam, predTrainLog > 0.5)
accuracy = (3052+954)/nrow(train)
accuracy  # 0.9990025

## Problem 3.5
# What is the training set AUC of spamLog
ROCRPredTrain = prediction(predTrainLog, train$spam)
auc = as.numeric(performance(ROCRPredTrain, "auc") @ y.values)
auc  # 0.9999959

## Problem 3.6
# What is the training set accuracy of spamCART, using a threshold of 0.5
table(train$spam, predTrainCART > 0.5)
accuracy = (2885+894)/nrow(train)
accuracy  # 0.942394

## Problem 3.7
# What is the training set AUC of spamCART
predictionTrainCART = prediction(predTrainCART, train$spam)
auc = as.numeric(performance(predictionTrainCART, "auc") @ y.values)
auc  # 0.9696044

## Problem 3.8
# What is the training set accuracy of spamRF, using a threshold of 0.5
table(train$spam, predTrainRF >= 0.5)
accuracy = (3013+914)/nrow(train)
accuracy  # 0.9793017

## Problem 3.9
# What is the training set AUC of spamRF
predictionTrainRF = prediction(predTrainRF, train$spam)
auc = as.numeric(performance(predictionTrainRF, "auc") @ y.values)
auc  # 0.9979116

#############
## Summary ##
#############

## Accuracy
# CART = 0.942394
# RF = 0.9793017
# Log Reg = 0.9990025

## AUC
# CART = 0.9696044
# RF = 0.9979116
# Log Reg = 0.9999959

## Problem 4.1
# Obtain predicted probabilities for the testing set for each model
predTestCART = predict(spamCART, newdata = test)[,2]
predTestRF = predict(spamRF, newdata = test, type = "prob")[,2]
predTestLog = predict(spamLog, newdata = test, type = "response")
# What is the testing set accuracy of spamLog, using a threshold of 0.5
table(test$spam, predTestLog > 0.5)
accuracy = (1257+376)/nrow(test)
accuracy  # 0.9505239

## Problem 4.2
# What is the testing set AUC of spamLog
ROCRPredTest = prediction(predTestLog, test$spam)
auc = as.numeric(performance(ROCRPredTest, "auc") @ y.values)
auc  # 0.9627517

## Problem 4.3
# What is the testing set accuracy of spamCART, using a threshold of 0.5
table(test$spam, predTestCART > 0.5)
accuracy = (1228+386)/nrow(test)
accuracy  # 0.9394645

## Problem 4.4
# What is the testing set AUC of spamCART
predictionTestCART = prediction(predTestCART, test$spam)
auc = as.numeric(performance(predictionTestCART, "auc") @ y.values)
auc  # 0.963176

## Problem 4.5 
# What is the testing set accuracy of spamRF, using a threshold of 0.5
table(test$spam, predTestRF >= 0.5)
accuracy = (1290+386)/nrow(test)
accuracy  # 0.975553

## Problem 4.6
# What is the testing set AUC of spamRF
predictionTestRF = prediction(predTestRF, test$spam)
auc = as.numeric(performance(predictionTestRF, "auc") @ y.values)
auc  # 0.9975656

#############
## Summary ##
#############

## Accuracy
# CART = 0.9394645
# RF = 0.975553
# Log Reg = 0.9505239

## AUC = 
# CART = 0.963176
# RF = 0.9975656
# Log Reg = 0.9627517

##########
## Note ##
##########

# The logistic regression model demonstrated the greatest degree of overfitting because the model obtained nearly
# perfect accuracy and AUC on the training set but far-from-perfect accuracy and AUC on the testing set.






















