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

## AUC 
# CART = 0.963176
# RF = 0.9975656
# Log Reg = 0.9627517

##########
## Note ##
##########

# The logistic regression model demonstrated the greatest degree of overfitting because the model obtained nearly
# perfect accuracy and AUC on the training set but far-from-perfect accuracy and AUC on the testing set.

############# 
## Part II ##
############

## Problem 5.1
# False positive --> test indicates an email is spam, but it is in fact not spam. For a user that does not check their 
# spam folder often this type of error can be costly.
# False negative --> test indicates an email is not spam, but it is in fact spam. For a user who is particularly 
# annoyed by spam in their inbox, this type of error would be costly.

## Problem 6.1
# Obtain the word counts for each email
wordCount = rowSums(as.matrix(dtm))

## Problem 6.2
# Plot the distribution of wordCount in the dataset
hist(wordCount)  # Nearly all of the observations are in the left of the graph --> the distribution is skew right

## Problem 6.3
# Plot the distribution of log(wordCount)
hist(log(wordCount))  # The data is not skewed

## Problem 6.4
# Create a variable that is equal to log(wordCount)
emailsSparse$logWordCount = log(wordCount)
# Plot logWordCount against whether a message is spam
boxplot(logWordCount ~ spam, data = emailsSparse)
# or
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

## Problem 6.5
# Hypothesis: because logWordCount differs between spam and ham messages, it might be useful in predicting whether an 
# email is spam

# Split emailsSparse into new training and testing sets
train2 = subset(emailsSparse, split == TRUE)
test2 = subset(emailsSparse, split == FALSE)
# Train a CART tree using train2 and the default parameters
spam2CART = rpart(spam ~ ., data = train2, method = "class")
# Train a random forest with the default parameters
set.seed(123)
spam2RF = randomForest(spam ~ ., data = train2, method = "class")
# Is the new logWordCount variable integrated into the tree
prp(spam2CART)

## Problem 6.6
# Perform test-set predictions using the new CART and random forest models
predTest2CART = predict(spam2CART, newdata = test2)[,2]
predTest2RF = predict(spam2RF, newdata = test2, type = "prob")[,2]
# What is the test-set accuracy of spam2CART, using a threshold of 0.5
table(test2$spam, predTest2CART > 0.5)
accuracy = (1214+384)/nrow(test2)
accuracy  # 0.9301513

## problem 6.7
# What is the test-set AUC of spam2CART
predictionTest2CART = prediction(predTest2CART, test2$spam)
auc = as.numeric(performance(predictionTest2CART, "auc") @ y.values)
auc  # 0.9582438

## Problem 6.8
# What is the test-set accuracy of spam2RF, using threshold 0.5
table(test2$spam, predTest2RF >= 0.5)
accuracy = (1296+383)/nrow(test2)
accuracy  # 0.9772992

## Problem 6.9
# What is the test-set AUC of spam2RF
predictionTest2RF = prediction(predTest2RF, test2$spam)
auc = as.numeric(performance(predictionTest2RF, "auc") @ y.values)
auc  # 0.9980905

#############
## Summary ##
#############

## Accuracy
# CART = 0.9301513
# RF = 0.9772992

## AUC 
# CART = 0.9582438
# RF = 0.9980905

##########
## Note ##
##########

# Adding the logWordCount variable didn't result in improved results on the test set for the CART or random forest model

## Problem 7.1




