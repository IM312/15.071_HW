## edX MITx 15.071x The Analytics Edge
## Assignment 4-2 Predicting Earnings from Census Data
## Dataset --> census.csv

## Libraries
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(randomForest)


## Problem 1.1
# Load the dataset
census = read.csv("census.csv")
str(census)
# Split the data
spl = sample.split(census$over50k, SplitRatio = 0.6)
set.seed(2000)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)
# Build a logistic regression model using all of the independent variables to predict "over50k"
mod1 = glm(over50k ~ ., data = train, family = binomial)
# What variables are significant
summary(mod1)

## Problem 1.2
# What is the accuracy of the model on the testing set using a threshold of 0.5
predictTest = predict(mod1, newdata = test, type = "response")
table(test$over50k, predictTest > 0.5)
accuracy = (9051+1888)/nrow(test)
accuracy  # 0.855

## Problem 1.3
# What is the baseline accuracy for the testing set
table(test$over50k)
accuracy = 9713/nrow(test)
accuracy

## Problem 1.4
# What is the AUC for this model on the test set
ROCRpred = prediction(predictTest, test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc  # 0.906

# ROCR Plot
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))


#####################
## Interpretation ##
####################
# Given a random individual from the dataset who earned over $50k and a random indivdual from the dataset who did not earn
# over $50k, the model will correctly classify which is which 90.6% of the time.

## Problem 2.1
# Build a CART model using the default parameters
mod2 = rpart(over50k ~., data = train, method = "class")
# Plot the tree. How many splits does the tree have
fancyRpartPlot(mod2)
# or
prp(mod2)
# or
plot(mod2)
text(mod2)

## Problem 2.2
# The first level split is on the "relationship" variable

## Problem 2.3
# The second level split is on the "education" and "capitalgain" variables

## Problem 2.4
# What is the accuracy of the CART model on the testing set (threshold=0.5)
predictMod2 = predict(mod2, newdata = test, type = "class")
table(test$over50k, predictMod2)
accuracy = (9224+1558)/nrow(test)
accuracy  # 0.843

## Problem 2.5
# Plot the ROC curve for the CART model
predictROC = predict(mod2, newdata = test)
ROCRpred = prediction(predictROC [,2], test$over50k)
ROCRperf = performance(pred, "tpr", "fpr")
plot(ROCRperf)

## Problem 2.6
# What is the AUC of the CART model on the test set
AUC = as.numeric(performance(ROCRpred, "auc")@y.values)
AUC

## Problem 3.1
# Down-sample the training set
set.seed(1)
trainSmall = train[sample(nrow(train), 2000),]
str(trainSmall)
# Try building the random forest model
mod3 = randomForest(over50k ~ ., data = trainSmall)
# No error?!?

## Problem 3.2
# Rebuild the model without the "nativecountry" variable
mod4 = randomForest(over50k ~ . - nativecountry, data = trainSmall)
# What is the accuracy of the model on the test set
set.seed(1)
predictForest = predict(mod4, newdata = test)
table(test$over50k, predictForest)
accuracy = (8963+1914)/nrow(test)
accuracy

## Problem 3.3
# Look at the number of times, aggregated over all of the trees in the random forest model, that a certain variable is
# selected for a split
vu = varUsed(mod4, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(mod4$forest$xlevels[vusorted$ix]))
# The most important variable in terms of splits is "age".

## Problem 3.4
# Compute the "impurity" metric, which measures how homogenous each bucket of the tree is. Whenever a variable is selected
# and a split is performed, the impurity is dereased. One way to measure variable importance is to average the 
# reduction in impurity, taken over all the times that a variable is selected for splitting
varImpPlot(mod4)
# "Occupation" should be the most important in terms of mean reduction in impurity

## Problem 4.1



















