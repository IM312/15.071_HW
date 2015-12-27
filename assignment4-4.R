## edX MITx 15.071x The Analytics Edge
## Assignment 4-4 Understanding Why People Vote
## Dataset --> gerber.csv

## Load libraries
library(ROCR)

## Problem 1.1
# Load the data file
gerber = read.csv("gerber.csv")
# What proportion of people voted in the election
summary(gerber$voting)  # This is the baseline accuracy (0.3159)
# or
table(gerber$voting)  #  The baseline is the proprtion who did not vote (0.6841)

## Problem 1.2
# Which treatment group had the largest fraction of voters
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

## Problem 1.3
# Build a logistic regression model for voting using the four treatment group variables
mod1 = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(mod1)

## Problem 1.4
# Using a threshold of 0.3, what is the accuracy of the model
predictGerber = predict(mod1, type = "response")
table(gerber$voting, predictGerber > 0.3)
accuracy1 = (134513+51966)/nrow(gerber)
accuracy1  #0.54196

## Problem 1.5
# What is the accuracy of the model using a threshold of 0.5
table(gerber$voting, predictGerber > 0.5)
accuracy2 = (235388+0)/(235388+108696)
accuracy2  # 0.68410

## Problem 1.6
# Compute the AUC of the model
ROCRpred = prediction(predictGerber, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

####################
## Interpretation ##
####################
# Even though all of the variables are significant, the model does not improve over the baseline
# of predicting that someone will not vote, and the AUC is low

## Problem 2.1









