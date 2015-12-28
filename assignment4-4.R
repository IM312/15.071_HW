## edX MITx 15.071x The Analytics Edge
## Assignment 4-4 Understanding Why People Vote
## Dataset --> gerber.csv

## Load libraries
library(ROCR)
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)


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
# of predicting that someone will not vote. Additionally, the AUC is low

## Problem 2.1
# Build a CART tree using all data and the same four treatment variables. Don't use the option method = "class"
mod2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
# Plot the tree
fancyRpartPlot(mod2) # No splits!

## Problem 2.2
# Build a new tree using the following command:
mod3 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
fancyRpartPlot(mod3)

## Problem 2.3
# From the tree, what fraction of "Civic Duty" people voted
# 0.31

## Problem 2.4
# Make a new tree that includes the "sex" variable
mod4 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
fancyRpartPlot(mod4)
# or
prp(mod4)

## Problem 3.1
# Create a tree with just the control variable
mod5 = rpart(voting ~ control, data = gerber, cp = 0.0)
prp(mod5, digits = 6)
# What is the absolute value difference in the predicted probabiliy of voting between being in the control group versus 
# being in a different group
abs(0.296638-0.34)
# Create a tree with the control and sex variables
mod6 = rpart(voting ~ control + sex, data = gerber, cp = 0.0)
fancyRpartPlot(mod6)
prp(mod6, digits = 6)
abs(0.334176-0.345818)  

####################
## Interpretation ##
####################

# Men and women are affected about the same by NOT being in the control group. For women, not being in the control group 
# increases the fraction voting by (0.334176-0.290456=0.04372). For men, not being in the control group increases the 
# fraction voting by (0.345818-0.302795=0.04302).

## Problem 3.3
# Create a logistic regression model using "sex" and "control"
mod7 = glm(voting ~ sex + control, data = gerber, family = "binomial")
summary(mod7)

####################
## Interpretation ##
####################

# The sex variable in negative, reflecting that women are less likely to vote

## Problem 3.4
# Evaluate the interaction of the Woman and Control variable in the logistic regression model (log reg can't consider
# exactly the joint possibility of being a women in the control group)
Possibilities = data.frame(sex=c(0,0,1,1), control = c(0,1,0,1))
# Evaluate the logistic regression model using the predict function
predict(mod7, newdata = Possibilities, type = "response")
# What is the absolute difference between the tree and the logistic regression model for the (Woman, Control) case
abs(0.290456-0.2908065)

## Problem 3.5
# Add a new term to the logistic regression, that is the combination of the Sex and control variables - so that if this
# new variable is 1, that means the person is a woman AND in the control group
mod8 = glm(voting ~ sex + control + sex:control, data = gerber, family = "binomial")
# How do you interpret the coefficient for the new variable in isolation
summary(mod8)

####################
## Interpretation ##
####################

# The sex:control variable is negative, relfecting that a woman in the control group is less likely to vote

## Problem 3.6
# Evaluate the new logistic regression model using the predict function
predict(mod8, newdata = Possibilities, type = "response")
# What is the difference between the log reg model and the CART model for the (Woman, Control) case
abs(.290456-.2904558)

####################
## Interpretation ##
####################

# The difference between the logistic regression model and the CART model is practically zero

## Problem 3.7
# By using a combination of two variables, it's possible to capture non-linear relationships with logistic regression.
# However, not all possible interaction terms should be included in a logistic regression model due to the potential for
# overfitting

## DONE!!








