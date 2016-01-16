## edX MITx 15.071x The Analytics Edge
## Assignment 6-3 Predicting Medical Costs with Cluster-then-Predict
## Dataset --> reimbursement.csv

## Load libraries
library(caret)
library(flexclust)



## Problem 1.1 
# Load the dataset
claims = read.csv("reimbursement.csv")
# How many Medicare beneficiaries are included in the dataset
str(claims)

## Problem 1.2
# What proportion of patients have at least one of the chronic conditions described
has.condition = subset(claims, alzheimers == 1 | arthritis == 1 | cancer == 1 | copd == 1 | 
                           depression == 1 | diabetes == 1 | heart.failure == 1 | ihd == 1 |
                           kidney == 1 | osteoporosis ==1 | stroke == 1)
nrow(has.condition)
proportion = 280427/458005
proportion

## Problem 1.3
# What is the max correlation between independent variables in the dataset
cor(claims)  # Recommended solution doesn't answer the question
cor(claims$ihd, claims$diabetes)

## Problem 1.4
# Plot the histogram of the dependent variable
hist(claims$reimbursement2009, col=2)

## Problem 1.5
# Log transform the two reimbursement variables
claims$reimbursement2008 = log(claims$reimbursement2008+1)
claims$reimbursement2009 = log(claims$reimbursement2009+1)
# +1 is used to avoid log-transformed values of negative infinity for patients with $0 reimbursement

## Problem 1.6
# Plot the histogram of the log-transformed dependent variable
hist(claims$reimbursement2009, col=1)
# What proportion of beneficiaries had $0 in reimbursements in 2009
no.reimb = subset(claims, reimbursement2009 == 0)
nrow(no.reimb)
proportion = 90498/458005
proportion

## Problem 2.1
# Randomly select 70% of the data for a training set and 30% for a testing set
set.seed(144)
spl = sample(1:nrow(claims), size = 0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]
# Use train data frame to train a linear reg model to predict reimbursement2009
lm.claims = lm(reimbursement2009 ~ ., data = train)
# What is the training set Multiple R-squared of lm.claims
summary(lm.claims)

## Problem 2.2
# Obtain testing set predictions from lm.claims
predictions = predict(lm.claims, newdata = test)
# What is the testing set RMSE of the model
RMSE = sqrt(mean((predictions - test$reimbursement2009)^2))
RMSE  # 1.849212
# or
SSE = sum((predictions - test$reimbursement2009)^2)
RMSE = sqrt(SSE/nrow(test))
RMSE

## Problem 2.3
# What is the naive baseline model of lm.claims
NBM = mean(train$reimbursement2009)
NBM  # 6.101444

## Problem 2.4
# What is the testing set RMSE of the naive baseline model
RMSE = sqrt(mean((NBM - test$reimbursement2009)^2))
RMSE  # 3.593273

## Problem 2.5
# What is the testing set RMSE of the smart baseline model (reimbursement2009 = reimbursement2008)
RMSE = sqrt(mean((test$reimbursement2009 - test$reimbursement2008)^2))
RMSE  # 2.094668

## Problem 3.1 - Clustering medicare beneficiaries
# Remove the dependent variable
train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

## Problem 3.2
# Normalize by the mean and standard deviation of the variables in the training set
prepoc = preProcess(train.limited)
train.norm = predict(prepoc, train.limited)
test.norm = predict(prepoc, test.limited)
# What is the mean of the arthritis variable in train.norm
mean(train.norm$arthritis)
# What is the mean of the arthritis variable in test.norm
mean(test.norm$arthritis)

## Problem 3.4
# Run the k-means clustering with 3 clusters on train.norm
set.seed(144)
km = kmeans(train.norm, centers = 3)
# Which cluster can be described as "older-than-average beneficiaries with below average incidence
# of stroke and above-average 2008 reimbursements
km$centers

## Problem 3.5
# Use flexclust package to obtain training set and testing set cluster assignments
km.kcaa = as.kcca(km, train.norm)
cluster.train = predict(km.kcaa)
cluster.test = predict(km.kcaa, newdata = test.norm)
# How many test-set observations were assigned to Cluster 2
table(cluster.test)

## Problem 4.1
# Build data frames containing the elements in the train data frame assigned to the clusters
train1 = subset(train, cluster.train == 1)
train2 = subset(train, cluster.train == 2)
train3 = subset(train, cluster.train == 3)
# Similarly, build the data frames for the test data
test1 = subset(test, cluster.test == 1)
test2 = subset(test, cluster.test == 2)
test3 = subset(test, cluster.test == 3)
# Which training set data frame has the highest average value of the dependent variable
summary(train1$reimbursement2009)
summary(train2$reimbursement2009)
summary(train3$reimbursement2009)

## Problem 4.2
# Build linear regression models, which predict reimbursement2009 using all the variables
lm1 = lm(reimbursement2009 ~ ., data = train1)
lm2 = lm(reimbursement2009 ~ ., data = train2)
lm3 = lm(reimbursement2009 ~ ., data = train3)
# Which variables have a positive sign for the coefficient in at least one of lm1, lm2, lm3 and
# a negative sign for at least on of lm1, lm2, and lm3
summary(lm1)
summary(lm2)
summary(lm3)

## Problem 4.3
# Make test-set predictions
pred.test1 = predict(lm1, newdata = test1)
pred.test2 = predict(lm2, newdata = test2)
pred.test3 = predict(lm3, newdata = test3)
# Which vector of test-set predictions has the smallest average predicted reimbursement amount
summary(pred.test1)
summary(pred.test2)
summary(pred.test3)

## Problem 4.4
# Obtain the test-set RMSE of each cluster
RMSE1 = sqrt(mean((pred.test1 - test1$reimbursement2009)^2))
RMSE2 = sqrt(mean((pred.test2 - test2$reimbursement2009)^2))
RMSE3 = sqrt(mean((pred.test3 - test3$reimbursement2009)^2))
# Which cluster has the largest test-set RMSE
RMSE1
RMSE2
RMSE3

## Problem 4.5
# Combine all the test-set predictions into a single vector and all the true outcomes into a 
# single vector
all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
# What is the test-set RMSE of the cluster-then-predict approach
RMSE = sqrt(mean((all.predictions - all.outcomes)^2))
RMSE

## DONE!!









