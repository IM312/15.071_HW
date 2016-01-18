## edX MITx 15.071x The Analytics Edge
## Kaggle Comp - What makes an eBay listing succesful
## Dataset --> 

## Load libraries
library(tm)


# Read in the data
train = read.csv("eBayiPadTrain.csv", stringsAsFactors = FALSE)
test = read.csv("eBayiPadTest.csv", stringsAsFactors = FALSE)
summary(train)

# Simple model excluding description and unique variables
nonvars = c("description", "UniqueID")
ExTrain = train[,!(names(train) %in% nonvars)]
SimpMod = glm(sold ~ ., data = ExTrain, family = binomial)
summary(SimpMod)

# Make Predictions
PredTest1 = predict(SimpMod, newdata = test, type = "response")

 
# Generate submission file
Submission = data.frame(UniqueID = test$UniqueID, Probability1 = PredTest2)
write.csv(Submission, "submission2.csv", row.names = FALSE)

# 





