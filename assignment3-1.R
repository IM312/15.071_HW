## edX MITx 15.071x The Analytics Edge
## Assignment 3-1 Popularity of Music Records
## Dataset --> songs.csv

## Problem 1.1
# Load the dataset
songs = read.csv("songs.csv")
str(songs)
# How many obs (songs) are from the year 2010
table(songs$year)
## Problem 1.2
# How many songs does the dataset include for Michael Jackson
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)
## Problem 1.3
# Which songs by MJ made it to the Top 10
MichaelJackson[c("songtitle", "Top10")]
## Problem 1.4
# What are the values of the time signature variable
table(songs$timesignature)
## Problem 1.5
# What is the song with the highest tempo
which.max(songs$tempo)
songs$songtitle[6206]


## Problem 2.1
# Split the data into training and test sets
SongsTrain = subset(songs, year<= 2009)
SongsTest = subset(songs, year==2010)
# How many obs are in the training set
str(SongsTrain)
## Problem 2.2
# Create the prediction model using only numerical variables
# Define a vector of vars that won't be used in the model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
# Remove nonvars from training and test sets
SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]
# Build logistic regression model using glm function
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)














