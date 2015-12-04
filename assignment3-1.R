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









