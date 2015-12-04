## edX MITx 15.071x The Analytics Edge
## Assignment 1-2 Stock Dynamics

## Load data
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

## Problem 1.1 Summary Statistics

# Look at data structure
str(IBM)
str(GE)
str(ProcterGamble)
str(CocaCola)
str(Boeing)

# Convert Date factor to an object
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%M/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%M/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%M/%d/%y")

# How many obs are there in each data set?
str(IBM)

## Problem 1.2-4
# What is the earliest year in our datasets?
# What is the latest year in our datasets?
# What is the mean stock price of IBM?
summary(IBM)

## Problem 1.5
# What is the minimum stock price of GE?
summary(GE)

## Problem 1.6
# What is the maximum stock price of CocaCola?
summary(CocaCola)

## Problem 1.7
# What is the median stock price of Boeing?
summary(Boeing)

## Problem 1.8
# What is the standard deviation of the stock price of Procter & Gamble?
sd(ProcterGamble$StockPrice)



