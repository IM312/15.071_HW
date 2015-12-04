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

## Problem 2.1
# Plot CocaCola's stock prices
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

## Problem 2.2 
# Add a line for Procter & Gamble
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

## Problem 2.3
# Add a vertical line at a certain date
abline(v=as.Date(c("1983-01-01")), lwd=2)

## Problem 3.1
# Look at how stock prices changes from 1995-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(1,210))
# Plot the obs for the other companies
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
# Which stock fell the most in March 2000
abline(v=as.Date((c("2000-03-01"))))
## Problem 3.3
# Comaring Sept 1997 to Nov 1997, which companis saw a decreasing trend in their stock price
abline(v=as.Date((c("1997-09-01"))))
abline(v=as.Date((c("1997-11-01"))))

## Problem 4.1
# Use tapply to calculate the mean stock price of IBM, sorted by months
tapply(IBM$StockPrice, months(IBM$Date), mean)
# Compare the monthly averages to the overall average stock price
summary(IBM$StockPrice)
# Repeat the tapply for the remaining companies
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
# the above functions are only returning the mean for Dec???

## DONE




