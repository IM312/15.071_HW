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
