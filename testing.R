rm(list = ls())
# Predict future revenues
library(rvest)
library(ggplot2)
library(scales)
library(chron)
library(timeDate)
library(reshape2)
source("loadAndPrepare.R")
source("regressionAnalysis.R")
source("util.R")
df <- loadFromNumbers()
#df<-loadFromNumbers("World-Is-Not-Enough-The")
#df<-loadFromNumbers("Grudge-Match")
#df<-loadFromNumbers("Moana")
#df<-loadFromNumbers("Interstellar")
#df<-loadFromNumbers("John-Wick")
#df<-loadFromNumbers("American-Sniper")
#df<-loadFromNumbers("Kong-Skull-Island")

srData<-srAnalysis(df)
mrData<-mrAnalysis(df)

#Example access 
srData[[1]] #get simple regression trend chart
mrData[[1]] #gets multiple regression chart actual vs Predicted

