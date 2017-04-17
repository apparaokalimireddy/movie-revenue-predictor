# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rvest)
library(ggplot2)
library(scales)
library(chron)
library(timeDate)
library(reshape2)
source("loadAndPrepare.R")
source("regressionAnalysis.R")
source("util.R")

shinyServer(function(input, output) {
  analysisData <- reactive({
    err.handler <- function(err){
      stop("Unable to retrieve data or data do not have enough samples")
    }
    movieId<-numbersMovieId(input$searchterm)
    df <- tryCatch(loadFromNumbers(movieId), error=err.handler)
    c(movieId, mrAnalysis(df))
  })

  output$movieName <- renderText({
    data<-analysisData()
    movieDisplayText(data[[1]])
  })
  
  output$summaryPlot <- renderPlot({
    data<-analysisData()
    data[[2]]
  })

  output$summaryTable <- renderTable({
    data<-analysisData()
    data[[5]]$PredictedRevenues<-paste("$",format(round(data[[5]]$PredictedRevenues), big.mark=","),sep="")
    data[[5]]<-data[[5]][22:28, c("Day", "Weekday", "PredictedRevenues")] #Show only future predictions
  }, digits=0)
  
  output$keyStats <- renderTable({
    data<-analysisData()
    dat<-data[[8]]
    autocor<-data[[6]]
    stat_names<-c("R Squared", "Adj R Squared", "Std Error", "Autocorrelation")
    stat_values<-c(dat$r.squared, dat$adj.r.squared, dat$sigma, autocor)
    dat<-data.frame(stat_names, stat_values)
  }, include.colnames=FALSE)

    output$dataTable <- renderTable({
    data<-analysisData()
    data[[5]]$Date<-format(data[[5]]$Date,'%Y-%m-%d')
    data[[5]]$Revenues<-paste("$",format(round(data[[5]]$Revenues), big.mark=","),sep="")
    data[[5]]$PredictedRevenues<-paste("$",format(round(data[[5]]$PredictedRevenues), big.mark=","),sep="")
    data[[5]] # Show complete data including sample and predictions
  }, digits=0)
  
})
