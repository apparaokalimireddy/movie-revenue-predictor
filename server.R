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
    c(movieId, mrAnalysis(df), srAnalysis(df), df)
  })

  # Render movie image
#  output$sideImage <- renderImage({
#    movieId = analysisData()[[1]]
#    return(list(
#      src = paste("http://www.the-numbers.com/images/movies/opusdata/", movieId, ".jpg", sep=''),
#      filetype = "image/jpeg",
#      alt = ""
#   ))
#  })
  output$sideImage<-renderText({
    movieId = analysisData()[[1]]
    err.handler.img<-function(err) {return("https://upload.wikimedia.org/wikipedia/en/d/dc/Academy_Award_trophy.jpg")}
    src = tryCatch(paste("http://www.the-numbers.com/images/movies/opusdata/", movieId, ".jpg", sep=''), error=err.handler.img)
    c('<br/><img width=75% src="',src,'">')
  })
  
  output$movieName <- renderText({
    data<-analysisData()
    movieDisplayText(data[[1]])
  })

  #
  # Summary Tab output
  #
  output$summaryPlot <- renderPlot({
    data<-analysisData()
    data[[2]]
  })

  output$summaryTable <- renderTable({
    data<-analysisData()[[5]]
    
    data$PredictedRevenues<-paste("$",format(round(data$PredictedRevenues), big.mark=","),sep="")
    data<-data[22:28, c("Day", "Weekday", "PredictedRevenues")] #Show only future predictions
  }, digits=0, bordered = TRUE, spacing = 'xs')
  
  output$keyStats <- renderTable({
    data<-analysisData()[[8]]
    autocor<-analysisData()[[6]]
    stat_names<-c("R Squared", "Adj R Squared", "Std Error", "Autocorrelation")
    stat_values<-c(data$r.squared, data$adj.r.squared, data$sigma, autocor)
    data<-data.frame(stat_names, stat_values)
  }, include.colnames=FALSE, bordered=TRUE, spacing = 'xs')

  output$dataTable <- renderTable({
    data<-analysisData()[[5]]
    data$Date<-format(data$Date,'%Y-%m-%d')
    data$Revenues<-paste("$",format(round(data$Revenues), big.mark=","),sep="")
    data$PredictedRevenues<-paste("$",format(round(data$PredictedRevenues), big.mark=","),sep="")
    data # Show complete data including sample and predictions
  }, digits=0, bordered=TRUE, spacing = 'xs')
  
  #
  # Multiple Regression Tab output
  #
  # Key Statistics
  output$keyStatsAnalysis <- renderTable({
    data<-analysisData()[[8]]
    autocor<-analysisData()[[6]]
    stat_names<-c("R Squared", "Adj R Squared", "Std Error", "Autocorrelation")
    stat_values<-c(data$r.squared, data$adj.r.squared, data$sigma, autocor)
    data<-data.frame(stat_names, stat_values)
  }, include.colnames=FALSE, bordered = TRUE, spacing = 'xs')
  
  # Analysis of Variance
  output$anova <- renderTable({
    d<-as.data.frame(analysisData()[[7]])
    d
  }, bordered = TRUE, spacing = 'xs')
  
  # Coefficients
  output$coeff <- renderTable({
    # Get Matix of coefficients and convert to data frame
    d<-as.data.frame(analysisData()[[8]]$coefficients)
    colnames(d)<-c("Estimate", "Std Error", "t value", "Pr(>|t|)")
    d
  }, bordered = TRUE,colnames = TRUE, rownames = TRUE, spacing = 'xs')  
  # Residual Plot
  output$resPlot <- renderPlot({
    data<-analysisData()
    data[[4]]
  })
  
  # Histogram of Residuals
  output$histPlot <- renderPlot({
    data<-analysisData()
    data[[3]]
  })

  #
  # Simple Regression
  #
  # Revenue Trend plot
  output$trendPlot <- renderPlot({
    data<-analysisData()
    data[[11]]
  })
  # Key Statistics for simple regression
  output$keyStatsSimple <- renderTable({
    data<-analysisData()[[16]]
    autocor<-analysisData()[[15]]
    stat_names<-c("R Squared", "Adj R Squared", "Std Error", "Autocorrelation")
    stat_values<-c(data$r.squared, data$adj.r.squared, data$sigma, autocor)
    data<-data.frame(stat_names, stat_values)
  }, include.colnames=FALSE, bordered = TRUE, spacing = 'xs')
  # Residual Plot simple regression
  output$resPlotSimple <- renderPlot({
    data<-analysisData()
    data[[13]]
  })
  
  # Histogram of Residuals - simple regression
  output$histPlotSimple <- renderPlot({
    data<-analysisData()
    data[[12]]
  })

  # Generate report
  output$report <- downloadHandler(
    # For PDF output, change this to "report.html"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(dat = analysisData())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})
