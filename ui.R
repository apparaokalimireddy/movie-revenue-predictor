
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Movie Revenue Predictor"),

  sidebarLayout(
    sidebarPanel(
      textInput("searchterm", "Enter Movie Name:", randomMovie()),
      submitButton("Submit"),
      htmlOutput("sideImage", align="middle")
      ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      h3(textOutput("movieName")),
      div(downloadButton('report', 'Report'), style="float:right"),
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", fluidRow(
                    column(12,plotOutput("summaryPlot")), 
                    column(6,h4("7 day forecast"), tableOutput("summaryTable")),
                    column(6,h4("Key Statistics"), tableOutput("keyStats"))
                  )),
                  tabPanel("Simple Regression", fluidRow(
                    column(12,h4("Key Statistics"), tableOutput("keyStatsSimple")),
                    column(12, plotOutput("trendPlot")),
                    column(12, plotOutput("resPlotSimple")),
                    column(12, plotOutput("histPlotSimple"))
                  )),
                  tabPanel("Multiple Regression", fluidRow(
                    column(12,h4("Key Statistics"), tableOutput("keyStatsAnalysis")),
                    column(6,h4("Coefficients"), tableOutput("coeff")),
                    column(6,h4("Analysis of Variance"), tableOutput("anova")),
                    column(12,plotOutput("resPlot")), 
                    column(12,plotOutput("histPlot")) 
                  )), 
                  tabPanel("Data", tableOutput("dataTable"))
      )
    )
  )
))
