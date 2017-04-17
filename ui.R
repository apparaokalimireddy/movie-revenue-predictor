
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
      textInput("searchterm", "Enter Movie Name:", ""),
      submitButton("Submit")
      ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      h3(textOutput("movieName")),
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", fluidRow(
                    column(12,plotOutput("summaryPlot")), 
                    column(6,h4("7 day forecast"), tableOutput("summaryTable")),
                    column(6,h4("Key Statistics"), tableOutput("keyStats"))
                  )
                  ),
                  tabPanel("Analysis", verbatimTextOutput("analysis")), 
                  tabPanel("Data", tableOutput("dataTable"))
      )
    )
  )
))
