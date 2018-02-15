#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if (!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
require(shiny)
require(rbokeh)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Chromatogram Annotation Tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       fileInput(inputId="tic",
                 label="Total Ion Chromatogram:",
                 multiple=TRUE,
                 placeholder="No file selected"),
       fileInput(inputId="analytes",
                 label="Analyte Retention Times:",
                 multiple=TRUE,
                 placeholder="No file selected"),
       selectInput(inputId="format",
                   label="Output format:",
                   choices=c(".png", ".jpg", ".tif"),
                   selected=".tif")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("test"),
      #rbokehOutput("chromatogram", width="100%", height="100%")
      fluidRow(
        column(8,
               plotOutput("chromatogram", click="chrom_click", width="100%", height="400px"),
               plotOutput("chromview", brush=brushOpts("view_brush", direction="x"), width="100%", height="100px")
               ),
        column(4,
               plotOutput("peakview", height="500px")
        )
      )
    )
  )
))
