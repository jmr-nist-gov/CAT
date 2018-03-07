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
require(shinythemes)
require(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(#theme=shinytheme("flatly"),
  useShinyjs(),
  fluidRow(
    titlePanel(NULL, "CAT v0.5"),
    column(12, h1("Chromatogram Annotation Tool")),
    # hr(),
    # 
    # # Sidebar with a slider input for number of bins 
    # # sidebarLayout(
    # # sidebarPanel(
    column(12,
           hr(),
           h4("Load files"),
           fluidRow(
             column(6,
                    fileInput(inputId = "tic",
                              label = NULL,
                              multiple = TRUE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                              placeholder = "Total Ion Chromatogram",
                              width = "100%")),
             column(6, 
                    fileInput(inputId = "analytes",
                              label = NULL,
                              multiple = TRUE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                              placeholder = "Analyte Retention Times",
                              width = "100%"))
             # selectInput(inputId = "format",
             #             label = "Output format:",
             #             choices = c(".png", ".jpg", ".tif"),
             #             selected = ".tif")
           )
    )
  ),
  hr(),
  # Show a plot of the generated distribution
  # mainPanel(
  shinyjs::hidden(
    div(id = "mask",
      fluidRow(
        column(7,
               h3("Chromatogram"),
               p("Navigate (drag to zoom)"),
               plotOutput("chromview", 
                          brush = brushOpts("view_brush", direction = "x", delayType = "debounce"), 
                          width = "100%", height = "150px"),
               p("Click a peak to view it"),
               plotOutput("chromatogram", 
                          click = "chrom_click", 
                          width = "100%", height = "425px")
        ),
        column(5,
               h3("Peak View"),
               htmlOutput("peakDescribe"),
               plotOutput("peakview", 
                          dblclick = "peak_dblclick",
                          height = "600px"),
               actionButton(inputId = "annotateMore",
                            label = "Annotate this peak",
                            icon = icon("pencil"),
                            width = "100%")
               )
      ),
      fluidRow(
        column(12,
               actionButton(inputId = "saveIt",
                            label = "Save Annotations",
                            icon = icon("download"),
                            width = "100%")
        )
      )
    )
  )
)
)
