#--------------------------------------------------------------
#
# Chromatogram Annotation Tool (CAT)
#   - Current Version: 0.5
#   - Author: Jared M. Ragland
#   - Affiliation Institute: NIST
#   - Affiliation Division: Chemical Sciences Division (646)
#   - Affiliation Group: Environmental Specimen Bank Group (06)
#   - Affiliation Program: Data Tool Development
#   - Last Updated: 20180301
#   - Contact: jared.ragland@nist.gov
#   - Source: https://github.com/jmr-nist-gov/CAT
#
#--------------------------------------------------------------
#
# A web application to quickly and transparently annotate any
# given chromatographic run with labels for peaks.  Includes
# support for manual annotation and multiple method files for
# complex chromtographic runs with different target analytes.
#
#--------------------------------------------------------------

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(shiny)) install.packages("shiny")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(shinyjs)) install.packages("shinyjs")
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(#theme=shinytheme("flatly"),
  useShinyjs(),
  fluidRow(
    titlePanel(NULL, "CAT v0.5"),
    column(12, 
           column(10, 
                  h1("Chromatogram Annotation Tool")),
           column(2, 
                  actionButton("help", "Instructions", width="100%"),
                  actionButton("about", "About the CAT", width="100%"))
    ),
    column(12,
           hr(),
           h4("Load files"),
           fluidRow(
             column(4,
                    fileInput(inputId = "tic",
                              label = NULL,
                              multiple = TRUE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                              placeholder = "Total Ion Chromatogram",
                              width = "100%")),
             column(4,
                    fileInput(inputId = "ions",
                              label = NULL,
                              multiple = TRUE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                              placeholder = "Selected Ion(s) Chromatogram",
                              width = "100%")),
             column(4, 
                    fileInput(inputId = "analytes",
                              label = NULL,
                              multiple = TRUE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                              placeholder = "Analyte Retention Times",
                              width = "100%"))
           )
    )
  ),
  hr(),
  shinyjs::hidden(
    div(id = "mask",
      fluidRow(
        column(7,
               h3("Chromatogram"),
               p("Click a peak to view it"),
               plotOutput("chromatogram", 
                          click = "chrom_click", 
                          width = "100%", height = "425px"),
               p("Navigate (drag to select a zoom region)"),
               plotOutput("chromview", 
                          brush = brushOpts("view_brush", direction = "x", delayType = "debounce"), 
                          width = "100%", height = "150px")
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
