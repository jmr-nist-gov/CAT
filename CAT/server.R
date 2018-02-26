#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tidyverse)
require(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  tic <- reactive(read.csv(req(input$tic$datapath), stringsAsFactors = TRUE, header = TRUE))
  analytes <- reactive({
    read.csv(req(input$analytes$datapath), stringsAsFactors = TRUE, header = TRUE) %>%
      mutate(VJ = as.numeric(Type)-2)
  })
  plotIt <- function() {
    if (is.null(input$analytes)) {
      ggplot(tic(), aes(x = Time, y = Abundance)) +
        geom_line() +
        theme_classic() +
        theme(axis.title.x = element_blank())
    } else {
      yLabs <- sapply(analytes()$RT, 
                      function(x) {
                        tic() %>% 
                          filter(between(Time, x-0.1, x+0.1)) %>%
                          pull(Abundance) %>%
                          max()
        })
      ggplot(tic(), aes(x = Time, y = Abundance)) +
        geom_vline(data = analytes(), aes(xintercept = RT, colour =
                                            Type)) +
        geom_text(data = analytes(), 
                  aes(x = RT, y = yLabs*1.01, label = Analyte, colour = Type, vjust = VJ), hjust=1) +
        geom_line() +
        theme_classic() +
        coord_cartesian(ylim=c(0, max(tic()$Abundance)*1.1)) +
        theme(axis.title.x = element_blank(),
              legend.position = "bottom")+
        labs(colour="Expected RT of:")
    }
  }
  
  output$chromview <- renderPlot({
    p <- plotIt()
    p$layers <- p$layers[-2]
    p + guides(colour = "none")
  })
  output$chromatogram <- renderPlot({
    plotIt() +
      coord_cartesian(xlim = c(min(tic()$Time),  max(tic()$Time) / 4)) +
      guides(colour = "none")
  })
  output$peakview <- renderPlot({
    plotIt() + 
      coord_cartesian(xlim = c(min(tic()$Time), min(tic()$Time) + 2)) +
      labs(x = "Time (min)")
  })
  
  observeEvent({
    input$view_brush
  }, {
    xmin <- input$view_brush$xmin
    xmax <- input$view_brush$xmax
    ymax <-
      tic() %>% filter(Time >= xmin &
                         Time <= xmax) %>% pull(Abundance) %>% max()
    output$chromatogram <- renderPlot({
      plotIt() +
        coord_cartesian(xlim = c(xmin, xmax),
                        ylim = c(0, ymax*1.01)) +
        guides(colour = "none")
    })
  })
  observeEvent({
    input$chrom_click
  }, {
    x <- input$chrom_click$x
    xmod <- 0.1
    ymax <-
      tic() %>% filter(between(Time, x - xmod, x + xmod)) %>% pull(Abundance) %>% max()
    xact <- tic() %>% filter(Abundance == ymax) %>% pull(Time)
    desc <- paste("Actual RT: ", round(xact, 3))
    if (!is.null(input$analytes)) {
      xexp <- analytes() %>% filter(between(RT, x-xmod, x+xmod)) %>% pull(RT)
      peakName <- analytes() %>% filter(between(RT, x-xmod, x+xmod)) %>% pull(Analyte)
      if (is.na(peakName[1])) {
        desc <- paste("Unidentified Peak - ", desc, collapse = "<br/>")
      } else {
        desc <- paste(peakName, " - ", desc, "/ Expected RT: ", xexp, collapse="<br/>")
      }
    } else {
      xexp <- NULL
      peakName <- NULL
    }
    peakPlot <- plotIt() +
      geom_vline(xintercept = xact,
                 linetype = "longdash",
                 colour = "black") +
      coord_cartesian(xlim = c(x - 0.25, x + 0.25),
                      ylim = c(0, ymax * 1.01)) +
      labs(x = "Time (min)")
    output$peakview <- renderPlot(peakPlot)
    output$peakDescribe <- renderUI(HTML(desc))
    output$chromatogram <- renderPlot({
      xmin <- isolate(input$view_brush$xmin)
      xmax <- isolate(input$view_brush$xmax)
      ymax <- tic() %>% filter(between(Time, xmin, xmax)) %>% pull(Abundance) %>% max()
      plotIt() +
        coord_cartesian(xlim = c(xmin, xmax),
                        ylim = c(0, ymax*1.01)) +
        annotate("rect", xmin = (x-0.25), xmax = (x+0.25), ymin = -Inf, ymax = Inf, fill="grey50", alpha=0.1) +
        guides(colour = "none")
    })
  })
  
})
