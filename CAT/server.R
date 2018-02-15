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
  trace <- reactive(read.csv(input$tic$datapath, header=TRUE))
  #analytes <- reactive(read.csv(input$analytes))
  
  # output$test <- renderTable(trace())
   
  # output$chromatogram <- renderRbokeh({
  #   figure() %>% 
  #     ly_lines(trace()$Time, trace()$Abundance)
  # })
  output$chromatogram <- renderPlot({
    ggplot(trace(), aes(x=Time, y=Abundance))+
      geom_line()+
      theme_classic()+
      labs(title="Interactive Chromatogram")+
      theme(axis.title.x=element_blank())
  })
  output$chromview <- renderPlot({
    ggplot(trace(), aes(x=Time, y=Abundance))+
      geom_line()+
      scale_y_log10()+
      theme_classic()+
      labs(x="Time (min)")
    })
  output$peakview <- renderPlot({
    ggplot(trace(), aes(x=Time, y=Abundance))+
      geom_line()+
      theme_classic()+
      labs(title="Peak View",
           x="Time (min)")
    })
  observeEvent({
    input$view_brush},{
    xmin <- input$view_brush$xmin
    xmax <- input$view_brush$xmax
    ymax <- trace() %>% filter(Time>=xmin & Time<=xmax) %>% pull(Abundance) %>% max()
    output$chromatogram <- renderPlot({
      ggplot(trace(), aes(x=Time, y=Abundance))+
        geom_line()+
        theme_classic()+
        coord_cartesian(xlim=c(xmin, xmax),
                        ylim=c(0, ymax))+
        labs(x="Time (min)")
      })
    })
  observeEvent({
    input$chrom_click},{
      x <- input$chrom_click$x
      ymax <- trace() %>% filter(Time>=x-0.1 & Time<=x+0.1) %>% pull(Abundance) %>% max()
      output$peakview <- renderPlot({
        ggplot(trace(), aes(x=Time, y=Abundance))+
          geom_line()+
          theme_classic()+
          coord_cartesian(xlim=c(x-0.5, x+0.5),
                          ylim=c(0, ymax*1.1))+
          labs(x="Time (min)")
        })
      })
  
})
