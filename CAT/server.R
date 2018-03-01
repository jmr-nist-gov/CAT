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

x <- c("shiny", "tidyverse", "DT", "shinyjs", "ggrepel")
lapply(x, require, character.only = TRUE)


shinyServer(function(session, input, output) {
  useShinyjs()
  disable("annotateMore")
  disable("saveIt")
  dat <- reactiveValues()
  
  # Draw line annotations on the overview and zoomed chromatograms
  addAnnotationMarks <- function() {
    if (!is.null(dat$analytes)) {
      dat$analytes <- dat$analytes %>%
        mutate(yLabs = sapply(RT,
                              function(x) {
                                dat$tic %>%
                                  filter(between(Time, x-0.1, x+0.1)) %>%
                                  pull(Abundance) %>%
                                  max()
                              })
        )
      dat$chromOver <- dat$chromOver +
        geom_vline(data = dat$analytes, aes(xintercept = RT, colour = as.factor(Type))) +
        labs(colour = "Expected RT: ")
      dat$chromZoom <- dat$chromZoom +
        geom_segment(data = dat$analytes,
                     aes(x = RT,
                         xend = RT,
                         y = -Inf,
                         yend = yLabs,
                         colour = as.factor(Type)))
    }
  }
  
  # Load a .csv file of the total ion chromatogram trace
  #  - Should expand this to allow for ion specificity
  getTic <- observeEvent(input$tic, {
    shinyjs::toggle("mask", anim = FALSE)
    dat$tic <- read.csv(
      input$tic$datapath,
      stringsAsFactors = TRUE, header = TRUE)
    dat$chromBase <- dat$tic %>%
      ggplot(aes(x = Time, y = Abundance))+
      geom_line()  +
      theme_classic() +
      theme(legend.position = "none") +
      labs(x = "")
    dat$chromZoom <- dat$chromBase +  
      coord_cartesian(ylim=c(0.1, max(dat$tic$Abundance)*1.1),
                      xlim=c(min(dat$tic$Time), max(dat$tic$Time)/4)) +
      xlab("Time (min)")
    dat$chromOver <- dat$chromBase +
      scale_y_log10() +
      theme(legend.position = 'top')
    if (!is.null(input$analytes)) {
      addAnnotationMarks()
    }
  })
  
  # Load a .csv file of expected analytes, their retention times, and their category
  getAna <- observeEvent(input$analytes, {
    tmp <- read.csv(
      input$analytes$datapath,
      stringsAsFactors = FALSE,
      header = TRUE) %>%
      mutate(yLabs = 0)
    if (is.null(dat$analytes)) {
      dat$analytes <- tmp
    } else {
      dat$analytes <- rbind(dat$analytes, tmp)
    }
    if (!is.null(input$tic)) {
      addAnnotationMarks()
    }
  })
  
  # Add annotation labels to the zoomed view - THIS SHOULD BE COLLAPSED INTO A SINGLE FUNCTION WITH LABELPEAK()
  labelZoom <- function() {
    if ("analytes" %in% names(dat)) {
      zLimX <- dat$chromZoom$coordinates$limits$x
      dat$chromZoom +
        geom_text_repel(data = dat$analytes %>% filter(between(RT, zLimX[1]-0.25, zLimX[2]+0.25)),
                        aes(x = RT,
                            y = yLabs*1.05,
                            label = Analyte,
                            colour = as.factor(Type)))
    } else {
      dat$chromZoom
    }
  }
  
  # Add annotation labels to the peak view - THIS SHOULD BE COLLAPSED INTO A SINGLE FUNCTION WITH LABELZOOM()
  labelPeak <- function() {
    if ("analytes" %in% names(dat)) {
      zLimX <- dat$chromPeak$coordinates$limits$x
      dat$chromPeak +
        geom_text_repel(data = dat$analytes %>% filter(between(RT, zLimX[1]-0.25, zLimX[2]+0.25)),
                        aes(x = RT,
                            y = yLabs*1.05,
                            label = Analyte,
                            colour = as.factor(Type)))
    } else {
      dat$chromPeak
    }
  }  
  
  output$chromview <- renderPlot(dat$chromOver)
  output$chromatogram <- renderPlot(labelZoom())
  
  # When drawing on a section of the overview chromatogram, focus the zoomed chromatogram on that area.
  observeEvent(input$view_brush, {
    xmin <- input$view_brush$xmin
    xmax <- input$view_brush$xmax
    ymax <-
      dat$tic %>% filter(between(Time, xmin, xmax)) %>% pull(Abundance) %>% max()
    dat$chromZoom <<- dat$chromZoom +
      coord_cartesian(xlim = c(xmin, xmax),
                      ylim = c(0, ymax*1.05))
    output$chromatogram <- renderPlot(labelZoom())
  })
  
  # When clicking on the zoomed chromatogram, show a peak and enable manual annotation
  observeEvent(input$chrom_click, {
    x <- input$chrom_click$x
    xmod <- 0.25
    ymax <- dat$tic %>% filter(between(Time, x - xmod, x + xmod)) %>% pull(Abundance) %>% max()
    xact <- dat$tic %>% filter(Abundance == ymax) %>% pull(Time)
    desc <- paste("Actual RT: ", round(xact, 3))
    if (!is.null(dat$analytes)) {
      xexp <- dat$analytes %>% filter(between(RT, x-xmod, x+xmod)) %>% pull(RT)
      peakName <- dat$analytes %>% filter(between(RT, x-xmod, x+xmod)) %>% pull(Analyte)
      if (is.na(peakName[1])) {
        desc <- paste("Unidentified Peak - ", desc, collapse = "<br/>")
      } else {
        desc <- paste(peakName, " - ", desc, "/ Expected RT: ", xexp, collapse="<br/>")
      }
    } else {
      xexp <- NULL
      desc <- paste("Unidentified Peak - ", desc, collapse = "<br/>")
    }
    output$peakDescribe <- renderUI(HTML(desc))
    output$chromatogram <- renderPlot({
      labelZoom() +
        annotate("rect", xmin = (x-0.25), xmax = (x+0.25), ymin = -Inf, ymax = Inf, fill="grey50", alpha=0.1)
    })
    dat$chromPeak <- dat$chromZoom +
      coord_cartesian(xlim = c(x-0.25, x+0.25),
                      ylim = c(0, ymax*1.05)) +
      annotate("segment", x=xact, xend=xact, y=-Inf, yend=ymax, linetype="longdash")
    output$peakview <- renderPlot(labelPeak())
    enable("annotateMore")
  })
  
  # Get manual annotation from user
  observeEvent(input$annotateMore, {
    xZ <- dat$chromPeak$coordinates$limits$x
    ymax <- dat$tic %>% filter(between(Time, xZ[1], xZ[2])) %>% pull(Abundance) %>% max()
    thisRT <<- dat$tic %>% filter(Abundance == ymax) %>% pull(Time) %>% round(3)
    showModal(modalDialog(
      title <- NULL,
      textInput(inputId = "newAnnotation",
                label = paste("Manually add an annotation to the peak at", round(thisRT, 3), "minutes", sep=" "),
                width = "100%"),
      footer = tagList(
        actionButton("addAnnotation", "Add"),
        modalButton("Cancel")
      )
    ))
  })

  # Add the manual annotation to the data file and redraw
  observeEvent(input$addAnnotation, {
    yLabel = dat$tic %>% 
      filter(
        between(Time, thisRT-0.1, thisRT+0.1)
        ) %>% 
      pull(Abundance) %>% 
      max()
    if (is.null(dat$analytes)) {
      dat$analytes <- list(Analyte = input$newAnnotation,
                           RT = thisRT,
                           Type = "Manual",
                           yLabs = yLabel)
      dat$analytes <- as.data.frame(dat$analytes, stringsAsFactors = FALSE)
    } else {
      dat$analytes <- rbind(dat$analytes, 
                            as.data.frame(
                              list(
                                Analyte = input$newAnnotation, 
                                RT = thisRT, 
                                Type = "Manual", 
                                yLabs = yLabel)))
    }
    addAnnotationMarks()
    rm(thisRT, envir = parent.env(parent.env(parent.env(environment()))))
    removeModal()
  })
  
})
