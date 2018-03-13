#--------------------------------------------------------------
#
# Chromatogram Annotation Tool (CAT)
#   - Current Version: 0.6
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

x <- c("shiny", "tidyverse", "DT", "shinyjs", "ggrepel", "scales")
lapply(x, require, character.only = TRUE)


shinyServer(function(session, input, output) {
  # SETUP VALUES -------------------------------------------------------------------------------------------------------
  useShinyjs()
  version <- 0.5
  disable("annotateMore")
  dat <- reactiveValues()
  fromClick <- reactiveVal()
  fromClick(FALSE)
  
  # INPUT HANDLERS -----------------------------------------------------------------------------------------------------
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
      theme(legend.position = "none")
    dat$chromZoom <- dat$chromBase +  
      scale_y_continuous(labels = scientific) +
      coord_cartesian(ylim=c(0.1, max(dat$tic$Abundance)*1.1),
                      xlim=c(min(dat$tic$Time), max(dat$tic$Time)/4)) +
      theme(axis.title.x = element_blank())
    dat$chromOver <- dat$chromBase +
      scale_y_log10(labels = scientific) +
      theme(legend.position = 'bottom') +
      xlab("Time (min)")
    if (!is.null(input$analytes)) {
      addAnnotationMarks(TRUE)
    } else {
      dat$breaks <- round(range(dat$tic$Time), 0)
    }
  })
  
  # Load a .csv file of expected analytes, their retention times, and their category
  getAna <- observeEvent(input$analytes, {
    tmp <- read.csv(
      input$analytes$datapath,
      stringsAsFactors = FALSE,
      header = TRUE)
    if (!is.null(dat$tic)) {
      tmp <- tmp %>%
        mutate(yLabs = sapply(RT,
                              function(x) {
                                dat$tic %>%
                                  filter(between(Time, x-0.1, x+0.1)) %>%
                                  pull(Abundance) %>%
                                  max()
                              })
        )
    }
    if (is.null(dat$analytes)) {
      dat$analytes <- tmp
    } else {
      dat$analytes <- rbind(dat$analytes, tmp)
    }
    if (!is.null(input$tic)) {
      addAnnotationMarks(TRUE)
    }
  })
  
  # OPERATIONAL FUNCTIONS ----------------------------------------------------------------------------------------------
  # Draw line annotations on the overview and zoomed chromatograms
  addAnnotationMarks <- function(genYLabs = FALSE) {
    dat$chromOver <- dat$chromOver +
      geom_vline(data = dat$analytes, aes(xintercept = RT, colour = as.factor(Type))) +
      labs(colour = "Annotation: ")
    if (dim(dat$analytes %>% filter(Type !="Manual"))[1] > 0) {
      dat$chromZoom <- dat$chromZoom +
        geom_segment(data = dat$analytes %>% filter(Type !="Manual"),
                     aes(x = RT,
                         xend = RT,
                         y = -Inf,
                         yend = yLabs,
                         colour = as.factor(Type)))
    }
    dat$breaks <- round(range(dat$tic$Time), 0)
    updateSavePlot()
  }
  
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
  
  # Get the retention time of the greatest intensity in the current peak plot
  getRT <- function() {
    xZ <- dat$chromPeak$coordinates$limits$x
    ymax <- dat$tic %>% filter(between(Time, xZ[1], xZ[2])) %>% pull(Abundance) %>% max()
    RT <- dat$tic %>% filter(Abundance == ymax) %>% pull(Time)
    return(RT)
  }
  
  # Get the manual annotation properties from the user
  manualAnnotationPrompt <- function(fromClick = FALSE) {
    if (fromClick) {
      prompt <- paste("Manually add an annotation at", 
                      round(input$peak_dblclick$x, 3), 
                      "minutes at", 
                      round(input$peak_dblclick$y, 0), 
                      "intensity.", 
                      sep=" ")
    } else {
      prompt <- paste("Manually add an annotation to the peak at", round(getRT(), 3), "minutes.", sep=" ")
    }
    showModal(modalDialog(
      title <- NULL,
      textInput(inputId = "newAnnotation",
                label = prompt,
                width = "100%"),
      footer = tagList(
        actionButton("addAnnotation", "Add"),
        modalButton("Cancel")
      )
    ))
  }
  
  # Add the manual annotations to the plots
  manualAnnotationAdd <- function(clickCheck = FALSE) {
    annotationNew <- input$newAnnotation
    if (clickCheck) {
      yLabel <- input$peak_dblclick$y
      RT <- input$peak_dblclick$x
    } else {
      RT <- getRT()
      yLabel = dat$tic %>% 
        filter(
          between(Time, RT-0.1, RT+0.1)
        ) %>% 
        pull(Abundance) %>% 
        max()
      output$peakDescribe <- renderUI(HTML(paste(annotationNew, " - Actual RT: ", round(RT, 3))))
    }
    if (is.null(dat$analytes)) {
      dat$analytes <- list(Analyte = annotationNew,
                           RT = RT,
                           Type = "Manual",
                           yLabs = yLabel)
      dat$analytes <- as.data.frame(dat$analytes, stringsAsFactors = FALSE)
    } else {
      dat$analytes <- rbind(dat$analytes, 
                            as.data.frame(
                              list(
                                Analyte = annotationNew, 
                                RT = RT, 
                                Type = "Manual", 
                                yLabs = yLabel)))
    }
    addAnnotationMarks()
    removeModal()
  }
  
  # Add split reference lines to the save plot to guide splitting
  updateSavePlot <- function() {
    if (!is.null(input$savePlotClick$x)) {
      dat$breaks <- c(dat$breaks, input$savePlotClick$x)
    }
    dat$chromSave <- dat$chromOver + 
      geom_vline(xintercept = dat$breaks, lwd = 2)
    output$savePlot <- renderPlot(dat$chromSave)
  }
  
  # Set up download
  createDownloads <- function(fTitle, fName, fType) {
    dat$breaks <- sort(dat$breaks)
    nb <- length(dat$breaks)
    files <- ""
    xSize <- 9
    ySize <- 6
    for (b in c(1:(nb-1))) {
      tTitle <- paste0(fTitle, " (", b, " of ", nb-1, ")")
      tName <- paste0(fName, " (", b, " of ", nb-1, ")", fType)
      files[b] <- tName
      xMin <- dat$breaks[b]-0.25
      xMax <- dat$breaks[b+1]+0.25
      tmpDat <- dat$analytes %>% filter(between(RT, xMin, xMax))
      yMax <- dat$tic %>% filter(between(Time, xMin, xMax)) %>% pull(Abundance) %>% max()
      tmp <- dat$chromBase +
        geom_segment(data = tmpDat,
                     aes(x = RT,
                         xend = RT,
                         y = -Inf,
                         yend = yLabs,
                         colour = as.factor(Type))) +
        geom_text_repel(data = tmpDat,
                        aes(x = RT,
                            y = yLabs*1.05,
                            label = Analyte,
                            colour = as.factor(Type)),
                        show.legend = FALSE) +
        scale_x_continuous(limits = c(xMin, xMax)) +
        scale_y_continuous(labels = scientific, limits = c(0, yMax * 1.1)) +
        theme(plot.caption = element_text(size = 5),
              legend.position = "bottom") +
        labs(title = tTitle,
             subtitle = paste0(round(dat$breaks[b],2)-0.25, "-", round(dat$breaks[b+1],2)+0.25, " minutes."),
             colour = "Annotation:",
             x = "Time (min)",
             y = "Abundance (m/z)",
             caption = paste("Created with Chromatogram Annotation Tool v", 
                           version, 
                           "\nA product of NIST Data Tool Development\nPowered by RStudio and Shiny", 
                           sep=""))
      switch (fType,
              .png = ggsave(tName, tmp, device = "png", width = xSize, height = ySize, units = "in"),
              .jpg = ggsave(tName, tmp, device = "jpeg", width = xSize, height = ySize, units = "in"),
              .tif = ggsave(tName, tmp, device = "tiff", width = xSize, height = ySize, units = "in"),
              .pdf = ggsave(tName, tmp, device = "pdf", width = xSize, height = ySize, units = "in")
                )
    }
    zipName <- paste0(fName, ".zip")
    zip(zipName, files)
    files <- c(files, zipName)
    return(files)
  }
  
  output$saveThis <- downloadHandler(
    filename = function() {
      paste0(input$saveFile, ".zip")
    },
    content = function(file) {
      appDir <- getwd()
      setwd(paste0(appDir, "/output/"))
      files <- createDownloads(input$saveTitle, 
                               input$saveFile, 
                               input$saveType)
      file.copy(files[length(files)], file)
      unlink(files)
      setwd(appDir)
      removeModal()
    },
    contentType = "application/zip"
  )
  
  # OUTPUTS ------------------------------------------------------------------------------------------------------------
  output$chromview <- renderPlot(dat$chromOver)
  output$chromatogram <- renderPlot(labelZoom())
  
  # EVENT OBSERVERS ----------------------------------------------------------------------------------------------------
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
    xmod <- 0.1
    xwid <- 0.25
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
        annotate("rect", xmin = (x-xwid), xmax = (x+xwid), ymin = -Inf, ymax = Inf, fill="grey50", alpha=0.1)
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
    fromClick(FALSE)
    manualAnnotationPrompt(fromClick())
  })

  # Add the manual annotation to the data file and redraw
  observeEvent(input$addAnnotation, {
    manualAnnotationAdd(fromClick())
  })
  
  # Add manual annotation from double clicking the peak view
  observeEvent(input$peak_dblclick, {
    fromClick(TRUE)
    manualAnnotationPrompt(fromClick())
  })
  
  # Get download parameters
  observeEvent(input$saveIt, {
    showModal(modalDialog(
      title <- h4("Click to set download split points."),
      fluidRow(
        column(12,
               plotOutput("savePlot",
                          click = "savePlotClick",
                          height = "200px")
               ),
        column(12,
               textInput(inputId = "saveTitle",
                         label = "Title:",
                         width = "100%")
               ),
        column(8,
               textInput(inputId = "saveFile",
                         label = "File name:",
                         width = "100%")
        ),
        column(4,
               selectInput(inputId = "saveType",
                           label = "Format:",
                           choices = c(".png", ".jpg", ".tif", ".pdf"),
                           selected = ".tif",
                           width = "100%")
        )
      ),
      footer = tagList(
        downloadButton("saveThis", "Save"),
        modalButton("Cancel")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # Get split points for download
  observeEvent(input$savePlotClick, {
    updateSavePlot()
  })

  # Only allow the Save button to activate when a title and file name have been provided
  observeEvent({
    input$saveFile
    input$saveTitle},
    {
      if (input$saveFile != "" & input$saveTitle != "") {
        enable("saveThis")
      } else {
        disable("saveThis")
      }
    })
})
