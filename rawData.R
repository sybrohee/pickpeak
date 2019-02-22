library(plotly)
library(shiny)
library(DT)

# module UI function
rawDataViewerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    tabsetPanel(
        type = "tabs",
        id ="tabsData",
        tabPanel(
            "Peaks",
            DT::dataTableOutput(ns("peakstable"))
        ),
        tabPanel(
            "Points",
            DT::dataTableOutput(ns("pointstable"))
        )
    )
  )
}

# module server function
rawDataViewer <- function(input, output, session, fsa.data, raw.data.peaks.filter) {
    ns <- session$ns
    
    output$peakstable <- DT::renderDataTable({
      req(fsa.data()$peaks)
      rawPeaks <- fsa.data()$peaks
      # filter dyes
      rawPeaks <- rawPeaks[dye %in% raw.data.peaks.filter$rawDataPeaksFilterDye()]
      datatable(rawPeaks)
      
      
    }) 
}
