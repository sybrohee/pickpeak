
library(shiny)
library(DT)

# module UI function
peakAnalyzerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,uiOutput(ns("markers")))
  )
}

# module server function
peakAnalyzer <- function(input,output,session,selected.scale, data) {
  ns <- session$ns
  output$markers <- renderUI({


    req(selected.scale$selectedScale() != 'Raw')
    req(selected.scale$scalingDye() != 'None')
    req(data()$standardized.data)    
    
    analysis.dir <- "www/data/markers"
    available.analyzes <- fread(file.path(analysis.dir,"markers.tab"))
    analyzes <- available.analyzes$analysis
    files <- file.path(analysis.dir, available.analyzes$marker.file)
    names(files) <- analyzes
    selectInput(ns("markers"), label = "Markers",  choices = c("None",files), selected = 'None')
  })

  return(
    list(
        selectedMarkers = reactive(input$markers)
    )
  )

}
