
library(shiny)
library(DT)

# module UI function
peakAnalyzerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12, uiOutput(ns("markers"))),
    column(12, uiOutput(ns("removeStutters")))
    
  )
}

# module server function
peakAnalyzer <- function(input,output,session,selected.scale, data) {
  ns <- session$ns
  
  markers <- reactiveValues(markersList = NULL)
  
  output$markers <- renderUI({

    req(selected.scale$selectedScale() != 'Raw')
    req(selected.scale$scalingDye() != 'None')
    req(data()$standardized.data) 

    
    analysis.dir <- "www/data/markers"
    available.analyzes <- fread(file.path(analysis.dir,"markers.tab"))
    analyzes <- available.analyzes$analysis
    print(available.analyzes)
    files <- file.path(analysis.dir, available.analyzes$marker.file)
    names(files) <- analyzes
    markers$markersList = available.analyzes
    selectInput(ns("markers"), label = "Markers",  choices = c("None",files), selected = 'None')
  })
  
  
  output$removeStutters <- renderUI({
    req(selected.scale$selectedScale() != 'Raw')
    req(selected.scale$scalingDye() != 'None')
    req(data()$standardized.data) 
    checkboxInput(ns("removeStutters"), label = "Remove stutter peaks", value = T)
  })
  
  

  return(
    list(
        selectedMarkers = reactive(input$markers),
        markersList =  reactive(markers$markersList),
        removeStutters = reactive(input$removeStutters)
    )
  )

}
