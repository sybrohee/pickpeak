
library(shiny)
library(DT)

# module UI function
analysisParametersUI<- function(id){
  ns <- NS(id)

    column(12,
        uiOutput(ns("sampleIdSelector")),
    fluidRow(
        column(10, 
            uiOutput(ns("minPeakHeight")),
            uiOutput(ns("ladderSample"))
            )
        )
    )

}

# module server function
analysisParameters <- function(input,output,session, data) {
  ns <- session$ns
  
  parameters <- reactiveValues(minPeakHeight = list(), ladderSample = list())
  
  
  default.min.peak <-  2000
  
  output$sampleIdSelector <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)
    selectInput(ns("sampleIdSelector"), label = "Sample analysis parameters",  choices = ids, selected = ids[1])
  })
  
  output$minPeakHeight <- renderUI({
    req(data()$data$intensities$id)
    default.val <- default.min.peak
    if (!is.null(parameters$minPeakHeight[[input$sampleIdSelector]])) {
      default.val <- parameters$minPeakHeight[[input$sampleIdSelector]]
    }    
    numericInput(ns("minPeakHeight"), label = "min peak height",  value = default.val)
  })
  output$ladderSample <- renderUI({
    req(data()$data$intensities$id)
    default.val <- F
    if (!is.null(parameters$ladderSample[[input$sampleIdSelector]])) {
      default.val <- parameters$ladderSample[[input$sampleIdSelector]]
    }
    checkboxInput(ns("ladderSample"), label = "ladder sample", value = default.val)
  })
  
  observeEvent(input$ladderSample,{
    parameters$ladderSample[[input$sampleIdSelector]] <- input$ladderSample
    parameters$minPeakHeight[[input$sampleIdSelector]] <- input$minPeakHeight
    other.ids <- setdiff(unique(data()$data$intensities$id), input$sampleIdSelector)
    if (input$ladderSample) {
      # only one ladder per set of experiment
      for (other.id in other.ids) {
        parameters$ladderSample[[other.id]] <- F
      }
    }
  })
  
  observeEvent(input$minPeakHeight,{
    parameters$ladderSample[[input$sampleIdSelector]] <- input$ladderSample
    parameters$minPeakHeight[[input$sampleIdSelector]] <- input$minPeakHeight
    other.ids <- setdiff(unique(data()$data$intensities$id), input$sampleIdSelector)
    for (other.id in other.ids) {
		if (is.null(parameters$minPeakHeight[[other.id]])) {
		  parameters$minPeakHeight[[other.id]] <- default.min.peak
		}
     }
  })  


  return(list(
    minPeakHeight = reactive(parameters$minPeakHeight),
    ladderSample = reactive(parameters$ladderSample)
    
  ))
    


}
