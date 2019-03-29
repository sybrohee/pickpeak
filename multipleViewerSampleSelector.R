
library(shiny)
library(DT)

# module UI function
multipleViewersampleSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("sampleSelector"))
}

# module server function
multipleViewersampleSelector <- function(input,output,session, data) {
  ns <- session$ns

  output$sampleSelector <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)

    pickerInput(ns("sampleSelector"), label = "Samples", choices = ids, selected = ids,multiple = TRUE,options=list(`actions-box` = TRUE))
  })    
  return(list(selectedSamples = reactive(input$sampleSelector)))

}
