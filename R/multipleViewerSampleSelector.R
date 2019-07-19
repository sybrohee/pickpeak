
library(shiny)
library(DT)

#' module multipleViewersampleSelector UI function
#' @export
multipleViewersampleSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("sampleSelector"))
}

#' module multipleViewersampleSelector server function
#' @export
multipleViewersampleSelector <- function(input,output,session, data) {
  ns <- session$ns

  output$sampleSelector <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)

    pickerInput(ns("sampleSelector"), label = "Samples", choices = ids, selected = ids,multiple = TRUE,options=list(`actions-box` = TRUE))
  })    
  return(list(selectedSamples = reactive(input$sampleSelector)))

}
