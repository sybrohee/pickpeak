
library(shiny)
library(DT)

# module UI function
rawDataPeaksFilterUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("rawDataPeaksFilterDye"))
}

# module server function
rawDataPeaksFilter <- function(input,output,session, data) {
  ns <- session$ns
  output$rawDataPeaksFilterDye <- renderUI({
    req(data()$dyes)
    checkboxGroupInput(ns("rawDataPeaksFilterDye"), label = "Dyes",  choices = as.vector(data()$dyes), selected = as.vector(data()$dyes))
  })
  
  return(list(rawDataPeaksFilterDye = reactive(input$rawDataPeaksFilterDye)))

}
