
library(shiny)
library(DT)

# module UI function
scaleSelectorUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,uiOutput(ns("selectedScale"))),
    column(6,uiOutput(ns("scalingDye")))
  )
}

# module server function
scaleSelector <- function(input,output,session, data) {
  ns <- session$ns
  output$selectedScale <- renderUI({
    req(data()$dyes)
    selectInput(ns("selectedScale"), label = "Ladder",  choices = c("Raw", "LIZ500", 'ILS500'), selected = 'Raw')
  })
  output$scalingDye <- renderUI({
    req(data()$dyes)
    req(input$selectedScale != 'Raw')
    selectInput(ns("scalingDye"), label = "Standard",  choices = c('None', as.vector(data()$dyes)), selected = 'None')
  })
    
  return(
    list(
        selectedScale = reactive(input$selectedScale),
        scalingDye = reactive(input$scalingDye)
    )
  )

}
