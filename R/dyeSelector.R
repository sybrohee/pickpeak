
library(shiny)
library(DT)

# module UI function
dyeSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("selectedDyes"))
}

# module server function
dyeSelector <- function(input,output,session, data) {
  ns <- session$ns
  output$selectedDyes <- renderUI({
    req(data()$dyes)
    checkboxGroupInput(ns("selectedDyes"), label = "Dyes",  choices = as.vector(data()$dyes), selected = as.vector(data()$dyes))
  })
  
  return(list(selectedDyes = reactive(input$selectedDyes)))

}
