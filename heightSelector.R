
library(shiny)
library(DT)

# module UI function
heightSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("selectedHeight"))
}

# module server function
heightSelector <- function(input,output,session, data, selected.dyes) {
  ns <- session$ns
  output$selectedHeight <- renderUI({
    req(data()$data$intensities)
    req(selected.dyes$selectedDyes())
    selected.dyes.vec <- selected.dyes$selectedDyes()
    maxval <- max(data()$data$intensities[,..selected.dyes.vec])
    maxval <- 1000*(ceiling(maxval/1000))
    minval <- min(data()$data$intensities[,..selected.dyes.vec])
    minval <- 1000*(floor(minval/1000))
    sliderInput(ns("selectedHeight"), label = "Y-axis", minval, maxval, value = c(minval, maxval), 500, tick = F)
  })
  
  return(list(selectedHeight = reactive(input$selectedHeight)))

}
