
library(shiny)
library(DT)

# module UI function
heightSelectorUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6, uiOutput(ns("selectedMinHeight"))),
    column(6, uiOutput(ns("selectedMaxHeight")))
  )
		
}

# module server function
heightSelector <- function(input,output,session, data, selected.dyes) {
  ns <- session$ns

  
  output$selectedMinHeight <- renderUI({
    req(data()$data$intensities)
    req(selected.dyes$selectedDyes())
    selected.dyes.vec <- selected.dyes$selectedDyes()
    maxval <- max(data()$data$intensities[,..selected.dyes.vec])
    maxval <- 1000*(ceiling(maxval/1000))
    minval <- min(data()$data$intensities[,..selected.dyes.vec])
    minval <- 1000*(floor(minval/1000))
    numericInput(ns("selectedMinHeight"), value = minval, label = "min Y", min = minval, max = maxval, step = 500)
  })
  
  output$selectedMaxHeight <- renderUI({
    req(data()$data$intensities)
    req(selected.dyes$selectedDyes())
    selected.dyes.vec <- selected.dyes$selectedDyes()
    maxval <- max(data()$data$intensities[,..selected.dyes.vec])
    maxval <- 1000*(ceiling(maxval/1000))
    minval <- min(data()$data$intensities[,..selected.dyes.vec])
    minval <- 1000*(floor(minval/1000))
    numericInput(ns("selectedMaxHeight"), value = maxval, label = "max Y", min = minval, max = maxval, step = 500)
  })
  
  
  return(
    list(
      selectedMinHeight = reactive(input$selectedMinHeight),
      selectedMaxHeight = reactive(input$selectedMaxHeight)
    )
  )

}
