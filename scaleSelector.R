
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
scaleSelector <- function(input,output,session, data, parameters) {
  values <- reactiveValues(scalingDye = 'None', selectedScale = 'Raw')
  ns <- session$ns
  output$selectedScale <- renderUI({
    req(data()$dyes)
    selectInput(ns("selectedScale"), label = "Ladder",  choices = c("Raw", "LIZ500", 'ILS500', 'LIZ120'), selected = values$selectedScale)
  })
  output$scalingDye <- renderUI({
    req(data()$dyes)
    req(input$selectedScale != 'Raw')
    selectInput(ns("scalingDye"), label = "Standard",  choices = c('None', as.vector(data()$dyes)), selected = values$scalingDye)
  })
  observeEvent(input$scalingDye, {
		values$scalingDye <- input$scalingDye;
	}
  )
  observeEvent(input$selectedScale, {
		if (input$selectedScale == 'Raw') {
			values$scalingDye <- "None"
		}
		values$selectedScale <- input$selectedScale
	}
  ) 
  observeEvent(parameters$minPeakHeight(), {
	updateSelectInput(session, "scalingDye", selected = 'None')
	updateSelectInput(session, "selectedScale", selected = 'Raw')
  })

  
  
  return(
    list(
        selectedScale = reactive(values$selectedScale),
        scalingDye = reactive(values$scalingDye)
    )
  )

}
