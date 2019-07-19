
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
scaleSelector <- function(input,output,session, data, scales, parameters, predefined.parameters) {
  values <- reactiveValues(scalingDye = 'None', selectedScale = 'Raw')
  
  observeEvent(predefined.parameters()$selection, {
	req(predefined.parameters)
	scalingDye <- predefined.parameters()$parameters[id == predefined.parameters()$selection][['standard.dye']]
	selectedScale <- predefined.parameters()$parameters[id ==  predefined.parameters()$selection][['ladder']]
	updateSelectInput(session, "selectedScale", selected = selectedScale)
    
div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")
  })
  
  ns <- session$ns
  output$selectedScale <- renderUI({
    req(data()$dyes)
    div(style="display:inline-block",selectInput(ns("selectedScale"), label = "Size standard",  choices = c("Raw", names(scales)), selected = values$selectedScale), style="float:right")
    
  })
  
  output$scalingDye <- renderUI({
    req(data()$dyes)
    req(input$selectedScale != 'Raw')
    div(style="display:inline-block",selectInput(ns("scalingDye"), label = "Scaling dye",  choices = c('None', as.vector(data()$dyes)), selected = values$selectedScale), style="float:right")    
    
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
		req(predefined.parameters()$parameters)
    	scalingDye <- predefined.parameters()$parameters[id == predefined.parameters()$selection][['standard.dye']]
		req(scalingDye)
     	updateSelectInput(session, "scalingDye", selected = scalingDye)
	}
  ) 
  observeEvent(parameters$minPeakHeight(), {
    if (is.null(predefined.parameters()$selection)) {
		updateSelectInput(session, "scalingDye", selected = 'None')
		updateSelectInput(session, "selectedScale", selected = 'Raw')
	}
  })

  
  
  return(
    list(
        selectedScale = reactive(values$selectedScale),
        scalingDye = reactive(values$scalingDye)
    )
  )

}
