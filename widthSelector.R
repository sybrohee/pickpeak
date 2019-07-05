
library(shiny)
library(DT)

# module UI function
widthSelectorUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6, uiOutput(ns("selectedMinWidth"))),
    column(6, uiOutput(ns("selectedMaxWidth")))
  )
}

# module server function
widthSelector <- function(input,output,session, data, selected.scale) {
  ns <- session$ns

  
  output$selectedMinWidth <- renderUI({

    req(data()$data$intensities)
    req(selected.scale$selectedScale())

    x.scale <- "time"

    
    if (selected.scale$selectedScale() == 'Raw' || selected.scale$scalingDye() == 'None') {
      x.scale = 'time'
    } else if (!is.null(data()$data$intensities[['sizes']])) {
      x.scale <- "sizes"
    }
    
    vals <- data()$data$intensities[[x.scale]]
    minmaxvals <- range(vals[is.finite(vals)], na.rm =T )


    maxval <- 100*(ceiling(minmaxvals[2]/100))
    minval <- 100*(floor(minmaxvals[1]/100))
    
    numericInput(ns("selectedMinWidth"), value = minval, label = "min X", min = minval, max = maxval, step = 5000)
  })
  
  output$selectedMaxWidth <- renderUI({

    req(data()$data$intensities)
    req(selected.scale$selectedScale())

    x.scale <- "time"

    
    if (selected.scale$selectedScale() == 'Raw' || selected.scale$scalingDye() == 'None') {
      x.scale = 'time'
    } else if (!is.null(data()$data$intensities[['sizes']])) {
      x.scale <- "sizes"
    }
    
    vals <- data()$data$intensities[[x.scale]]
    minmaxvals <- range(vals[is.finite(vals)], na.rm =T )


    maxval <- 100*(ceiling(minmaxvals[2]/100))
    minval <- 100*(floor(minmaxvals[1]/100))
    
    numericInput(ns("selectedMaxWidth"), value = maxval, label = "min X", min = minval, max = maxval, step = 5000)
  })
  
  
  
  
  
  return(list(
	selectedMinWidth = reactive(input$selectedMinWidth),
	selectedMaxWidth = reactive(input$selectedMaxWidth)
	
  ))

}

