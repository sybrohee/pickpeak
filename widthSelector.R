
library(shiny)
library(DT)

# module UI function
widthSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("selectedWidth"))
}

# module server function
widthSelector <- function(input,output,session, data, selected.scale) {
  ns <- session$ns
  output$selectedWidth <- renderUI({

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
    sliderInput(ns("selectedWidth"), label = "X-axis", minval, maxval, value = c(minval, maxval), 50, tick = F)
  })
  
  return(list(selectedWidth = reactive(input$selectedWidth)))

}

