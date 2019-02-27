
library(shiny)
library(DT)

# module UI function
singleExperimentFiltersAndLayoutsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
        uiOutput(ns("singleExperimentFilterExp")),
        uiOutput(ns("singleExperimentFilterDyes")),
        uiOutput(ns("singleExperimentYaxis"))
    )
  )
}

# module server function
singleExperimentFiltersAndLayouts <- function(input,output,session, data) {
  ns <- session$ns
  output$singleExperimentFilterDyes <- renderUI({
    req(data()$data$dyes)
    checkboxGroupInput(ns("singleExperimentFilterDyes"), label = "Dyes",  choices = as.vector(data()$data$dyes), selected = as.vector(data()$data$dyes))
  })
  output$singleExperimentFilterExp <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)
    selectInput(ns("singleExperimentFilterExp"), label = "Sample",  choices = ids, selected = ids[1])
  })  
  output$singleExperimentYaxis <- renderUI({
    req(data()$data$intensities)
    req(data()$peaks)
   
    
    selected.dyes.vec <- input$singleExperimentFilterDyes;
    print(selected.dyes.vec)
    maxval <- max(data()$peaks[id == input$singleExperimentFilterExp & !is.na(system) & dye %in% selected.dyes.vec]$peak.height)
    maxval <- 100*(ceiling(maxval/100))
    minval <- min(data()$peaks[id == input$singleExperimentFilterExp & !is.na(system) & dye %in% selected.dyes.vec]$peak.height)
    minval <- 100*(floor(minval/100))
    sliderInput(ns("singleExperimentYaxis"), label = "Y-axis", minval, maxval, value = c(minval, maxval), 500, tick = F)
  })  
  return(
    list(
        singleExperimentFilterDyes = reactive(input$singleExperimentFilterDyes),
        singleExperimentFilterExp = reactive(input$singleExperimentFilterExp),
        singleExperimentYaxis = reactive(input$singleExperimentYaxis)
        
        
    )
  )

}
