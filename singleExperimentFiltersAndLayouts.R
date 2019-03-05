
library(shiny)
library(DT)

# module UI function
singleExperimentFiltersAndLayoutsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
        uiOutput(ns("singleExperimentFilterExp")),
        uiOutput(ns("singleExperimentSystemDyeSelector")),
        uiOutput(ns("singleExperimentFilterSystem")),
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
    req(input$singleExperimentSystemDyeSelector == 'dye')
    pickerInput(ns("singleExperimentFilterDyes"), label = "Dyes", choices = as.vector(data()$data$dyes), selected = as.vector(data()$data$dyes),multiple = TRUE,options=list(`actions-box` = TRUE))
    
  })
  output$singleExperimentFilterSystem <- renderUI({
    req(data()$data$dyes)
    req(input$singleExperimentSystemDyeSelector == 'system')
    systems <- sort(unique(as.vector(data()$peaks$system)))
    pickerInput(ns("singleExperimentFilterSystem"), label = "Systems", choices = systems, selected = systems,multiple = TRUE,options=list(`actions-box` = TRUE))
  })  
  output$singleExperimentSystemDyeSelector <- renderUI({
    req(data()$data$intensities$id)
    selectInput(ns("singleExperimentSystemDyeSelector"), label = "Filter peaks by",  choices = c('system', 'dye'), selected = 'system')
  })
  output$singleExperimentFilterExp <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)
    selectInput(ns("singleExperimentFilterExp"), label = "Sample",  choices = ids, selected = ids[1])
  })  
  output$singleExperimentYaxis <- renderUI({
    req(data()$data$intensities)
    req(data()$peaks)
    req(length(input$singleExperimentFilterDyes) > 0 || length(input$singleExperimentFilterSystem) > 0)
    minval <- 0
    maxval <- 0
    if (length(input$singleExperimentFilterDyes) > 0 && input$singleExperimentSystemDyeSelector == 'dye') {
        selected.dyes.vec <- input$singleExperimentFilterDyes;
        maxval <- max(data()$peaks[id == input$singleExperimentFilterExp & !is.na(system) & dye %in% selected.dyes.vec]$peak.height)
        maxval <- 100*(ceiling(maxval/100))+100
    } else if (length(input$singleExperimentFilterSystem) > 0 && input$singleExperimentSystemDyeSelector == 'system') {
        selected.systems.vec <- input$singleExperimentFilterSystem;
        maxval <- max(data()$peaks[id == input$singleExperimentFilterExp & !is.na(system) & system %in% selected.systems.vec]$peak.height)
        maxval <- 100*(ceiling(maxval/100))+100
    }
    sliderInput(ns("singleExperimentYaxis"), label = "Y-axis", -100, maxval, value = c(-10, maxval), 100, tick = F)
  })  
  return(
    list(
        singleExperimentFilterDyes = reactive(input$singleExperimentFilterDyes),
        singleExperimentFilterExp = reactive(input$singleExperimentFilterExp),
        singleExperimentYaxis = reactive(input$singleExperimentYaxis),
        singleExperimentSystemDyeSelector = reactive(input$singleExperimentSystemDyeSelector),
        singleExperimentFilterSystem = reactive(input$singleExperimentFilterSystem)
    )
  )

}
