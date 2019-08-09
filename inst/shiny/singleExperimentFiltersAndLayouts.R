
library(shiny)
library(DT)

#' module singleExperimentFiltersAndLayouts UI function
#' @export
singleExperimentFiltersAndLayoutsUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
        uiOutput(ns("singleExperimentFilterExp")),
        uiOutput(ns("singleExperimentSystemDyeSelector")),
        uiOutput(ns("singleExperimentFilterSystem")),
        uiOutput(ns("singleExperimentFilterDyes")),
        uiOutput(ns("allSystemsSameLine")),
        uiOutput(ns("singleExperimentYaxis"))
    )
  )
}

#' module singleExperimentFiltersAndLayouts server function
#' @export
singleExperimentFiltersAndLayouts <- function(input,output,session, data) {
  ns <- session$ns
  output$singleExperimentFilterDyes <- renderUI({
    req(data()$data$dyes)
    req(input$singleExperimentSystemDyeSelector == 'dye')
	req(input$singleExperimentFilterExp)
    pickerInput(ns("singleExperimentFilterDyes"), label = "Dyes", choices = as.vector(data()$data$dyes), selected = as.vector(data()$data$dyes),multiple = TRUE,options=list(`actions-box` = TRUE))
    
  })
  output$allSystemsSameLine <- renderUI({
    req(data()$data$dyes)
    req(input$singleExperimentSystemDyeSelector == 'dye')
	req(input$singleExperimentFilterExp)
	req(length(input$singleExperimentFilterDyes) == 1)
    checkboxInput(ns("allSystemsSameLine"), label = "All systems together", value = F)

    
  })  
  
  output$singleExperimentFilterSystem <- renderUI({
    req(data()$data$dyes)
    req(data()$markers)
    req(input$singleExperimentFilterExp)
    req(input$singleExperimentSystemDyeSelector == 'system')

    systems <- sort(unique(as.vector(data()$markers$system)))
    default.selected.systems <- systems
    default.selected.systems <- sort(unique(as.vector(data()$peaks[id == input$singleExperimentFilterExp]$system)))
    if (length(default.selected.systems) == 0) {
      default.selected.systems <- systems
    }
    pickerInput(ns("singleExperimentFilterSystem"), label = "Systems", choices = systems, selected = default.selected.systems, multiple = TRUE,options=list(`actions-box` = TRUE))
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
    req(data()$markers)
    req(data()$data$intensities)
    req(
		(length(input$singleExperimentFilterDyes) > 0 && input$singleExperimentSystemDyeSelector == 'dye') || 
		(length(input$singleExperimentFilterSystem) > 0 &&  input$singleExperimentSystemDyeSelector == 'system')
	)
    minval <- 0
    maxval <- 0
    selected.systems.vec <- NULL
    selected.dyes.vec <- NULL
    if (length(input$singleExperimentFilterDyes) > 0 && input$singleExperimentSystemDyeSelector == 'dye') {
        selected.dyes.vec <- input$singleExperimentFilterDyes;
        selected.systems.vec <- unique(data()$markers[dye %in% selected.dyes.vec]$system)
    } else if (length(input$singleExperimentFilterSystem) > 0 && input$singleExperimentSystemDyeSelector == 'system') {
        selected.systems.vec <- input$singleExperimentFilterSystem;
        selected.dyes.vec <- unique(data()$markers[system %in% selected.systems.vec]$dye)
    } 
    needed.cols <- c("sizes", selected.dyes.vec)

	all.intensities <- melt(data()$data$intensities[id == input$singleExperimentFilterExp, ..needed.cols], id.vars = c("sizes"),  measure.vars = selected.dyes.vec, variable.factor = F, variable.name = 'dye');
	all.intensities$sizes2 <- all.intensities$sizes + 1

	markers.pos <- data()$markers[system %in% selected.systems.vec]

	setkey(markers.pos, dye,start.pos, end.pos)
	all.intensities.overlap <- foverlaps(all.intensities,markers.pos, by.x =c('dye', 'sizes', 'sizes2'), nomatch = F)
    maxval <- max(all.intensities.overlap$value)
    maxval <- 100*(ceiling(maxval/100))+100
    minval <- min(all.intensities.overlap$value)
    minval <- 100*(floor(minval/100))-100
    sliderInput(ns("singleExperimentYaxis"), label = "Y-axis", minval, maxval, value = c(minval, maxval), 100, tick = F)
  })  
  
  
  
  
  return(
    list(
        singleExperimentFilterDyes = reactive(input$singleExperimentFilterDyes),
        singleExperimentFilterExp = reactive(input$singleExperimentFilterExp),
        singleExperimentYaxis = reactive(input$singleExperimentYaxis),
        singleExperimentSystemDyeSelector = reactive(input$singleExperimentSystemDyeSelector),
        singleExperimentFilterSystem = reactive(input$singleExperimentFilterSystem),
        allSystemsSameLine =  reactive(input$allSystemsSameLine)
    )
  )

}
