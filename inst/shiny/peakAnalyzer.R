

#' module peakAnalyzer UI function
#' @export
peakAnalyzerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12, uiOutput(ns("markers"))),
    column(12, uiOutput(ns("removeStutters"))),
    br()
  )
}

#' module peakAnalyzer server function
#' @export
peakAnalyzer <- function(input,output,session,selected.scale, parameters, data, global.parameters, predefined.parameters) {
  ns <- session$ns
  
  markers <- reactiveValues(markersList = NULL, selected.marker = "None")
  
  observeEvent(selected.scale$scalingDye(), {
    req(predefined.parameters()$selection)
    if (selected.scale$scalingDye() != "None") {
	  analysis.dir <- file.path(global.parameters$datadir, "markers");
	  available.analyzes <- fread(file.path(analysis.dir,"markers.tab"))
	  analyzes <- available.analyzes$analysis
	  files <- file.path(analysis.dir, available.analyzes$marker.file)
	  names(files) <- analyzes
	  selected <- predefined.parameters()$parameters[id ==  predefined.parameters()$selection][['analysis_type']]
	  markers$selected.marker <- selected
      updateSelectInput(session, "markers", selected = files[selected])
    }
  })
  
  output$markers <- renderUI({
    req(selected.scale$selectedScale() != 'Raw')
    req(selected.scale$scalingDye() != 'None')
    analysis.dir <- file.path(global.parameters$datadir, "markers");
    available.analyzes <- fread(file.path(analysis.dir,"markers.tab"))
    analyzes <- available.analyzes$analysis
    files <- file.path(analysis.dir, available.analyzes$marker.file)
	names(files) <- analyzes
    markers$markersList = available.analyzes
    selectInput(ns("markers"), label = "Markers",  choices = c("None",files), selected = files[markers$selected.marker])
  })
  
  
  output$removeStutters <- renderUI({
    req(selected.scale$selectedScale() != 'Raw')
    req(selected.scale$scalingDye() != 'None')
    req(data()$standardized.data) 
    checkboxInput(ns("removeStutters"), label = "Remove stutter peaks", value = T)
  })
  

  observeEvent(parameters$minPeakHeight(), {
    if (is.null(predefined.parameters()$selection)) {
		updateSelectInput(session, "markers", selected = 'None')
	}
  })  
  

  return(
    list(
        selectedMarkers = reactive(input$markers),
        markersList =  reactive(markers$markersList),
        removeStutters = reactive(input$removeStutters)
    )
  )

}
