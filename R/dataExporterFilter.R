
library(shiny)
library(DT)

# module UI function

#' module dataExporterFilter UI function
#' @importFrom shiny NS uiOutput
#' @export
dataExporterFilterUI <- function(id){
  ns <- NS(id)
  fluidRow(
	column(8, uiOutput(ns("exportType"))),
	column(8, multipleViewersampleSelectorUI(ns("mymultipleExportersampleSelector"))),
	column(8, uiOutput(ns("exportFormat")))
  )
}

# module server function
#' module dataExporterFilter server function
#' @export
dataExporterFilter <- function(input,output,session, fsa.data, selected.scale, exportTable) {
  ns <- session$ns
  output$exportType <- renderUI({
  
    standard.dye <- selected.scale$scalingDye()
	if (is.null(standard.dye)) {
      standard.dye = "None"
	}
	choices.vec <- vector()
	if (!is.null(fsa.data()$data$intensities)) {
	  choices.vec <- append(choices.vec, 'Raw data')


	}
	if (standard.dye != "None") {
	  choices.vec <- append(choices.vec, 'Standardized data')
	}
	if (! is.null(fsa.data()$peaks)) {
	  choices.vec <- c(choices.vec, 'Peaks', 'Systems')
	  
	  
	}

		
    selectInput(ns("exportType"), label = "Type of export",  choices = choices.vec)
  })
  
  output$exportFormat <- renderUI({
	req(input$exportType)
	export.choices <- c("Coma-separated value" = "csv", "Tab-delimited" = "tab")
	if (input$exportType == 'Peaks' || input$exportType == 'Systems') {
		export.choices <- c(c("Excel" = "xlsx"),  export.choices)
	}
	
	selectInput(ns("exportFormat"), label = "File format",  choices = export.choices)
    
  })
  
  exported.selected.samples <- callModule(multipleViewersampleSelector, "mymultipleExportersampleSelector",  reactive(fsa.data()))
  
  
  
  
  return(
	list(
		exportType = reactive(input$exportType),
		selected.samples = reactive(exported.selected.samples$selectedSamples()),
		exportFormat = reactive(input$exportFormat)
		)
	)

}
