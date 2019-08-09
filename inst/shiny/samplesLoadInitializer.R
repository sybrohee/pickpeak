#' module samplesLoadInitializer UI function
#' @export
samplesLoadInitializerUI <- function(id){
  ns <- NS(id)
  column(12, 
	fluidRow(
		column(5, uiOutput(ns("loadButton"))),
		column(5, reinitializerUI(ns("myreinitializer2")))
	
	),
	
  
	fluidRow(
	
	
		column(12, DT::dataTableOutput(ns("fileList")))
	)
  )
    
}

#' module samplesLoadInitializer server function
#' @export
samplesLoadInitializer <- function(input,output,session, selected.samples) {
  ns <- session$ns
  
  output$loadButton <- renderUI({

    actionButton(ns("loadButton"), label = "Load")
  })
  callModule(reinitializer, "myreinitializer2")
  
  output$fileList <- DT::renderDataTable( {
	req(selected.samples())
	datatable(
		selected.samples()[, 'name'],
		rownames = FALSE,
		colnames = "",
        selection = 'none',
        options = list(
            scroller = TRUE,
            scrollY = TRUE,
            lengthChange = FALSE,
            searching = FALSE,
			info = FALSE,
			paging =FALSE
		)
	)
  })

  return(
	list(
		loadButton = reactive(input$loadButton)

	)
  )

}
