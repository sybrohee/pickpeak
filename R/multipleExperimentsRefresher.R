#' module multipleExperimentsRefresher UI function
#' @export
multipleExperimentsRefresherUI <- function(id){
  ns <- NS(id)
  fluidRow(
	column(6, uiOutput(ns("refreshButton"))),
	column(6, tags$strong(""))
  )
}

#' module multipleExperimentsRefresher server function
#' @export
multipleExperimentsRefresher <- function(input,output,session, data) {
  ns <- session$ns
  
  output$refreshButton <- renderUI({
    req(data()$data$intensities$id)
    actionButton(ns("refreshButton"), label = "Refresh")
  })
  

  return(
	list(
		refreshButton = reactive(input$refreshButton)

	)
  )

}
