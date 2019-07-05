# module UI function
multipleExperimentsRefresherUI <- function(id){
  ns <- NS(id)
  fluidRow(
	column(6, uiOutput(ns("refreshButton"))),
	column(6, tags$strong(""))
  )
}

# module server function
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
