library(shiny)

# module UI function
multipleViewerPageSelectorUI <- function(id){
  ns <- NS(id)
  fluidRow(
	column(6, uiOutput(ns("samplesPerPage"))),
	column(5, uiOutput(ns("pageNb")))
  )
}

# module server function
multipleViewerPageSelector <- function(input,output,session, data, selected.samples, parameters) {
  ns <- session$ns

  output$samplesPerPage <- renderUI({
    req(data()$data$intensities$id)
    numericInput(ns("samplesPerPage"), label = "Samples per page",  value = 6)
  })
  
  output$pageNb <- renderUI({
    req(data()$data$intensities$id)
    req(input$samplesPerPage)
    above.samples <- vector()
    ids <- names(parameters()$aboveSample())
	for (id in ids) {
		if (parameters()$aboveSample()[[id]]) {
			above.samples <- append(above.samples, id)
		}
	}
	nbabovesamples <- length(above.samples)
	allselectedsamples <- setdiff(selected.samples(), above.samples)
    nbselectedsamples <- length(selected.samples())
    sample.per.pages <- input$samplesPerPage - nbabovesamples

    req(input$samplesPerPage < nbselectedsamples)
    maxval <- ceiling(nbselectedsamples/sample.per.pages )

    numericInput(ns("pageNb"), label = "Page",  value = 1, min = 1, max = maxval)
  })
  
  return(
	list(
			samplesPerPage = reactive(input$samplesPerPage),
			pageNb = reactive(input$pageNb)
			
		)
  )

}
