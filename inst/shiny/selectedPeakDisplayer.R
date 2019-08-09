

# module UI function
#' module selectedPeakDisplayer  UI function
#' @export
      
selectedPeakDisplayerUI <- function(id){
  ns <- NS(id)
  fluidRow(
	column(12, uiOutput(ns("selectedPeak"))), 
	column(12, DT::dataTableOutput(ns("selectedPeakDT"))
	)
  )

}

# module UI function
#' module selectedPeakDisplayer  server function
#' @export
selectedPeakDisplayer <- function(input,output, session, selectedPeak) {
  ns <- session$ns
  selected.peak.table <- reactive({
		req (nrow(selectedPeak()) > 0)
        data.table(
            c("system", "dye","size", "height"),
            c(selectedPeak()$system, selectedPeak()$dye, floor(selectedPeak()$size),selectedPeak()$height) 
        )    
  })
  
  output$selectedPeak <- renderText({
    req(selectedPeak())
    req(selected.peak.table())
    "<b>Selected peak</b><br>"
  })
  
  output$selectedPeakDT <- DT::renderDataTable({
    req(selectedPeak())
    req(selected.peak.table())
    datatable(selected.peak.table(),
        rownames = FALSE,
        colnames = "",
        selection = 'none',
        options = list(
            searching = FALSE,
            bSort=FALSE,
            dom = 't'
        )
    )
  })

}
