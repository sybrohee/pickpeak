#' module samplesLoadInitializer UI function
#' @export
reinitializerUI <- function(id){
  ns <- NS(id)
  column(12, 
	fluidRow(
		useShinyjs(),                                          
		extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}", functions = c('reset')),                      
		actionButton(ns("reset_button"), "Reset Page")
	)
  )
}

#' module samplesLoadInitializer server function
#' @export
reinitializer <- function(input,output,session) {
  ns <- session$ns
  observeEvent(input$reset_button, {js$reset()})          
  
                                                           
  

}
