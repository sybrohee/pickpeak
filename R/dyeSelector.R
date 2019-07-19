


#' module dyeSelector server function
#' @export
dyeSelectorUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("selectedDyes"))
}

#' module dyeSelector UI function
#' @export
dyeSelector <- function(input,output,session, data) {
  ns <- session$ns
  output$selectedDyes <- renderUI({
    req(data()$dyes)
    checkboxGroupInput(ns("selectedDyes"), label = "Dyes",  choices = as.vector(data()$dyes), selected = as.vector(data()$dyes))
  })
  
  return(list(selectedDyes = reactive(input$selectedDyes)))

}
