
# module UI function
#' module singleExperimentPeakAnalyzer UI function
#' @export
singleExperimentPeakAnalyzerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,HTML("<br><b>Min peak height</b>")),
    column(6, numericInput(ns("minValueFilterThresholdField"), "", 30)),
    column(3, style = "margin-top: 20px", actionButton(ns("minValueFilterThresholdButton"), "Filter out")),
    column(9, style = "margin-top: 20px", actionButton(ns("includeExcludeButton"), "In/ex clude selected peak"))
  )

}
#' module singleExperimentPeakAnalyzer server function
#' @export
singleExperimentPeakAnalyzer <- function(input,output, session) {
  ns <- session$ns
  
  return(list(minValueFilterThresholdField = reactive(input$minValueFilterThresholdField),
                minValueFilterThresholdButton = reactive(input$minValueFilterThresholdButton),
                includeExcludeButton = reactive(input$includeExcludeButton))
                )

}
