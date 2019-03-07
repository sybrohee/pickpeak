
library(shiny)
library(DT)

# module UI function
singleExperimentPeakAnalyzerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,HTML("<br><b>Mean peak height</b>")),
    column(6, numericInput(ns("minValueFilterThresholdField"), "", 30)),
    column(3, style = "margin-top: 20px", actionButton(ns("minValueFilterThresholdButton"), "Filter out")),
    column(12, HTML("<br>    ")),
    column(6, htmlOutput(ns("selectedPeak"))),
    column(3, uiOutput(ns("includeExcludeButton"))),
    column(12, DT::dataTableOutput(ns("selectedPeakDT")))
    
    
  )

}

# module server function
singleExperimentPeakAnalyzer <- function(input,output, session, selectedPeak) {
  ns <- session$ns
  selected.peak.table <- reactive({
        data.table(
            c("system", "dye","size", "height"),
            c(selectedPeak()$system, selectedPeak()$dye, floor(selectedPeak()$size),selectedPeak()$height) 
        )    
  })
  
  output$selectedPeak <- renderText({
    req(selectedPeak())
    "<b>Selected peak</b><br>"
  })
  output$selectedPeakDT <- renderDataTable({
    req(selectedPeak())
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
  
  output$includeExcludeButton <- renderUI({
    req(selectedPeak())
    text <- "Filter out"
    if (!selectedPeak()$keep) {
      text <- "Include"
    }
    actionButton(ns("includeExcludeButton"), text)
  })
  
  
  
  return(list(minValueFilterThresholdField = reactive(input$minValueFilterThresholdField),
                minValueFilterThresholdButton = reactive(input$minValueFilterThresholdButton),
                includeExcludeButton = reactive(input$includeExcludeButton))
                )

}
