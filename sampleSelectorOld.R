# Variant Explorer shiny App
# Nicolas Simonis, Institut de Pathologie et Genetique, Gosselies

library(shiny)
library(DT)

# module UI function
sampleSelectorUI <- function(id){
  ns <- NS(id)
  tagList(
      column(6,uiOutput(ns("selectsamplesbutton")))
    )
}

myModal <- function(session) {
    ns <- session$ns
    modalDialog(title="Load data",size='l',

            fluidRow(
                DT::dataTableOutput(ns("sampleTableSelector")),
                actionButton(ns("selectButton"), "Select those samples")

        )
    )
}



# module server function
sampleSelector <- function(input,output,session) {
  sample.files.table <- function() {
    sample.files <- list.files("www", pattern = "fsa", recursive = T)
    sample.files.tab <- data.table(matrix(unlist(strsplit(sample.files, "/")), ncol = 3, byrow = T))
    names(sample.files.tab) <- c("project", "project_date", "sample_name")
    sample.files.tab$filepath <-  sample.files
    return(sample.files.tab)
  }  
  ns <- session$ns
  values <- reactiveValues(selectedSamples = NULL, loadDataButton = NULL)
  sample.files.dt <- reactiveValues(table = sample.files.table())
  
  
  
  output$sampleTableSelector <- DT::renderDataTable({
    DT::datatable(sample.files.dt$table    ) 
  })
  
  output$selectsamplesbutton <- renderUI(
    actionButton(ns("selectsamplesbutton"), "Load Data")
  )



  
  observeEvent(input$selectsamplesbutton, {
    values$selectedSamples = NULL
    values$loadDataButton = NULL
    showModal(myModal(session))
  }) 
  
  

  observeEvent(input$selectButton, {
    values$selectedSamples <- sample.files.table()[input$sampleTableSelector_rows_selected,"filepath"]
    values$loadDataButton <-  input$loadDataButton
    removeModal()
  }) 
  
  
  
  samplesResult <- list(
    selectedSamples =  reactive(values$selectedSamples),
    selectButton = reactive(values$selectButton)
  )
  return(samplesResult)
}
