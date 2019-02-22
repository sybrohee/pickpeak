
library(shiny)
library(DT)

# module UI function
sampleSelectorUI <- function(id){
  ns <- NS(id)
  tagList(
      fileInput(ns("fileList"), "Choose FSA File",
                 multiple = TRUE,
                 accept = c("fsa"))
    )
}

# module server function
sampleSelector <- function(input,output,session) {
  return(list(selectedSamples = reactive(input$fileList)))
}
