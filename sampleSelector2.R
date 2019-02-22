
library(shiny)
library(DT)
library(data.table)
source("mysql_queries.R")

projectSelectorUI <- function(id) {
  ns <- NS(id)
  tagList(
	  fluidRow(actionButton(ns("openCreateProjectButton"), "Load / create project"))
  )
}

myProjectCreationModal <- function(session) {
  ns <- session$ns
  modalDialog(
    mainPanel(
        fluidRow(
            DT::dataTableOutput(ns("existingProjectsTable")),
            actionButton(ns("loadProjectBtn"), "Load Project")
            
        ),
        fluidRow(
            textInput(ns("newProjectNameInput"), "Enter new project name", ""),
            actionButton(ns("createProjectButton"), "Create new project")
        ),
        width = 12
    )
  )
}
projectSelector <- function(input,output,session, config) {
  existingProjects <- function(){
    executeQuery(getProjects, mysqlConfig=config$mysql)
  }
  values <- reactiveValues(projectName = "-1",
                           projectId = "-1")
  project.dt <- reactiveValues(table = existingProjects())
  # open modal on button click
  observeEvent(input$openCreateProjectButton,
               showModal(myProjectCreationModal(session))
  )
  


  

  
  output$existingProjectsTable <- DT::renderDataTable({
    myDT <- project.dt$table
    DT::datatable(
        myDT,
        options = list(), 
        selection = 'single',
        rownames = FALSE
    )
  })
  
  
  
  
  observeEvent(input$loadProjectBtn, {

    if (! is.null(input$existingProjectsTable_rows_selected)) {
      values$projectName <- project.dt$table[input$existingProjectsTable_rows_selected, "name"]
      values$projectId <- project.dt$table[input$existingProjectsTable_rows_selected, "project_id"] 
    } else {
      values$projectName <- "-1"
    }
    removeModal() 
  })
  
  observeEvent(input$createProjectButton, {
    project_name = input$newProjectNameInput
    executeQuery(createProject,  queryParams=list(project_name=project_name), mysqlConfig=config$mysql)
    project.dt$table = existingProjects()
  })  
  
  
  
  
  
  result = list(
	  projectName = reactive(values$projectName),
	  loadProjectBtn = reactive(input$loadProjectBtn),
	  projectId = reactive(values$projectId)
  )
   return(result)
}
