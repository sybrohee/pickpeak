
library(shiny)
library(DT)

# module UI function
loadSaveParamsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("openModalBtn"))

}




# module server function
loadSaveParams <- function(input,output,session, data, global.parameters) {
	ns <- session$ns
	predef.params <- reactiveValues(predefined.parameters = NULL, mode = "loader", newLoad = NULL)
  
	observeEvent(input$openModalBtn,
		ignoreNULL = T,   # Show modal on start up
		{
			predef.params$mode <- "loader"
			showModal(myModal())
		}
	)
	observeEvent(input$closeModalButton,
		ignoreNULL = T,   # Show modal on start up
        removeModal()
	)	
	observeEvent(input$editActionButton,
		ignoreNULL = T,   # Show modal on start up
        predef.params$mode <- "editor"
	)
	
	observeEvent(input$saveActionButton,
		ignoreNULL = T,   
		
        {
			predef.file <- file.path(global.parameters$predefined_parameters)
			req(file.exists(predef.file))
			parameters <- fread(predef.file)        
			paramlist <- names(parameters)
			toAdd <- list()
			if (input$loadParams %in%  parameters$id) {
				rowi <- which(parameters$id == input$loadParams)
				parameters <- parameters[-rowi,]
			}
			for (paramName in paramlist) {
				toAdd[[paramName]] = input[[paramName]]
			}
			parameters <- unique(rbindlist(list(parameters, toAdd), use.names = T))
			write.table(parameters, predef.file, sep = "\t", quote = F, row.names = F)
			predef.params$predefined.parameters = parameters
			updateSelectInput(session = session, inputId = "loadParams", choices = c('None', parameters$id), selected = input[['id']])
			predef.params$mode = "loader"
        }
	)	
	
	observeEvent(input$loadActionButton,
		ignoreNULL = T,   
        {
			predef.params$mode = "loader"
        }
	)	    	
    
    

	
	myModal <- function() {
    
		modalDialog(
			footer = NULL,
			
			fluidRow(
				column(1, ""),
				column(10, 
					fluidRow(
						column(11, uiOutput(ns("loadParams")))
						

					),
					fluidRow(
						column(11, uiOutput(ns("paramTable")))
					)
				)
				
			),
			fluidRow(
				column(2, ""),
				column(2, uiOutput(ns("loadActionButton"))),
				column(2, uiOutput(ns("editActionButton"))),					
				column(2, uiOutput(ns("saveActionButton"))),
				column(2, uiOutput(ns("closeModalButton")))#,
			)
		)
	}
    
	output$loadParams <- renderUI({
		predef.file <- file.path(global.parameters$predefined_parameters)
		req(file.exists(predef.file))
		parameters <- fread(predef.file)
		predef.params$predefined.parameters = parameters
		selectInput(ns("loadParams"), label = "Predefined params",  choices = c('None', parameters$id), selected = 'None')
	})
	
	output$loadActionButton <- renderUI({
		req(input$loadParams != 'None')
		req(predef.params$mode == "loader")
		predef.params$newLoad <- input$loadActionButton		
		actionButton(ns("loadActionButton"), "Load")

	})
	
	output$editActionButton <- renderUI({
		req(input$loadParams != 'None')
		req(predef.params$mode == "loader")
		actionButton(ns("editActionButton"), "Edit/Create")
	})

	
	output$saveActionButton <- renderUI({
		req(input$loadParams != 'None')
		req(predef.params$mode == "editor")
		
		actionButton(ns("saveActionButton"), "Save")
	})
	
	output$openModalBtn <- renderUI({
		req(data()$data$intensities$id)
		list(actionButton(ns("openModalBtn"), "Load / Manage"), br(),br())
	})
	
	output$closeModalButton <- renderUI({
		req(data()$data$intensities$id)
		actionButton(ns("closeModalButton"), "Close")
	})	
	
	output$paramTable <- renderUI({
		req(input$loadParams != 'None')
		req(predef.params$mode == "editor")
		paramlist <- names(predef.params$predefined.parameters)
		idi <- input$loadParams
		lapply(paramlist, 
			function(paramName) { 
				val <- NA
				if (!is.null(predef.params$predefined.parameters[id == idi][[paramName]])) {
					val <- predef.params$predefined.parameters[id == idi][[paramName]]
				}
				label.id <- paramName
				if (grepl("Dye", paramName)) {
				  dyeVal <- as.numeric(gsub( "Dye", "",paramName))
				  #print(data()$data$dyes)
				  #print(dyeVal)
				  if (!is.na(data()$data$dyes[dyeVal])) {
					label.id <- paste0(label.id, " (",data()$data$dyes[dyeVal],")")
				  }
				}
				if (is.na(as.numeric(val))) {
					textInput(ns(paramName), label = label.id,  value = val)
				} else {
					numericInput(ns(paramName), label = label.id,  value = val)
				}
			}
		)
	})
	
    return(list(
		parameters =  reactive(predef.params$predefined.parameters),
		loadParamButton = reactive(predef.params$newLoad),
		openModalBtn = reactive(input$openModalBtn),
		selection = reactive(input$loadParams)
	))

}
