

# module UI function
#' module analysisParameter UI function

#' @importFrom shiny NS uiOutput
#' @export
analysisParametersUI<- function(id) {
  ns <- NS(id)
  uiOutput(ns("openModalBtn"))
  
}





# module server function
#' module analysisParameter server function
#' @importFrom shiny observeEvent showModal removeModal actionButton modalDialog selectInput renderUI 
#' @export

analysisParameters <- function(input,output,session, data, predefined.parameters) {
  ns <- session$ns
  default.min.peak <- 2000
  parameters <- reactiveValues(minPeakHeight = list(), aboveSample= list(), ladderSample = list(), idi = NULL, sample.min.peak = list(), global.default.min.peak = default.min.peak)

  observeEvent(predefined.parameters()$selection, {
	req(predefined.parameters)
	req(data()$data$dyes)
	dyes <- data()$data$dyes
	for (i in 1:5) {
	    dye <- dyes[i]
		val <- predefined.parameters()$parameters[id == predefined.parameters()$selection][[paste0("Dye", i)]]
		for (idi in unique(data()$data$intensities$id)) {
			parameters$minPeakHeight[[idi]][[dye]] <- val
		}
	}
  })
  
  
    
  # open modal on button click
  observeEvent(input$openModalBtn,
               ignoreNULL = TRUE,   # Show modal on start up
               showModal(myModal())
  )
  
  observeEvent(data()$data$intensities$id, {
# 	if (! is.null(data()$data$intensities$id)) {
		parameters$minPeakHeight <- list() 
		parameters$ladderSample  <- list()
		parameters$idi = NULL
		parameters$sample.min.peak = list()
		parameters$global.default.min.peak = default.min.peak
		parameters$default.min.peak= NULL
# 	}
  })
  
  output$openModalBtn <- renderUI({
    req(data()$data$intensities$id)
    actionButton(ns("openModalBtn"), "Sample settings")
  })
  #' @export
  myModal <- function() {
    
    modalDialog(
		footer = NULL,
		fluidRow(
			column(7, 
				fluidRow(
					column(12,
						uiOutput(ns("sampleIdSelector"))
					),
					column(12, 
						uiOutput(ns("minPeakHeight")),
						uiOutput(ns("ladderSample")),
						uiOutput(ns("aboveSample"))
					),
					column(3, 
						actionButton(ns("submitModalButton"), "Submit")
					)
				)
			),
			column(1, br()),
			column(3, 
				fluidRow(
					uiOutput(ns("defaultMinPeak")),
					uiOutput(ns("samplesorsample")),
					actionButton(ns("cloneButton"), "Clone value")
				)
				
			)	
		)
	)
  }
  
  observe({
    req(data()$data$intensities$id)
	if (is.null(parameters$idi)) {
	  ids <- unique(data()$data$intensities$id)
	  parameters$idi <- ids[1]
	}
  })
  
  output$sampleIdSelector <- renderUI({
    req(data()$data$intensities$id)
    ids <- unique(data()$data$intensities$id)
	
    
    selectInput(ns("sampleIdSelector"), label = "Sample analysis parameters",  choices = ids, selected = ids[1])
  })
  
  
  output$samplesorsample <- renderUI({
    selectInput(ns("samplesorsample"), label = "Standard",  choices = c('All samples', 'This sample'), selected = 'This sample')
  })
  
  output$minPeakHeight <- renderUI({
    req(data()$data$intensities$id)
	req(data()$data$dyes)
	req(input$sampleIdSelector)	
	dyes <- data()$data$dyes
	
	idi <- input$sampleIdSelector

	lapply(dyes, 
		function(dye) { 
			#print(paste(idi, dye))
			val <- default.min.peak
			if (!is.null(parameters$minPeakHeight[[idi]][[dye]])) {
				val <- parameters$minPeakHeight[[idi]][[dye]]
# 				print(paste(idi, dye, val))
			}
			list(
				fluidRow(
					column(3, tags$b(dye)), 
					column(6, tags$div(id = "inline", numericInput(ns(dye), label = "",  value = val))), 
					column(3, actionButton(ns(paste0("clone",dye)), "All samples"))
					
				),
				fluidRow(br())
			)
			
		}
	)
  })
  
  output$defaultMinPeak <- renderUI({
    val <- parameters$global.default.min.peak
    if (!is.null(parameters$sample.min.peak[[parameters$idi]])) {
      val <- parameters$sample.min.peak[[parameters$idi]]
    }
	numericInput(ns("defaultMinPeak"), label = "Default min peak",  value = val)
  })  

  
  output$ladderSample <- renderUI({
    req(data()$data$intensities$id)
	req(input$sampleIdSelector)
    default.val <- F
    if (!is.null(parameters$ladderSample[[input$sampleIdSelector]])) {
      default.val <- parameters$ladderSample[[input$sampleIdSelector]]
    }
    checkboxInput(ns("ladderSample"), label = "ladder sample", value = default.val)
  })
  output$aboveSample <- renderUI({
    req(data()$data$intensities$id)
	req(input$sampleIdSelector)
    default.val <- F
    if (!is.null(parameters$aboveSample[[input$sampleIdSelector]])) {
      default.val <- parameters$aboveSample[[input$sampleIdSelector]]
    }
    checkboxInput(ns("aboveSample"), label = "Always above", value = default.val)
  })  
  
  observeEvent (input$sampleIdSelector, {
	save.values()
  })
  

  
  observeEvent(input$submitModalButton, {
    save.values()
	shiny::removeModal()
  })
  
  observeEvent(input$cloneButton, {
	req(data()$data$dyes)
	req(data()$data$intensities$id)
	results <- list()
	ids <-   unique(data()$data$intensities$id)
	if (input$samplesorsample == 'This sample') {
	  ids <- c(parameters$idi)
	}
	for (dye in data()$data$dyes) {
	  updateNumericInput(session = session, inputId = dye, value = input$defaultMinPeak)
	  for (idi in ids) {
		#print(paste("cloning",idi, dye, input$defaultMinPeak))
	    parameters$minPeakHeight[[idi]][[dye]] <- input$defaultMinPeak
	    parameters$sample.min.peak[[idi]] <- input$defaultMinPeak
	  }
	  
	}
    

  })
  
  observe({
	req(data()$data$dyes)
	req(data()$data$intensities$id)
	lapply(
		data()$data$dyes,
		function(dye) {
			observeEvent(input[[paste0(paste0("clone",dye))]], {
				req(data()$data$dyes)
				req(data()$data$intensities$id)
				results <- list()
				ids <-   unique(data()$data$intensities$id)
				for (dye in data()$data$dyes) {
					for (idi in ids) {
						parameters$minPeakHeight[[idi]][[dye]] <- input[[dye]]
						parameters$sample.min.peak[[idi]] <- input$defaultMinPeak
					}
				}
			})
		}
	)
  })

  save.values <- reactive( {
    req(data()$data$intensities$id)
	req(data()$data$dyes)
	req(input$sampleIdSelector)
	dyes <- data()$data$dyes

	
	lapply(dyes, 
		function(dye) {
			if (!is.null(input[[dye]])) {
				parameters$minPeakHeight[[parameters$idi]][[dye]] <- input[[dye]]
			}
		}
	)
	parameters$ladderSample[[parameters$idi]] <- input$ladderSample
	parameters$aboveSample[[parameters$idi]] <- input$aboveSample
	
	parameters$sample.min.peak[[parameters$idi]] <- input$defaultMinPeak
	parameters$idi <- input$sampleIdSelector    
  })

  collectMinPeakHeights <- reactive({
	result <- list()
	req(data()$data$dyes)
	req(data()$data$intensities$id)
	for (dye in data()$data$dyes) {
	  for (idi in unique(data()$data$intensities$id)) {
	    val <- parameters$minPeakHeight[[idi]][[dye]]
	    if (is.null(val)) {
	      val <- parameters$sample.min.peak[[idi]]
	    } 
	    if (is.null(val)) {
	      val <- parameters$global.default.min.peak
	    } 
	    #print(paste("collecting", idi, dye, val))
	    result[[idi]][[dye]] <- val
	  }
	  
	}
	#print(result)
	result
    
  })

  return(list(
    minPeakHeight = reactive(collectMinPeakHeights()),
    ladderSample = reactive(parameters$ladderSample),
    aboveSample = reactive(parameters$aboveSample)
  ))
    


}

