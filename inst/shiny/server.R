#' @export

library(shiny)
library(data.table)
library(plotly)
library(shinyalert)
library(rjson)
library(DT)
library(XLConnect)
library(seqinr)
library(shinyjs)
library(pracma)
library(shinyWidgets)


shinypackage_server <- function(input, output, session) {

	  global.parameters <- list()
	  if (is.null(launch_param) || launch_param == "") {
		## No param file specified, only default parameter will be used
		temp.dir <- tempdir();
		config.file.path <- file.path(system.file( "shiny",package = 'pickpeak'), "www", "configuration_files")
		file.copy(config.file.path, temp.dir,recursive = T )
		global.parameters$datadir <- file.path(temp.dir,  "configuration_files", "data");
		global.parameters$predefined_parameters <- file.path(temp.dir,"configuration_files", "config", "predef.tab");
	  } else {
		global.parameters <- fromJSON(file = launch_param)

	  }
	  colors <- fromJSON(file = file.path(global.parameters$datadir, "dyes", "colors.json"))
	  scales <- fromJSON(file = file.path(global.parameters$datadir, "dyes", "scales.json"))		  
      fsa.data <- reactiveValues(data = NULL, standardized.data = NULL, bins = NULL, markers = NULL, peaks = NULL, annotatedpeaks = NULL, binpeaks = NULL)
      predefined.parameters <- reactiveValues(parameters = NULL, selection = NULL)
	  
      selected.samples <- callModule(sampleSelector, "mysampleselector")
      callModule(reinitializer, "myreinitializer")
      
      
      all.selected.samples <- reactiveValues(selected.samples = NULL, done = F)
      sample.load.initializer <- callModule(samplesLoadInitializer, "mysamplesloadinitializer",reactive(all.selected.samples$selected.samples))
      
      lsParams <- callModule(loadSaveParams, "myloadSaveParams", reactive(fsa.data),global.parameters)
      
      observeEvent(lsParams$openModalBtn(), {
			  updateSelectInput(session, inputId = "analysistype", selected = "custom")
		    }
      )
	  output$mode <- reactive({
		req(!is.null(sample.load.initializer$loadButton()))

		if (sample.load.initializer$loadButton() == 0) {
			return ("loading")
			
		} else {
			return("analysis")
		}
	  })
	  outputOptions(output, 'mode', suspendWhenHidden = FALSE)

  observeEvent(selected.samples$selectedSamples(),  {
		fsa.data$standardized.data <- NULL
		fsa.data$bins <- NULL
		fsa.data$markers <-  NULL
		fsa.data$peaks <- NULL
		fsa.data$binpeaks <- NULL
		predefined.parameters$parameters <- NULL
		predefined.parameters$selection  <- NULL
		predefined.parameters$loadParamButton <- NULL
		

		all.selected.samples$selected.samples <- rbindlist(list(all.selected.samples$selected.samples,selected.samples$selectedSamples()),use.names = T, fill = F, idcol = F)
		
      })
      
      observeEvent(lsParams$loadParamButton(), {
	    
		predefined.parameters$parameters <- lsParams$parameters()
		predefined.parameters$selection  <- lsParams$selection()
		predefined.parameters$loadParamButton <- lsParams$loadParamButton() 
      })
      parameters <- callModule(analysisParameters, "myAnalysisParameters", reactive(fsa.data), reactive(predefined.parameters))
      selected.scale <- callModule(scaleSelector, "myscaleselector", reactive(fsa.data$data),scales, parameters, reactive(predefined.parameters))
      selected.analysis <- callModule(peakAnalyzer, "mypeakanalyzer", selected.scale,parameters, reactive(fsa.data), global.parameters, reactive(predefined.parameters))
      
      selected.dyes <- callModule(dyeSelector, "mydyeselector", reactive(fsa.data$data))
      singleExperimentFiltersAndLayouts <- callModule(singleExperimentFiltersAndLayouts,'mySingleExperimentFiltersAndLayouts',reactive(fsa.data))
      
      selected.height <- callModule(heightSelector, "myheightselector", reactive(fsa.data), selected.dyes)
      selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)
      pageRefreshed <- callModule(multipleExperimentsRefresher,"mymultipleexperimentsrefresher", reactive(fsa.data))
      multipleViewerSamplesSelected <- callModule(multipleViewersampleSelector, "mymultipleViewersampleSelector", reactive(fsa.data))
      pageSelected <- callModule(multipleViewerPageSelector, "mymultipleviewerpageselector", reactive(fsa.data), reactive(multipleViewerSamplesSelected$selectedSamples()),reactive(parameters))
      
	  dataExporterFilters <- callModule(dataExporterFilter, "mydataExportFilter",reactive(fsa.data), selected.scale)
	  selected.peaks <- reactiveValues(selected.peak = NULL, exportPeaksTable = NULL, annotatedPeaks = NULL)
      singlePeakAnalyzer <- reactiveValues(minValueFilterThresholdField = NULL, minValueFilterThresholdButton = NULL,includeExcludeButton = NULL)      
	  output$files <- reactive({length(all.selected.samples$selected.samples$datapath)})

	  
	  outputOptions(output, 'files', suspendWhenHidden = FALSE)
	  
	  selected.samples.done <- F
      observe({
	    
        req(all.selected.samples$selected.samples$datapath)
        fsa.data$data <- my.read.fsa(all.selected.samples$selected.samples$datapath)
		fsa.data$standardized.data <- NULL
		fsa.data$bins <- NULL
		fsa.data$markers <-  NULL
		fsa.data$peaks <- NULL
		fsa.data$binpeaks <- NULL
		predefined.parameters$parameters <- NULL
		predefined.parameters$selection  <- NULL
		predefined.parameters$loadParamButton <- NULL
		output$predefinedparametersTitle <- renderUI(
			{
				list(tags$b("Predefined parameters"), br(),br())
			}
	    )
	    output$analysistype <- renderUI(
			{
				selectInput(inputId = "analysistype", label = "Analysis parameters", c("predefined", "custom"), selected = "predefined")
			}
	    )
      })

	  
      observe({
#         req(selected.scale$selectedScale())
        req(length(parameters$minPeakHeight()) > 0)
        standard.dye <- selected.scale$scalingDye()
        if (is.null(standard.dye)) {
          standard.dye = "None"
        }

		fsa.data$standardized.data <- NULL
		fsa.data$bins <- NULL
		fsa.data$markers <-  NULL
		fsa.data$peaks <- NULL
		fsa.data$binpeaks <- NULL
		std.data <- scale.timeseries(fsa.data$data, ladder = selected.scale$selectedScale(), scales = scales, standard.dye = standard.dye, minpeakheights = parameters$minPeakHeight())
		if (!is.null(std.data$error)) {
			shinyalert(text = paste0(std.data$error, ". Could not determine the size of the segments. Check parameters."))
			predefined.parameters$parameters <- NULL
			predefined.parameters$selection <- NULL
			
			
		}
		fsa.data$standardized.data <- std.data
        selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)      

      })
	  callModule(multipleExperimentViewer, "myMultipleExperimentViewer", fsa.data, colors, multipleViewerSamplesSelected, selected.height, selected.width, selected.scale, selected.dyes, reactive(pageSelected$samplesPerPage()), reactive(pageSelected$pageNb()), reactive(parameters$aboveSample()), pageRefreshed$refreshButton, sample.load.initializer$loadButton)
     
      observe({
        req(all.selected.samples$selected.samples$datapath)
        callModule(linearRegressionViewer, "myLinearRegressionViewer", fsa.data)
      })
      
      
      observe({
        req(selected.analysis$selectedMarkers())
        if (selected.analysis$selectedMarkers() == 'None') {
			fsa.data$bins <- NULL
			fsa.data$markers <-  NULL
			fsa.data$peaks <- NULL
			fsa.data$binpeaks <- NULL 
        }
        
	  })
      
      observe({
        req(selected.analysis$selectedMarkers() != 'None')
        req(selected.analysis$selectedMarkers())
        req(selected.scale$scalingDye() != 'None')
        req(selected.scale$selectedScale() != 'Raw')
        req(is.null(fsa.data$standardized.data$error))
        markers <- fread(selected.analysis$selectedMarkers())
        fsa.data$markers <- markers
        bin.file <- file.path(global.parameters$datadir,'markers',selected.analysis$markersList()[marker.file == basename(selected.analysis$selectedMarkers())]$bin.file)
        ladder.samples <- vector()
        ids <- names(parameters$ladderSample())
		for (id in ids) {
			if (parameters$ladderSample()[[id]]) {
				ladder.samples <- append(ladder.samples, id)
			}
		}
        
		peaks <- peaks.to.markers(fsa.data, parameters$minPeakHeight(),  selected.analysis$removeStutters())


        if (file.exists(bin.file) && length(ladder.samples) > 0) {

          markers.bins <-  read.bin.file(bin.file)
		  peaks.bin <- markedpeaks.to.real.bins(markers.bins, peaks, ladder.samples)
		  if (length(peaks.bin$error) > 0) {
			shinyalert(text = peaks.bin$error)
    	  }
		  bin.offset <- bins.position(markers.bins, peaks.bin$binnedpeaks, ladder.samples);
		  fsa.data$peaks <- peaks.bin$binnedpeaks
		  fsa.data$bins <- bin.offset

        } else {
		  fsa.data$peaks <- peaks
		  fsa.data$bins <- NULL
        }
		selected.peaks <- callModule(singleExperimentViewer, "mySingleExperimentViewer", fsa.data, colors, singleExperimentFiltersAndLayouts$singleExperimentFilterDyes, singleExperimentFiltersAndLayouts$singleExperimentFilterExp, singleExperimentFiltersAndLayouts$singleExperimentYaxis,singleExperimentFiltersAndLayouts$singleExperimentFilterSystem, singleExperimentFiltersAndLayouts$singleExperimentSystemDyeSelector, singleExperimentFiltersAndLayouts$allSystemsSameLine,singlePeakAnalyzer$minValueFilterThresholdField, singlePeakAnalyzer$minValueFilterThresholdButton, singlePeakAnalyzer$includeExcludeButton, reactive(ladder.sample))
		singlePeakAnalyzer <- callModule(singleExperimentPeakAnalyzer, "mySingleExperimentPeakAnalyzer")              
		callModule(selectedPeakDisplayer, "myselectedpeakdisplayer", reactive(selected.peaks$selected.peak()))
        callModule(dataExporter, "mydataexporter", dataExporterFilters, reactive(selected.peaks$annotatedPeaks()), reactive(fsa.data), colors, selected.analysis$selectedMarkers, selected.analysis$markersList, reactive(fsa.data$data$expdate))
      })
      
      
   
}
