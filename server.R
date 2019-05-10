library(data.table)
library(plotly)
library(shinyalert)
library(rjson)
source("multipleViewerPageSelector.R")
source("multipleViewerSampleSelector.R")
source("functions.R")
source("selectedPeakDisplayer.R")
source("loadSaveParams.R")
source("sampleSelector.R")
source("dyeSelector.R")
source("scaleSelector.R")
source("multipleExperimentViewer.R")
source("singleExperimentViewer.R")
source("linearRegressionViewer.R")
source("peakAnalyzer.R")
source("heightSelector.R")
source("dataExporter.R")
source("dataExporterFilter.R")
source("widthSelector.R")

source("singleExperimentFiltersAndLayouts.R")
source("singleExperimentPeakAnalyzer.R")
source("analysisParameters.R")


shinyServer(function(input, output,session) {
	  global.parameters <- fromJSON(file = "../pp_localConfigShiny.json")
	  colors <- fromJSON(file = file.path(global.parameters$datadir, "dyes", "colors.json"))
	  scales <- fromJSON(file = file.path(global.parameters$datadir, "dyes", "scales.json"))
      fsa.data <- reactiveValues(data = NULL, standardized.data = NULL, bins = NULL, markers = NULL, peaks = NULL, annotatedpeaks = NULL, binpeaks = NULL)
      predefined.parameters <- reactiveValues(parameters = NULL, selection = NULL)

      selected.samples <- callModule(sampleSelector, "mysampleselector")
      lsParams <- callModule(loadSaveParams, "myloadSaveParams", reactive(fsa.data),global.parameters)
      
      observeEvent(lsParams$openModalBtn(), {
			updateSelectInput(session, inputId = "analysistype", selected = "custom")
		}
      )
      
      observeEvent(selected.samples$selectedSamples,  {
		fsa.data$standardized.data <- NULL
		fsa.data$bins <- NULL
		fsa.data$markers <-  NULL
		fsa.data$peaks <- NULL
		fsa.data$binpeaks <- NULL
		predefined.parameters$parameters <- NULL
		predefined.parameters$selection  <- NULL
		predefined.parameters$loadParamButton <- NULL		
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
      
      selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)      
      
      selected.height <- callModule(heightSelector, "myheightselector", reactive(fsa.data), selected.dyes)
      selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)
      multipleViewerSamplesSelected <- callModule(multipleViewersampleSelector, "mymultipleViewersampleSelector", reactive(fsa.data))
      pageSelected <- callModule(multipleViewerPageSelector, "mymultipleviewerpageselector", reactive(fsa.data), reactive(multipleViewerSamplesSelected$selectedSamples()),reactive(parameters))
      
	  dataExporterFilters <- callModule(dataExporterFilter, "mydataExportFilter",reactive(fsa.data), selected.scale)
# 	  selected.peaks <- reactiveValues(selected.peak = NULL, exportPeaksTable = NULL, annotatedPeaks = NULL)
#       singlePeakAnalyzer <- reactiveValues(minValueFilterThresholdField = NULL, minValueFilterThresholdButton = NULL,includeExcludeButton = NULL)      


      observe({
	    
        req(selected.samples$selectedSamples()$datapath)

        fsa.data$data <- my.read.fsa(selected.samples$selectedSamples()$datapath)
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
        req(selected.scale$selectedScale())
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
      observe({
        req(selected.scale$selectedScale())
        req(selected.dyes$selectedDyes())
        req(selected.height$selectedHeight())
        req(selected.width$selectedWidth())
        req(fsa.data$standardized.data)
        above.samples <- vector()
        ids <- names(parameters$aboveSample())
		for (id in ids) {
			if (parameters$aboveSample()[[id]]) {
				above.samples <- append(above.samples, id)
			}
		}        
        callModule(multipleExperimentViewer, "myMultipleExperimentViewer", fsa.data, colors, multipleViewerSamplesSelected, selected.height, selected.width, selected.scale, selected.dyes, reactive(pageSelected$samplesPerPage()), reactive(pageSelected$pageNb()), reactive(above.samples))

      })
      observe({
        req(selected.samples$selectedSamples()$datapath)
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
        bin.file <- file.path("www","data", "markers",selected.analysis$markersList()[marker.file == basename(selected.analysis$selectedMarkers())]$bin.file)
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
      
      
   
})
