library(data.table)
library(plotly)
library(shinyalert)


source("functions.R")
source("exportPeaks.R")
source("sampleSelector.R")
source("dyeSelector.R")
source("scaleSelector.R")
source("multipleExperimentViewer.R")
source("singleExperimentViewer.R")
source("linearRegressionViewer.R")
source("peakAnalyzer.R")
source("heightSelector.R")
source("widthSelector.R")
source("rawData.R")
source("rawDataFilter.R")
source("singleExperimentFiltersAndLayouts.R")
source("singleExperimentPeakAnalyzer.R")
source("analysisParameters.R")


shinyServer(function(input, output,session) {
      colors <- list("6-FAM" = list(color = '#0101DF', cval = "blue"),
                "VIC" =  list(color = '#31B404', cval = "green"),
                "NED" = list(color = '#FFFF00', cval = "yellow"),
                "PET" =  list(color = '#FF0000', cval = "red"),
                "LIZ" =  list(color = '#FFBF00', cval = "orange"),
				"ROX"= list(color = "#FF0000", cval = "red"),
				"HEX"= list(color = "#00FF00", cval = "green")
				)
      fsa.data <- reactiveValues(data = NULL, standardized.data = NULL, bins = NULL, markers = NULL, peaks = NULL, binpeaks = NULL)
      selected.samples <- callModule(sampleSelector, "mysampleselector")
      selected.dyes <- callModule(dyeSelector, "mydyeselector", reactive(fsa.data$data))
      rawDataPeaksFilter <- callModule(rawDataPeaksFilter, "rawdatapeaksfilter", reactive(fsa.data$data))
      singleExperimentFiltersAndLayouts <- callModule(singleExperimentFiltersAndLayouts,'mySingleExperimentFiltersAndLayouts',reactive(fsa.data))
      selected.scale <- callModule(scaleSelector, "myscaleselector", reactive(fsa.data$data))
      selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)      
      selected.analysis <- callModule(peakAnalyzer, "mypeakanalyzer", selected.scale, reactive(fsa.data))
      selected.height <- callModule(heightSelector, "myheightselector", reactive(fsa.data), selected.dyes)
      selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)
      callModule(rawDataViewer, "myrawdataviewer", reactive(fsa.data), rawDataPeaksFilter)
      parameters <- callModule(analysisParameters, "myAnalysisParameters", reactive(fsa.data))
      observe({
        req(selected.samples$selectedSamples()$datapath)
        fsa.data$data <- my.read.fsa(selected.samples$selectedSamples()$datapath)

      })
      observe({
        req(selected.scale$selectedScale())
        req(length(parameters$minPeakHeight()) > 0)
        standard.dye <- selected.scale$scalingDye()

        if (is.null(standard.dye)) {
          standard.dye = "None"
        }
        print(parameters$minPeakHeight())
        fsa.data$standardized.data <- scale.timeseries(fsa.data$data, ladder = selected.scale$selectedScale(), standard.dye = standard.dye, minpeakheights = parameters$minPeakHeight())


        selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)      

      })
      observe({
        req(selected.scale$selectedScale())
        req(selected.dyes$selectedDyes())
        req(selected.height$selectedHeight())
        req(selected.width$selectedWidth())
        req(fsa.data$standardized.data$intensities)
        callModule(multipleExperimentViewer, "myMultipleExperimentViewer", fsa.data,colors, selected.height, selected.width,selected.scale, selected.dyes)
        
        
      })
      observe({
        req(selected.samples$selectedSamples()$datapath)        
        callModule(linearRegressionViewer, "myLinearRegressionViewer", fsa.data)
      })
      observe({
        req(selected.analysis$selectedMarkers() != 'None')

        markers <- fread(selected.analysis$selectedMarkers())
        fsa.data$markers <- markers
        bin.file <- file.path("www","data", "markers",selected.analysis$markersList()[marker.file == basename(selected.analysis$selectedMarkers())]$bin.file)
        ladder.sample <- NULL
        ids <- names(parameters$ladderSample())
		for (id in ids) {
			if (parameters$ladderSample()[[id]]) {
				ladder.sample <- id
			}
		}
        
		peaks <- peaks.to.markers(fsa.data, parameters$minPeakHeight(),  selected.analysis$removeStutters())

        if (file.exists(bin.file) && !is.null(ladder.sample)) {
          markers.bins <-  read.bin.file(bin.file)
		  myfsa <- list(data = fsa.data$data, standardized.data  =  fsa.data$standardized.data, markers = fsa.data$markers, bins= markers.bins, peaks=peaks)
		  save(file = "www/brol.rdata", list = c("myfsa"))                
    
		  peaks.bin <- markedpeaks.to.real.bins(markers.bins, peaks, ladder.sample)
		  if (length(peaks.bin$error) > 0) {
		    print(peaks.bin$error)

			shinyalert(text = peaks.bin$error)
    	  }
		  
		  bin.offset <- bins.position(markers.bins, peaks.bin$binnedpeaks, ladder.sample);
		  # AJOUT DES BINS VIRTUELS
		  
		  
		  fsa.data$peaks <- peaks.bin$binnedpeaks
		  fsa.data$bins <- bin.offset

        } else {
		  fsa.data$peaks <- peaks
		  fsa.data$bins <- NULL
        }



        selected.peaks <- callModule(singleExperimentViewer, "mySingleExperimentViewer", fsa.data, colors, singleExperimentFiltersAndLayouts$singleExperimentFilterDyes, singleExperimentFiltersAndLayouts$singleExperimentFilterExp, singleExperimentFiltersAndLayouts$singleExperimentYaxis,singleExperimentFiltersAndLayouts$singleExperimentFilterSystem, singleExperimentFiltersAndLayouts$singleExperimentSystemDyeSelector,singlePeakAnalyzer$minValueFilterThresholdField, singlePeakAnalyzer$minValueFilterThresholdButton, singlePeakAnalyzer$includeExcludeButton, reactive(ladder.sample))
        singlePeakAnalyzer <- callModule(singleExperimentPeakAnalyzer, "mySingleExperimentPeakAnalyzer", reactive(selected.peaks$selected.peak()) )     
        callModule(exportPeaks, "myExportPeaks",  reactive(selected.peaks$exportPeaksTable()), colors, selected.analysis$selectedMarkers, selected.analysis$markersList, reactive(fsa.data$data$expdate), singleExperimentFiltersAndLayouts$singleExperimentFilterExp)
      })
   
})
