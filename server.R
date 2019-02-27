library(data.table)
library(plotly)
source("functions.R")
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



shinyServer(function(input, output,session) {
      fsa.data <- reactiveValues(data = NULL, standardized.data = NULL, markers = NULL, peaks = NULL)
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
      
      observe({
        req(selected.samples$selectedSamples()$datapath)
        print(selected.samples$selectedSamples()$datapath)
        fsa.data$data <- my.read.fsa(selected.samples$selectedSamples()$datapath)

      })
      observe({
        req(selected.scale$selectedScale())
        standard.dye <- selected.scale$scalingDye()

        print(standard.dye)
        if (is.null(standard.dye)) {
          standard.dye = "None"
        }
        
        fsa.data$standardized.data <- scale.timeseries(fsa.data$data, ladder = selected.scale$selectedScale(), standard.dye = standard.dye)
        selected.width <- callModule(widthSelector, "mywidthselector", reactive(fsa.data), selected.scale)      

      })
      observe({
        req(selected.scale$selectedScale())
        req(selected.dyes$selectedDyes())
        req(selected.height$selectedHeight())
        req(selected.width$selectedWidth())
        req(fsa.data$standardized.data$intensities)
        print(fsa.data$standardized.data$intensities)
        callModule(multipleExperimentViewer, "myMultipleExperimentViewer", fsa.data,selected.height, selected.width,selected.scale, selected.dyes)
        
        
      })
      observe({
        req(selected.samples$selectedSamples()$datapath)        
        callModule(linearRegressionViewer, "myLinearRegressionViewer", fsa.data)
      })
      observe({
        req(selected.analysis$selectedMarkers() != 'None')
        markers <- fread(selected.analysis$selectedMarkers())
        fsa.data$markers <- markers
        fsa.data$peaks <- peaks.to.markers(fsa.data)
        callModule(singleExperimentViewer, "mySingleExperimentViewer", fsa.data, singleExperimentFiltersAndLayouts$singleExperimentFilterDyes, singleExperimentFiltersAndLayouts$singleExperimentFilterExp, singleExperimentFiltersAndLayouts$singleExperimentYaxis)
      })
   
})
