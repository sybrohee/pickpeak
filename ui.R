source("sampleSelector.R")
source("peakAnalyzer.R")
source("dyeSelector.R")
source("scaleSelector.R")
source("multipleExperimentViewer.R")
source("singleExperimentViewer.R")
source("heightSelector.R")
source("widthSelector.R")
source("rawData.R")
source("rawDataFilter.R")
source("exportPeaks.R")
source("analysisParameters.R")
source("singleExperimentPeakAnalyzer.R")
source("singleExperimentFiltersAndLayouts.R")

source("linearRegressionViewer.R")

library(shiny)
library(plotly)
library(shinyalert)



shinyUI(
  fluidPage(
    tags$head(tags$style(HTML('#peakFilterPanel {border: 1px solid #D7D7D7; border-radius: 5px;}'))),  
    useShinyalert(),

    fluidRow(
        column(
            2, 
            column(10, sampleSelectorUI("mysampleselector")),
            conditionalPanel(
                condition = "input.tabs1 == 'Experiment viewer'",
                column(1, fluidRow()),
                column(11, 
                    fluidRow(
                        column(10, analysisParametersUI("myAnalysisParameters"))
                       
                    ),
                    HTML("<br>"),
                    fluidRow(
                        column(10, id = 'peakFilterPanel', 
                                            scaleSelectorUI("myscaleselector"),
                                            peakAnalyzerUI("mypeakanalyzer")
                        )
                    ),
                    HTML("<br>"),
                    fluidRow(
                        column(5, dyeSelectorUI("mydyeselector")),
                        column(5, 
                            fluidRow(
                                heightSelectorUI("myheightselector"),
                                widthSelectorUI("mywidthselector")
                            )
                        
                        )
                    )

                )

                
            ),
            conditionalPanel(
                condition = "input.tabs1 == 'Raw data' && input.tabsData == 'Peaks'",
                fluidRow(
                    column(5, rawDataPeaksFilterUI('rawdatapeaksfilter'))
                )
            ),
            conditionalPanel(
                condition = "input.tabs1 == 'System viewer'",
                column(1, fluidRow()),
                column(11, 
                    fluidRow(
                        column(10, id = 'peakFilterPanel',singleExperimentFiltersAndLayoutsUI('mySingleExperimentFiltersAndLayouts'))
                    ),
                    HTML("<br>"),
                    fluidRow(
                        column(10, id = 'peakFilterPanel', singleExperimentPeakAnalyzerUI('mySingleExperimentPeakAnalyzer'))
                    ),
                    HTML("<br>"),
                    fluidRow(
                        column(10, id = 'peakFilterPanel', exportPeaksUI('myExportPeaks'))
                    )                    
                )
            )            
        ),
        column(
            10,
            tabsetPanel(
                type = "tabs",
                id ="tabs1",
                tabPanel(
                    "Experiment viewer",
                    multipleExperimentViewerUI("myMultipleExperimentViewer")
                ),
                tabPanel(
                    "System viewer",
                    singleExperimentViewerUI("mySingleExperimentViewer")
                ),                
     
                tabPanel(
                    "Standardization",
                    linearRegressionViewerUI("myLinearRegressionViewer")

                ),
                tabPanel(
                    "Raw data",
                    rawDataViewerUI("myrawdataviewer")

                )                
            )
                
        )        
    )     
    
  )
)
