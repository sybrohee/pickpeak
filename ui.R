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
source("singleExperimentFiltersAndLayouts.R")

source("linearRegressionViewer.R")

library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    fluidRow(
        column(
            2, 
            sampleSelectorUI("mysampleselector"),
            conditionalPanel(
                condition = "input.tabs1 == 'Multiple experiment viewer'",
                fluidRow(
                    column(5, dyeSelectorUI("mydyeselector")),
                    column(5, 
                        fluidRow(
                            heightSelectorUI("myheightselector"),
                            widthSelectorUI("mywidthselector")
                        )
                    
                    )
                ),
                scaleSelectorUI("myscaleselector"),
                peakAnalyzerUI("mypeakanalyzer")
            ),
            conditionalPanel(
                condition = "input.tabs1 == 'Raw data' && input.tabsData == 'Peaks'",
                fluidRow(
                    column(5, rawDataPeaksFilterUI('rawdatapeaksfilter'))
                )
            ),
            conditionalPanel(
                condition = "input.tabs1 == 'Single experiment viewer'",
                fluidRow(
                    column(7, singleExperimentFiltersAndLayoutsUI('mySingleExperimentFiltersAndLayouts'))
                )
            )            
        ),
        column(
            10,
            tabsetPanel(
                type = "tabs",
                id ="tabs1",
                tabPanel(
                    "Multiple experiment viewer",
                    multipleExperimentViewerUI("myMultipleExperimentViewer")
                ),
                tabPanel(
                    "Single experiment viewer",
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
