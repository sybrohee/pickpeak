source("loadSaveParams.R")
source("sampleSelector.R")
source("peakAnalyzer.R")
source("selectedPeakDisplayer.R")
source("dyeSelector.R")
source("scaleSelector.R")
source("multipleViewerSampleSelector.R")
source("multipleExperimentViewer.R")
source("singleExperimentViewer.R")
source("heightSelector.R")
source("widthSelector.R")
source("dataExporterFilter.R")

source("dataExporter.R")
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
						column(11, id = 'peakFilterPanel',
							fluidRow(
								column(11,
									uiOutput("analysistype")
								)
							),
							conditionalPanel(condition = "input.analysistype == 'custom'",
								fluidRow(
									column(12, 
											analysisParametersUI("myAnalysisParameters"),
											scaleSelectorUI("myscaleselector"),
											peakAnalyzerUI("mypeakanalyzer")
									)
								)
							),
							conditionalPanel(condition = "input.analysistype == 'predefined'",
								fluidRow(
									column(12, uiOutput("predefinedparametersTitle"))
								),
								fluidRow(
									column(12, loadSaveParamsUI("myloadSaveParams"))
								)
							)

						)
					),
                    HTML("<br>"),
                    fluidRow(
						column(11, id = 'peakFilterPanel',
							fluidRow(
								column(4, dyeSelectorUI("mydyeselector")),
								column(8, multipleViewersampleSelectorUI("mymultipleViewersampleSelector"))
							),
							fluidRow(
								column(1, fluidRow(" ")),
								column(9, 
									fluidRow(
										heightSelectorUI("myheightselector"),
										widthSelectorUI("mywidthselector")
									)
								
								),
								column(1, fluidRow(" "))
							)
						)
					)

                )

                
            ),
            conditionalPanel(
                condition = "input.tabs1 == 'Data export'",
                fluidRow(
                    column(10, dataExporterFilterUI('mydataExportFilter'))
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
                        column(10, id = 'peakFilterPanel', 
							singleExperimentPeakAnalyzerUI('mySingleExperimentPeakAnalyzer'),
							selectedPeakDisplayerUI("myselectedpeakdisplayer")
						)
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

                ) ,
                tabPanel(
                    "Data export",
                    dataExporterUI("mydataexporter")

                )                 
            )
                
        )        
    )     
    
  )
)
