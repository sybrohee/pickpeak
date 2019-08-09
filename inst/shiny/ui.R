# library(shiny)
# library(data.table)
# library(plotly)
# library(shinyalert)
# library(rjson)
# library(DT)
# library(XLConnect)
# library(seqinr)
# library(shinyjs)
# library(pracma)
# library(shinyWidgets)

source("analysisParameters.R")
source("dataExporterFilter.R")
source("dataExporter.R")
source("dyeSelector.R")
source("exportPeaks.R")
source("functions.R")
source("heightSelector.R")
source("linearRegressionViewer.R")
source("loadSaveParams.R")
source("multipleExperimentsRefresher.R")
source("multipleExperimentViewer.R")
source("multipleViewerPageSelector.R")
source("multipleViewerSampleSelector.R")
source("peakAnalyzer.R")
source("reinitializer.R")
source("sampleSelector.R")
source("samplesLoadInitializer.R")
source("scaleSelector.R")
source("selectedPeakDisplayer.R")
source("server.R")
source("singleExperimentFiltersAndLayouts.R")
source("singleExperimentPeakAnalyzer.R")
source("singleExperimentViewer.R")
source("widthSelector.R")


shinypackage_ui <- function(launch_param) {
  fluidPage(
    useShinyjs(),
    tags$head(tags$style(HTML('#peakFilterPanel {border: 1px solid #D7D7D7; border-radius: 5px;}'))),  
    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                #inline .form-group { display: table-row;}")
    ),
    useShinyalert(),

    fluidRow(
		
        column(
            2, 
            column(11, id = 'peakFilterPanel',
				conditionalPanel(
					condition = "output.mode == 'loading'",
					fluidRow(
						column(12, 
							sampleSelectorUI("mysampleselector")
						
						
						) #only present when in loading mode
					),
					conditionalPanel(
						condition = "output.files > 0",
						fluidRow(
							column(12, 
								samplesLoadInitializerUI("mysamplesloadinitializer"),
								fluidRow(
									column(1,  HTML("<br>&nbsp;"))
								)							
							
							) #only present when in loading mode and at least one file selected
						)
					)

				)
				
				
				
  		    ),


            conditionalPanel(
                condition = "input.tabs1 == 'Experiment viewer'",
                column(1, fluidRow()),
                column(11, 
					fluidRow(

					  conditionalPanel(
							condition = "output.files > 0 && output.mode == 'analysis'",
							
							fluidRow(
								column(1, 
									fluidRow(
										HTML("&nbsp;")
									)
								),							
								column(4, 
									fluidRow(
										reinitializerUI("myreinitializer")
									)
								),
								column(1, 
									fluidRow(
										HTML("&nbsp;")
									)
								),							
								column(4, 
									fluidRow(
										multipleExperimentsRefresherUI("mymultipleexperimentsrefresher")
									)
								)
							),
							
							fluidRow(HTML("<BR>")),
							
							
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
						)
					),
                    HTML("<br>"),
                    
                    fluidRow(
					  conditionalPanel(
							condition = "output.files > 0 && output.mode == 'analysis'",                 
							column(11, id = 'peakFilterPanel',
								fluidRow(
									column(4, dyeSelectorUI("mydyeselector")),
									column(8, multipleViewersampleSelectorUI("mymultipleViewersampleSelector"))
								),
								fluidRow(
									column(12, multipleViewerPageSelectorUI("mymultipleviewerpageselector"))
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
								),
								fluidRow(
									column(10, fluidRow(" "))
								)

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
}
