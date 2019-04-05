library(plotly)
library(shiny)
library(DT)
library(XLConnect)
# module UI function
dataExporterUI <- function(id){
  ns <- NS(id)
  fluidRow(
	DT::dataTableOutput(ns("exportTable")),
	downloadButton(ns("export"), "Export")
  )
}





# module server function
dataExporter <- function(input, output, session, dataExporterFilters, fsa.data, annotated.peaks, colors, markers, markersList, seqdates) {
    ns <- session$ns
    table <- reactiveValues(activeTable = NULL)
    
	runName <- reactive( {
		sample.date <- seqdates()[1]
		shortName = "run"
		if (is.null(markersList())) {
		  shortName = "run"
		} else {
		
			shortName <- markersList()[marker.file == basename(markers())][['runName']]
		}
		result <- paste(shortName, sample.date ,sep = "-")
		if (result == "") {
		  result <- "run"
		}
		result
	})    
    
    rawDataExport <- reactive({
		result <- fsa.data()$data$intensities;
		result$sizes <- NULL    
		result <- result[id %in% dataExporterFilters$selected.samples()]

		result
    })

    stddataExport <- reactive({
		result <- fsa.data()$standardized.data$intensities[id %in% dataExporterFilters$selected.samples()];
		result$sizes <- round(result$sizes, 0)
		result
    })    
    peaksExport <- reactive({
		result <- annotated.peaks()[keep == T & id %in% dataExporterFilters$selected.samples()]
        result$endpos.size <- round(result$endpos.size, 0)
        result$startpos.size <- round(result$startpos.size, 0)
		result$maxpos.size <- round(result$maxpos.size, 0)		
		result
    })    
    
    systemExport <- reactive({
    
		result.table.list <- list()
		ids <- unique(annotated.peaks()[keep == T]$id)
		ids <- intersect(ids,dataExporterFilters$selected.samples())
		bins <- !(is.null(annotated.peaks()$bin))
		for (k in 1:length(ids)) {
			
			idi <- ids[k]
			annotated.peaks.idi <- annotated.peaks()[id == idi]
			result.table <- unique(data.table(
				"Sample Name" = idi,
				"run name" =  runName(),
				"Marker" = annotated.peaks.idi$system,
				"Dye" =  annotated.peaks.idi$dye
			))
			result.table <- result.table[!is.na(Marker)]
			supcols <- max(table(annotated.peaks.idi[keep == T & !is.na(system)]$system))

			print(result.table)
			for (s in 1:supcols) {
				newsize <- paste("Size", s)
				newheight <- paste("Height", s)
				newbin <- paste("bin", s)
				result.table[[newsize]] <- NA
				result.table[[newheight]] <- NA
				result.table[[newbin]] <- NA
				result.table[[newsize]] <- as.numeric(result.table[[newsize]])
				result.table[[newheight]] <- as.numeric(result.table[[newheight]])
				result.table[[newbin]] <- as.character(result.table[[newbin]])
				if (! bins) {
				  result.table[[newbin]] <-NULL
				}
			}


			
			for (i in 1:nrow(result.table)) {
				# get system 
				systemi <- result.table[i][['Marker']]
				if (is.na(systemi)) next
				peaks <- annotated.peaks.idi[system == systemi & !is.na(system)]

				result.table[Marker == systemi][["Dye"]] <- toupper(substr(colors[[result.table[Marker == systemi][["Dye"]]]]$cval, 1,1))
				for (j in 1:nrow(peaks)) {
					result.table[Marker == systemi][[paste("Size", j)]] <- peaks$maxpos.size[j]
					result.table[Marker == systemi][[paste("Height", j)]] <- peaks$peak.height[j]
					if (!is.null(peaks$bin[j]) && bins) {
						result.table[Marker == systemi][[paste("bin", j)]] <- peaks$bin[j]
					}
				}

			}

			result.table.list[[k]] <- result.table
		}
		rbindlist(result.table.list, use.names = T, fill = T)
	})
    
    
    output$exportTable <- DT::renderDataTable({
      req(dataExporterFilters$exportType())
      result <- data.table()
      if (dataExporterFilters$exportType() == 'Raw data') {
		result <- rawDataExport()
      } else if (dataExporterFilters$exportType() == 'Standardized data') {
		result <- stddataExport()
      } else if (dataExporterFilters$exportType() == 'Peaks') {
		result <- peaksExport()
	  } else if (dataExporterFilters$exportType() == 'Systems') {
	    result <- systemExport()
	  }
	  table$activeTable <- result


      datatable(
		result,
		extension = 'Scroller',
        options = list(
            scroller = TRUE,
            scrollY = 600,        
            scrollX = TRUE,
            dom = 'Bfrtip',
            searching = FALSE
        ),
        rownames = FALSE,
        selection = list(mode='none')
      )
      
    }) 
    
  output$export <- downloadHandler(
    filename = function(file) {
      paste0(runName(), ".", dataExporterFilters$exportFormat())
    },
    content = function(file){
		if (dataExporterFilters$exportFormat() == 'xlsx') {
			## create xls workbook and save to sheets
			wb = loadWorkbook(file,create=TRUE)
			createSheet(wb,name="Pickpeakdata")
			writeWorksheet(wb,table$activeTable,sheet = "Pickpeakdata")
			setColumnWidth(wb, sheet = "Pickpeakdata", column = 1:ncol(table$activeTable), width = -1)
			saveWorkbook(wb)
		} else if (dataExporterFilters$exportFormat() == 'csv') {
			write.table(table$activeTable, file = file, sep = ",", quote = F, row.names = F)
		} else if (dataExporterFilters$exportFormat() == 'tab') {
			write.table(table$activeTable, file = file, sep = "\t", quote = F, row.names = F)
		}
    }
    ,contentType="application/xls"
  )
  1

    
}
