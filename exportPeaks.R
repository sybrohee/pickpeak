# Variant Explorer shiny App
# Nicolas Simonis, Institut de Pathologie et Genetique, Gosselies

# Export Module

library(shiny)
library(shinyjs)
library(XLConnect)

exportPeaksUI <- function(id){
  ns <- NS(id)
  fluidRow(
     column(10,uiOutput(ns("runName"))) ,
     column(10,downloadButton(ns("ExportPeaks"), "Export peaks (xlsx)"))
  )
}

exportPeaks <- function(input,output,session, exportPeaksTable, colors, markers, markersList, seqdates, selected.sample ) {
  ns <- session$ns
  
  runName <- reactive( {
    sample.date <- seqdates()[selected.sample()]
    shortName <- markersList()[marker.file == basename(markers())][['runName']]
    paste(shortName, sample.date ,sep = "-")  
  })
  

  
  
  output$runName <- renderUI({


    textInput(ns("runName"), "Run name", value = runName())
  })
 
  output$ExportPeaks <- downloadHandler(
    filename = function(file) {
      paste0(input$runName, ".xlsx")
    },
    content = function(conn){
      result.table <- unique(data.table(
        "Sample Name" = selected.sample(),
        "run name" = input$runName,
        "Marker" = exportPeaksTable()$system,
        "Dye" =  exportPeaksTable()$dye
      ))
      # max number of detected peaks per system
      supcols <- max(table(exportPeaksTable()$system))
      for (i in 1:supcols) {
        newsize <- paste("Size", i)
        newheight <- paste("Height", i)
        result.table[[newsize]] <- NA
        result.table[[newheight]] <- NA
        result.table[[newsize]] <- as.numeric(result.table[[newsize]])
        result.table[[newheight]] <- as.numeric(result.table[[newheight]])
      }
      
      for (i in 1:nrow(result.table)) {
        # get system 
        systemi <- result.table[i][['Marker']]
        peaks <- exportPeaksTable()[system == systemi]
        result.table[Marker == systemi][["Dye"]] <- toupper(substr(colors[[result.table[Marker == systemi][["Dye"]]]]$cval, 1,1))
        for (j in 1:nrow(peaks)) {
          result.table[Marker == systemi][[paste("Size", j)]] <- peaks$size[j]
          result.table[Marker == systemi][[paste("Height", j)]] <- peaks$height[j]
        }
        print(peaks$size[j])
        print(result.table)
      }
      
      
      ## create xls workbook and save to sheets
      wb = loadWorkbook(conn,create=TRUE)
      createSheet(wb,name="Peaks")
      writeWorksheet(wb,result.table,sheet = "Peaks")
      setColumnWidth(wb, sheet = "Peaks", column = 1:ncol(result.table), width = -1)
      saveWorkbook(wb)
    }
    ,contentType="application/xls"
  )
  return(1)
}


