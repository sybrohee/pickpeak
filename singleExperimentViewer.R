library(plotly)
library(shiny)
library(DT)

# module UI function
singleExperimentViewerUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
        column(4, 
            DT::dataTableOutput(ns("exportPeaksTable"))
        ),    
        column(8,
            plotlyOutput(ns("singleExperimentPlot"), height = "800px")
        )

        
    ) 
  )

}

# module server function
singleExperimentViewer <- function(input, output, session, fsa.data, singleExperimentFilterDyes, singleExperimentFilterExp, singleExperimentYaxis) {
    ns <- session$ns
    annotated.peaks <- reactiveValues(mypeaks = {data.table(fsa.data$peaks, keep = T)})
    
    output$exportPeaksTable <- DT::renderDataTable({
      req(fsa.data$peaks)
      peaksToExportDT <- annotated.peaks$mypeaks[keep == T]
      peaksToExportDT$pos <- paste0("[",peaksToExportDT$start.pos,"-", peaksToExportDT$end.pos,"]")
      names(peaksToExportDT)[which(names(peaksToExportDT) == 'maxpos.size')] <- "size"
      names(peaksToExportDT)[which(names(peaksToExportDT) == 'peak.height')] <- "height"

      peaksToExportDT <- peaksToExportDT[id %in% singleExperimentFilterExp()]
      peaksToExportDT <- peaksToExportDT[!is.na(system)]
      peaksToExportDT <- peaksToExportDT[dye %in% singleExperimentFilterDyes()]
      colorder <-  c("system", "size", "height", "cytoband"  , "pos", "dye"  , "color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep")
      colstohide <- c("color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep", "start.pos",  "end.pos")
      setcolorder(peaksToExportDT, colorder)
      
      print(peaksToExportDT)
      datatable(
        peaksToExportDT,
        options = list(
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = I('colvis'),
            columnDefs = list(
                list(targets = (which(names(peaksToExportDT) %in% colstohide)-1), visible = FALSE)
            )
        ),
        rownames = FALSE,
        selection = list(mode='single'),
      ) %>% formatRound("size",0) %>%
            formatRound("endpos.size",0) %>%
            formatRound("startpos.size",0)
      
      
    }) 
    
    output$singleExperimentPlot <- renderPlotly({
        colors <- c("6-FAM" = '#0101DF',
                "VIC" = "#31B404",
                "NED" = "#FFFF00",
                "PET" = "#FF0000",
                "LIZ" = "#FFBF00")
        f <- list(
            family = "sans serif",
            size = 10,
            color = "#000000"
        )


#         print(singleExperimentFilterDyes())
        idi <- singleExperimentFilterExp()
        dyes <- unique(annotated.peaks$mypeaks$dye) 
        dyes <- intersect(singleExperimentFilterDyes(), dyes)
        systems <- sort(unique(annotated.peaks$mypeaks$system))

        intensities <- fsa.data$standardized.data$intensities
        plots <- list()

        

        for (systemi in systems) {
          dyei <- unique(annotated.peaks$mypeaks[system == systemi]$dye)
          if (! dyei %in% dyes) next;
          startpos <-  fsa.data$markers[system == systemi]$start.pos
          endpos <- fsa.data$markers[system == systemi]$end.pos
          intensities.id <- intensities[id == idi  & intensities$sizes >= startpos & intensities$sizes <= endpos]
          if (nrow(intensities.id) == 0) next;
          annots = annots <- list( text = paste(dyei, systemi), font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
          p <- plot_ly() %>% layout(height = 800, annotations = annots,  yaxis = list(range = singleExperimentYaxis()));
          p <- add_trace(p, x = intensities.id[['sizes']], y= intensities.id[,get(dyei)],  type = 'scatter', mode = 'lines', line = list(color =  colors[dyei]), showlegend = F, hoverinfo = 'x+y')
          if (!is.null(peaks)) {
            if (length( annotated.peaks$mypeaks[!is.na(system) & system == systemi &id == idi & dye == dyei][['maxpos.size']]) > 0) { 
              p <- add_trace(p, x = annotated.peaks$mypeaks[!is.na(system)& system == systemi  & id == idi & dye == dyei][['maxpos.size']], name = paste0(sample(letters, 2), collapse = ""), y = annotated.peaks$mypeaks[!is.na(system) & system == systemi & id == idi & dye == dyei][['peak.height']], marker = list(size = 6, color = colors[dyei],line = list(color = colors[dyei], width = 1)), text = annotated.peaks$mypeaks[!is.na(system)& system == systemi  &id == idi & dye == dyei][['system']], showlegend = F, hoverinfo = 'text');
            }
          }          
          plots[[systemi]] <- p 
        }
        
        print(names(plots))
        sp <- subplot(plots,nrows = ceiling(length(names(plots))/2))
        sp
        
    })    

}
