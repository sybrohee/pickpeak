library(plotly)
library(shinyWidgets)

library(DT)

# module UI function
singleExperimentViewerUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
        column(4, 
            htmlOutput(ns("exportPeaksTableTitle")),
            DT::dataTableOutput(ns("exportPeaksTable")),
            htmlOutput(ns("filteredOutPeaksTitle")),
            DT::dataTableOutput(ns("filteredOutPeaks"))
        ),    
        column(8,
            plotlyOutput(ns("singleExperimentPlot"), height = "800px")
        )
    ) 
  )

}

# module server function
singleExperimentViewer <- function(input, output, session, fsa.data, singleExperimentFilterDyes, singleExperimentFilterExp, singleExperimentYaxis, singleExperimentFilterSystem, singleExperimentSystemDyeSelector,minValueFilterThresholdField, minValueFilterThresholdButton, includeExcludeButton) {
    ns <- session$ns
    proxyFO <- dataTableProxy("filteredOutPeaks")
    proxySE <- dataTableProxy("exportPeaksTable")
    
    
    annotated.peaks <- reactiveValues(mypeaks = {data.table(fsa.data$peaks, keep = T)})
    selected.peak <- reactiveValues(peak = NULL)
    
    colorder <-  c("system", "size", "height", "cytoband"  , "pos", "dye"  , "color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep")
    colstohide <- c("color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep", "start.pos",  "end.pos")    
    
    observeEvent(includeExcludeButton(), {
      newval <- T
      if (selected.peak$peak$keep) {
        newval <- F
      }
      annotated.peaks$mypeaks[id == singleExperimentFilterExp() & peak.height == selected.peak$peak$height & system == selected.peak$peak$system & maxpos.size == selected.peak$peak$size & dye == selected.peak$peak$dye][["keep"]] <- newval
    })
    observeEvent(minValueFilterThresholdButton(), {
      min.thr <- minValueFilterThresholdField()
      idi <- singleExperimentFilterExp()
      annotated.peaks$mypeaks[id == idi & peak.height < min.thr]$keep  <- F
    })
    observeEvent(input$filteredOutPeaks_rows_selected, {
      selected.peak$peak <- peaksToFilterOutDT()[input$filteredOutPeaks_rows_selected,]

      selectRows(proxySE, NULL)

    })    
    observeEvent(input$exportPeaksTable_rows_selected, {
      selected.peak$peak <- peaksToExportDT()[input$exportPeaksTable_rows_selected,]

      selectRows(proxyFO, NULL)
    })    

    
    peaksToExportDT <- reactive({
      req(fsa.data$peaks)
      peaksToExportDT <- annotated.peaks$mypeaks[keep == T]
      peaksToExportDT$pos <- paste0("[",peaksToExportDT$start.pos,"-", peaksToExportDT$end.pos,"]")
      names(peaksToExportDT)[which(names(peaksToExportDT) == 'maxpos.size')] <- "size"
      names(peaksToExportDT)[which(names(peaksToExportDT) == 'peak.height')] <- "height"

      peaksToExportDT <- peaksToExportDT[id %in% singleExperimentFilterExp()]
      peaksToExportDT <- peaksToExportDT[!is.na(system)]
      if (singleExperimentSystemDyeSelector() == 'dye') {
        peaksToExportDT <- peaksToExportDT[dye %in% singleExperimentFilterDyes()]
      } else if (singleExperimentSystemDyeSelector() == 'system') {
        peaksToExportDT <- peaksToExportDT[system %in% singleExperimentFilterSystem()]
      }
      setcolorder(peaksToExportDT, colorder)
      peaksToExportDT
    })
    
    peaksToFilterOutDT <- reactive({
      req(fsa.data$peaks)
      peaksToFilterOutDT <- annotated.peaks$mypeaks[keep == F]
      peaksToFilterOutDT$pos <- paste0("[",peaksToFilterOutDT$start.pos,"-", peaksToFilterOutDT$end.pos,"]")
      names(peaksToFilterOutDT)[which(names(peaksToFilterOutDT) == 'maxpos.size')] <- "size"
      names(peaksToFilterOutDT)[which(names(peaksToFilterOutDT) == 'peak.height')] <- "height"

      peaksToFilterOutDT <- peaksToFilterOutDT[id %in% singleExperimentFilterExp()]
      peaksToFilterOutDT <- peaksToFilterOutDT[!is.na(system)]
      if (singleExperimentSystemDyeSelector() == 'dye') {
        peaksToFilterOutDT <- peaksToFilterOutDT[dye %in% singleExperimentFilterDyes()]
      } else if (singleExperimentSystemDyeSelector() == 'system') {
        peaksToFilterOutDT <- peaksToFilterOutDT[system %in% singleExperimentFilterSystem()]
      }
      

      setcolorder(peaksToFilterOutDT, colorder)
      peaksToFilterOutDT
    })
    
    output$exportPeaksTableTitle <- renderText({
        req(nrow(peaksToExportDT()) > 0)
        "<h3>Selected peaks</H3>"
    })
    output$filteredOutPeaksTitle <- renderText({
        req(nrow(peaksToFilterOutDT()) > 0)
        "<h3>Excluded peaks</h3>"
    })
    
    output$filteredOutPeaks <- DT::renderDataTable({
      req(nrow(peaksToFilterOutDT()) > 0)
      datatable(
        peaksToFilterOutDT(),
        extension = 'Scroller',
        options = list(
            scroller = TRUE,
            scrollY = 250,        
            scrollX = TRUE,
            dom = 'Bfrtip',
            searching = FALSE,
            buttons = I('colvis'),
            columnDefs = list(
                list(targets = (which(names(peaksToFilterOutDT()) %in% colstohide)-1), visible = FALSE)
            )
        ),
        rownames = FALSE,
        selection = list(mode='single'),
      ) %>% formatRound("size",0) %>%
            formatRound("endpos.size",0) %>%
            formatRound("startpos.size",0)
      
      
    })
    
    output$exportPeaksTable <- DT::renderDataTable({
      req(fsa.data$peaks)
      datatable(
        peaksToExportDT(),
        extension = 'Scroller',
        options = list(
            searching = FALSE,
            scroller = TRUE,
            scrollY = 300,  
            dom = 'Bfrtip',
            buttons = I('colvis'),
            columnDefs = list(
                list(targets = (which(names(peaksToExportDT()) %in% colstohide)-1), visible = FALSE)
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
        req(length(singleExperimentFilterDyes()) > 0 || length(singleExperimentFilterSystem()) > 0)
        selected.peak <- peaksToExportDT()[input$exportPeaksTable_rows_selected]
        
        




        idi <- singleExperimentFilterExp()
        dyes <- unique(annotated.peaks$mypeaks$dye) 
        systems <- sort(unique(annotated.peaks$mypeaks$system))
        if (singleExperimentSystemDyeSelector() == "dye") {
          dyes <- intersect(singleExperimentFilterDyes(), dyes)
        } else if (singleExperimentSystemDyeSelector() == "system") {
          systems <- intersect(systems, singleExperimentFilterSystem())
        
        }

        intensities <- fsa.data$standardized.data$intensities
        plots <- list()
        shapelist <- list()
        

        for (systemi in systems) {
          dyei <- unique(annotated.peaks$mypeaks[system == systemi]$dye)
          if (! dyei %in% dyes) next;
          startpos <-  fsa.data$markers[system == systemi]$start.pos
          endpos <- fsa.data$markers[system == systemi]$end.pos
          intensities.id <- intensities[id == idi  & intensities$sizes >= startpos & intensities$sizes <= endpos]
          if (nrow(intensities.id) == 0) next;
          annots <- list( text = paste(dyei, systemi), font = f, xref = 'paper', yref ='paper', yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
          p <- plot_ly() %>% layout(annotations = annots, yaxis = list(range = singleExperimentYaxis()));
          p <- add_trace(p, x = intensities.id[['sizes']], y= intensities.id[,get(dyei)],  type = 'scatter', mode = 'lines', line = list(color =  colors[dyei]), showlegend = F, hoverinfo = 'x+y')
          if (length( annotated.peaks$mypeaks[!is.na(system) & system == systemi &id == idi & keep == T &  dye == dyei][['maxpos.size']]) > 0) { 
#             print(data.table(annotated.peaks$mypeaks[!is.na(system)& system == systemi & keep == T & id == idi & dye == dyei][['maxpos.size']],  annotated.peaks$mypeaks[!is.na(system) & system == systemi & keep == T & id == idi & dye == dyei][['peak.height']]))
            p <- add_trace(p, x = annotated.peaks$mypeaks[!is.na(system)& system == systemi & keep == T & id == idi & dye == dyei]$maxpos.size, name = paste0(sample(letters, 2), collapse = ""), y = annotated.peaks$mypeaks[!is.na(system) & system == systemi & keep == T & id == idi & dye == dyei]$peak.height, marker = list(size = 6, color = colors[dyei],line = list(color = colors[dyei], width = 1)), text = annotated.peaks$mypeaks[!is.na(system)& system == systemi  & keep == T&id == idi & dye == dyei][['system']], showlegend = F, hoverinfo = 'text',type = 'scatter', mode = 'markers');
          }
        
          plots[[systemi]] <- p 
          if (nrow(selected.peak) > 0 &&  selected.peak$system == systemi) {
            rect.col <- "blue"
            opacity = 0.3
            plotnb <- length(plots)
            shapelist <- append(shapelist, list(type = 'rect', fillcolor = rect.col, line = list( color = rect.col), opacity = opacity,x0 =  selected.peak$startpos.size, x1 = selected.peak$endpos.size, y0 = 0, y1 =  selected.peak$height+0.1*selected.peak$height, xref = paste0("x", plotnb), yref = paste0("y", plotnb)))
          }          
        }
        
        sp <- subplot(plots,nrows = ceiling(length(names(plots))/2))
        sp %>% layout(shapes = shapelist)
        
    })
    return(list(selected.peak = reactive(selected.peak$peak)))

}
