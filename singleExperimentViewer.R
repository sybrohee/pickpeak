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
singleExperimentViewer <- function(input, output, session, fsa.data, colors, singleExperimentFilterDyes, singleExperimentFilterExp, singleExperimentYaxis, singleExperimentFilterSystem, singleExperimentSystemDyeSelector,allSystemsSameLine,minValueFilterThresholdField, minValueFilterThresholdButton, includeExcludeButton, ladder.sample) {
    ns <- session$ns
    proxyFO <- dataTableProxy("filteredOutPeaks")
    proxySE <- dataTableProxy("exportPeaksTable")
    
    

    selected.peak <- reactiveValues(peak = NULL, onclick = NULL, selected.range = NULL)
    curves.description <- reactiveValues(curves = NULL)
    plot.description <- reactiveValues(description = NULL)
    annotated.peaks <- reactiveValues(peaks = data.table(fsa.data$peaks, keep = T))
    addpeaks <- reactiveValues(system = NULL, from = NULL, to = NULL)
    
    
    
    colorder <-  c("system", "size", "height", "cytoband"  , "pos", "dye"  , "color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep")
    colstohide <- c("color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep", "start.pos",  "end.pos")    
    
    if (!is.null(fsa.data$peaks$bins)) {
      colorder <-  c("system", "bin", "size", "height", "cytoband"  , "pos", "dye"  , "color", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep")
      colstohide <- c("color","binsize" ,"minborder", "maxborder", "peak.maxpos.time",   "peak.startpos.time", "peak.endpos.time","id","startpos.size",  "endpos.size", "keep", "start.pos",  "end.pos")    
        
    }
    
    observeEvent(includeExcludeButton(), {
      req(selected.peak$peak)
      newval <- T
      
      
      
      if (selected.peak$peak$keep) {
        newval <- F
      }

      annotated.peaks$peaks[id == singleExperimentFilterExp() & peak.height == selected.peak$peak$height & system == selected.peak$peak$system & maxpos.size == selected.peak$peak$size & dye == selected.peak$peak$dye][["keep"]] <- newval

    })
    
    observeEvent(minValueFilterThresholdButton(), {
      min.thr <- minValueFilterThresholdField()
      idi <- singleExperimentFilterExp()
      annotated.peaks$peaks[id == idi & peak.height < min.thr]$keep  <- F
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
      req(singleExperimentSystemDyeSelector())
      peaksToExportDT <- annotated.peaks$peaks[keep == T]
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
#       print(annotated.peaks$peaks[keep == F])
      req(fsa.data$peaks)
      req(singleExperimentSystemDyeSelector())

      peaksToFilterOutDT <- annotated.peaks$peaks[keep == F]
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

		req(singleExperimentYaxis())

		
		req(singleExperimentFilterExp())

		req(length(singleExperimentFilterDyes()) > 0 || length(singleExperimentFilterSystem()) > 0)

        f <- list(
            family = "sans serif",
            size = 10,
            color = "#000000"
        )
        
        selected.peak <- selected.peak$peak

        idi <- singleExperimentFilterExp()
        dyes <- unique(fsa.data$markers$dye) 
        systems <- sort(unique(fsa.data$markers$system))
        
        if (singleExperimentSystemDyeSelector() == "dye") {
          dyes <- intersect(singleExperimentFilterDyes(), dyes)
        } else if (singleExperimentSystemDyeSelector() == "system") {
          systems <- intersect(systems, singleExperimentFilterSystem())
        }

        intensities <- fsa.data$standardized.data$intensities
        plots <- list()
        
        
        onOneLine <- F
        print("lala3")
        print(allSystemsSameLine())
		if (!is.null(allSystemsSameLine()) && allSystemsSameLine() && length(dyes) == 1 && singleExperimentSystemDyeSelector() == "dye") {
		  onOneLine <- T
		}
		if (onOneLine) {
		  markers.sorted <- fsa.data$markers[order(start.pos)]
		  
          systems <- unique(markers.sorted[dye == dyes]$system)
        }
 

        curves <- vector()
        for (systemi in systems) {
		  shapelist <- list()
          dyei <- unique(fsa.data$markers[system == systemi]$dye)
          
          if (! dyei %in% dyes) next;
          
          startpos <-  fsa.data$markers[system == systemi]$start.pos
          endpos <- fsa.data$markers[system == systemi]$end.pos

          
          
          intensities.id <- intensities[id == idi  & intensities$sizes >= startpos & intensities$sizes <= endpos]

          if (nrow(intensities.id) == 0) next;
          annots <- list( text = paste(dyei, systemi), font = f, xref = 'paper', yref ='paper', yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
          
          p <- plot_ly(source = "B" ) %>% layout(annotations = annots, yaxis = list(range = singleExperimentYaxis()));
          p <- add_trace(p, x = intensities.id[['sizes']], y= intensities.id[,get(dyei)],  type = 'scattergl', mode = 'lines', line = list(color =  colors[[dyei]]$color), showlegend = F, hoverinfo = 'x+y')
          curves <- append(curves, paste("points",systemi, idi,dyei, sep = "%%"))
          
          
          
          if (length( annotated.peaks$peaks[!is.na(system) & system == systemi &id == idi & keep == T &  dye == dyei][['maxpos.size']]) > 0) { 
            p <- add_trace(p, x = annotated.peaks$peaks[!is.na(system) & system == systemi & keep == T & id == idi & dye == dyei]$maxpos.size, name = paste0(sample(letters, 2), collapse = ""), y = annotated.peaks$peaks[!is.na(system) & system == systemi & keep == T & id == idi & dye == dyei]$peak.height, marker = list(size = 10, color = colors[[dyei]]$color,line = list(color = colors[[dyei]]$color, width = 1)), text = annotated.peaks$peaks[!is.na(system)& system == systemi  & keep == T&id == idi & dye == dyei][['system']], showlegend = F, hoverinfo = 'text',type = 'scattergl', mode = 'markers');
            curves <- append(curves, paste("peaks", systemi,idi,dyei, sep = "%%"))
          }
          
          if (!is.null(fsa.data$peaks$bins)) {
            ladder.bins <- fsa.data$bins[system == systemi]
            p <- add_trace(p, x = ladder.bins$inferred.pos, opacity = 0.01, y =  rep(singleExperimentYaxis()[2], nrow(ladder.bins)), name = paste0(sample(letters, 2), collapse = ""), marker = list(size = 12, color = 'white',line = list(color = "white", width = 3)), text = ladder.bins$bin, showlegend = F, hoverinfo = 'text',type = 'scattergl', mode = 'markers');
            curves <- append(curves, paste("hover", systemi,idi,dyei, sep = "%%"))
          }

          plots[[systemi]] <- p
          plotnb <- length(plots)
		  xref.name = "x"
          yref.name = "y"
          if (plotnb > 1) {
			xref.name = paste0("x", plotnb)
			yref.name = paste0("y", plotnb)          
		  }
		  plot.description$description[[xref.name]] <- systemi
		  plot.description$description[[yref.name]] <- systemi
		  
          # selected peaks surrounding
          if (!is.null(selected.peak) &&  selected.peak$keep && selected.peak$system == systemi) {
            rect.col <- "blue"
            opacity = 0.3
            
            shapelist[[length(shapelist)+1]] <- list(type = 'rect', fillcolor = rect.col, line = list( color = rect.col), opacity = opacity,x0 =  selected.peak$startpos.size, x1 = selected.peak$endpos.size, y0 = 0, y1 =  selected.peak$height+0.1*selected.peak$height, xref = xref.name, yref = yref.name)
          }
          # display ladder
		  if (!is.null(fsa.data$peaks$bins)) {
		    ladder.bins <- fsa.data$bins[system == systemi]
		    rect.col <- "grey"
		    for (i in 1:nrow(ladder.bins)) {
		      shapelist[[length(shapelist)+1]] <- list(type = 'rect', fillcolor = rect.col, line = list( color = rect.col), opacity = 0.2,x0 = ladder.bins$inferred.pos[i]-ladder.bins$minborder[i], x1 = ladder.bins$inferred.pos[i]+ladder.bins$maxborder[i], y0 = singleExperimentYaxis()[1], y1 =  singleExperimentYaxis()[2], xref = xref.name, yref = yref.name)
		    }
		  }
		  plots[[systemi]] <- p %>% layout(shapes = shapelist)
        }

        curves.description$curves = curves
		splots.rows <- ceiling(length(names(plots))/2)
		if (onOneLine) {
		  splots.rows <- 1
		}
        sp <- subplot(plots,nrows = splots.rows)
        sp 
        
    })
    
    
    observe({
        selected.peak$onclick <- event_data("plotly_click", source = "B")
        req(!is.null(selected.peak$onclick))
        print(selected.peak$onclick)
        curve.nb <- selected.peak$onclick[['curveNumber']]
        curve.id <- curves.description$curves[curve.nb+1]
        req(is.character(curve.id))

        if (is.character(curve.id) ) {
            curve.id.vec <- vector()
            curve.id.vec.list <- strsplit(curve.id, "%%")
            if (length(curve.id.vec.list) > 0) {
                curve.id.vec <- curve.id.vec.list[[1]]
            }
			print(curve.id)
			
            systemi <- curve.id.vec[2]
            idi <- curve.id.vec[3]
            dyei <- curve.id.vec[4]
            peak <-  annotated.peaks$peaks[!is.na(system) & system == systemi & keep == T & id == idi & dye == dyei  & peak.height == selected.peak$onclick$y]
#             print(systemi)
#             print(idi)
#             print(dyei)
#             print(selected.peak$onclick$y)

            if (nrow(peak) > 0 && length(curve.id.vec) > 0) {
                selected.peak$peak <- peaksToExportDT()[system == systemi &  floor(peaksToExportDT()$size) == floor(selected.peak$onclick$x) & height == selected.peak$onclick$y ]
                selectRows(proxySE, NULL)
                selectRows(proxyFO, NULL)
                
            }
        }


    })

    observe({
        selected.peak$selected.range <- event_data("plotly_brushed", source = "B")
		req(!is.null(selected.peak$selected.range))
		print(selected.peak$selected.range)
		print(plot.description$description)
		systemi <- plot.description$description[[names(selected.peak$selected.range)[1]]]
		range.values <- unlist(selected.peak$selected.range)
		xrange.values <- range.values[grepl("^x", names(range.values))]
		xrange <- range(xrange.values)
		print(paste("Adding a peak for ", systemi, "from", paste(xrange, collapse = " to ")))
		addpeaks$system <- systemi
		addpeaks$from <- xrange[1]
		addpeaks$to <- xrange[2]
		showModal(myAddPeakModal())
    })
    
	myAddPeakModal <- function() {
    
		modalDialog(
			footer = NULL,
			fluidRow(
				column(1, ""),
				column(11,
					fluidRow(
						fluidRow(
							column(3, tags$b("system")), 
							column(8, uiOutput(ns("system")))
						),
						br(),
						
						fluidRow(
							column(3, tags$b("From")), 
							column(3, uiOutput(ns("startsize")))
						),
						br(),
						fluidRow(
							column(3, tags$b("To")), 
							column(3, uiOutput(ns("endsize")))
						),
						br(),
						fluidRow(
							column(3, tags$b("Size at max height")), 
							column(6, uiOutput(ns("maxsize")))
						),
						br(),
						fluidRow(
							column(3, tags$b("Max Height")), 
							column(6, uiOutput(ns("height")))
						)
						
						
					)
				)
			),
			fluidRow(
				column(2, ""),					
				column(2, uiOutput(ns("saveButton"))),
				column(2, uiOutput(ns("closeModalButton")))#,
			)
		)
	}
    
    output$system <- renderUI({
		req(fsa.data$markers$system)
        systems <- sort(unique(fsa.data$markers$system))
        systemi <- systems[1]
        if (!is.null(addpeaks$system)) {
          systemi <- addpeaks$system
        }
        tags$div(id = "inline", selectInput(ns("system"), label = "",  choices = systems, selected = systemi))
    })
    output$startsize <- renderUI({
		req(fsa.data$markers$system)
        val <- floor(addpeaks$from)
        minval <- NULL
        maxval <- NULL
        if (!is.null(input$system)) {
			minval <- floor(fsa.data$markers[system == input$system]$start.pos)
			maxval <- ceiling(fsa.data$markers[system == input$system]$end.pos)
        }
        
		tags$div(id = "inline", numericInput(ns("startsize"), label = "", value = val, min = minval, max = maxval))
    })    
    output$endsize <- renderUI({
		req(fsa.data$markers$system)
        val <- floor(addpeaks$to)
        minval <- NULL
        maxval <- NULL
        if (!is.null(input$system)) {
			minval <- floor(fsa.data$markers[system == input$system]$start.pos)
			maxval <- ceiling(fsa.data$markers[system == input$system]$end.pos)
        }
        
		tags$div(id = "inline", numericInput(ns("endsize"), label = "", value = val, min = minval, max = maxval))
    })       
	addedPeak.maxSize <- reactive({
		req(input$system)
		req(input$startsize)
		req(singleExperimentFilterExp())
		req(input$endsize)
		req(addedPeak.maxHeight())
		dye <- fsa.data$markers[system == input$system]$dye
		intensities <-fsa.data$standardized.data$intensities[id == singleExperimentFilterExp()  & sizes >= input$startsize & sizes <= input$endsize]
		intensities <- intensities[order(get(dye))]
# 		print(intensities)
		tail(intensities, n = 1)$sizes

	})
	addedPeak.maxHeight <- reactive({
		req(input$system)
		req(input$startsize)
		req(singleExperimentFilterExp())
		req(input$endsize)
		dye <- fsa.data$markers[system == input$system]$dye
		intensities <-fsa.data$standardized.data$intensities[id == singleExperimentFilterExp()  & sizes >= input$startsize & sizes <= input$endsize]
		intensities <- intensities[order(get(dye))]
		tail(intensities, n = 1)[[dye]]
		
	})	
	
    output$maxsize <- renderUI({
		toString(round(addedPeak.maxSize(),0));
    })   
    output$height <- renderUI({
		toString(round(addedPeak.maxHeight(),0));
		
    })  
    
	output$closeModalButton <- renderUI({
		actionButton(ns("closeModalButton"), "Close")
	})	    
    
	observeEvent(input$closeModalButton,
		ignoreNULL = T,   # Show modal on start up
        removeModal()
	)
    
	output$saveButton <- renderUI({
		actionButton(ns("saveButton"), "OK")
	})	    
    
	observeEvent(input$saveButton, ignoreNULL = T, {
			print(annotated.peaks$peaks)
			dye <- fsa.data$markers[system == input$system]$dye
			start.pos <- fsa.data$markers[system == input$system]$start.pos
			end.pos <- fsa.data$markers[system == input$system]$end.pos
			cytoband <- fsa.data$markers[system == input$system]$cytoband
			newPeak <- list(
				startpos.size = input$startsize,
				endpos.size = input$endsize,
				maxpos.size = addedPeak.maxSize(),
				peak.height = addedPeak.maxHeight(),
				system = input$system,
				cytoband = cytoband,
				id = singleExperimentFilterExp(),
				dye = dye,
				start.pos = start.pos,
				end.pos = end.pos,
				keep = T
				)
			
			annotated.peaks$peaks <- rbindlist(list(annotated.peaks$peaks, newPeak), fill = T,  use.names = T)
			print(annotated.peaks$peaks)
			removeModal()
		}
	)    
    return(list(selected.peak = reactive(selected.peak$peak),
                exportPeaksTable = reactive(peaksToExportDT()),
                annotatedPeaks = reactive(annotated.peaks$peaks))
                )

}
