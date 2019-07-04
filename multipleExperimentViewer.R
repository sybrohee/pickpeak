library(plotly)
library(shiny)
library(DT)

# module UI function
multipleExperimentViewerUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
        column(10,
            plotlyOutput(ns("multipleExperimentPlot"), height = "800px"),
            downloadButton(ns("export"), "Export")
        ),
        column(2, 
            DT::dataTableOutput(ns("onClick"))
        )
        
    ) 
  )

}

# module server function
multipleExperimentViewer <- function(input, output, session, fsa.data,  colors, selected.samples, selected.height , selected.width, selected.scale, selected.dyes, nbSamplesPerPage, pageNb, above.samples) {
    ns <- session$ns
    curves.description <- reactiveValues(curves = NULL)
    
    output$multipleExperimentPlot <- renderPlotly({
		req(length(selected.samples$selectedSamples()) > 0)
        f <- list(
            family = "sans serif",
            size = 10,
            color = "#000000"
        )
		

        
        
        intensities <- fsa.data$standardized.data$intensities
        peaks <- fsa.data$peaks
        #print(peaks)

        
        x.scale <- "sizes"
        x.peaks = "maxpos.size"
        if (selected.scale$selectedScale() == 'Raw' || selected.scale$scalingDye() == 'None' ) {
            x.scale = 'time'
            x.peaks = "peak.maxpos.time"
        } 
        ids <- unique(intensities$id)
        ids <- sort(intersect(ids, selected.samples$selectedSamples()))
        ids <- setdiff(ids, above.samples())
        channels <- selected.dyes$selectedDyes()
        nbAboveSamples <- length(above.samples())
		nbSamplesPerPage <- nbSamplesPerPage()- nbAboveSamples
		page.nb <- pageNb()
		if (is.null(pageNb())) {
			page.nb <- 1
		}
		startindex <- 1+((page.nb-1)*nbSamplesPerPage)
		endindex <- min(startindex+nbSamplesPerPage-1, length(ids))
		# print(above.samples())
		# print(startindex)
		# print(endindex)
		ids <- c(above.samples(), ids[startindex:endindex])

        minval <- min(intensities[,..channels])               
        plots <- list()
        layouts <- list()
        curves <- vector()
       # print(intensities)
        shapelist <- list()
        for (idi in ids) {
            intensities.id <- intensities[id == idi]
            annots <- list( text = idi, font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
            p <- plot_ly() %>% layout(height = 800, annotations = annots,xaxis = list(title = x.scale, titlefont = f, range = selected.width$selectedWidth()) , yaxis = list(title = idi, titlefont = f, range = selected.height$selectedHeight()) );
            for (channel in channels) {
              p <- add_trace(p, x = intensities.id[[x.scale]], y= intensities.id[,get(channel)],  type = 'scatter', mode = 'lines', line = list(color =  colors[[channel]]$color), showlegend = F, hoverinfo = 'x+y')
              curves <- append(curves, paste("points", idi,channel, sep = "%%"))
              if (!is.null(peaks)) {
                if (length(peaks[!is.na(system) &id == idi & dye == channel][[x.peaks]]) > 0) { 
                    p <- add_trace(p, x = peaks[!is.na(system) &id == idi & dye == channel][[x.peaks]], name = paste0(sample(letters, 2), collapse = ""), y = peaks[!is.na(system) & id == idi & dye == channel][['peak.height']], marker = list(size = 6, color = colors[[channel]]$color,line = list(color = colors[[channel]]$color, width = 1)), text = peaks[!is.na(system) &id == idi & dye == channel][['system']], showlegend = F, hoverinfo = 'text');
                    curves <- append(curves, paste("peaks", idi,channel, sep = "%%"))
                }
              }
			  if (!is.null(fsa.data$peaks$bins) && length(channels) == 1) {
# 				ladder.bins <- fsa.data$bins[dye == channel]
				p <- add_trace(p, x = ladder.bins[dye == channel]$inferred.pos, opacity = 0.01, y =  rep(selected.height$selectedHeight()[2], nrow(ladder.bins[dye == channel])), name = paste0(sample(letters, 2), collapse = ""), marker = list(size = 12, color = 'white',line = list(color = "white", width = 3)), text = ladder.bins[dye == channel]$bin, showlegend = F, hoverinfo = 'text',type = 'scatter', mode = 'markers');
				curves <- append(curves, paste("hover",idi,channel, sep = "%%"))
			  }
			  plots[[idi]] <- p
			  plotnb <- length(plots)
			  xref.name = "x"
			  yref.name = "y"		      
			  if (plotnb > 1) {
				xref.name = paste0("x", plotnb)
				yref.name = paste0("y", plotnb)          
			  }			  
          # display ladder
			  if (!is.null(fsa.data$peaks$bins) && length(channels) == 1) {
				ladder.bins <- fsa.data$bins[dye == channel]
				rect.col <- "grey"
				for (i in 1:nrow(ladder.bins)) {
					shapelist[[length(shapelist)+1]] <- list(type = 'rect', fillcolor = rect.col, line = list( color = rect.col), opacity = 0.2,x0 = ladder.bins$inferred.pos[i]-ladder.bins$minborder[i], x1 = ladder.bins$inferred.pos[i]+ladder.bins$maxborder[i], y0 = selected.height$selectedHeight()[1], y1 =  selected.height$selectedHeight()[2], xref = xref.name, yref = yref.name)
				}
			  }			  
            }
        }
        sp <- subplot(plots, nrows = length(ids), shareX = TRUE, titleY = TRUE)
        curves.description$curves = curves
		sp <- sp %>% layout(shapes = shapelist)
		sp
    })
    

    
    
    
    output$onClick <- DT::renderDataTable({
      req(fsa.data$peaks)
      d <- event_data("plotly_click")
      req(!is.null(d))

      curve.id <- curves.description$curves[d$curveNumber+1]
      curve.id.vec <- strsplit(curve.id, "%%")[[1]]
      sample.id <- curve.id.vec[2]
      channel <- curve.id.vec[3]
      xval <- d$x
      yval <- d$y
      peak <- fsa.data$peaks[id == sample.id & dye == channel & floor(maxpos.size) == floor(xval) & peak.height == yval]
      req(nrow(peak) > 0)

      mssystem <- peak[["system"]]
      maxtime <- peak[["peak.maxpos.time"]]
      starttime <- peak[["peak.startpos.time"]]
      endtime <- peak[["peak.endpos.time"]]
      maxsize <- peak[["maxpos.size"]]
      startsize <- peak[["startpos.size"]]
      endsize <- peak[["endpos.size"]] 
      height <- peak[["peak.height"]]
      result.vec <- c(
        sample = sample.id,
        position = paste0(floor(maxsize), " (",floor(startsize),"-",floor(endsize), ")"),
        intensity = height,
        time = paste0(maxtime, " (",starttime,"-",endtime, ")")
     )
     result.dt <- data.table(result.vec)
     row.names(result.dt) <- names(result.vec)
     names(result.dt) <- paste(mssystem, channel)
     datatable(result.dt, options = list(dom = 't'))
    })
    
	runName <- reactive( {
		sample.date <- fsa.data$data$expdate[[1]]
		shortName = paste0("run-", sample.date)
		result <- shortName
		if (sample.date == "") {
		  result <- "run"
		}
		result
	})        
    
	output$export <- downloadHandler(
		filename = function(file) {
		"muc.pdf"
		},
		content = function(file) {
			
		req(length(selected.samples$selectedSamples()) > 0)
		# print(file)
		intensities <- fsa.data$standardized.data$intensities
		peaks <- fsa.data$peaks
		x.scale <- "sizes"
		x.peaks = "maxpos.size"
		if (selected.scale$selectedScale() == 'Raw' || selected.scale$scalingDye() == 'None' ) {
			x.scale = 'time'
			x.peaks = "peak.maxpos.time"
		} 
		ids <- unique(intensities$id)
		# print(ids)
		all.ids <- sort(setdiff(ids, above.samples()))

		channels <- selected.dyes$selectedDyes()
		nbAboveSamples <- length(above.samples())
		nbSamplesPerPage <- nbSamplesPerPage() - nbAboveSamples
	    page.nbs <- ceiling(length(all.ids) / nbSamplesPerPage)

		pdf(file)
		par(mfrow = c(nbSamplesPerPage+nbAboveSamples,1), mar=c(1,1,1,1))
		for (page.nb in 1:page.nbs) {
			startindex <- 1+((page.nb-1)*nbSamplesPerPage)
			endindex <- min(startindex+nbSamplesPerPage-1, length(all.ids))
			# print(startindex)
			# print(endindex)
			# 
			ids <- c(above.samples(), all.ids[startindex:endindex])
			for (idi in ids) {
				intensities.id <- intensities[id == idi]
				plot(0,0, col = "white", xlim = selected.width$selectedWidth(), ylim = selected.height$selectedHeight(), main = idi);
				for (channel in channels) {
					lines(intensities.id[[x.scale]], intensities.id[,get(channel)], col = colors[[channel]]$color)
				}
				grid()
				
			}
		
		}
		
		dev.off()

		}
		,contentType="application/pdf"
	)    
		

}
