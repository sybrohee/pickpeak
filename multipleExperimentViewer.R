library(plotly)
library(shiny)
library(DT)

# module UI function
multipleExperimentViewerUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
        column(10,
            plotlyOutput(ns("multipleExperimentPlot"), height = "800px")
        ),
        column(2, 
            DT::dataTableOutput(ns("onClick"))
        )
        
    ) 
  )

}

# module server function
multipleExperimentViewer <- function(input, output, session, fsa.data, selected.height , selected.width, selected.scale, selected.dyes) {
    ns <- session$ns
    curves.description <- reactiveValues(curves = NULL)
    
    output$multipleExperimentPlot <- renderPlotly({
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


        
        
        
        intensities <- fsa.data$standardized.data$intensities
        peaks <- fsa.data$peaks

        
        x.scale <- "sizes"
        x.peaks = "maxpos.size"
        if (selected.scale$selectedScale() == 'Raw') {
            x.scale = 'time'
            x.peaks = "peak.maxpos.time"
        } 
        ids <- unique(intensities$id)
        channels <- selected.dyes$selectedDyes()

        minval <- min(intensities[,..channels])               
        plots <- list()
        layouts <- list()
        curves <- vector()
        for (idi in ids) {
            intensities.id <- intensities[id == idi]
            annots <- list( text = idi, font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
            p <- plot_ly() %>% layout(height = 800, annotations = annots,xaxis = list(title = x.scale, titlefont = f, range = selected.width$selectedWidth()) , yaxis = list(title = idi, titlefont = f, range = selected.height$selectedHeight()) );
            for (channel in channels) {
              p <- add_trace(p, x = intensities.id[[x.scale]], y= intensities.id[,get(channel)],  type = 'scatter', mode = 'lines', line = list(color =  colors[channel]), showlegend = F, hoverinfo = 'x+y')
              curves <- append(curves, paste("points", idi,channel, sep = "%%"))
              if (!is.null(peaks)) {
                if (length(peaks[!is.na(system) &id == idi & dye == channel][[x.peaks]]) > 0) { 
                    p <- add_trace(p, x = peaks[!is.na(system) &id == idi & dye == channel][[x.peaks]], name = paste0(sample(letters, 2), collapse = ""), y = peaks[!is.na(system) & id == idi & dye == channel][['peak.height']], marker = list(size = 6, color = colors[channel],line = list(color = colors[channel], width = 1)), text = peaks[!is.na(system) &id == idi & dye == channel][['system']], showlegend = F, hoverinfo = 'text');
                    curves <- append(curves, paste("peaks", idi,channel, sep = "%%"))
                }
              }
            }
            plots[[idi]] <- p 
            
        }
        sp <- subplot(plots, nrows = length(ids), shareX = TRUE, titleY = TRUE)
#         print(curves)
        curves.description$curves = curves
#         sp <- layout(sp, annotations = layouts, title = "MUCOMUC")
        sp
    })
    output$onClick <- DT::renderDataTable({
      req(fsa.data$peaks)
      d <- event_data("plotly_click")
      req(!is.null(d))
#       print(d)
      curve.id <- curves.description$curves[d$curveNumber+1]
      curve.id.vec <- strsplit(curve.id, "%%")[[1]]
      sample.id <- curve.id.vec[2]
      channel <- curve.id.vec[3]
      xval <- d$x
      yval <- d$y
      peak <- fsa.data$peaks[id == sample.id & dye == channel & floor(maxpos.size) == floor(xval) & peak.height == yval]
      req(nrow(peak) > 0)
#       print(fsa.data$peaks)
#       print(peak)
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

}
