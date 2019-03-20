library(plotly)
library(shiny)
library(DT)

# module UI function
linearRegressionViewerUI <- function(id){
  ns <- NS(id)
  fluidRow(
    textOutput(ns("noComputedRegression")),
    plotlyOutput(ns("linearRegressionPlot"))
  )
}

# module server function
linearRegressionViewer <- function(input, output, session, fsa.data) {
    ns <- session$ns
     
    output$noComputedRegression <- renderText({
        resultText = ""
        if (length(fsa.data$standardized.data$peaks) == 0) {
            resultText <- "No channel or ladder has been selected to estimate the molecular weigths";
        } else {
            resultText <- ""
        }
    
    })
    output$linearRegressionPlot <- renderPlotly({
      peaks <- fsa.data$standardized.data$peaks
      models <- fsa.data$standardized.data$models
      if (length(peaks) > 0 && is.data.table(peaks[[1]])) {
        f <- list(
            family = "sans serif",
            size = 14,
            color = "#000000"
        )
        y <- list(
            title = "Sizes",
            titlefont = f
        )
        x <- list(
            title = "Time",
            titlefont = f
        )        


# annotations
       
        ids <- unique(fsa.data$standardized.data$intensities$id)
        plots <- list()
        for (idi in ids) {
     
            peaks.id <- peaks[[idi]]
            lm.id <- models[[idi]]
            x.reg <- c(min(peaks.id$peak.maxpos),max(peaks.id$peak.maxpos))
            y.reg <- (lm.id$coefficients[2]*x.reg) + lm.id$coefficients[1]
            annots <- list( text = idi, font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",  x = 0.5,  y = 1,  showarrow = FALSE )
            p <- plot_ly() %>% layout( yaxis = y, xaxis = x );
            p <- add_trace(p, x= peaks.id$peak.maxpos, y = peaks.id$size,type = 'scatter', mode =  'lines+markers',showlegend = F)
            p <- add_trace(p, x = x.reg, y = y.reg, mode = 'lines', line = list(color =  "grey", dash = 'dot'),showlegend = F) %>%  layout(annotations = annots)
            
            plots[[idi]] <- p
        }
        subplot(plots,titleX = TRUE, titleY = TRUE)
      } else {
        plotly_empty();
      }
    })
}
