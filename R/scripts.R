library(data.table)
library(plotly)

# source("functions.R")
# fsa.dir <- "/home/sylvain/shiny-server/mygenescan/www/aneuploidies/Aneu-280119/"
# fsa <- data.table(read.fsa(path = fsa.dir, pretrim = "QSTR-", posttrim = "(-DE){0,1}_[0-9\\-]+.fsa"))
# 
# samples <- unique(fsa$tag)
# channels <- unique(fsa$chan)
# channel.colors <- c("orange", "blue", "darkgreen", "pink", "black")
# 
# 
# p <- plot_ly()%>%
#   layout(title = "",
#          xaxis = list(title = "Time"),
#          yaxis = list (title = "Peaks") )
#          
#          
# for (i in 1:length(channels)) {
#   p <- p %>% add_trace(data = fsa[tag == samples[1] & chan == channels[i]], x = ~time,  y = ~peak, mode = 'lines', line = list(color = channel.colors[i], width = 2)) 
# }

#####################################################
library(Fragman)
fsa.dir <- "/home/sylvain/shiny-server/mygenescan/www/aneuploidies/Aneu-280119/"
fsa <- storing.inds(fsa.dir)
# par(mfrow = c(3,1))
# channel <- 3
# plot(fsa[[1]][,channel], type = "l", col = "blue")
# plot(fsa[[2]][,channel], type = "l", col = "blue")
# plot(fsa[[3]][,channel], type = "l", col = "blue")


fsa.ladder <- seq(75,500, by = 25)
ladder.info.attach(stored=fsa, ladder=fsa.ladder)
# fsa.analysis <- overview2(my.inds=fsa, ladder=fsa.ladder, channel = 1,init.thresh=5000)
# res <- score.markers(my.inds=fsa, panel=fsa.analysis$channel_1,ladder=fsa.ladder, electro=FALSE)
