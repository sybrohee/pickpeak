
source("../../functions.R")

fsa.data.raw <- my.read.fsa(list.files("/home/sylvain/shiny-server/pickpeak/www/aneuploidies/Aneu-280119/", full.names = T))
markers <- fread("/home/sylvain/shiny-server/pickpeak/www/data/markers/markers_qstr_elucigene.tab")
fsa.data.std <- scale.timeseries(fsa.data.raw)
intensities <- fsa.data.std$intensities
dyes <- fsa.data.raw$dyes
ids <- unique(intensities$id)

fsa.data <- list(data = list(dyes = dyes),
                 standardized.data = fsa.data.std,
                 markers = markers)


# all.peaks.dt <- data.table(fsa.data)

# ids = 'QSTR-19GR001203-DE'
# dyes = '6-FAM'


muc <- peaks.to.markers(fsa.data)
