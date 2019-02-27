library(seqinr)
library(pracma)
library(data.table)

my.read.fsa <- function(files) {
  result <- list()
  abif.data <- data.table()
  dyes <- c()
  for (file in files) {
    abif <- read.abif(file)
    abif.data.i <- data.table(abif$Data$DATA.1, abif$Data$DATA.2, abif$Data$DATA.3, abif$Data$DATA.4,  abif$Data$DATA.105)
    dyes.i <- c(abif$Data$DyeN.1, abif$Data$DyeN.2, abif$Data$DyeN.3, abif$Data$DyeN.4,  abif$Data$DyeN.5)  
    names(abif.data.i) <- dyes.i
    dyes <- dyes.i
    names(dyes) <- c("DyeN.1", "DyeN.2","DyeN.3","DyeN.4","DyeN.5")
    abif.data.i$id <- abif$Data$SpNm.1
    abif.data.i$time <- 1:nrow(abif.data.i)
    if (nrow(abif.data) == 0) {
      abif.data <- abif.data.i
    } else {
      abif.data <- rbind(abif.data, abif.data.i)
    }
  }
  return (list (intensities = abif.data, dyes = dyes))

}


peaks.to.markers <- function(fsa.data) {
    dyes <- fsa.data$data$dyes
    ids <- unique(fsa.data$standardized.data$intensities$id)
    all.peaks.dt <- data.table()
#     dyes = '6-FAM'
#     ids = 'QSTR-19GR001202-DE'
    
    for (dyei in dyes) {
        for (idi in ids) {
            peaks.dt <- data.table(findpeaks(fsa.data$standardized.data$intensities[id == idi][[dyei]], zero = "+",minpeakdist = 5, minpeakheight = 30))
#             print(peaks.dt)
#             stop()
            model.id <- fsa.data$standardized.data$models[[idi]]
            if (nrow(peaks.dt) > 0) {
                names(peaks.dt) <- c("peak.height", "peak.maxpos.time", "peak.startpos.time", "peak.endpos.time")
                peaks.dt$id <- idi
                peaks.dt$dye <- dyei
                peaks.dt$maxpos.size <- (peaks.dt$peak.maxpos.time*model.id$coefficients[2]) + model.id$coefficients[1]
                peaks.dt$startpos.size <- (peaks.dt$peak.startpos.time*model.id$coefficients[2]) + model.id$coefficients[1]
                peaks.dt$endpos.size <- (peaks.dt$peak.endpos.time*model.id$coefficients[2]) + model.id$coefficients[1]
                if (nrow(all.peaks.dt) == 0) {
                    all.peaks.dt <- peaks.dt
                } else {
                    all.peaks.dt <- rbind(all.peaks.dt, peaks.dt)
                }
            }
        }
    }
#     print(all.peaks.dt)
    markers <- fsa.data$markers
    setkey(markers, "dye", "start.pos", "end.pos")
    all.peaks.overlaps <-  foverlaps(all.peaks.dt, markers , by.x=c("dye", "startpos.size", "endpos.size"))
    return(all.peaks.overlaps)
}

scale.timeseries <- function(fsa.raw.data, time = time, ladder = 'LIZ500', standard.dye = 'LIZ') {
  results <- list()
  models <- list()
  peaks <- list()
  scales <- list()
  intensities <- fsa.raw.data$intensities
  if (!(standard.dye %in% names(intensities)) || ladder == 'Raw') {
    intensities$sizes <- intensities$time

  } else {
    ids <- unique(intensities$id)
    scales$LIZ500 <- c(35, 50, 75, 100, 139, 150, 160, 200, 250, 300, 340, 350, 400, 450, 490, 500)
    scalei <- scales[[ladder]]
    for (idi in ids) {

        peaks.dt <- data.table(findpeaks(intensities[id == idi][[standard.dye]], minpeakheight = 600, zero = "+"))
        names(peaks.dt) <- c("peak.height", "peak.maxpos", "peak.startpos", "peak.minpos")

        valid.peaks <- tail(peaks.dt, n = length(scalei))
        valid.peaks$sizes <- scalei
        lm.model <- lm(sizes~peak.maxpos, valid.peaks)
    #     print(lm.model)
        intensities[id == idi, sizes := lm.model$coefficients[1] + time*lm.model$coefficients[2]]
        models[[idi]] <- lm.model
        peaks[[idi]] <- valid.peaks

    }
  }
  results$intensities <- intensities
  results$models <- models
  results$peaks <- peaks
  return (results)
}

read.fsa <- function(files = NULL, path = "./", sig.channel = 1:3, lad.channel = 105, pretrim = FALSE,
                     posttrim = ".fsa", thresh = -100, verbose = TRUE){

  if(is.null(files))
    files <- list.files(path, pattern = "\\.fsa$", full.names = TRUE)
  else
    files <- paste(path, files, sep = "")

  res <- do.call(rbind, lapply(files, function(file) {
    if (verbose) message(file)
    abif <- read.abif(file)
    tag <- tag.trimmer(basename(file), pretrim, posttrim)
    
    lad.dat <- abif$Data[[paste('DATA.', lad.channel, sep='')]]
    
    res1 <- data.frame(tag = as.character(rep(tag, length(lad.dat))),
                       chan = as.character(rep("standard", length(lad.dat))),
                       time = as.numeric(1:length(lad.dat)),
                       peak = as.numeric(lad.dat),
                       stringsAsFactors = F)
    
    for (i in sig.channel) {
      chan.dat <- abif$Data[[paste('DATA.', i, sep='')]]
      res1 <- rbind(res1, data.frame(tag = as.character(rep(tag, length(chan.dat))),
                                     chan = as.character(rep(i, length(chan.dat))),
                                     time = as.numeric(1:length(chan.dat)),
                                     peak = as.numeric(chan.dat)),
                                     stringsAsFactors = F)
    }
    res1
  }))
    
  if (thresh > -10) res <- subset(res, peak > thresh)
  return(res)
}

tag.trimmer <- function(x, pretrim = FALSE, posttrim = FALSE) {
  if(! is.na(pretrim)) {
    x <- sub(paste("^", pretrim, sep = ""), "", x)
  }
  if(! is.na(posttrim)){
    x <- sub(paste(posttrim, "$", sep = ""), "", x)
  }
  x
} 
