library(seqinr)
library(pracma)
library(data.table)

my.read.fsa <- function(files) {
  result <- list()
  abif.data <- data.table()
  dyes <- c()
  expdate <- c()
  for (file in files) {
    abif <- read.abif(file)
#     print(abif$DATA.5)
    abif.data.i <- data.table(abif$Data$DATA.1, abif$Data$DATA.2, abif$Data$DATA.3, abif$Data$DATA.4,  abif$Data$DATA.105)
    dyes.i <- c(abif$Data$DyeN.1, abif$Data$DyeN.2, abif$Data$DyeN.3, abif$Data$DyeN.4,  abif$Data$DyeN.5) 
    
    names(abif.data.i) <- dyes.i
    dyes <- dyes.i
    names(dyes) <- c("DyeN.1", "DyeN.2","DyeN.3","DyeN.4","DyeN.5")
    abif.data.i$id <- abif$Data$SpNm.1
    abif.data.i$time <- 1:nrow(abif.data.i)
    day <- paste0("0", abif$Data$RUND.1$day)
    month <- paste0("0", abif$Data$RUND.1$month)
    year <- toString(abif$Data$RUND.1$year)
    
    expdate[abif.data.i$id] <- paste0(substr(day, nchar(day)-1, nchar(day)),
                         substr(month, nchar(month)-1, nchar(month)),
                         substr(year, nchar(year)-1, nchar(year))
                         )
    
    if (nrow(abif.data) == 0) {
      abif.data <- abif.data.i
    } else {
      abif.data <- rbind(abif.data, abif.data.i)
    }
  }
  return (list (intensities = abif.data, dyes = dyes, expdate = expdate))

}


peaks.to.markers <- function(fsa.data, minpeakheights, removeStutters) {
    dyes <- fsa.data$data$dyes
    ids <- unique(fsa.data$standardized.data$intensities$id)
    all.peaks.dt <- data.table()
#     dyes = '6-FAM'
#     ids = 'QSTR-19GR001202-DE'
    
    for (dyei in dyes) {
        for (idi in ids) {
            min.peak.height = 30
            if (!is.null(minpeakheights[[idi]])) {
              min.peak.height = minpeakheights[idi] 
            }
            peaks.dt <- data.table(findpeaks(fsa.data$standardized.data$intensities[id == idi][[dyei]], zero = "+",minpeakdist = 3, minpeakheight = min.peak.height))
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
    all.peaks.dt <- all.peaks.dt[order(id, dye, maxpos.size)]
    all.peaks.dt$diff <- c(diff(floor(all.peaks.dt$maxpos.size)), 0)
    all.peaks.dt$nextvalue <- c(all.peaks.dt$peak.height[-1], 0)
    all.peaks.dt$diffvalues.div <- all.peaks.dt$peak.height / all.peaks.dt$nextvalue

    
    stutterslines <- which(all.peaks.dt$diffvalues.div < 0.1 & all.peaks.dt$diff == 4)
    if (removeStutters && length(stutterslines) > 0) {
      all.peaks.dt <- all.peaks.dt[-stutterslines,]
    }
    all.peaks.dt$diff <- NULL
    all.peaks.dt$nextvalue <- NULL
    all.peaks.dt$diffvalues.div <- NULL
    markers <- fsa.data$markers
    setkey(markers, "dye", "start.pos", "end.pos")
    all.peaks.overlaps <-  foverlaps(all.peaks.dt, markers , by.x=c("dye", "startpos.size", "endpos.size"))

    return(all.peaks.overlaps)
}


markedpeaks.to.real.bins <- function(bins,peaks, ladder.sample) {
  error.systems <- vector()
  
  for (s in unique(bins$system)) {
    if (nrow(bins[system == s & virtual == F]) != nrow(peaks[system == s & id == ladder.sample])) {
      error.message <- paste0("Could not bin system ", s)
      print(error.message)
      error.systems <- append(error.systems, s)
    } else {
      peaks[system == s& id == ladder.sample, bin := bins[system == s & virtual == F]$bin]
      peaks[system == s& id == ladder.sample, binsize := bins[system == s & virtual == F]$size]
      peaks[system == s& id == ladder.sample, minborder := bins[system == s & virtual == F]$minborder]
      peaks[system == s& id == ladder.sample, maxborder := bins[system == s & virtual == F]$maxborder]
      peaks$offset <-  peaks$binsize - peaks$maxpos.size

    }
  }

  ladder.peaks <- peaks[id == ladder.sample & !is.na(bin)]
  ladder.peaks$bin.start.pos <- ladder.peaks$maxpos.size - ladder.peaks$minborder
  ladder.peaks$bin.end.pos <- ladder.peaks$maxpos.size + ladder.peaks$maxborder
  ladder.peaks[,c('cytoband',"dye", 'start.pos','end.pos','color','peak.height','peak.maxpos.time','peak.startpos.time','peak.endpos.time','id','maxpos.size','startpos.size','endpos.size'):=NULL]

  samples.peaks <- peaks[id != ladder.sample]
  samples.peaks <- samples.peaks[, c('bin', 'binsize', 'minborder','offset', 'maxborder') :=NULL]
  setkey(ladder.peaks, "system", "bin.start.pos", "bin.end.pos")
  ladder.vs.samples <- foverlaps(samples.peaks, ladder.peaks, by.x = c("system", "startpos.size", "endpos.size"))
  ladder.vs.samples[,c('bin.start.pos', 'bin.end.pos') :=  NULL]
  result.error <- NULL
  if (length(error.systems) > 0) {
    result.error <- paste0("Could not bin system ", paste(error.systems, collapse = ", "), ". This could be due to a wrong threshold value for the ladder sample(s)")
  }

  
  result <- list(
	binnedpeaks = rbindlist(list(ladder.vs.samples,peaks[id == ladder.sample] ), use.names=T),
	error = result.error
  )

  return (result)
}


scale.timeseries <- function(fsa.raw.data, time = time, ladder = 'LIZ500', standard.dye = 'LIZ', minpeakheight) {
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
    scales$ILS500 <- c(60, 65, 80, 100, 120, 140, 160, 180, 200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475, 500)
    scalei <- scales[[ladder]]
    for (idi in ids) {

        peaks.dt <- data.table(findpeaks(intensities[id == idi][[standard.dye]], minpeakheight = minpeakheight, zero = "+"))
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


read.bin.file <- function (bin.file) {
  temp <- fread(bin.file)
  if (temp$V1[1] == 'Version') { #idx formatted file
    temp.list <- list()
    marker.name <- NA
    for (i in 1:nrow(temp)) {
      
      if (!is.na(temp$V3[i]) && !is.na(temp$V4[i])) {
        start <- temp$V3[i]
        end <- temp$V4[i]
        bin.id <- temp$V1[i]
        size <- temp$V2[i]
        virtual <- F
        if (temp$V5[i] == 'virtual') {
          virtual <- T
		}
        temp.list[[length(temp.list)+1]] <-  data.table(bin=bin.id,	size=size,	minborder=start,	maxborder=end,	system = marker.name, virtual = virtual)
        
      } else if (temp$V1[i] == 'Marker Name') {
		marker.name = temp$V2[i]
      }
    }

	result <- rbindlist(temp.list)
	result$size <- as.numeric(result$size)
	result$minborder <- as.numeric(result$minborder)
	result$maxborder <- as.numeric(result$maxborder)	
  } else if (names(temp)[1] == 'bin') {
    result <- temp
  }
  return (result)

  
}

bins.position <- function(bins, peaks, ladder.sample) {
  ladder.peaks <- peaks[id == ladder.sample,c('bin', 'system', 'maxpos.size', 'dye')]
  
  offset.bins <- merge(ladder.peaks, bins, by = c('bin', 'system'), all.y = T)
  offset.bins <- offset.bins[order(system, size)]
  system.dye <- unique(offset.bins[!is.na(dye), c('system' ,'dye')])
  print(system.dye)
  dyes <- unique(ladder.peaks$dye)
  systems <- unique(system.dye$system)
  for (systemi in systems) {
    offset.bins[system == systemi, dye := system.dye[system == systemi]$dye]
  }
  offset.bins[virtual == F, inferred.pos := maxpos.size]
  for (i in 1:nrow(offset.bins)) {
    if (offset.bins$virtual[i]) {
      if (offset.bins$system[i-1] != offset.bins$system[i]) next;
      if (i+1 > nrow(offset.bins)) next;
      if (i == 1) next;
      if (offset.bins$system[i+1] != offset.bins$system[i]) next;
      previous.obssize <- offset.bins$inferred.pos[i-1]
      previous.size <- offset.bins$size[i-1]
      sizei <- offset.bins$size[i]
      next.obssize <- NA
      j <- 1
#       print(offset.bins)
      while (is.na(next.obssize) && j < 3) {
		next.obssize <- offset.bins$inferred.pos[i+j]
		next.size <- offset.bins$size[i+j]
		j <- j+1
      }
      offset.bins$inferred.pos[i] <- previous.obssize  + ((sizei-previous.size)*(previous.size-next.size)/(previous.obssize-next.obssize))
#       print(i)
    }

  }
  
  for (i in 1:nrow(offset.bins)) {
    if (is.na(offset.bins$inferred.pos[i]) && offset.bins$virtual[i]) {
#       print(i)
      systemi <- offset.bins$system[i]
      if (sum(!is.na(offset.bins[system == systemi]$size)) > 2 && sum(!is.na(offset.bins[system == systemi]$inferred.pos)) > 2) {
		lm.model <- lm(offset.bins[system == systemi]$size ~ offset.bins[system == systemi]$inferred.pos)
		offset.bins$inferred.pos[i] <-  lm.model$coefficients[1] + offset.bins$size[i]*lm.model$coefficients[2]
	  }
    }
  }
  

  return(offset.bins)
}
