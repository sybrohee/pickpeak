
#' Wrapper around seqinr::read.abif to generate a list with the content of a set of FSA files
#' @import data.table
#' @importFrom seqinr read.abif
#' @param files vector of FSA file(s)
#' @param dyes.default vector containing the id of the dye name (default is c(Dye1 = '6-FAM', Dye2 = 'HEX', Dye3 = 'NED', Dye4 = 'ROX'))
#' @return a list containing abif.data : a data.table containing the raw data of the fsa file; dyes : of vector containing the names of dyes; expdate : a named vector containing the experiment date
#' @export
my.read.fsa <- function(files, dyes.default = c(Dye1 = '6-FAM', Dye2 = 'HEX', Dye3 = 'NED', Dye4 = 'ROX')) {
  result <- list()
  abif.data <- data.table()
  dyes <- c()
  expdate <- c()
  
  allcols <- c("1", "2", "3", "4", "105")
  dye.names <- c("DyeN.1", "DyeN.2","DyeN.3","DyeN.4","DyeN.5")
  

  
  for (file in files) {
    abif <- read.abif(file)
	abif.data.i <- NULL
	dyes.i <- vector()
	for (c in 1:length(allcols)) {
		if (! is.null(abif$Data[[paste0("DATA.",allcols[c])]]) ) {
			if (is.null(abif.data.i)) {
			  abif.data.i <- data.table( abif$Data[[paste0("DATA.",allcols[c])]])
			} else {
			  abif.data.i[[dye.names[c]]] <- abif$Data[[paste0("DATA.",allcols[c])]]
			}
			dye.i <- abif$Data[[paste0("DyeN.", c)]]
			if (dye.i %in% names(dyes.default)) { 
				dye.i <- dyes.default[dye.i]
			}
			dyes.i[dye.names[c]] <- dye.i
			
		}
    }
	expdate


	
	
	names(abif.data.i) <- dyes.i[1:length(dyes.i)]
	dyes <- dyes.i
	
	abif.data.i$id <- gsub('[^\x20-\x7E]', "", abif$Data$SpNm.1)
	abif.data.i$id <- gsub('\\+$', "", abif.data.i$id)
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


#' Given the (rescaled) intensities, detects the peaks and annotate them with the system they are supposed to belong to.
#' @importFrom data.table foverlaps
#' @importFrom pracma findpeaks
#' @param fsa.data 
#' @param minpeakheights do not consider peaks that have lower intensities than minpeakheights. Must be a list[[samplename]][[dyename]]
#' @param boolean removes stutter peaks
#' @return for each sample and each probe a set of peaks (in a data.table) together with the system they are interesting with.
#' @export
peaks.to.markers <- function(fsa.data, minpeakheights,  removeStutters) {
    dyes <- fsa.data$data$dyes
    ids <- unique(fsa.data$standardized.data$intensities$id)
    all.peaks.dt <- data.table()

    
    for (dyei in dyes) {
        for (idi in ids) {
            min.peak.height = minpeakheights[[idi]][[dyei]]
            peaks.dt <- data.table(pracma::findpeaks(fsa.data$standardized.data$intensities[id == idi][[dyei]],  nups = 3, zero = "+",minpeakdist = 3, minpeakheight = min.peak.height))

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
    all.peaks.overlaps <-  data.table::foverlaps(all.peaks.dt, markers , by.x=c("dye", "startpos.size", "endpos.size"))

    return(all.peaks.overlaps)
}


#' This function annotates the detected peaks in the ladder sample(s)  and the peaks that are annotated for this ladder.
#' @import data.table
#' @param bins result of the read.bin.file function
#' @param peaks data.table with the detected peaks
#' @param ladder.samples a vector containing the names of the ladder samples in the peaks data.table
#' @return an annotated set of bins in a list
#' @export
markedpeaks.to.real.bins <- function(bins, peaks, ladder.samples) {
  error.systems <- vector()
  
  for (ladder.sample in ladder.samples) {
	for (s in unique(bins$system)) {
		if (nrow(bins[system == s & virtual == F]) != nrow(peaks[system == s & id == ladder.sample])) {
			error.message <- paste0("Could not bin system ", s, " for ladder sample ", ladder.sample)
			error.systems <- append(error.systems, s)
		} else {
			peaks[system == s& id == ladder.sample, bin := bins[system == s & virtual == F]$bin]
			peaks[system == s& id == ladder.sample, binsize := bins[system == s & virtual == F]$size]
			peaks[system == s& id == ladder.sample, minborder := bins[system == s & virtual == F]$minborder]
			peaks[system == s& id == ladder.sample, maxborder := bins[system == s & virtual == F]$maxborder]
			
		}
	}
  }

  ladder.peaks <- peaks[id %in% ladder.samples & !is.na(bin)]
  ladder.peaks <- ladder.peaks[, maxpos.size := mean(maxpos.size), by = list(system, bin )]
  
  ladder.peaks$bin.start.pos <- ladder.peaks$maxpos.size - ladder.peaks$minborder
  ladder.peaks$bin.end.pos <- ladder.peaks$maxpos.size + ladder.peaks$maxborder
  ladder.peaks[,c('cytoband',"dye", 'start.pos','end.pos','color','peak.height','peak.maxpos.time','peak.startpos.time','peak.endpos.time','id','maxpos.size','startpos.size','endpos.size'):=NULL]
  ladder.peaks <- unique(ladder.peaks)
	


  samples.peaks <- peaks[!(id %in% ladder.sample)]
  samples.peaks <- samples.peaks[, c('bin', 'binsize', 'minborder', 'maxborder') :=NULL]
  setkey(ladder.peaks, "system", "bin.start.pos", "bin.end.pos")
  

  ladder.vs.samples <- data.table::foverlaps(samples.peaks, ladder.peaks, by.x = c("system", "startpos.size", "endpos.size"))
  ladder.vs.samples[,c('bin.start.pos', 'bin.end.pos') :=  NULL]
  result.error <- NULL
  if (length(error.systems) > 0) {
    result.error <- paste0("Could not bin systems ", paste(error.systems, collapse = ", "), " using all the ladder samples. This could be due to not accurate threshold value for the ladder sample(s). This may lead to a less accurate bin labelling.")
  }


  
  
  result <- list(
	binnedpeaks = rbindlist(list(ladder.vs.samples,peaks[id %in% ladder.sample] ), use.names=T),
	error = result.error
  )
  

  return (result)
}


#' Based on a standard.dye and a ladder, the function convert the time into molecular weight (based on a linear regression model)
#' @import data.table
#' @importFrom pracma findpeaks
#' @param fsa.raw.data data.table containing the itensity values for each dye and each sample
#' @param time column of fsa.raw.data data.table containing the values that should be scaled according to the scaling dye
#' @param scales list of scales into data.tables
#' @param ladder type of scaling system
#' @param standard.dye id of the dye containing the scaling
#' @param minpeakheights minimal intensity value to be considered as a peak
#' @param removeOutlyers deprecated, not used anymore
#' @param minDist minimal time distance between two peaks 
#' @return a data table containing the molecular size values (obtained by normalizing accoding to a given scale of a given dye) vs the intensity values
#' @export
scale.timeseries <- function(fsa.raw.data, time = time, scales, ladder = 'LIZ500', standard.dye = 'LIZ', minpeakheights = NULL, removeOutlyers = TRUE, minDist = 10) {
  # min.peak.height is a list of list
  # list(sample1(dye1=minval,dye2=minval, ...),
  #      sample2(dye1=minval,dye2=minval, ...)
  # )
  results <- list()
  models <- list()
  peaks <- list()

  intensities <- fsa.raw.data$intensities
  error <- ""
  if (!(standard.dye %in% names(intensities)) || ladder == 'Raw') {
    intensities$sizes <- intensities$time

  } else {
    ids <- unique(intensities$id)


    scalei <- scales[[ladder]]

    for (idi in ids) {
		print("TUPUESDUCUL")
        minval <- minpeakheights[[idi]][[standard.dye]]
        peaks.dt <- data.table(pracma::findpeaks(intensities[id == idi][[standard.dye]], minpeakheight = minval, zero = "+"))
        if (nrow(peaks.dt) < length(scalei)) {
			error <- paste("Not enough peaks detected for sample", idi);
			intensities$sizes <- intensities$time
			break;
        }
        
        
        
        names(peaks.dt) <- c("peak.height", "peak.maxpos", "peak.startpos", "peak.minpos")
        
        
        peaksdist <- abs(peaks.dt$peak.maxpos - c(peaks.dt$peak.maxpos[2:nrow(peaks.dt)], 0))
        peaks.dt$dist <- peaksdist
        valid.peaks <- peaks.dt[peaksdist > minDist,]
        median.height <- median(tail(valid.peaks$peak.height, n = length(scalei)))
        
        valid.peaks <- valid.peaks[peak.height < 2*median.height]
        valid.peaks <- tail(valid.peaks, n = length(scalei))
        valid.peaks$sizes <- scalei
        # print(valid.peaks)
        lm.model <- lm(sizes~peak.maxpos, valid.peaks)
        intensities[id == idi, sizes := lm.model$coefficients[1] + time*lm.model$coefficients[2]]
        models[[idi]] <- lm.model
        peaks[[idi]] <- valid.peaks

    }
  }
  if (error == "") {
	results$intensities <- intensities
	results$models <- models
	results$peaks <- peaks
	results$error <- NULL
	print(results$intensities)
  } else {
	results$intensities <- intensities
	results$models <- NULL
	results$peaks <- NULL
	results$error <- error
  }
  return (results)
}



#' Convert a bin file (GeneMapper idx format) to a data.Table that can be used by 
#' @importFrom data.table fread
#' @importFrom pracma findpeaks
#' @param bin.file as used by GeneMapper that contains the position of the bins of a ladder sample.
#' @return a data table containing for each peak the bin it is inferred to belong to
#' @export
read.bin.file <- function (bin.file) {
  temp <- data.table::fread(bin.file)
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

#' Annotate all the peaks with their corresponding bin
#' @import data.table 
#' @param bins result of the read.bin.file function
#' @param peaks data.table produced by peaks.to.marker
#' @param ladder.samples vector of ladder samples
#' @return a data table containing the position of the annotated bins
#' @export

bins.position <- function(bins, peaks, ladder.samples) {
  ladder.peaks <- peaks[id %in% ladder.samples & !is.na(system),c('bin', 'system', 'maxpos.size', 'dye')]
  ladder.peaks <- unique( ladder.peaks[,maxpos.size := mean(maxpos.size), by = list(system, bin)])


  
  offset.bins <- merge(ladder.peaks, bins, by = c('bin', 'system'), all.y = T)
  offset.bins <- offset.bins[order(system, size)]
  system.dye <- unique(offset.bins[!is.na(dye), c('system' ,'dye')])
#   print(system.dye)
  dyes <- unique(ladder.peaks$dye)
  systems <- unique(system.dye$system)
  for (systemi in systems) {
    offset.bins[system == systemi, dye := system.dye[system == systemi]$dye]
  }
  offset.bins[virtual == F, inferred.pos := maxpos.size]
  previous.observed.not.virtual <- NA;
  for (i in 1:nrow(offset.bins)) {
    if (offset.bins$virtual[i]) {

      if (offset.bins$system[i-1] != offset.bins$system[i]) next;
      if (i+1 > nrow(offset.bins)) next;
      if (is.na(previous.observed.not.virtual)) next;
      if (i == 1) next;
      if (offset.bins$system[i+1] != offset.bins$system[i]) next;
      previous.obssize <- previous.observed.not.virtual
      previous.size <- previous.size.not.virtual
      sizei <- offset.bins$size[i]
      next.obssize <- NA
      j <- 1
#       print(offset.bins)
      while (is.na(next.obssize) && (i+j) < nrow(offset.bins) &&  offset.bins$system[i+j] == offset.bins$system[i]) {
		next.obssize <- offset.bins$inferred.pos[i+j]
		next.size <- offset.bins$size[i+j]
		j <- j+1
      }
      offset.bins$inferred.pos[i] <- previous.obssize  + ((sizei-previous.size)*(previous.size-next.size)/(previous.obssize-next.obssize))
    } else if (!offset.bins$virtual[i] && i > 1 && offset.bins$system[i-1] == offset.bins$system[i]) {
	  previous.size.not.virtual <-  offset.bins$size[i]
      previous.observed.not.virtual <-  offset.bins$inferred.pos[i]
    }
    

  }
#   print(offset.bins)
  for (i in 1:nrow(offset.bins)) {
    if (is.na(offset.bins$inferred.pos[i]) && offset.bins$virtual[i]) {

      systemi <- offset.bins$system[i]
      if (sum(!is.na(offset.bins[system == systemi]$size)) > 2 && sum(!is.na(offset.bins[system == systemi]$inferred.pos)) > 2) {
# 		print(offset.bins[system == systemi])
		lm.model <- lm(offset.bins[system == systemi]$size ~ offset.bins[system == systemi]$inferred.pos)
		offset.bins$inferred.pos[i] <-  lm.model$coefficients[1] + offset.bins$size[i]*lm.model$coefficients[2]
	  }
    }
  }
  

  return(offset.bins)
}
