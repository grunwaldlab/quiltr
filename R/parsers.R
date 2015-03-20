#===================================================================================================
#' Parses the output of the Nanodrop spectrophotometer
#'
#' Parses the output of the Nanodrop spectrophotometer into a dataframe.
#' @param path The file path to the .tsv output file of Nanodrop measurments.
#' @param average If true, all numeric data for measurments with the same ID are averaged.
#' @keywords nanodrop Nanodrop spectrophotometer
#' @export
read_nanodrop_tsv <- function(path, average=TRUE) {
  data_format <- "%m/%d/%Y %I:%M:%S %p"
  data <- do.call(rbind, lapply(path, read.csv, sep='\t'))
  names(data) <- c("measurment", "sample_id", "user", "time",
                   "concentration", "unit", "a260", "a280",
                   "a260_a280", "a260_a230", "type", "factor")
  data$time <- lubridate::mdy_hms(data$time)
  if (average) {
    data <- aggregate(. ~ sample_id, data = data, mean)
    class(data$time) = 'POSIXct'
    data$time = format(data$time, data_format)
  }
  data$time <- lubridate::mdy_hms(data$time)
  data <- data[order(data$time), ]
  row.names(data) <- 1:nrow(data)
  data$sample_id <- as.character(data$sample_id)
  return(data)
}

#===================================================================================================
#' Parses the spectrum output of the Nanodrop spectrophotometer
#'
#' Parses the spectrum output of the Nanodrop spectrophotometer into a dataframe.
#' @param path The file path to the spectrum .tsv output file of Nanodrop measurments.
#' @param average If true, all numeric data for measurments with the same ID are averaged.
#' @keywords nanodrop Nanodrop spectrophotometer spectrum
#' @export
read_nanodrop_spectrum_tsv <- function(path, average=TRUE) {
  split_at <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  data <- sapply(path, function(x) c(readLines(x), '', ''))
  data <- split_at(data, which(data == ''))
  data <- data[sapply(data, length) > 1]
  data[-1] <- lapply(data[-1], function(x) x[-1]) 
  sample_id <- sapply(data, function(x) x[1])
  sample_date <- sapply(data, function(x) x[2])
  sample_date <- lubridate::mdy_hms(sample_date)
  data <- lapply(data, function(x) x[-(1:3)]) 
  wavelength <- lapply(data, function(x) as.numeric(gsub("\t.*$", '', x)))
  absorbance <- lapply(data, function(x) as.numeric(gsub("^.*\t", '', x)))
  names(absorbance) <- sample_id
  if (length(unique(wavelength)) != 1) {
    stop("wavelengths are not the same between samples")
  }
  data <- plyr::ldply(absorbance)
  if (average) {
    data <- aggregate(. ~ .id, data = data, mean)
    sample_id <- data$.id
    data <- t(data[,-1])
  }
  rownames(data) <- wavelength[[1]]
  colnames(data) <- sample_id
  return(data)
}

#===================================================================================================
#' Parses the output of the Qubit fluorometer
#'
#' Parses the output of the Qubit fluorometer into a dataframe. Since the Qubit stores data in the oppisite
#' order that the tubes are inserted, the rows order will be reversed (i.e. the first row will be the first
#' measured).
#' NOTE: if a sample is too low and has a < symbol, this function does not handle it correctly yet!
#' @param path The file path to the output file of Qubit measurments.
#' @param volume_used The volume of sample added during dilution.
#' @keywords qubit Qubit
#' @export
read_qubit <- function(path, volume_used=NULL) {
  data <- read.csv(path, header=TRUE, fileEncoding="latin1")
  data <- data[rev(1:nrow(data)),]
  colnames(data) <- c("name", "time", "dilute_concentration", "dilute_unit", "concentration", "unit",
                      "assay", "sample_volume", "dilution_factor", "std_1_rfu", "std__rfu", "std_3_rfu",
                      "excitation", "green_rfu", "far_red_rfu")
  numeric_cols = c("dilute_concentration", "concentration", "sample_volume","dilution_factor",
                   "std_1_rfu", "std__rfu", "std_3_rfu", "green_rfu", "far_red_rfu")   
  data[,numeric_cols] = apply(data[,numeric_cols], 2, function(x) as.numeric(as.character(x)))
  if (!is.null(volume_used)) { 
    data$concentration <- data$dilute_concentration * (200 / volume_used)
  }
  data$time <- lubridate::ymd_hms(data$time)
  row.names(data) <- 1:nrow(data)
  return(data)
}
