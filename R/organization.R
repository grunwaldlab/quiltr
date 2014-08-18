#' Saves tabular data in a standard tab-separated value (.tsv) format
#'
#' Saves tabular data in a standard tab-separated value (.tsv) format with a standardized file name.
#' @param data A data frame to be saved
#' @param exp Numeric vector for experiment ID. For example c(2,3) would be experiment 2-3. This should be unique.
#' @param description A string describing what is to be saved. This will be converetd to lower case and spaces will be replaced by underscores.
#' @param ext The file extension (default: .tsv).
#' @param path The path to where the file will be saved (Default: data).
#' @export
save_table <- function(data, exp, description, ext="tsv", path="data") {
  description <- tolower(gsub(" ", "_", description, fixed=TRUE))
  file_name <- paste(sapply(exp, sprintf, fmt="%04d"), collapse="-")
  file_name <- paste(file_name, description, sep="-")
  file_name <- paste(file_name, ext, sep=".")
  write.table(data,
              file.path(path, file_name),
              quote=FALSE,
              sep='\t',
              col.names = NA)
}



#' Keeps track of experiment start and stop times
#'
#' Prints and logs experiment starting and finishing times to global data.frame 'exp_time_'.
#' @param exp numeric vector for experiment ID. For example c(2,3) would be experiment 2-3. This should be unique.
#' @param started time experiment was started in YYYY-MM-DD HH:MM format.
#' @param finished time experiment was finished in YYYY-MM-DD HH:MM format.
#' @param display if TRUE, print markdown display of start and finish times.
#' @export
#' @importFrom lubridate ymd_hm as.period
log_time <- function(exp, started=NA, finished=NA, display=TRUE) {
  exp <- paste(exp, collapse="-")
  if (!is.na(started)) {
    started <- ymd_hm(started)
  }
  if (!is.na(finished)) {
    finished <- ymd_hm(finished)
  }
  if (!is.na(started) && !is.na(finished)) {
    duration <- finished - started
  } else {
    duration <- NA
  }
  if (!exists("exp_time_")) {
    exp_time_ <<- data.frame(exp=c(), started=c(), finished=c())
  }
  exp_time_ <<- rbind(exp_time_,
                      data.frame(exp = exp,
                                 started = started,
                                 finished = finished,
                                 duration = duration))
  
  if (display) {
    text <- c()
    if (!is.na(started)) {
      text <- c(text, paste("*Started:*", started))
    }
    if (!is.na(finished)) {
      text <- c(text, paste("*Finished:*", finished))
    }
    if (!is.na(started) && !is.na(finished)) {
      text <- c(text, paste("*Duration:*", as.period(duration)))
    }
    writeLines(paste(text, collapse=paste0(rep("&nbsp;", 12), collapse="")))
    writeLines("")
  }
}