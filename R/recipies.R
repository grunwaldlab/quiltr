volume_for_dilution <- function(initial_conc, final_volume, final_conc = min(initial_conc)) {
  initial_volume <- (final_conc * final_volume) / initial_conc
  volume_added <- final_volume - initial_volume
  return(list(initial_volume=initial_volume, volume_added=volume_added))
}

volume_for_dilution_table <- function(initial_conc, final_volume, final_conc = min(initial_conc), id=names(initial_conc), display=TRUE, ...) {
  if (!require(knitr)) {
    stop("install package knitr")
  }
  
  #Generate table of dilution values
  data <- volume_for_dilution(initial_conc, final_volume, final_conc)
  output_table <- data.frame(ID=1:length(data[[1]]))
  if (!is.null(id)) {
    output_table[,'Sample'] <- id
  }
  output_table[,'Initial Conc.'] <- initial_conc
  output_table[, 'Sample Vol.'] <- data$initial_volume
  output_table[, 'Solvent Vol.'] <- data$volume_added
  
  #generate header and contextual table columns
  output_header <- sprintf("Dilution table for **%d** samples.\n", length(initial_conc))
  if (length(unique(final_volume)) == 1) {
    output_header <- c(output_header, sprintf("All samples will be diluted to a final volume of **%f**.", final_volume))
  } else {
    output_table[, 'Final Vol.'] <- data$final_volume
  }
  if (length(unique(final_conc)) == 1) {
    output_header <- c(output_header, sprintf("All samples will be diluted to a final concentration of **%f**.", final_conc))
  } else {
    output_table[, 'Final Conc.'] <- data$final_conc
  }
  
  #print results
  if (display) {
    writeLines(output_header)
    writeLines("")
    kable(output_table, ...)    
  }
  
  return(output_table)
}

#' Creates table for PCR recipie
#'
#' Creates a markdown table for a PCR master mix recipe.
#' @param count The number of PCR reactions.
#' @param additives The non-mastermix ingredients that will be added. Accepts a named numeric vector of volumes.
#' @param additive_concentration The concentration of non-mastermix ingredients. Accepts a named numeric vector of concentrations.
#' @keywords PCR
#' @export
#' @examples
#' pcr_table(count=8, additives=c(DNA=1, other=7))
pcr_table <- function(count, additives=c(DNA=1), additive_concentration=rep('', length(additives))) {
  if (!require(knitr)) {
    stop("install package knitr")
  }
  master_mix_volume = 19.75 - sum(additives)
  data <- data.frame(Component=c("Water", "10x Buffer", "dNTP", "Primer 1", "Primer 2", "Taq", names(additives)),
                     Concentration=c("", "", "10mM", "10&mu;M", "10&mu;M", "", additive_concentration),
                     Single=c(master_mix_volume, 2.5, .5, 1, 1, .25, additives),
                     stringsAsFactors=FALSE)
  data$Total <- data$Single * count
  data$Safe <- data$Total * 1.1
  data[data$Component %in% names(additives), c("Total", "Safe")] = 0
  data <- rbind(data, c(Component="Total", Concentration="", Single=sum(data$Single), Total=sum(data$Total), Safe=sum(data$Safe)))
  
  writeLines(paste("PCR ingredients for ", count, ", ", data$Single[nrow(data)], "&mu;L reactions:", sep=""))
  writeLines("")
  
  kable(data, format = "markdown", )
  return(data)
}


#' Creates table for serial dilutions
#'
#' Creates a markdown table of dilution factors and final concentrations for serial dilutions given 
#' a range of desired concentrations, the number of dilutions, and the final volume. 
#' @param range The inclusive range of concentrations to produce. Takes a numeric vector of length 2.
#' @param dilutions The number of dilutions to perform. Takes a numeric vector of length 1.
#' @param volume The final volume of the dilutions. 
#' @param units The units of volume used. Default: "&mu;L".
#' @keywords dilutions dilute serial
#' @export
#' @examples
#' serial_dilution_table(c(5,.00005), 5, 75)
serial_dilution_table <- function(range, dilutions, volume, units="&mu;L") {
  if (!require(knitr)) {
    stop("requires package knitr")
  }
  significant_figures <- 3
  base <- 10
  dilution_exp <- diff(log(range, base)) / (dilutions) #exponent to raise for each dilution
  dilution_factor <- base^dilution_exp
  each_addition <-  dilution_factor * range[1] * volume / (range[1] - dilution_factor * range[1])
  data <- data.frame(N=0:dilutions, Dilution = base^(dilution_exp * 0:(dilutions)))
  data$Concentration = data$Dilution * range[1]
  data <- format(data, scientific=FALSE, digits=significant_figures)
  writeLines(paste("**Serial dilution table**\nFor each dilution, dilute ", 
                   signif(each_addition, significant_figures), units,
                   " of the previous sample in ",
                   signif(volume, significant_figures), units,
                   " of solvent:\n", sep=""))
  kable(data, format = "markdown")
  return(data)
}