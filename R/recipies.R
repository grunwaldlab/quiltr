volume_for_dilution <- function(initial_conc, final_volume, final_conc = min(initial_conc)) {
  initial_volume <- (final_conc * final_volume) / initial_conc
  volume_added <- final_volume - initial_volume
  return(list(initial_volume=initial_volume, volume_added=volume_added))
}

#' Creates table of dilutions instructions
#'
#' Creates table of dilutions instructions.
#' @param initial_conc A numeric vector of initial concentrations of samples.
#' @param final_volume The desired final volume after dilution.
#' @param final_conc The desired final concentration. (Default: minimum of initial concentration)
#' @param id The row names for the samples. (Default: the names of the initial_conc vector)
#' @param display If True, the table is printed in markdown format. 
#' @param ... Extra arguments are passed to print_table 
#' @keywords dilution
#' @export
#' @importFrom knitr kable
volume_for_dilution_table <- function(initial_conc, final_volume, final_conc = min(initial_conc), id=names(initial_conc), display=TRUE, ...) {
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
    print_table(output_table)    
  }
  
  return(output_table)
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
#' @importFrom knitr kable
serial_dilution_table <- function(range, dilutions, volume, units="&mu;L") {
  significant_figures <- 4
  base <- 10
  dilution_exp <- diff(log(range, base)) / (dilutions) #exponent to raise for each dilution
  dilution_factor <- base^dilution_exp
  each_addition <-  dilution_factor * range[1] * volume / (range[1] - dilution_factor * range[1])
  data <- data.frame(N=0:dilutions, Dilution = base^(dilution_exp * 0:(dilutions)))
  data$Concentration = data$Dilution * range[1]
#  text_data <- format(data, scientific=FALSE, digits=significant_figures)
  writeLines(paste("**Serial dilution table**\nFor each dilution, dilute ", 
                   signif(each_addition, significant_figures), units,
                   " of the previous sample in ",
                   signif(volume, significant_figures), units,
                   " of solvent:\n", sep=""))
  writeLines("")
  print_table(data)
  return(data)
}

#' Creates a markdown recipie for Qubit preparation 
#'
#' Creates a markdown recipie for Qubit preparation. 
#' @param count The number of samples to be measured.
#' @param expected The expected concentration of the DNA to be measured. Used to estimate the volume_added if
#'  not specified.
#' @param volume_added The amount of sample to be diluted to 200&mu;L.
#' @param standards TRUE/FALSE for wether standards will be made. 
#' @param safety_factor A factor that will be multiplied to the exact amount of working solution needed.
#' @keywords Qubit qubit
#' @export
#' @examples
#' qbit_recipie(8, 5)
qubit_br_recipie <- function(count, expected=NA, volume_added=NA, standards=TRUE, safety_factor=1.1) {
  sig_figs <- 4
  optimize_volume_added <- function(expected) {
    mid_range = 1 #the middle of the Qubit detection range (.01 to 5ng/uL) of the diluted sampled
    ideal <- mid_range*200/expected
    if (ideal < 1) {
      ideal <- 1
    } else if (ideal > 20) {
      ideal <- 20
    }
    return(ideal)
  }
  if (is.na(expected) == is.na(volume_added)) {
    stop("Either 'expected' or 'volume_added' must be specified, but not both.")
  }
  if (is.na(expected) == FALSE) {
    if (expected < .1 || expected > 1000) {
      stop("The Qubit BR Assay is only accurate for initial sample concentrations from 0.1ng/&mu;L to 1000ng/&mu;L.")
    }    
  }
  if (is.na(volume_added) == FALSE) {
    if (volume_added < 1 || volume_added > 20) {
      stop("'volume added' must be between 1 and 20.")
    }    
  }
  if (is.na(volume_added)) {
    volume_added <- optimize_volume_added(expected)
  }
  if (safety_factor < 1) {
    stop("safety_factor must be at least 1.")
  }
  final_tube_volume <- 200
  tube_working_volume <- final_tube_volume - volume_added
  working_volume <- tube_working_volume * count
  if (standards) {
    working_volume <- working_volume + 2 * 190
    standards_text <- " and 2 standards"
  } else {
    standards_text <- ""
  }
  working_volume <- working_volume * safety_factor
  reagent_volume <- working_volume * (1/200)
  buffer_volume <- working_volume * (199/200)
  max_detection <- 5*(200/volume_added) #These values were inferred from the manual
  min_detection <- .01*(200/volume_added) #These values were inferred from the manual
  text <- paste("**Qubit recipie:**\n\n",
                "For **", count, "** samples", standards_text, ", make **", signif(working_volume, sig_figs),
                "&mu;L** of working solution by mixing **", signif(reagent_volume, sig_figs),
                "&mu;L** of Qubit reagent with **", signif(buffer_volume, sig_figs), "&mu;L** of buffer.\n", 
                "A safety factor of ", safety_factor, " has been applied to these volumes to allow for error.\n\n",
                "Add **", signif(volume_added, sig_figs), "&mu;L** of each sample to **",
                signif(tube_working_volume, sig_figs), "&mu;L** of working solution in appropriate measurment tubes.\n\n",
                sep = '')
  if (standards) {
    text <- paste(text, "Prepare 2 standards by adding **10&mu;L** of each standard to **190&mu;L** of working solution.\n\n", sep="")
  }
  text <- paste(text, "It should be possible to quantify samples with an original concentration between **", 
                signif(min_detection, sig_figs), "ng/&mu;L** and **", signif(max_detection, sig_figs),
                "ng/&mu;L**\n", sep="")
  text <- paste(text, "\n", sep="")
  writeLines(text)
}