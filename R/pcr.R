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
#' @importFrom knitr kable
pcr_table <- function(count, additives=c(DNA=1), additive_concentration=rep('', length(additives))) {
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
  print_table(data)
  writeLines("")
  return(data)
}
