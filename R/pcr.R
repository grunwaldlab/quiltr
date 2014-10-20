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

#===================================================================================================
#' Creates a thermocycler profile
#' 
#' Creates a thermocycler profile in either graphical or verbal form. This is intended to be used
#' with arbitrary profiles. For generic PCR profiles, use \code{\link{pcr_profile}}.
#' 
#' @param profile (\code{list(list(vector(2)))}) A list of named 2-element numeric vectors, each
#'  element in the list representing a stage in a profile with the 2-element numeric vector 
#'  representing the temperature and time. Optionally, a named list of the data structure described
#'  above can be supplied to split the stages into named groups (see example). 
#' @param width (\code{numeric}) The relative width of groupings in the graphical output. By
#'  default, the width of each group will be proportional to the number of stage it contains.
#' @seealso \code{\link{pcr_profile}
#' @export
thermocycler_profile <- function(profile, width = NULL) {
  profile <- melt(profile)
  profile <- cbind(profile$value[seq(2, nrow(profile), 2)], profile[seq(1, nrow(profile), 2), ])
  names(profile) <- c("time", "temp", "stage", "group")
  profile$group <- factor(profile$group, levels = unique(profile$group), ordered = TRUE)
  
  duration_to_range <- function(data) {
    data$time <- cumsum(data$time)
    start_times <- c(1, data$time[-nrow(data)] + 1)
    data <- adply(data, 1, function(x) x[c(1,1), ]) #duplicate each row 
    data$time[seq(1, nrow(data), 2)] <- start_times #replace odd rows with start time
    return(data)
  }
  
  profile <- ddply(profile, "group", duration_to_range)
  ggplot(data=profile, aes(x=time, y=temp)) + geom_path() + facet_wrap(~ group, scales="free_x")
}

#===================================================================================================
#' Creates a thermocycler profile for PCR
#' 
#' Creates a thermocycler profile for PCR in either graphical or verbal form. This is intended to be
#' used for mostly generic profiles. For more advanced profiles, use
#' \code{\link{thermocycler_profile}}. All temperature and time parameters take \code{numeric}
#' inputs in celsius and seconds respectively.
#' 
#' @param cycles (\code{integer}) 
#' @param init_tm The initial heating temperature used before cycling starts.
#' @param init_time The duration of the initial heating used before cycling starts.
#' @param denat_tm The temperature of the denaturation step.
#' @param denat_time The duration of the denaturation step.
#' @param anneal_tm The temperature of the annealing step.
#' @param anneal_time The duration of the annealing step.
#' @param elong_tm The temperature of the elongation step.
#' @param elong_time The duration of the elongation step.
#' @param final_tm The temperature of the final elongation step used after cycling completes.
#' @param final_time The duration of the final elongation step used after cycling completes.
#' @param hold_tm The temperature used after all other steps to hold the sample.
#' @keywords PCR
#' @seealso \code{\link{thermocycler_profile}
#' @export
pcr_profile <- function(cycles = 30, init_tm = 95, init_time = 300, denat_tm = 96,
                        denat_time = 25, anneal_tm = 55, anneal_time = 30, elong_tm = 72,
                        elong_time = 100, final_tm = 72, final_time = 600, hold_tm = 5) {
  profile <- list("initialization" = list("initial denaturation" = c(init_tm, init_time)),
                  "amplification"  = list("denaturation" = c(denat_tm, denat_time),
                                          "annealing" = c(anneal_tm, anneal_time),
                                          "elongation" = c(elong_tm, elong_time)),
                  "resolution"     = list("final elongation" = c(final_tm, final_time),
                                          "holding" = c(hold_tm, Inf)))
  thermocycler_profile(profile)
}
