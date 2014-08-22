#' Adds lines in a ggplot2 plot for the maximal absorption of compunds relevent to molecular biology
#'
#' Adds lines in a ggplot2 plot for the maximal absorption of compunds relevent to molecular biology.
#' @export
#' @importFrom ggplot2 geom_vline
nanodrop_absorbtion_maxima <- function() {
  absorption_maxima <- data.frame(Substance=c("DNA", "Protein"), value=c(260, 280))
  geom_vline(data=absorption_maxima,
             aes(xintercept=value, 
                 linetype=Substance),
             show_guide = TRUE)
}