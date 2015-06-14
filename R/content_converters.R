#===================================================================================================
#' Convert rmd to html
#' 
#' Convert rmd to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
convert_rmd_to_html <- function(input, output = tempfile(fileext = ".html")) {
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(dirname(input))
  rmarkdown::render(input, output_file = basename(output), output_dir = dirname(output), 
                    quiet = FALSE)
}

#===================================================================================================
#' Convert html to html
#' 
#' Convert html to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
convert_html_to_html <- function(input, output = tempfile(fileext = ".html")) {
  file.copy(input, output)
  return(output)
}
