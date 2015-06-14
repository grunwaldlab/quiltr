#===================================================================================================
#' Convert rmd to html
#' 
#' Convert rmd to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_rmd_to_html <- function(input, output = tempfile(fileext = ".html")) {
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(dirname(input))
  rmarkdown::render(input, output_file = basename(output), output_dir = dirname(output), 
                    quiet = FALSE)
}


#===================================================================================================
#' Convert md to html
#' 
#' Convert md to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_md_to_html <- function(input, output = tempfile(fileext = ".html")) {
  quiltr_convert_rmd_to_html(input, output)
}


#===================================================================================================
#' Convert html to html
#' 
#' Convert html to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_html_to_html <- function(input, output = tempfile(fileext = ".html")) {
  file.copy(input, output)
  return(output)
}

#===================================================================================================
#' Convert py to html
#' 
#' Convert python to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_py_to_html <- function(input, output = tempfile(fileext = ".html")) {
  pandoc_command <- paste("pandoc", "-s", "--highlight-style pygments", "-o", output)
  content <- paste0("## ", basename(input), "\n\n", 
                    "```python\n", readChar(input, nchars = 10000000), "```")
  system(pandoc_command, input = content)
  return(output)
}
