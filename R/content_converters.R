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
                    quiet = TRUE)
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
#' Convert txt to html
#' 
#' Convert text files to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_txt_to_html <- function(input, output = tempfile(fileext = ".html")) {
  content <- paste0("# ", basename(input), "\n\n", 
                    "```\n", readChar(input, nchars = 10000000), "```")
  cat(knitr::knit2html(text = content, output = output, quiet = TRUE), file = output)
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
  if (pandoc_is_available()) {
    pandoc_command <- paste("pandoc", "-s", "--highlight-style pygments", "-o", output)
    content <- paste0("# ", basename(input), "\n\n", 
                      "```python\n", readChar(input, nchars = 10000000), "```")
    system(pandoc_command, input = content)    
  } else {quiltr_convert_txt_to_html(input, output)}
  return(output)
}


#===================================================================================================
#' Convert pdf to html
#' 
#' Display pdf files via html embed
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_pdf_to_html <- function(input, output = tempfile(fileext = ".html")) {
  content <- paste0("<h1>", basename(input), "</h1>\n\n", 
                    '<embed src="', basename(input), 
                    '" width="100%" height="600px" type="application/pdf">')
  cat(content, file = output)
  return(output)
}
