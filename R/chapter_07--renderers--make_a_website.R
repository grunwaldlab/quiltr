#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # 
#' @param file_paths (\code{character})
#' Files to render.
#' 
#' @param output_path (\code{character} of length 1)
#' Location to write the output directory.
#' The website will be made in a directory called \code{ouput_name} in this location.
#' If \code{NULL} or \code{NA}, the website will be made in a temporary directory.
#' @param output_name (\code{character} of length 1)
#' The name of the output directory.
#' @param overwrite (\code{logical} of length 1)
#' If \code{TRUE}, an existing directory with the same name as the output directory will be overwritten.
#' 
#' @param full_copy (\code{logical} of length 1)
#' If \code{TRUE}, The entire target directory will be copied instead of just necessary files.
#' It is possible that more than just the target directory will be copied if there are files
#' in the target directory with dependencies outside it.
#' Enough of the file structure will be copied to included all dependencies. 
#' @param open (\code{logical} of length 1) If \code{TRUE}, open the newly created website in an 
#' internet browser or the RStudio viewer window.
#' 
#' @param name (\code{character} of length 1)
#' The name on the link to the homepage of the website. 
#' @param theme (\code{character} of length 1) The bootstrap theme used in the website. For current
#' options, see \url{http://rmarkdown.rstudio.com/html_document_format.html#appearance-and-style}
#  At the time of writing, valid options are:
#' \itemize{
#'   \item "default"
#'   \item "cerulean"
#'   \item "journal"
#'   \item "flatly"
#'   \item "readable"
#'   \item "spacelab"
#'   \item "united"
#'   \item "cosmo"
#' }
#' 
#' @param display (\code{logical} or \code{character})
#' Whether or not to display the content of files.
#' @param execute (\code{logical} or \code{character})
#' Whether or not to execute code files.
#' @param link (\code{logical} or \code{character})
#' Whether or not to link raw file content.
#' 
#' \href{https://github.com/grunwaldlab/quiltr/issues/58}{#58}
#' @param path_parser (\code{function})
#' Defines a function that determines how file paths are used to infer the placement of file content in the menu hierarchy.
#' The function should take a absolute file/folder path (\code{character} of length 1) and return one or more menu hierarchy elements (\code{character}).
#' The function is called once for each folder/file in the file path and the values returned are combined to make the menu hierarchy.
#' @param placement (\code{character}) 
#' Specify a custom menu heirarchy placement for file content.
#' This can override or add to the hierarchy inferred from file/directory names (\code{path_parser}).
#' The following special values can be included: 
#' \describe{
#'   \item{+}{Insert the heirarchy elements that would have resulted from (\code{path_parser}).}
#' }
#' 
#' @return (\code{character} of length 1) The file path to the created website's home page 
#' (\code{index.html})
#' 
#' @examples
#' \dontrun{
#' 

quilt_wesbite <- function()