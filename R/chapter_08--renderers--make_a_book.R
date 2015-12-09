#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # 



#' @title
#' Render files into a book
#' 
#' @description 
#' Makes a book style representation of a set of files.
#' This function is typically used via the \code{\link{quilt}} function rather than being executed directly.
#' 
#' @param file_paths (\code{character})
#' Files to render.
#' 
#' @param output_path (\code{character} of length 1)
#' Location to write the output directory.
#' The output will be made in a directory called \code{ouput_name} in this location.
#' If \code{NULL} or \code{NA}, it will be made in a temporary directory.
#' @param output_name (\code{character} of length 1)
#' The name of the output file.
#' @param overwrite (\code{logical} of length 1)
#' If \code{TRUE}, an file with the same name as the output file will be overwritten.
#' 
#' @param open (\code{logical} of length 1) If \code{TRUE}, open the newly created book in an 
#' RStudio viewer window.
#' 
#' @param name (\code{character} of length 1)
#' The name of the book as a whole. 
#' If \code{NULL} or \code{NA}, no name will be used.
#' 
#' @param display (\code{logical} or \code{character})
#' Whether or not to display the content of files.
#' @param execute (\code{logical} or \code{character})
#' Whether or not to execute code files.
#' 
#' \href{https://github.com/grunwaldlab/quiltr/issues/58}{#58}
#' @param path_parser (\code{function})
#' Defines a function that determines how file paths are used to infer the placement of file content in the organizational hierarchy.
#' The function should take a absolute file/folder path (\code{character} of length 1) and return one or more hierarchy elements (\code{character}).
#' The function is called once for each folder/file in the file path and the values returned are combined to make the hierarchy.
#' By default, a function is used that simply returns the name of each folder, capitalized and with underscores converted to spaces.
#' @param placement (\code{character}) 
#' Specify a custom organizational heirarchy placement for file content.
#' This can override or add to the hierarchy inferred from file/directory names (\code{path_parser}).
#' The following special values can be included: 
#' \describe{
#'   \item{+}{Insert the heirarchy elements that would have resulted from (\code{path_parser}) at this location.}
#' }
#' 
#' @return (\code{character} of length 1) The file path to the created file
#' (\code{index.html})
#' 
quilt_book <- function(file_paths = list.files(full.names = TRUE, include.dirs = TRUE, recursive = TRUE),
                       output_path = getwd(), output_name = "website", overwrite = FALSE, open = TRUE,
                       name = NA, display = TRUE, execute = FALSE, path_parser = quiltr::use_folder_names, placement = NULL) {
  
}