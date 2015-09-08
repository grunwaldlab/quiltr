#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # 
#|
#|
#| ## Finding quilt renderers
#|
#| In order to make it easy to extend quilt to other output types, we will identify renderer function by their name, rather than defining the min a list explicitly.
#| Functions in any namespace prefixed with `quilt_` will be considered quiltr renderes and the rest of the function name will be the output type.
#|
#' @title
#' Find quiltr renderers
#'
#' @description
#' Finds the list of renderers for \code{quilt}.
#' Renderer functions are identified by their name in any namespace.
#' 
#' @param pattern (\code{character})
#' A regular expression with a single capture group used to identify renderer functions. 
#' The capture group identifies the output type.
#' 
#' @return \code{list} of functions
#' 
get_quilt_renderers <- function(pattern = "quilt_(*)") {
  quiltr_functions <- unclass(lsf.str(envir = asNamespace(package), all = T))
  function_names <- quiltr_functions[grep(pattern, quiltr_functions)]
  renderers <- mget(function_names, inherits = TRUE)
  names(renderers) <- tolower(stringr::str_match(function_names, pattern)[, 2])
  return(converters)
}