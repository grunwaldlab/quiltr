#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting global options
#|
#| Global options can inflence how local options are read from configurations files and what renders are executed, amoung other things.
#| Therefore, global options must be determined before local options.
#| Global cannot be path-specific and only use configuration files in `config_path`.
#| Currently, all options of `quilt` are global options and all options of renders are local options.
#| Even though they are not options of the renders they can still be output-type specific.
#| This because some code that uses the options, such as the code that determines local options, is in the same loop that executes the renderer functions.\
#|
#|
#| ## Design citeria
#|
#| * The arguments should include the list of renderer functions, and a configuration file path or folder/name.
#| * The output should be a named list of named lists, representing the options for each output type. 
#|   The first dimension groups options by output type, and the second is the lists of options.
#| * The `config_path` option should allow to specify other configuration files and the order they are applied. 
#|
#| ## The code
#|
#' @title
#' Get global options values from configuration files.
#' 
#' @description 
#' Find and parse configuration files in the given path or starting configuraiton file.
#' Return a two dimensional list of output types vs option values.
#' The `config_path` option in configuration files can be used to add additional configuration files recursivly.
#' 
#' @param  main_function (\code{character} of length 1)
#' The name of the function to get global option names and default values from.
#' 
#' @param  sub_functions (\code{character})
#' The name of the functions called by \code{main_function}.
#' This is used to identify invalid options in configuration files.
#' 
#' @param  config_path  (\code{character} of length 1)
#' The path to a configuration file or a folder that might contain one.
#' If a path to a folder is supplied, the \code{config_name} option is used to look for the file.
#'  
#' @param  config_name (\code{character}) 
#' The name(s) of configuration files without file extension(s).
#' 
#' @return named \code{list} of named \code{list}s
#' The first dimension is output types and the second a list of settings for that output type.
#|
#|
get_global_options <- function(main_function, sub_functions, config_path, config_name) {
  
}