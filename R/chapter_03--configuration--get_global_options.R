#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting global options
#|
#| Some of `quilt`'s options can inflence how other options are interpreted.
#| For example, `config_name` determines the name used by configuration files, which in turn are used to specify other `quilt` and renderer options.
#| Therefore the values of these options must be determined before those of other options.
#| This is the reason the concept of "global options" was introduced. 
#| Global cannot be path-specific and only use configuration files in `config_path`.
#| Currently, all options of `quilt` are global options and all options of renders are local options.
#| However, global options can still be output-type specific.
#| This because some code that uses the options, such as the code that determines local options, is in the same loop that executes the renderer functions.
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
  
  #| ### Define default output structure
  #| The first thing we will do is make the output two-dimensional list output structure.
  #| It will be populated with default values of `main_funciton` (`quilt` in this case).
  #| To make the structure we need the list of output types and the list of `main_funciton` options.
  output_types <- get_output_types(sub_functions)
  defaut_options <- as.list(formals("quilt"))
  output <- t(vapply(output_types,
                     USE.NAMES = TRUE, FUN.VALUE = defaut_options, 
                     FUN = function(x) defaut_options))
  
  #| ### Read configuration file(s)
  #| Since a `config_path` specified in a configuration file can redirect to multiple configuration file, this will be a recursive process.
  valid_options <- unique(unlist(lapply(sub_functions, function(x) names(formals(x)))))
  read_config_path <- function(config_path, config_name, group = NA) {
    options <- parse_configuration(paths = config_path, 
                                   config_name = config_name,
                                   valid_options = valid_options,
                                   global_options =  names(defaut_options),
                                   group_prefixes = output_types)
    if ( ! is.na(group)) { options[ , "group"] = group }
    config_path_settings <- options[ options[ , "option"] == "config_path", ]
    if (length(config_path_settings) > 1) {
      redirect <- function(settings) {
        if (normalizePath(settings$config_path) == normalizePath(config_path)) {
          return(options)
        } else {
          return(read_config_path(settings$config_path,
                                  config_name,
                                  settings$group))
        }
      }
      sub_config_data <- apply(path_settings, MARGIN = 1, FUN = redirect)
      return(sub_config_data)
    } else {
      return(options)
    }
  }
  settings <- read_config_path(config_path, config_name)
  
  #| ### Apply configuration file settings and return
  for (setting in settings) {
    output[setting$group, setting$option] <- setting$value
  }
  return(output)
}