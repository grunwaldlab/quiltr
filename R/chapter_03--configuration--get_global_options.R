#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting global options
#|
#| Some of `quilt`'s options can influence how other options are interpreted.
#| For example, `config_name` determines the name used by configuration files, which in turn are used to specify other `quilt` and renderer options.
#| Therefore the values of these options must be determined before those of other options.
#| This is the reason the concept of "global options" was introduced. 
#| Global cannot be path-specific and only use configuration files in `config_path`, whereas other options can be set in configuration files throughout the target folder.
#| However, global options can still be output-type specific.
#| This because some code that uses the options, such as the code that determines local options, is in the same loop that executes the renderer functions.
#| Currently, all options of `quilt` are global options and all options of renders are local options.
#|
#|
#| ## Design citeria
#|
#| * The arguments should include the list of renderer functions, and a configuration file path or folder/name.
#| * The output should be named two-dimensional list, representing the options for each output type. 
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
#' @param  renderers (\code{character})
#' The name of the functions called by \code{main_function}.
#' This is used to identify invalid options in configuration files.
#' 
#' @param  config_path  (\code{character} of length 1)
#' The path to a configuration file or a folder that might contain a configuration file.
#' If a path to a folder is supplied, the \code{config_name} option is used to look for the file.
#'  
#' @param  config_name (\code{character}) 
#' The name(s) of configuration files without file extension(s).
#' 
#' @return named \code{list} of named \code{list}s
#' The first dimension is output types and the second a list of settings for that output type.
#|
#|
get_global_options <- function(main_function, renderers, config_path, config_name) {
  
  #| ### Define default output structure
  #| The first thing we will do is make the output two-dimensional list output structure.
  #| It will be populated with default values of `main_funciton` (`quilt` in this case).
  #| To make the structure we need the list of output types and the list of `main_funciton` options.
  output_types <- names(renderers)
  default_options <- as.list(formals("quilt"))
  global_options =  names(default_options)
  output <- t(vapply(output_types,
                     USE.NAMES = TRUE, FUN.VALUE = default_options, 
                     FUN = function(x) default_options))
  
  #| ### Read configuration file(s)
  #| Since a `config_path` specified in a configuration file can redirect to multiple configuration file, this will be a recursive process.
  read_config_path <- function(config_path, config_name, group = NA) {
    options <- parse_configuration(paths = config_path, 
                                   config_name = config_name,
                                   valid_options = valid_config_options(),
                                   global_options =  global_options)
    if ( ! is.na(group)) { options[ , "group"] = group }
    config_path_settings <- options[ options[ , "option"] == "config_path", , drop = FALSE]
    if (nrow(config_path_settings) > 0) {
      redirect <- function(settings) {
        original_wd <- getwd()
        on.exit(setwd(original_wd))
        setwd(dirname(settings$config_path))
        
        redirect_one <- function(path) {
          if (normalizePath(path) == normalizePath(config_path)) {
            return(options)
          } else {
            return(read_config_path(normalizePath(path),
                                    config_name,
                                    settings$group))
          }
        }
        x = do.call(rbind, lapply(settings$value, FUN = redirect_one))
        return(x)
      }
      sub_config_data <- do.call(rbind, apply(config_path_settings, MARGIN = 1, FUN = redirect))
      return(sub_config_data)
    } else {
      return(options)
    }
  }
  settings <- read_config_path(config_path, config_name)
  settings <- settings[settings[, "option"] %in% global_options, , drop = FALSE]
  
  #| ### Apply configuration file settings and return
  apply_setting <- function(setting) {
    if (is.na(setting$group)) {
      output[ , setting$option] <<- lapply(1:nrow(output), function(x) setting$value)
    } else {
      output[[setting$group, setting$option]] <<- setting$value
    }
  }
  apply(settings, MARGIN = 1, apply_setting)
  return(output)
}





#' @title 
#' Valid configuration file options
#' 
#' @description 
#' Returns the names of options that can appear in configuration files.
#' These include all of the options of \code{\link{quilt}} and any of its renderers.
#' Renderers used can be found by running \code{quiltr:::\link{get_quilt_renderers}}.
#' 
#' @return \code{character}
valid_config_options <- function() {
  renderers <- get_quilt_renderers()
  renderer_options <- unique(unlist(lapply(renderers, function(x) names(formals(x)))))
  quilt_options <- names(as.list(formals("quilt")))
  return(c(renderer_options, quilt_options))
}
