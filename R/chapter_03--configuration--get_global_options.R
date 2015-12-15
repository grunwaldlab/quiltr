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
#' @param  config_path  (\code{character} of length 1)
#' The path to a configuration file or a folder that might contain a configuration file.
#' If a path to a folder is supplied, the \code{config_name} option is used to look for the file.
#'  
#' @param  config_name (\code{character}) 
#' The name(s) of configuration files without file extension(s).
#' 
#' @param  default_format (\code{character}, possibly named)
#' 
#' 
#' @return named \code{list} of named \code{list}s
#' The first dimension is output types and the second a list of settings for that output type.
#|
#|
get_global_options <- function(config_path, config_name, default_format) {
  
  #| ### Read configuration file(s)
  #| Since a `config_path` specified in a configuration file can redirect to multiple configuration file, this will be a recursive process.
  settings <- read_global_options(config_path, config_name)
  
  #| ### Verify content ############################################################################
  #| We should vaildate the content of the settings now that it is in a form that is easy to parse. 
  #|
  #| Lets check for global options being given path-specific values.
  #| For options that are not given path-specific values, the path pattern returned by `reformat_configuration` should be `NA`.
  invalid_rows <- which(settings[, "option"] %in% names(global_options()) & !is.na(settings[, "raw_path"]))
  if (length(invalid_rows) > 0) {
    stop(paste0('Attempt to set global option "', settings[invalid_rows[1], "option"],
                '" to a path-specific value in configuration file "', settings[invalid_rows[1], "raw_path"], '".'))
  }
  #| Some options should be independent of output type, so they should not be assinged an output type
  output_independent <- c("path", "output_format")
  invalid_rows <- which(settings[, "option"] %in% output_independent & !is.na(settings[, "group"]))
  if (length(invalid_rows) > 0) {
    stop(paste0('Attempt to set output-type-independent option "', settings[invalid_rows[1], "option"],
                '" to a output-type-specific value in configuration file "', settings[invalid_rows[1], "path"], '".'))
  }
  
  
  
  #| ### Define default output structure
  #| The first thing we will do is make the output two-dimensional list output structure.
  #| It will be populated with default values of `main_funciton` (`quilt` in this case).
  #| To make the structure we need the list of output types and the list of `main_funciton` options.
  output_format_settings <- settings[settings[, "option"] == "output_format", ]
  if (length(output_format_settings) > 0) {
    output_types <- output_format_settings[length(output_format_settings), "value"]
  } else {
    output_types <- default_format
  }
  # If a name is not assigned to an output type, then assign the name of the output type
  if (is.null(names(output_types))) {
    output_type_names <- output_types
  } else {
    output_type_names <- names(output_types)
    output_type_names[output_type_names == ""] <- output_types[output_type_names == ""]
  }
  output <- t(vapply(output_type_names,
                     USE.NAMES = TRUE, FUN.VALUE = global_options(), 
                     FUN = function(x) global_options()))
  
  #| ### Filter
  settings <- settings[settings[, "option"] %in% names(global_options()), , drop = FALSE]
  settings <- settings[settings[, "group"] %in% output_type_names | is.na(settings[, "group"]), , drop = FALSE]
  
  
  #| ### Apply configuration file settings and return
  apply_setting <- function(setting) {
    if (is.na(setting$group)) {
      output[ , setting$option] <<- lapply(1:nrow(output), function(x) setting$value)
    } else {
      output[[setting$group, setting$option]] <<- setting$value
    }
  }
  apply(settings, MARGIN = 1, apply_setting)
  
  #| ### Overwrite output format with output type
  #| Otherwise, the output_format column would have a single value: the last defined in configuration files or the default. 
  #| This makes the column more useful
  output[ , "output_format"] <- output_types
  return(output)
}




#' @title 
#' Read global settings
#' 
#' @description 
#' Read the global settings in configuration files. 
#' Follow any configuration file redirections within the first file
#' 
#' @param  config_path  (\code{character} of length 1)
#' The path to a configuration file or a folder that might contain a configuration file.
#' If a path to a folder is supplied, the \code{config_name} option is used to look for the file.
#'  
#' @param  config_name (\code{character}) 
#' The name(s) of configuration files without file extension(s).
#' 
#' @param  parent_group  (\code{character} of length 1)
#' The group of the referring config redirection. 
#' Used during recursion. 
read_global_options <- function(config_path, config_name, parent_group = NA) {
  # Read configuration file
  settings <- parse_configuration(paths = config_path, 
                                  config_name = config_name,
                                  valid_options = valid_config_options())
  
  # If a group has been assigned from a parent configuration file then assign all settings in this file to that group.
  # Remove any settings explicitly assigned to other groups.
  if ( ! is.na(parent_group)) {
    settings <- settings[settings[ , "group"] == parent_group | is.na(settings[ , "group"]), , drop = FALSE]
    settings[ , "group"] <- parent_group
  }
  
  
  # Determine if the options defined in this file will be included
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(config_path)
  config_path_settings <- settings[ settings[ , "option"] == "config_path", , drop = FALSE]
  use_current_file_settings <- TRUE
  if ( nrow(config_path_settings) > 0) {
    config_sub_paths <- normalizePath(unlist(config_path_settings[, "value"]))
    if ( ! config_path %in% config_sub_paths ) {
      use_current_file_settings <- FALSE
    }
  }
 
  # Define function to handle settings one at a time
  process_setting <- function(setting) {
    if (setting$option == "config_path") {
      setting_sub_paths <- normalizePath(setting$value)
      setting_sub_paths <- setting_sub_paths[setting_sub_paths != config_path]
      if (length(setting_sub_paths) > 0) {
        return(do.call(rbind, lapply(setting_sub_paths, read_global_options,
                                     config_name = config_name, parent_group = setting$group)))
      }
    } else if (use_current_file_settings) {
      return(setting)
    }
    return(NULL)
  }
  
  do.call(rbind, apply(settings, MARGIN = 1, process_setting))
}






#' @title 
#' Valid global options
#' 
#' @description 
#' Returns the names and default values of quilt options.
#' 
#' @return \code{character}
global_options <- function() {
  quilt_args <- as.list(formals("quilt"))
  is_a_symbol <- vapply(quilt_args, is.symbol, FUN.VALUE = logical(1))
  eval_symbol <- function(symbol) {
    quilt_args[[which(symbol == names(quilt_args))]]
  }
  quilt_args[is_a_symbol] <- lapply(quilt_args[is_a_symbol], eval_symbol)
  return(quilt_args)
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
  quilt_options <- names(global_options())
  return(c(renderer_options, quilt_options))
}



