#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting local options
#|
#| 
#' @title 
#' Get local options from configuration files
#' 
#' @description 
#' Dertermines the options values of a renderer function for a set of file paths by parsing any 
#' configuration files that might be present in a set of folders.
#' Default option values defined in the renderer function are used if no configuration file settings apply. 
#' 
#' @param sub_function (\code{function} of length 1)
#' The function whose options are set using configuration files.
#' 
#' @param target_paths (\code{character})
#' Files paths that can each have a unique set of options values each.
#' These will correspond to one dimension of the output matrix.
#' 
#' @param config_paths (\code{character})
#' Folder paths where configuration files might exist.
#' 
#' @param config_name (\code{character} of length 1)
#' The file name of configuration files without the file extension.
#' 
#' 
#' @return a 2-dimensional \code{list}
#' 
#' Given a set of file paths and folder paths that might contain configuration files, this function
#' returns the local options that apply to 
get_path_specific_options <- function(sub_function, target_paths, config_paths, config_name) {
  main_function <- "quilt"
  valid_options <- c(unique(unlist(lapply(renderers, function(x) names(formals(x))))),
                     names(default_options))
  
  #| ### Define default output structure
  #| The first thing we will do is make the two-dimensional list output structure.
  #| It will be populated with default values of `sub_function` (`quilt` in this case).
  #| To make the structure we need the list of target_paths and the list of `main_funciton` options.
  default_options <- as.list(formals(sub_function))
  output <- t(vapply(target_paths,
                     USE.NAMES = TRUE, FUN.VALUE = default_options, 
                     FUN = function(x) default_options))
  
  
  #| ### Order `config_paths` by folder depth
  #| Options defined in parent directories should be overwritten by options defined in their children. 
  #| Therefore, configurations files in folders closer to root should be applied first.
  path_depth <- vapply(strsplit(config_paths, split = .Platform$file.sep, fixed = TRUE),
                       FUN = length, FUN.VALUE = numeric(1))
  config_paths <- config_paths[order(path_depth)]
  
  #| ### Parse configuration files
  #| All of the configuration files are parsed at the same time and consolidated into the same 2-dimensional list.
  config_data <- parse_configuration(paths = config_paths,
                                     valid_options = valid_config_options(), #not made yet
                                     config_name = global_options$config_name, 
                                     group_prefixes = names(get_quilt_renderers()))
  
  #| ### Apply each setting one at a time
  #| The following function will be run on each row of the parsed configuration file data and apply the changes specficied to the ouptut data. 
  apply_setting <- function(setting) {
    output[matches_pattern(setting$path), setting$option] <<- setting$value
  }
  apply(config_data, MARGIN = 1, FUN = apply_setting)
  
  #| Return the output data
  return(output)
}