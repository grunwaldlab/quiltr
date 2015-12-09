#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting local options
#|
#| File path specific options are supplied to each renderer function after being parsed from configuration files.
#| This chapter deals with the parsing of path-specific options given a set of configuration files and a set of target files. 
#| Which configuration and target files that are used is determined in previous chapters.
#|
#| ## The code
#|
#' @title 
#' Get local options from configuration files
#' 
#' @description 
#' Dertermines the options values of a renderer function for a set of file paths by parsing any 
#' configuration files that might be present in a set of folders.
#' Default option values defined in the renderer function are used if no configuration file settings apply. 
#' 
#' @param sub_function (\code{character} of length 1)
#' The name of the renderer function whose options are set using configuration files.
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

  #| ### Define default output structure
  #| The first thing we will do is make the two-dimensional list output structure.
  #| It will be populated with default values of `sub_function` (a renderer).
  #| To make the structure we need the list of target_paths and the list of `quiltr` options.
  renderer_options <- as.list(formals(get_quilt_renderers()[[sub_function]]))
  output <- t(vapply(target_paths,
                     USE.NAMES = TRUE, FUN.VALUE = renderer_options, 
                     FUN = function(x) renderer_options))
  
  
  #| ### Order `config_paths` by folder depth
  #| Options defined in parent directories should be overwritten by options defined in their children. 
  #| Therefore, configurations files in folders closer to root should be applied first.
  path_depth <- vapply(strsplit(config_paths, split = .Platform$file.sep, fixed = TRUE),
                       FUN = length, FUN.VALUE = numeric(1))
  config_paths <- config_paths[order(path_depth)]
  
  #| ### Parse configuration files
  #| All of the configuration files are parsed at the same time and consolidated into the same 2-dimensional list.
  config_data <- parse_configuration(paths = config_paths,
                                     valid_options = valid_config_options(),
                                     config_name = config_name, 
                                     group_prefixes = names(get_quilt_renderers()))
  
  #| ### Filter out non-applicable options 
  #| Only options that are specific to this renderer (i.e. output type) should be considered.
  config_data <- config_data[config_data[, "option"] %in% names(renderer_options), , drop = FALSE]
  config_data <- config_data[config_data[, "group"] %in% c(NA, sub_function), , drop = FALSE]
  
  #| ### Apply each setting one at a time
  #| The following function will be run on each row of the parsed configuration file data and apply the changes specficied to the ouptut data. 
  apply_setting <- function(setting) {
    matching_paths <- sys_glob(setting$path)
    output[target_paths %in% matching_paths, setting$option] <<- setting$value
  }
  apply(config_data, MARGIN = 1, FUN = apply_setting)
  
  #| Return the output data
  return(output)
}
#|
#|
#|
#| ### Interpreting path patterns
#|
#| The paths supplied to the above function might be in the form of path patterns.
#| Potentially these could be in the form of paths with wildcards ('*') or regex, although currently only wildcards are supported.
#| The `Sys.glob' function in the R base handles wildcard expansion, but does not understand double wildcards ('**').
#| Double wildcards can be used to match in any decendent of the preceding folder. 
#| This is useful for setting options for file types regardless of locations.
#| For example, `**.txt` would match all `*.txt` files in any decendent folder of the current working directory.
#| The function below acts as a replacement for `Sys.glob' that incorepates this ability. 
#|
#' @title 
#' Wildcard expansion
#' 
#' @description 
#' Like \code{\link[base]{Sys.glob}}, but also understands double wildcards for recursive matching using
#' double wildcards (\code{**})
#' 
#' @param path (\code{character} of length 1) The path to expand.
#' @param max_search_depth (\code{integer} of length 1) How deep to search.
#' 
#' @return \code{character} 
#' File paths matching the input pattern.
sys_glob <- function(path, max_search_depth = 50) {
  # Find location of double wildcard ---------------------------------------------------------------
  split_path <- strsplit(path, .Platform$file.sep)[[1]]
  pair_index <- which(grepl(pattern = "\\*\\*", split_path))
  if (length(pair_index) > 1) {
    stop(paste0("Currently, Quiltr only supports one double wildcard (**) per path. ",
                "The following path has more than one:\n\t", path))
  }
  if (length(pair_index) == 0) { return(Sys.glob(path)) }
  # List possible paths using single wildcards -----------------------------------------------------
  possibilities <- lapply(0:max_search_depth, rep, x = "*")
  
  split_path[pair_index] <- gsub(pattern = "\\*\\*", replacement = "*",
                                 split_path[pair_index])
  possible_paths <- lapply(possibilities,
                           function(x) c(split_path[seq(from = 1, length.out = pair_index - 1)],
                                         x,
                                         split_path[seq(from = pair_index, to = length(split_path))]))
  possible_paths <- lapply(possible_paths, function(x) do.call(file.path, as.list(x)))
  # Search all possible paths ----------------------------------------------------------------------
  unique(unlist(lapply(possible_paths, Sys.glob)))
}