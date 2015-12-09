#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Finding configuration files
#|
#| Although global options must be specified in/via a configuration file in the target directory, the locations of configuration files that can effect local options are more flexible.
#| There are three general locations that these files might appear relative to the target directory:
#|
#| * Parent folders: the folders that contain the target folder. For example, if a target folder was `/a/b/c`, the parents would be `/`, `/a`, and `/a/b`. 
#| * Target folders: the target folder(s) themselves. In the above example, this would be `/a/b/c`. Remember that although only one path can be specified using code, configuration files can redifined it to multiple paths.
#| * Child folders: any folders contained by the target folder(s). In the above example, this could include `/a/b/c/d`. 
#|
#| Any combination of these three groups could be used to find configuration files given one or more target folders and the name of the configuration files.
#| 
#| ## Design citeria
#|
#| * The arguments should include the target folders and the search location(s).
#| * The output should be a character vector of absolute folder paths in which configuration files might be found. 
#|   The function that parses configuration files (`read_configuration_files`) knows which file types are valid so this function will not attempt to find the config files themselves, only their potential locations.
#|
#| ## The code
#|
#' @title
#' Find locations of configuration files
#' 
#' @description 
#' Find the folder paths where configuration file might exist based off of a target directory and search strategie(s).
#' 
#' @param paths (\code{character})
#' The target paths that the search space will be relative to.
#' 
#' @param search_type (\code{character}) [not path-specific]
#' Where to look for configuration files relative to \code{path}.
#' Only applies to local options and is not affected by \code{config_path}.
#' 
#' Accepts one or more of the following values:
#' 
#' \describe{
#'   \item{parents}{Files in parent folders of \code{path}}
#'   \item{root}{Files in \code{path}}
#'   \item{children}{Files in child folders of \code{path}}
#' }
#' 
#' @return \code{character}
#' The folder paths where configuration files might be.
find_config_folders <- function(paths, search_type = c("parents", "root", "children")) {
  #| ### Argument validation
  #| It should be possible to make all input paths absolute (i.e. they should exist and have valid syntax).
  paths <- normalizePath(paths, mustWork = TRUE)
  #| All paths should be folders, not files. 
  if (!all(file.info(paths)$isdir)) {
    stop("Not all paths supplied are folders.")
  }
  #| The `search_type` should be one or more of those defined,
  search_type <- match.arg(arg = search_type,
                           choices = c("parents", "root", "children"),
                           several.ok = TRUE)
  
  #| ### Define search functions
  #| These will be the functions that handle each search type.
  #| Their output will be combined to when multiple strategies are used.
  #|
  get_parents <- function(path) {
    parts <- strsplit(path, split = .Platform$file.sep)[[1]] # split path into folder names
    output <- vapply(1:(length(parts) - 1),
                     function(i) paste(parts[1:i], collapse = .Platform$file.sep),
                     character(1))
    output[output == ""] <- .Platform$file.sep
    return(output)
  }
  
  get_root <- function(path) {
    return(path)
  }
  
  get_children <- function(path) {
    output <- list.dirs(path)
    output <- output[output != path] # do not return the path itself
    return(output)
  }
  
  search_functions <- list(parents = get_parents,
                           root = get_root,
                           children = get_children)
  
  #| ### Define function to process each input path
  #| To simplify the logic of the overall function, lets make a function to process a single input path and call it once for each path.
  #| This function should call the specified seach functions and concatenate their results.
  process_one <- function(path) {
    unlist(lapply(search_functions[search_type], function(f) f(path)))
  }
  
  #| ### Process each path and return results
  #| Now we just need to call `process_one` on each path
  unname(unlist(lapply(paths, process_one)))
} 

#' @title
#' Find locations of target files
#' 
#' @description 
#' Find the file paths of potential target files using folder paths and one or more search strategies.
#' 
#' @param paths (\code{character})
#' The target paths that the search space will be relative to.
#' 
#' @param search_type (\code{character}) [not path-specific]
#' Where to look for files relative to \code{path}.
#' 
#' Accepts one or more of the following values:
#' 
#' \describe{
#'   \item{parents}{Files in parent folders of \code{path}}
#'   \item{root}{Files in \code{path}}
#'   \item{children}{Files in child folders of \code{path}}
#' }
#' 
#' @return \code{character}
#' The file paths of potential target files.
get_target_paths <- function(paths, search_type = c("parents", "root", "children")) {
  NULL
}
