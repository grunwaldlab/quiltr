#===================================================================================================
#' Get paths to configuration files 
#' 
#' @param path (\code{character} of length 1) The path along which configuration files are searched.
#' @param name (\code{character} of length 1) The name of configuration files. 
#' @param root (\code{character} of length 1) Where along \code{path} to start seaching. Must be a 
#' parent directory of \code{path}.
#' @param must_exist (\code{logical} of length 1) If \code{FALSE}, return the paths where
#' configuration could be even if they dont exist.
#' 
#' @return \code{character} of absolute paths.
get_config_paths <- function(path, name, root, must_exist = TRUE) {
  # Validate input ---------------------------------------------------------------------------------
  path <- normalizePath(path)
  root <- normalizePath(root)
  if (!grepl(pattern = paste0("^", root), path)) {
    stop("'root' is not a parent directory of 'path'.")
  }
    # Get directories in search path -----------------------------------------------------------------
  if (root == dirname(path)) {
    split_rel_path <- ""
  } else {
    if (root == .Platform$file.sep) { root = "" }
    rel_path <- gsub(paste0("^", root), "", dirname(path))
    split_rel_path <- strsplit(rel_path, .Platform$file.sep)[[1]]
  }
  config_locations <- lapply(1:length(split_rel_path), function(i) split_rel_path[1:i])
  config_locations <- sapply(config_locations, function(x) do.call(file.path, as.list(x)))
  config_locations <- gsub(paste0("^", .Platform$file.sep), "", config_locations)
  search_path <- vapply(config_locations, FUN.VALUE = character(1),
                        function(x) ifelse(x == "", file.path(root, name), file.path(root, x, name)))
  # Check for configuration files in search path ---------------------------------------------------
  if (must_exist) { search_path <- search_path[file.exists(search_path)] }
  unname(search_path)
}


#===================================================================================================
#' Get value of option in config file
#' 
#' Get the value of a specific option in a specific configuration file.
#' 
#' @param path (\code{character} of length 1)
#' @param option (\code{character} of length 1)
#' 
#' @return depends on content of config file. Returns \code{NA} if the option cannot be found.
get_config_value <- function(path, option) {
  content <- yaml::yaml.load_file(input = path)
  if (option %in% names(content)) {
    return(unlist(content[[option]], recursive = FALSE))
  } else {
    return(NA)
  }
}



#===================================================================================================
#' Wildcard expansion
#' 
#' Like \code{\link{Sys.glob}}, but also understands double wildcards for recursive matching using
#' double wildcards (\code{**})
#' 
#' @param path (\code{character} of length 1) The path to expand.
#' @param max_search_depth(\code{integer} of length 1) How deep to search
#' 
#' @return \code{character}
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


#===================================================================================================
#' Get option value for a given context
#' 
#' Get option value for a given context
#' 
#' @param path (\code{character} of length 1)
#' @param option (\code{character} of length 1)
#' @param default The default value for the option
#' @param root (\code{character} of length 1)
#' @param config_name (\code{character} of length 1)
#' @param inherit (\code{logical} of length 1) If \code{FALSE}
#' 
get_option <- function(path, option, default, root, config_name, inherit = TRUE) {
  # Validate inputs --------------------------------------------------------------------------------
  path = normalizePath(path)
  root = normalizePath(root)
  # Get relevant configuration file paths ----------------------------------------------------------
  config_paths <- get_config_paths(path = path, name = config_name, root = root, must_exist = TRUE)
  output_value <- default
  # Look for options that apply to the path given in each configuration file -----------------------
  for (config_path in config_paths) {
    value <- get_config_value(path = config_path, option = option)
    if (length(value) > 1 || !is.na(value)) { # If the option is found in the config file...
      patterns = names(value)
      if (is.null(patterns)) { # If patterns are not specified...
        patterns = ifelse(inherit, "**", "*")
        value <- list(value)
      }
      for (index in seq_along(value)) {
        if (path %in% sys_glob(file.path(dirname(config_path), patterns[index]))) {
          output_value <- value[index]
        }
      }      
    }
  }
  return(output_value)
}