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
#' Get option value for a given context
#' 
#' Get option value for a given context
#' 
#' @param  context
#' @param  option
#' 
get_option <- function(context, option, root_context = NULL) {
  
  
  get_paths <- function(path, root = NULL) {
    path <- normalizePath(path)
    split_path <- strsplit(path, split = .Platform$file.sep)[[1]]
    if (!is.null(root)) {
      root <- normalizePath(root)
      split_root <- strsplit(root, split = .Platform$file.sep)[[1]]
      lapply(1:min(c(length(split_path), length(split_root))),
             function(i) split_path[i] == split_root[i])
    }
  }
}