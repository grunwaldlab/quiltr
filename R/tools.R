#===================================================================================================
#' Copy folders with links
#' 
#' Copies folders like \code{\link{file.copy}} except it replicates links correctly on unix-like 
#' systems.
#' 
#' @param from (\code{character}) The path to the folder to be copied
#' @param to (\code{character}) Where to copy the folder to. 
#' 
copy_folder_with_links <- function(from, to) {
  target <- file.path(to, basename(from))
  if (file.exists(target)) stop(paste0("Target folder ", target, " already exists."))
  # Get list of all files/folders to copy ----------------------------------------------------------
  path <- data.frame(target = list.files(from, recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
  path$from  <- file.path(from, path$target)
  path$to  <- file.path(to, basename(from), path$target)
  # Get type of file/folders -----------------------------------------------------------------------
  path$type <- factor(rep("file", nrow(path)), levels = c("file", "folder", "link"))
  path$type[file.info(path$from)$isdir] <- "folder"
  path$type[Sys.readlink(path$from) != ""] <- "link"
  # Remove all files that are descendants of links -------------------------------------------------
  is_child <- function(query, refs) {
    sapply(refs, function(x) grepl(paste0("^", x), query) & query != x)
  }
  path <- path[!sapply(path$from, function(x) any(is_child(x, path$from) & path$type == "link")), ]
  # Make copy --------------------------------------------------------------------------------------
  invisible(dir.create(target, recursive = TRUE))
  invisible(lapply(path$to[path$type == "folder"], dir.create, recursive = TRUE))
  if (sum(path$type == "file") > 0) invisible(file.copy(from = path$from[path$type == "file"],
                                                        to = path$to[path$type == "file"]))
  if (sum(path$type == "link") > 0) invisible(file.symlink(from = Sys.readlink(path$from[path$type == "link"]),
                                                           to = path$to[path$type == "link"]))
}



#===================================================================================================
#' Extract YAML attribute
#' 
#' Gets a given attribute, based on the key, from a YAML file. 
#' 
#' @param path (\code{character}) The path to a YAML file.
#' @param attribute (\code{character} of length 1) The key of the attribute to get.
#' @param default (\code{character} of length 1) the default to return of the key is not found.
get_rmd_yaml <- function(path, attribute, default = "") {
  do_once <- function(a_path) {
    content <- readChar(a_path, nchars = 10000)
    parsed_yaml <- yaml::yaml.load(stringr::str_match(content, "---\\\n(.*)---\\\n")[2])
    if (attribute %in% names(parsed_yaml)) return(parsed_yaml[[attribute]])
    return(as.character(default))
  }
  vapply(path, do_once, character(1)) 
}



#===================================================================================================
#' Get a directory common to all paths
#' 
#' Get a directory common to all paths
#' 
#' @param paths (\code{character}) Input paths
#' @param delim  (\code{character} of length 1) file path delimiter
#' 
#` form http://rosettacode.org/wiki/Find_common_directory_path
get_common_dir <- function(paths, delim = .Platform$file.sep)
{
  if (length(paths) < 2) return(paths)
  path_chunks <- strsplit(paths, delim)
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)
}


#===================================================================================================
#' Find files in parent directories
#'
#' Find a file in the current or a parent directory.
#'
#' @param path (\code{character}) One or more directories in which to start looking. If multiple 
#' directories are given then the search is started in their common parent directory.
#' @param query (\code{character}) Name of the file to find.
#' 
get_file_in_parent <- function(path, query) {
  if (length(path) > 1) path <- get_common_dir(path)
  path <- normalizePath(path)
  if (!file.info(path)$isdir) stop("Path supplied is not a directory.")
  path_part <- strsplit(path, .Platform$file.sep)[[1]]
  while (length(path_part) > 0) {
    search <- do.call(file.path, as.list(c(path_part, query)))
    if (file.exists(search)) return(search)
    path_part  <- path_part[-length(path_part)]
  }
  return(NULL)
}


#===================================================================================================
#' Get functions in package
#' 
#' Get functions in package that match a regular expression. 
#' Private functions are returned as well.
#' 
#' Inspired by:
#' http://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package
#' 
#' @param  package (\code{character} of length 1) The name of the package to search for functions
#' in. 
#' @param  pattern (\code{character} of length 1) A regular expression that functions' name must 
#' match. 
#' 
#' @return \code{list} of \code{function}
get_function <- function(package, pattern) {
  all_funcs <- unclass(lsf.str(envir = asNamespace(package), all = T))
  all_funcs[grep(pattern, all_funcs)]
}


#===================================================================================================
#' Check if pandoc exists
#' 
#' Try to run \code{pandoc -v} and see if an error is encountered. 
#' 
#' @return \code{logical} of length 1
pandoc_is_available <- function() {
  result <- system("pandoc -v", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (result == 0) {return(TRUE)} else {return(FALSE)}
}
