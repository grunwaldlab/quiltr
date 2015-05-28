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




#` http://rosettacode.org/wiki/Find_common_directory_path
get_common_dir <- function(paths, delim = .Platform$file.sep)
{
  path_chunks <- strsplit(paths, delim)
  
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)
  
}
