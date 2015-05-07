#===================================================================================================
#' Copy folders with links
#' 
#' Copies folders like \link{\code{file.copy}} except it replicates links correctly on unix-like 
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
  path$type <- factor("file", levels = c("file", "folder", "link"))
  path$type[file.info(path$from)$isdir] <- "folder"
  path$type[Sys.readlink(path$from) != ""] <- "link"
  # Remove all files that are descendants of links -------------------------------------------------
  is_child <- function(query, refs) {
    sapply(refs, function(x) grepl(paste0("^", x), query) & query != x)
  }
  path <- path[!sapply(path$from, function(x) any(is_child(x, path$from) & path$type == "link")), ]
  # Make copy --------------------------------------------------------------------------------------
  invisible(lapply(path$to[path$type == "folder"], dir.create, recursive = TRUE))
  invisible(file.copy(from = path$from[path$type == "file"], to = path$to[path$type == "file"]))
  invisible(file.symlink(Sys.readlink(path$from[path$type == "link"]), path$to[path$type == "link"]))
}
