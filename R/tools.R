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
  copy_path <- function(from, to) {
    if (Sys.readlink(from) != "") { #if it is a link
      file.link(from = to, to = Sys.readlink(from))
    } else if (file.info(from)$isdir) {
      dir.create(to, recursive = TRUE)
    } else {
      file.copy(from = from, to = to)
    }
  }
  from <- normalizePath(from)
  to <- normalizePath(to)
  target <- file.path(to, basename(from))
  if (file.exists(target)) stop(paste0("Directory '", target, "' already exists."))
  from_paths <- list.files(from, full.names = TRUE, recursive = TRUE, )
  to_paths <- file.path(to, basename(from), list.dirs(from, full.names = FALSE, recursive = TRUE))
  invisible(mapply(copy_path, from = from_paths, to = to_paths))
}




copy_folder_with_links <- function(from, to) {
#   from <- normalizePath(from)
#   to <- normalizePath(to)
  target <- file.path(to, basename(from))
  if (Sys.readlink(from) != "") { #if it is a link
    invisible(file.link(from = target, to = Sys.readlink(from)))
  } else if (file.info(from)$isdir) {
    invisible(dir.create(target, recursive = TRUE))
    from_paths <- list.files(from, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
    to_paths <- target
    if (length(from_paths) > 0) {
      invisible(mapply(copy_folder_with_links, from = from_paths, to = to_paths))
    }
  } else {
    invisible(file.copy(from = from, to = target))
  }
}




copy_folder_with_links <- function(from, to) {
  targets <- file.path(to, basename(from))
  dirs <- targets[file.info(from)$isdir]
  links <- targets[Sys.readlink(from) != ""]
  files <- targets[file.info(from)$isdir == FALSE & Sys.readlink(from) == ""]
  
  if(length(dirs) > 0) {
    dir.create(dirs)
  }
  if(length(files) > 0) {
    file.copy(from = from[file.info(from)$isdir], to = files)
  }
  if(length(links) > 0) {
    file.link(from = links, to = Sys.readlink(from)[Sys.readlink(from) != ""])
  }
  invisible(mapply(copy_folder_with_links, from = from_paths, to = to_paths))
  
  
  if (Sys.readlink(from) != "") { #if it is a link
    invisible(file.link(from = target, to = Sys.readlink(from)))
  } else if (file.info(from)$isdir) {
    invisible(dir.create(target, recursive = TRUE))
    from_paths <- list.files(from, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
    to_paths <- target
    if (length(from_paths) > 0) {
      invisible(mapply(copy_folder_with_links, from = from_paths, to = to_paths))
    }
  } else {
    invisible(file.copy(from = from, to = target))
  }
}