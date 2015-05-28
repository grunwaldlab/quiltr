#===================================================================================================
#' Get list of files ignored by git
#' 
#' Returns the list of all files ignored by git from anywhere within a git repository. 
#' 
#' @param path A a git repository or one of its subdirectories.
#'
get_git_ignored <- function(path) {
  # Move into git repository -----------------------------------------------------------------------
  original_wd <- getwd()
  setwd(path)
  on.exit(setwd(original_wd))
  # Use git to output ignored files ----------------------------------------------------------------
  git_output <- system("git clean -ndX", intern = TRUE)
  gsub("Would remove ", "", git_output)   
}


#===================================================================================================
#' Rsync files ignored by git
#' 
#' Copys files ignored by git to a remote computer using rsync. 
#' Both computers must have rsync installed. 
#' 
#' @param remote The path to the analogous verison of the git repository on the remote computer. 
#' @param user The user of the remote computer.
#' @param host The address of the remote computer. 
#' @param port The port of ssh agent on the remote computer. 
#' @param local A a git repository or one of its subdirectories on the local computer.
#' 
#' @export
rsync_push <- function(remote, user, host, port = 22, local) {
  # Move into git repository -----------------------------------------------------------------------
  original_wd <- getwd()
  setwd(local)
  on.exit(setwd(original_wd))
  # Rsync once for each ignored path ---------------------------------------------------------------
  to_push <- paste0("'", get_git_ignored(path = local), "'")
  command <- paste0("rsync -avh -e 'ssh -p ", port, "' --relative ",
                    paste(to_push, collapse = " "), " ", user, "@", host, ":", remote)
  system(command)
}


#===================================================================================================
#' Get list of remote files ignored by git
#' 
#' Returns the list of all files ignored by git in a remote git repository. 
#' 
#' @param path The path to a git repository on a remote file system.
#' @param user The user of the remote computer.
#' @param host The address of the remote computer. 
#' @param port The port of ssh agent on the remote computer. 
#'
get_remote_git_ignored <- function(path, user, host, port = 22) {
  command <- paste0("ssh ", user, "@", host, " -p ", port, " 'cd ", path, "; git clean -ndX'")
  git_output <- system(command, intern = TRUE)[-1]
  gsub("Would remove ", "", git_output)
}


#===================================================================================================
#' Rsync remote files ignored by git
#' 
#' Copys files ignored by git from a remote computer using rsync. 
#' Both computers must have rsync and git installed. 
#' 
#' Will not transfer empty folders.
#' 
#' @param remote The path to the analogous verison of the git repository on the remote computer. 
#' @param user The user of the remote computer.
#' @param host The address of the remote computer. 
#' @param port The port of ssh agent on the remote computer. 
#' @param local A a git repository or one of its subdirectories on the local computer.
#' 
#' @export
rsync_pull <- function(remote, user, host, port = 22, local) {
  # Get files to transfer --------------------------------------------------------------------------
  files_to_pull <- get_remote_git_ignored(path = remote, user = user, host = host, port = port)
  files_to_pull <- paste0(.Platform$file.sep, files_to_pull)
  is_dir <- grepl(paste0(.Platform$file.sep, "$"), files_to_pull)
  files_to_pull[is_dir] <- paste0(files_to_pull[is_dir], "***")
  files_to_pull <- c(files_to_pull, "*/")
  # Add trailing slash to directory names ----------------------------------------------------------
  if (!grepl(paste0(.Platform$file.sep, "$"), local)) local <- paste0(local, .Platform$file.sep)
  if (!grepl(paste0(.Platform$file.sep, "$"), remote)) remote <- paste0(remote, .Platform$file.sep)
  # Construct rsync command line -------------------------------------------------------------------
  remote <- paste0(user, "@", host,  ":", remote)
  includes <- paste(paste0("--include '", files_to_pull, "'"), collapse = " ")
  ssh <- paste0("-e 'ssh -p ", port, "'")
  excludes <-  "--exclude '*'"
  command <- paste("rsync -avh  --prune-empty-dirs", ssh, includes, excludes, remote, local)
  # Execute rsync command --------------------------------------------------------------------------
  system(command)
}