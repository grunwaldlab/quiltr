#===================================================================================================
#' Quilt and rsync result to remote server
#' 
#' Calls \code{\link{quilt}} and copies the output to a remote server using 
#' \href{https://rsync.samba.org/}{rsync}.
#' Ssh keys must be set up between the local and remote computers for this to work.
#' 
#' @param url (\code{character} of length 1) The url of the remote computer.
#' @param user (\code{character} of length 1) The user name to use on the remote computer.
#' @param path (\code{character} of length 1) The file path on the remote computer where the 
#' website will be copied.
#' @param  port (\code{integer} of length 1) The ssh port for the remote computer.
#' @param just_contents \code{logical} of length 1) If \code{TRUE}, all the contents of the website
#' directory, instead of the directory itself, are copied to the file path specified by
#' \code{remote}
#' @param ... all other options are passed to \code{\link{quilt}}.
#' 
#' @return (\code{character} of length 1) The path to the local copy of the website.
#' 
#' @export
quilt_to_remote <- function(url, user, path, port = 22, just_contents = FALSE, ...) {
  # Execute quilt with extra parameters ------------------------------------------------------------
  local_path <- do.call(quilt, list(...))
  # Use rsync to copy to remote --------------------------------------------------------------------
  rsync_command <- paste("rsync", "-a",
                         "--rsh", paste0('"ssh -p ', port, '"'),
                         paste0(local_path, ifelse(just_contents, .Platform$file.sep, "")),
                         paste0(user, "@", url, ":", path))
  system(rsync_command)
  return(local_path)
}
