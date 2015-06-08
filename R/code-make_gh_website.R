
#===================================================================================================
#' Make a github pages website 
#' 
#' WARNING: If you already have a website on a \code{gh-pages} branch then this function can 
#' delete it when \code{clean = TRUE} (the defualt), so use with care.
#' Works like \code{\link{make_website}}, except it places the output in a \code{gh-pages} branch
#' of the current git repository so that it is available online at
#' \code{http://user_name.github.io/repository_name/}.
#' 
#' @param reset_branch (\code{logical} of length 1) If \code{TRUE}, change back to the current git branch
#' after making the webpage.
#' @param commit (\code{logical} of length 1) If \code{TRUE}, commit all changes made to the website
#' on the \code{gh-pages} branch.
#' @param push (\code{character} of length 1) If supplied, the \code{gh-pages} branch is pushed 
#' to the specified remote. Usually this is \code{push = "origin"}.
#' @param clear (\code{logical} of length 1) If \code{TRUE}, the contents of the \code{gh-pages}
#' branch is deleted before the new website is copied. 
#' @param ... all other options are passed to \code{\link{make_website}}.
#' 
#' @export
make_gh_website <- function(reset_branch = TRUE, commit = TRUE, push = NULL, 
                            clear = TRUE, ...) {
  if ("path" %in% names(list(...)))
    path <- list(...)$path
  else
    path <- eval(as.list(args(make_website))$path)
  # Check if in git repository ---------------------------------------------------------------------
  git_path <- get_file_in_parent(path, ".git")
  if (is.null(git_path)) stop("Not currently in a git repository.")
  repository <- dirname(git_path)
  # Make website -----------------------------------------------------------------------------------
  website_path <- make_website(...)
  # Copy website to temporary directory ------------------------------------------------------------
  copy_location <- tempdir()
  file.copy(dirname(website_path), copy_location, overwrite = TRUE, recursive = TRUE)
  copy_path <- file.path(copy_location, basename(dirname(website_path)))
  # Switch to gh-pages branch ----------------------------------------------------------------------
  result <- system("git status",  ignore.stderr = TRUE, intern = TRUE)[[1]]
  original_branch <- rev(strsplit(result, split = " ")[[1]])[1]
  can_checkout <- system("git checkout gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (can_checkout == 1)
    can_checkout <- system("git checkout -b gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  # Delete previous website ------------------------------------------------------------------------
  if (clear) {
    to_delete <- list.files(repository, include.dirs = TRUE, all.files = TRUE, full.names = TRUE,
                            recursive = TRUE)
    to_delete <- to_delete[!grepl("\\.git", to_delete)]
    unlink(to_delete, recursive = TRUE)
  }
  # Copy website to repository ---------------------------------------------------------------------
  file.copy(list.files(copy_path, all.files = TRUE, include.dirs = TRUE, full.names = TRUE), 
            repository, overwrite = TRUE, recursive = TRUE)
  # Commit changes ---------------------------------------------------------------------------------
  if (commit) {
    result <- system("git add -A", ignore.stdout = TRUE, ignore.stderr = TRUE)
    result <- system('git commit -a -m "Automated commit by quiltr::make_gh_website"',
                     ignore.stdout = TRUE, ignore.stderr = TRUE)    
  }
  # Push changes to remote -------------------------------------------------------------------------
  if (!is.null(push)) {
    result <- system(paste("git push", push, "gh-pages"), ignore.stdout = TRUE,
                     ignore.stderr = TRUE)
  }
  # Switch back to original branch -----------------------------------------------------------------
  if (reset_branch) {
    result <- system(paste("git checkout", original_branch), ignore.stdout = TRUE,
                     ignore.stderr = TRUE)
  }
  return(website_path)
}
