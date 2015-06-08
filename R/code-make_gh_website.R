
#===================================================================================================
#' Make a github pages website 
#' 
#' WARNING: If you already have a website on a \code{gh-pages} branch then this function can 
#' delete it when \code{clean = TRUE} (the defualt), so use with care.
#' NOTE: Make sure to pull any changes to the \code{gh-pages} branch before running.
#' Works like \code{\link{make_website}}, except it places the output in a \code{gh-pages} branch
#' of the current git repository so that it is available online at
#' \code{http://user_name.github.io/repository_name/}.
#' 
#' @param reset_branch (\code{logical} of length 1) If \code{TRUE}, change back to the current git branch
#' after making the webpage.
#' @param commit (\code{logical} of length 1) If \code{TRUE}, commit all changes made to the website
#' on the \code{gh-pages} branch.
#' @param clear (\code{logical} of length 1) If \code{TRUE}, the contents of the \code{gh-pages}
#' branch is deleted before the new website is copied. 
#' @param ... all other options are passed to \code{\link{make_website}}.
#' 
#' @export
make_gh_website <- function(reset_branch = TRUE, commit = TRUE, clear = TRUE, ...) {
  if ("path" %in% names(list(...)))
    path <- list(...)$path
  else
    path <- eval(as.list(args(make_website))$path)
  # Check if in git repository ---------------------------------------------------------------------
  git_path <- get_file_in_parent(path, ".git")
  if (is.null(git_path)) stop("Not currently in a git repository.")
  repository <- dirname(git_path)
  # Make website -----------------------------------------------------------------------------------
  website_path <- do.call(make_website, list(...))
  # Copy website to temporary directory ------------------------------------------------------------
  copy_location <- tempdir()
  file.copy(dirname(website_path), copy_location, overwrite = TRUE, recursive = TRUE)
  copy_path <- file.path(copy_location, basename(dirname(website_path)))
  # Change current working directory ---------------------------------------------------------------
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(repository)
  # Switch to gh-pages branch ----------------------------------------------------------------------
  get_branch <- function() {
    result <- system("git status",  ignore.stderr = TRUE, intern = TRUE)[[1]]
    rev(strsplit(result, split = " ")[[1]])[1]    
  }
  original_branch <- get_branch()
  can_checkout <- system("git checkout gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (can_checkout == 1)
    can_checkout <- system("git checkout -b gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (get_branch() != "gh-pages") {
    result <- system("git checkout gh-pages", intern = TRUE)
    message(result)
    stop(paste0("Could not change to gh-pages branch. ",
                "Check that is it possible to change branches."))    
  }
  # Delete previous website ------------------------------------------------------------------------
  if (clear) {
    to_delete <- list.files(repository, include.dirs = TRUE, all.files = TRUE, full.names = TRUE)
    to_delete <- to_delete[!grepl("\\.git", to_delete)]
    to_delete <- to_delete[!grepl(file.path(repository, "."), to_delete, fixed = TRUE)]
    to_delete <- to_delete[!grepl(file.path(repository, ".."), to_delete, fixed = TRUE)]
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
  # Switch back to original branch -----------------------------------------------------------------
  if (reset_branch) {
    result <- system(paste("git checkout", original_branch), ignore.stdout = TRUE,
                     ignore.stderr = TRUE)
  }
  message(paste0("Website made in the 'gh-pages' branch of '", repository,
                 "'\nTo make the website available online:\n",
                 "\t1) Move current working directory to '", repository, "'\n",
                 "\t2) Type 'git push origin gh-pages'"))
  return(website_path)
}
