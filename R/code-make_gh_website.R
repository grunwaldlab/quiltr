
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
#' @param clear (\code{logical} of length 1) If \code{TRUE}, the contents of the \code{gh-pages}
#' branch is deleted before the new website is copied. 
#' @param push (\code{logical} of length 1) If \code{TRUE}, push the modifications made to
#' the \code{gh-pages} branch to the remote specified by \code{remote}. This makes the changes 
#' appear online.
#' @param branch (\code{character} of length 1) The name of the branch in the repository that the
#' website source exists. This is typically 'master'. 
#' @param remote (\code{character} of length 1) The name of the remote to push any changes to. Also
#' affects the inferred location of the website online. 
#' @param ... all other options are passed to \code{\link{make_website}}.
#' 
#' @export
make_gh_website <- function(reset_branch = TRUE, commit = TRUE, clear = TRUE, push = FALSE,
                            branch = "master", remote = "origin", ...) {
  get_branch <- function() {
    result <- system("git status",  ignore.stderr = TRUE, intern = TRUE)[[1]]
    rev(strsplit(result, split = " ")[[1]])[1]    
  }
  if ("path" %in% names(list(...)))
    path <- list(...)$path
  else
    path <- eval(as.list(args(make_website))$path)
  # Check if in git repository ---------------------------------------------------------------------
  git_path <- get_file_in_parent(path, ".git")
  if (is.null(git_path)) stop("Not currently in a git repository.")
  repository <- dirname(git_path)
  # Change current working directory ---------------------------------------------------------------
  original_wd <- getwd()
  setwd(repository)
  # Switch back to original branch when done -------------------------------------------------------
  original_branch <- get_branch()
  if (reset_branch) {
    on.exit(system(paste("git checkout", original_branch), ignore.stdout = TRUE,
                   ignore.stderr = TRUE), add = TRUE)
  }
  # Make website before stashing if its source is on the same branch -------------------------------
  if (branch == get_branch()) {
    website_path <- do.call(make_website, list(...))    
  }
  # Stash state of current branch ------------------------------------------------------------------
  stash_result <- system("git stash --all", intern = TRUE)
  if (!"No local changes to save" %in% stash_result) {
    on.exit(system("git stash apply", ignore.stdout = TRUE, ignore.stderr = TRUE), add = TRUE)
  }
  # Make website after stashing if its source is on a different branch -----------------------------
  if (branch != get_branch()) {
    system(paste("git checkout", branch), ignore.stdout = TRUE, ignore.stderr = TRUE)
    website_path <- do.call(make_website, list(...))
  }
  # Return to original working directory when done -------------------------------------------------
  on.exit(setwd(original_wd), add = TRUE)
  # Copy website to temporary directory ------------------------------------------------------------
  copy_location <- tempdir()
  file.copy(dirname(website_path), copy_location, overwrite = TRUE, recursive = TRUE)
  copy_path <- file.path(copy_location, basename(dirname(website_path)))
  # Switch to gh-pages branch ----------------------------------------------------------------------
  can_checkout <- system("git checkout gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (can_checkout == 1)
    can_checkout <- system("git checkout -b gh-pages", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (get_branch() != "gh-pages") {
    result <- system("git checkout gh-pages", intern = TRUE)
    message(result)
    stop(paste0("Could not change to gh-pages branch. ",
                "Check that is it possible to change branches."))    
  }
  # Check that local gh-pages is up to date -------------------------------------------------------
  result <- system2(strsplit("git fetch -v --dry-run", split = " ")[[1]],  stdout=TRUE, stderr=TRUE)
  if (any(grepl(" gh-pages   ->", result))
      && !any(grepl("^ = \\[up to date\\]      gh-pages", result))) {
    stop("The local copy of the 'gh-pages' branch is out of date. Pull changes before proceeding.")
  }
  # Delete previous website ------------------------------------------------------------------------
  if (clear) {
    to_delete <- list.files(repository, include.dirs = TRUE, all.files = TRUE, full.names = TRUE)
    to_delete <- to_delete[!grepl("\\.git$", to_delete)]
    to_delete <- to_delete[!grepl(file.path(repository, ".$"), to_delete)]
    to_delete <- to_delete[!grepl(file.path(repository, "..$"), to_delete)]
    to_delete <- to_delete[!grepl(file.path(repository, ".Rproj.user$"), to_delete)]
    unlink(to_delete, recursive = TRUE)
  }
  # Add .gitignore file ----------------------------------------------------------------------------
  cat(".Rproj.user\n.Rhistory\n.RData\n", file = file.path(repository, ".gitignore"))
  # Copy website to repository ---------------------------------------------------------------------
  file.copy(list.files(copy_path, all.files = TRUE, include.dirs = TRUE, full.names = TRUE), 
            repository, overwrite = TRUE, recursive = TRUE)
  # Commit changes ---------------------------------------------------------------------------------
  if (commit) {
    result <- system("git add -A", ignore.stdout = TRUE, ignore.stderr = TRUE)
    result <- system('git commit -a -m "Automated commit by quiltr::make_gh_website"',
                     ignore.stdout = TRUE, ignore.stderr = TRUE)    
  }
  # Infer website URL ------------------------------------------------------------------------------
  remote_result <- system("git remote -v", intern = TRUE)
  remote_result <- remote_result[grepl(paste0("^", remote, ".*\\(push\\)$"), remote_result)]
  remote_result <- stringr::str_match(remote_result, ".*github\\.com:(.*)/(.*)\\.git")[, ]
  remote_url <- paste0(strsplit(remote_result[1], split = "git@")[[1]][2])
  user <- remote_result[2]
  repo_name <- remote_result[3]
  website_url <- paste0("http://", user, ".github.io/", repo_name, "/")
  # Push changes -----------------------------------------------------------------------------------
  if (push) {
    system(paste0("git push ", remote, " gh-pages"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    message(paste0("Website made in the 'gh-pages' branch of '", repository,
                   "' and pushed to '", remote_url, "'.\n",
                   "The website should be available at: '", website_url, "'"))    
  } else {
    message(paste0("Website made in the 'gh-pages' branch of '", repository,
                   "'\nTo make the website available online:\n",
                   "\t1) Set current working directory to '", repository, "'\n",
                   "\t2) Enter the following in a terminal: 'git push ", remote, " gh-pages'\n", 
                   "Once pushed, the website should be available at: '", website_url, "'"))    
  }
  return(website_path)
}
