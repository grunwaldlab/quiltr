#===================================================================================================
#' Initialize notebook info
#' 
#' Initialize the info yaml file.
#'  
#'  @param notebook_path The path to the notebook root directory. 
#'  
init_info_yaml <- function(notebook_path) {
  info_path <- file.path(notebook_path, "info.yaml")
  data <- yaml::yaml.load_file(info_path)
  data$labtools_version_used <- as.character(packageVersion("labtools"))
  cat(yaml::as.yaml(data), file = info_path, append = FALSE)
}




#===================================================================================================
#' Initialize a lab notebook
#' 
#' Make the directory structure and code for a lab notebook. 
#' 
#' @param location Path to new notebook location.
#' @param name Name of new notebook folder. Format as you would a file name.
#' @param use_git If \code{TRUE}, use git with the new notebook.
#' @param use_packrat If \code{TRUE}, use packrat with the new notebook. 
#' @param add_timestamp If \code{TRUE}, the current date is added to the notebook name.
#' @param open If \code{TRUE}, the new notebook is opened in RStudio after creation (Only
#'  implemented in linux so far).
#' 
#' @export

new_notebook <- function(location, name = "notebook", use_git = TRUE, use_packrat = FALSE,
                         add_timestamp = TRUE, open = TRUE) {
  location <- normalizePath(location)
  # Copy template to destination -------------------------------------------------------------------
  template_name <- "notebook_template"
  timestamp <- format(Sys.time(), format="%Y_%m_%d")
  if (add_timestamp) name <- paste(timestamp, name, sep = "-")
  notebook_path <- file.path(location, name)
  template_path <- system.file(template_name, package = "labtools")
  if (file.exists(notebook_path)) stop("Notebook with that name already exists. Delete the existing notebook or choose a new name.")
  if (!file.exists(location)) dir.create(location, recursive = TRUE)
  file.copy(from = template_path, to = location, recursive = TRUE)
  file.rename(from = file.path(location, template_name), to = notebook_path) #rename root folder
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(notebook_path)
  notebook_proj_name <- paste0(name, ".Rproj")
  notebook_proj_path <- file.path(notebook_path, notebook_proj_name)
  file.rename(from = "notebook.Rproj", to = notebook_proj_name)
  # Initialize info.yaml ---------------------------------------------------------------------------
  init_info_yaml(notebook_path)
  # Initialize git repository ----------------------------------------------------------------------
  if (use_git) {
    system("git init")    
  }
  # Add templates ----------------------------------------------------------------------------------
  template_generators <- c(make_notebook_template_default)
  template_path <- file.path(notebook_path, "templates")
  invisible(lapply(template_generators, function(f) f(location = template_path)))
  # Initialize packrat -----------------------------------------------------------------------------
  if (use_packrat) {
    file.remove(file.path(notebook_path, "src", "build_all.R")) # temporarily hide build_all.R since packrat cant find labtools 
    packrat::init(notebook_path, restart = FALSE)
    install.packages("devtools")#, lib = file.path(notebook_path, "packrat", "lib"))
    devtools::install_github("zachary-foster/labtools")
    packrat::snapshot(project = notebook_path)
    file.copy(from = file.path(template_path, "src", "build_all.R"), 
              to = file.path(notebook_path, "src", "build_all.R"))
    getOption("restart")()
  }
  add_labtools_import_to_rprofile(".Rprofile")
  # Open the new note in RStudio -------------------------------------------------------------------
  if (open) {
    if (Sys.info()['sysname'] == "Linux") {
      command <- paste0("(rstudio ", notebook_proj_path, " &)")
      system(command)    
    }
  }
}


#===================================================================================================
#' Find README files
#' 
#' Locate all README files in the project and return their paths
#' 
#' @param path (\code{character} of length 1) Path somwhere under a R project in which to look for
#'   README files.
find_readmes <- function(path = get_project_root()) {
  path <- get_project_root(normalizePath(path))
  all_paths <- list.files(path, recursive = TRUE, )
  readme_paths <- all_paths[basename(all_paths) == "README.yaml"]
  file.path(path, readme_paths)
}


#===================================================================================================
#' Validate READMEs 
#' 
#' Check if a readme has an entry for all files/folders in its folder. 
#' 
#' NOT FINISHED
validate_readme <- function(readme_paths, missing = "warn", add = TRUE, order = TRUE) { 
  do_once <- function(readme_path) {
    path <- normalizePath(path)
    content_paths <- list.files(dirname(path))
    content_paths <- content_paths[content_paths != "README.yaml"]
    
    readme_content <- yaml::yaml.load_file(path)
  }
}


#===================================================================================================
#' Get list of files ignored by git
#' 
#' Returns the list of all files ignored by git from anywhere within a git repository. 
#' 
#' @param path A a git repository or one of its subdirectories.
#'
get_git_ignored <- function(path = get_project_root()) {
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
#' @param remote The address of the remote computer. 
#' @param port The port of ssh agent on the remote computer. 
#' @param local A a git repository or one of its subdirectories on the local computer.
#' 
#' @export
rsync_push <- function(remote, user, host, port = 22, local = get_project_root()) {
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
rsync_pull <- function(remote, user, host, port = 22, local = get_project_root()) {
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


#===================================================================================================
#' Add `library(labtools)` to .Rprofile
#' 
#' Adds \code{library(labtools)} to .Rprofile to that new notebooks automatically load
#' \code{labtools} when opened. 
#' 
#' @param profile_path The path to the .Rprofile ro modify.
add_labtools_import_to_rprofile <- function(profile_path) {
  data <- paste0("\n##### Added by labtools (v ", packageVersion("labtools"), ") #####\n",
                 "library(labtools)\n",
                 "#####")
  write(data, profile_path, append = TRUE)
}


#===================================================================================================
#' Makes a new note
#' 
#' Make a new note for the current time and notebook.
#' 
#' @param ... (\code{character}) One or more name corresponding to an organizational hierarchy.
#' @param template (\code{character}) The name of the template to use for the new note. Available
#' templates can be found in the \code{templates} folder in the root directory of the notebook. You
#' can add your own templates there and use their name in this function.
#' @param date (\code{character}) Not date in the form `yyyy_mm_dd`. 
#' @param notebook (\code{character}) The path to the notebook in which to write the note or one
#'   of its subdirectories.
#' @param change_wd (\code{logical}) If \code{TRUE}, change the current working directory to the new note.
#' @param open If \code{TRUE}, the new note is opened in RStudio after creation.
#'  
#' @export
new_note <- function(..., template = "default", date = NULL, notebook = get_project_root(),
                     change_wd = TRUE, open = TRUE) {
  # Parse arguments --------------------------------------------------------------------------------
  if (is.null(date)) date <- format(Sys.time(), format="%Y_%m_%d")
  names <- unlist(list(...))
  note_name <- paste(c(date, names), collapse = "-")
  note_location <- file.path(notebook, "content")
  note_path <- file.path(note_location, note_name)
  template_path <- file.path(notebook, "templates", template)
  # Create note directory --------------------------------------------------------------------------
  if (file.exists(note_path)) stop(paste0("Note already exists at path '", note_path, "'."))
  if (!file.exists(template_path)) stop(paste0("Cannot find template at '", template_path,
                                               "'. Check that it exists or pick another template."))
  copy_folder_with_links(from = template_path, to = note_location)
  file.rename(from = file.path(note_location, template), to = note_path)
  # Change current working directory to new note ---------------------------------------------------
  if (change_wd) setwd(note_path)
  # Open note after creation -----------------------------------------------------------------------
  if (open && rstudioapi::isAvailable()) {
    rmd_files <- list.files(note_path, pattern = "\\.Rmd$", ignore.case = TRUE, full.names = TRUE)
    if (length(rmd_files) > 0) file.edit(rmd_files)
  }
}



#===================================================================================================
#' Make default note template directory
#' 
#' @param location (\code{character} of length 1) Where to write the folder. 
make_notebook_template_default <- function(location) {
  location <- file.path(location, "default")
  if(file.exists(location)) stop(paste0("Target folder ", location, " already exists."))
  dir.create(location, recursive = TRUE)
  file.symlink(file.path("..", "..", "data"), file.path(location, "_data"))
  file.symlink(file.path("..", "..", "bin"), file.path(location, "_bin"))
  file.symlink(file.path("..", "..", "src"), file.path(location,  "_src"))
  file.symlink(file.path("..", "..", "references"), file.path(location, "_references"))
  dir.create( file.path(location, "scratch"), recursive = TRUE)
  default_gitignore = "scratch\n*.html\n"
  write(default_gitignore,  file.path(location, ".gitignore"))
  default_note = paste0('---\ntitle: "Untitled"\ndate: "', format(Sys.time(), format="%Y-%m-%d"),
                        '"\noutput: html_document\n---')
  write(default_note,  file.path(location, "notes.Rmd"))
}
