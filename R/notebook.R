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
#' 
#' @export
new_notebook <- function(location, name = "notebook", use_git = TRUE, use_packrat = TRUE,
                         add_timestamp = TRUE) {
  # Copy template to destination -------------------------------------------------------------------
  template_name <- "notebook_template"
  timestamp <- format(Sys.time(), format="%Y_%m_%d")
  if (add_timestamp) name <- paste(timestamp, name, sep = "-")
  notebook_path <- file.path(location, name)
  template_path <- system.file(template_name, package = "labtools")
  if (file.exists(notebook_path)) stop("Notebook with that name already exists. Delete the existing notebook or choose a new name.")
  if (!file.exists(location)) dir.create(location, recursive = TRUE)
  file.copy(from = template_path, to = location, overwrite = FALSE, recursive = TRUE)
  file.rename(from = file.path(location, template_name), to = notebook_path) #rename root folder
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(notebook_path)
  file.rename(from = "notebook.Rproj", to = paste0(name, ".Rproj"))
  # Initialize info.yaml ---------------------------------------------------------------------------
  init_info_yaml(notebook_path)
  # Initialize git repository ----------------------------------------------------------------------
  if (use_git) {
    system("git init")    
  }
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
#' @param date (\code{character}) Not date in the form `yyyy_mm_dd`. 
#' @param notebook (\code{character}) The path to the notebook in which to write the note or one
#'   of its subdirectories.
#' @param data_folder (\code{logical}) If \code{TRUE}, thea folder with the same name is made in
#' `notebook_path/data` and linked to this new note via `_data`.
#' 
#' @export
new_note <- function(..., date = NULL, notebook = get_project_root(), data_folder = FALSE) {
  if (is.null(date)) date <- format(Sys.time(), format="%Y_%m_%d")
  names <- unlist(list(...))
  note_name <- paste(c(date, names), collapse = "-")
  note_path <- file.path(notebook, "content", note_name)
  if (file.exists(note_path)) stop(paste0("Note already exists at path '", note_path, "'."))
  dir.create(note_path, recursive = TRUE)
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(note_path)
  if (data_folder) {
    data_path <- file.path(notebook, "data", note_name)
    setwd(original_wd)
    dir.create(data_path, recursive = TRUE)
    setwd(note_path)
    file.symlink(file.path("..", "..", "data", note_name), "_data")
  } else {
    file.symlink(file.path("..", "..", "data"), "_data")
  }
  file.symlink(file.path("..", "..", "bin"), "_bin")
  file.symlink(file.path("..", "..", "src"), "_src")
  file.symlink(file.path("..", "..", "references"), "_references")
}

                 