# #===================================================================================================
# #' Get configuration options 
# #' 
# #' @param option (\code{character}) The name of the option to query
# #' @param config_path (\code{character}) The path to the configuration file
# #' 
# #' @return (\code{character})
# get_config <- function(option = NULL, config_path = file.path(get_project_root(), "config.yaml")) {
#   data <- yaml::yaml.load_file(config_path)
#   if (!is.null(option)) data <- data[option]
#   return(unlist(data))
# }
# 
# 
# #===================================================================================================
# #' Initialize notebook info
# #' 
# #' Initialize the info yaml file.
# #'  
# #'  @param notebook_path The path to the notebook root directory. 
# #'  
# init_info_yaml <- function(notebook_path) {
#   info_path <- file.path(notebook_path, "info.yaml")
#   data <- yaml::yaml.load_file(info_path)
#   data$labtools_version_used <- as.character(packageVersion("labtools"))
#   cat(yaml::as.yaml(data), file = info_path, append = FALSE)
# }
# 
# 
# #===================================================================================================
# #' Initialize a lab notebook
# #' 
# #' Make the directory structure and code for a lab notebook. 
# #' 
# #' @param name Name of new notebook folder. Format as you would a file name.
# #' @param location Path to new notebook location. This is the directory that the new notebook 
# #'   directory will be made in.
# #' @param use_git If \code{TRUE}, use git with the new notebook.
# #' @param use_packrat If \code{TRUE}, use packrat with the new notebook. 
# #' @param add_timestamp If \code{TRUE}, the current date is added to the notebook name.
# #' @param open If \code{TRUE}, the new notebook is opened in RStudio after creation (Only
# #'  implemented in linux so far).
# new_notebook <- function(name = "notebook", location = getwd(), use_git = TRUE, use_packrat = FALSE,
#                          add_timestamp = TRUE, open = TRUE) {
#   # Parse arguments --------------------------------------------------------------------------------
#   name <- gsub(" ", "_", name)
#   location <- normalizePath(location)
#   # Copy template to destination -------------------------------------------------------------------
#   template_name <- "notebook_template"
#   timestamp <- format(Sys.time(), format="%Y_%m_%d")
#   if (add_timestamp) name <- paste(timestamp, name, sep = "-")
#   notebook_path <- file.path(location, name)
#   template_path <- system.file(template_name, package = "labtools")
#   if (file.exists(notebook_path)) stop("Notebook with that name already exists. Delete the existing notebook or choose a new name.")
#   if (!file.exists(location)) dir.create(location, recursive = TRUE)
#   file.copy(from = template_path, to = location, recursive = TRUE)
#   file.rename(from = file.path(location, template_name), to = notebook_path) #rename root folder
#   original_wd <- getwd()
#   on.exit(setwd(original_wd))
#   setwd(notebook_path)
#   notebook_proj_name <- paste0(name, ".Rproj")
#   notebook_proj_path <- file.path(notebook_path, notebook_proj_name)
#   file.rename(from = "notebook.Rproj", to = notebook_proj_name)
#   # Initialize info.yaml ---------------------------------------------------------------------------
#   init_info_yaml(notebook_path)
#   # Initialize git repository ----------------------------------------------------------------------
#   if (use_git) {
#     system("git init")    
#   }
#   # Add templates ----------------------------------------------------------------------------------
#   template_generators <- c(make_notebook_template_default,
#                            make_notebook_template_minimal,
#                            make_notebook_template_empty)
#   template_path <- file.path(notebook_path, "templates")
#   invisible(lapply(template_generators, function(f) f(location = template_path)))
#   # Initialize packrat -----------------------------------------------------------------------------
#   if (use_packrat) {
#     file.remove(file.path(notebook_path, "src", "build_all.R")) # temporarily hide build_all.R since packrat cant find labtools 
#     packrat::init(notebook_path, restart = FALSE)
#     install.packages("devtools")#, lib = file.path(notebook_path, "packrat", "lib"))
#     devtools::install_github("zachary-foster/labtools")
#     packrat::snapshot(project = notebook_path)
#     file.copy(from = file.path(template_path, "src", "build_all.R"), 
#               to = file.path(notebook_path, "src", "build_all.R"))
#     getOption("restart")()
#   }
#   add_labtools_import_to_rprofile(".Rprofile")
#   # Open the new note in RStudio -------------------------------------------------------------------
#   if (open) {
#     if (Sys.info()['sysname'] == "Linux") {
#       command <- paste0("(rstudio ", notebook_proj_path, " &)")
#       system(command)    
#     }
#   }
# }
# 
# 
# #===================================================================================================
# #' Find README files
# #' 
# #' Locate all README files in the project and return their paths
# #' 
# #' @param path (\code{character} of length 1) Path somwhere under a R project in which to look for
# #'   README files.
# find_readmes <- function(path = get_project_root()) {
#   path <- get_project_root(normalizePath(path))
#   all_paths <- list.files(path, recursive = TRUE, )
#   readme_paths <- all_paths[basename(all_paths) == "README.yaml"]
#   file.path(path, readme_paths)
# }
# 
# 
# #===================================================================================================
# #' Validate READMEs 
# #' 
# #' Check if a readme has an entry for all files/folders in its folder. 
# #' 
# #' NOT FINISHED
# validate_readme <- function(readme_paths, missing = "warn", add = TRUE, order = TRUE) { 
#   do_once <- function(readme_path) {
#     path <- normalizePath(path)
#     content_paths <- list.files(dirname(path))
#     content_paths <- content_paths[content_paths != "README.yaml"]
#     
#     readme_content <- yaml::yaml.load_file(path)
#   }
# }
# 
# 
# 
# 
# 
# #===================================================================================================
# #' Add `library(labtools)` to .Rprofile
# #' 
# #' Adds \code{library(labtools)} to .Rprofile to that new notebooks automatically load
# #' \code{labtools} when opened. 
# #' 
# #' @param profile_path The path to the .Rprofile ro modify.
# add_labtools_import_to_rprofile <- function(profile_path) {
#   data <- paste0("\n##### Added by labtools (v ", packageVersion("labtools"), ") #####\n",
#                  "library(labtools)\n",
#                  "#####")
#   write(data, profile_path, append = TRUE)
# }
# 
# 
# #===================================================================================================
# #' Makes a new note
# #' 
# #' Make a new note for the current time and notebook.
# #' 
# #' @param ... (\code{character}) One or more name corresponding to an organizational hierarchy.
# #' @param template (\code{character}) The name of the template to use for the new note. Available
# #' templates can be found in the \code{templates} folder in the root directory of the notebook. You
# #' can add your own templates there and use their name in this function.
# #' @param date (\code{character}) Not date in the form `yyyy_mm_dd`. 
# #' @param notebook (\code{character}) The path to the notebook in which to write the note or one
# #'   of its subdirectories.
# #' @param change_wd (\code{logical}) If \code{TRUE}, change the current working directory to the new note.
# #' @param open If \code{TRUE}, the new note is opened in RStudio after creation.
# #'  
# new_note <- function(..., template = "default", date = NULL, notebook = get_project_root(),
#                      change_wd = TRUE, open = TRUE) {
#   # Parse arguments --------------------------------------------------------------------------------
#   if (is.null(date)) date <- format(Sys.time(), format="%Y_%m_%d")
#   names <- unlist(list(...))
#   note_name <- paste(c(date, names), collapse = "-")
#   note_name <- gsub(" ", "_", note_name)
#   note_location <- file.path(notebook, "content")
#   note_path <- file.path(note_location, note_name)
#   template_path <- file.path(notebook, "templates", template)
#   # Create note directory --------------------------------------------------------------------------
#   if (file.exists(note_path)) stop(paste0("Note already exists at path '", note_path, "'."))
#   if (!file.exists(template_path)) stop(paste0("Cannot find template at '", template_path,
#                                                "'. Check that it exists or pick another template."))
#   copy_folder_with_links(from = template_path, to = note_location)
#   file.rename(from = file.path(note_location, template), to = note_path)
#   # Change current working directory to new note ---------------------------------------------------
#   if (change_wd) setwd(note_path)
#   # Open note after creation -----------------------------------------------------------------------
#   if (open && rstudioapi::isAvailable()) {
#     rmd_files <- list.files(note_path, pattern = "\\.Rmd$", ignore.case = TRUE, full.names = TRUE)
#     if (length(rmd_files) > 0) file.edit(rmd_files)
#   }
# }
# 
# 
# 
# #===================================================================================================
# #' Make default note template directory
# #' 
# #' @param location (\code{character} of length 1) Where to write the folder. 
# make_notebook_template_default <- function(location) {
#   location <- file.path(location, "default")
#   if(file.exists(location)) stop(paste0("Target folder ", location, " already exists."))
#   dir.create(location, recursive = TRUE)
#   file.symlink(file.path("..", "..", "data"), file.path(location, "_data"))
#   file.symlink(file.path("..", "..", "bin"), file.path(location, "_bin"))
#   file.symlink(file.path("..", "..", "src"), file.path(location,  "_src"))
#   file.symlink(file.path("..", "..", "references"), file.path(location, "_references"))
#   dir.create( file.path(location, "scratch"), recursive = TRUE)
#   default_gitignore = "scratch\n*.html\n"
#   write(default_gitignore,  file.path(location, ".gitignore"))
#   default_note = paste0('---\ntitle: "Untitled"\ndate: "', format(Sys.time(), format="%Y-%m-%d"),
#                         '"\noutput: html_document\n---')
#   write(default_note,  file.path(location, "notes.Rmd"))
# }
# 
# #===================================================================================================
# #' Make minimal note template directory
# #' 
# #' @param location (\code{character} of length 1) Where to write the folder. 
# make_notebook_template_minimal <- function(location) {
#   location <- file.path(location, "minimal")
#   if(file.exists(location)) stop(paste0("Target folder ", location, " already exists."))
#   dir.create(location, recursive = TRUE)
#   default_gitignore = "*.html\n"
#   write(default_gitignore,  file.path(location, ".gitignore"))
#   default_note = paste0('---\ntitle: "Untitled"\ndate: "', format(Sys.time(), format="%Y-%m-%d"),
#                         '"\noutput: html_document\n---')
#   write(default_note,  file.path(location, "notes.Rmd"))
# }
# 
# 
# #===================================================================================================
# #' Make empty note template directory
# #' 
# #' @param location (\code{character} of length 1) Where to write the folder. 
# make_notebook_template_empty <- function(location) {
#   location <- file.path(location, "empty")
#   if(file.exists(location)) stop(paste0("Target folder ", location, " already exists."))
#   dir.create(location, recursive = TRUE)
# }
# 
