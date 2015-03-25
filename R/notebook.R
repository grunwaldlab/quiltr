

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
  dir.create(location, recursive = TRUE)
  file.copy(from = template_path, to = location, overwrite = FALSE, recursive = TRUE)
  file.rename(from = file.path(location, template_name), to = notebook_path) #rename root folder
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(notebook_path)
  file.rename(from = "notebook.Rproj", to = paste0(name, ".Rproj"))
  # Initialize git repository ----------------------------------------------------------------------
  if (use_git) {
    system("git init")    
  }
  # Initialize packrat -----------------------------------------------------------------------------
  if (use_packrat) {
    packrat::init(notebook_path)
    install.packages("labtools")
    packrat::snapshot()
  }
}