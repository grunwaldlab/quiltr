#===================================================================================================
#' List Quiltr templates
#' 
#' Return available Quiltr templates. These can be made using \code{\link{make_quiltr_template}}.
#' 
#' @param  full_path (\code{logical} of length 1) If \code{TRUE}, return the full paths to templates 
#' instead of just their name. 
#' 
#' @return (\code{character}) Available Quiltr templates
#' 
#' @examples 
#' list_quiltr_templates()
#' 
#' @export
list_quiltr_templates <- function(full_path = FALSE) {
  tempate_dir <- system.file("templates", package = "quiltr")
  output <- list.dirs(tempate_dir, recursive = FALSE)
  if (!full_path) {
    output <- basename(output)
  }
  return(output)
}


#===================================================================================================
#' Make a template directory
#' 
#' Generates a template directory for use with \code{\link{quilt}} in the specified location.
#' 
#' @param path (\code{character} of length 1) Where to make the template directory. 
#' @param template (\code{character} of length 1) That name of the template to use.
#' To see a list of available templates, use \code{\link{list_quiltr_templates}}.
#' 
#' @return (\code{character} of length 1) The path to the template directory that was made.
#' 
#' @examples 
#' \dontrun{
#' 
#' # The following would make the directory "/home/my_name/documents/default"
#' make_quiltr_template(path = "/home/my_name/documents", template = "default")
#' }
#' 
#' @export
make_quiltr_template <- function(path, template = "default") {
  # Validate input ---------------------------------------------------------------------------------
  if (!file.exists(path)) {
    stop("Location to make template does not exist. Check that the `path` argument specifies a valid path")
  }
  if (!template %in% list_quiltr_templates()) {
    stop("Template not found. Check that the template specified is in the result of `list_quiltr_templates()`")
  }
  target_path <- file.path(path, template)
  if (file.exists(target_path)) {
    stop("The template directory that would be made already exists.")
  }
  # Copy template directory ------------------------------------------------------------------------
  template_dir_path <- system.file("templates", template, package = "quiltr")
  file.copy(template_dir_path, path, recursive = TRUE)
  return(target_path)
}
