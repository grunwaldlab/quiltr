#===================================================================================================
#' Quiltr template for default options
#' 
#' Makes a template directory that uses default quilt options.
#' This is mostly for demonstration and starting new custom projects.
#' 
#' @param path (\code{character} of length 1) Where to make the template directory.
#' @param name (\code{character} of length 1) The name of the template directory to make.
#' 
#' @export
quiltr_template_default <- function(path = getwd(), name = "quiltr_template_default") {
  # Make output directory --------------------------------------------------------------------------
  directory_path <- file.path(path, name)
  if (file.exists(directory_path)) {
    stop(paste0("The directory '", directory_path, "' already exists."))
  }
  dir.create(directory_path)
  # Make configuration file ------------------------------------------------------------------------
  config_path <- file.path(directory_path, eval(as.list(args(quilt))$site_config_name))
  cat(yaml::as.yaml(list(name = "Template")), file = config_path)
  # Make build script ------------------------------------------------------------------------------
  build_script_name <- "build_project.R"
  build_script_path <- file.path(directory_path, build_script_name)
  build_script_contents <- "#!/usr/bin/env Rscript\nquiltr::quilt()\n"
  cat(build_script_contents, file = build_script_path)
  Sys.chmod(build_script_path, "0775")
  # Make R project file ----------------------------------------------------------------------------
  proj_file_name <- paste0(name, ".Rproj")
  proj_file_path <- file.path(directory_path, proj_file_name)
  proj_file_contents <- yaml::as.yaml(list(Version = 1, RestoreWorkspace = "Default", SaveWorkspace = "Default", 
                                           AlwaysSaveHistory = "Default", EnableCodeIndexing = TRUE, 
                                           UseSpacesForTab = TRUE, NumSpacesForTab = 2L, Encoding = "UTF-8", 
                                           RnwWeave = "knitr", LaTeX = "pdfLaTeX", BuildType = "Custom", 
                                           CustomScriptPath = build_script_name))
  cat(proj_file_contents, file = proj_file_path)
  # Make example content ---------------------------------------------------------------------------
  example_dir_path <- system.file("file_templates", "quiltr_template_default", package = "quiltr")
  example_files <- list.files(example_dir_path, all.files = TRUE, full.names = TRUE,
                              include.dirs = TRUE)
  file.copy(example_files, directory_path, recursive = TRUE)
  return(directory_path)
}