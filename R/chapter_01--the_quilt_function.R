#| # The `quilt` function
#| 
#| ## Introduction
#|
#| This is the central function of the quiltr package.
#| It makes a sharable representation of file content in specified folders.
#| 
#| > **Note:** The representation (i.e. the output type) currently being developed is a static html website,
#| but a PDF book-style output is also a goal.
#|
#| The file content in the output is organized using file names and folder structure.
#| `quilt` can execute scripts/programs and integrate their code and results into the output;
#| this is especially useful for literate programming documents (e.g. Rmarkdown), but plain code files can also be executed.
#|
#| ### Design goals
#|
#| * The 
#|
#| ## The function documentation
#| 
#| The commented lines in the code below are [roxygen2](http://cran.r-project.org/web/packages/roxygen2/README.html)
#| documentation that is parsed into the embedded 
#| function help menu that can be accessed by entering `?quilt` in the R console. 
#| The function help menu documentation should serve as an introduction to the capibilities of `quilt`.
#| 
#| ### The "Title" and "Description"
#|
#| The following text is used for the "Title" and "Description" sections in the help menu.
#| They should contain the minimal amount of detail necessary to give a general impression; more
#| in-depth documentation appears in the "Details" section after the option documentation.
#|
#===================================================================================================
#' @title 
#' Renders the contents of folder(s) into a sharable form.
#' 
#' @description 
#' Currently, the only output type is a static website.
#' The content of files in target folder(s) can be displayed in the output or accessed by links.
#' Rmarkdown and some programming languages can be executed and their output included.
#' The organization of the output website is derived from file/folder names or configuration files. 
#' This function's options are best specified with configuration files rather than passing values
#' to the function itself.
#' See the \code{config_name} option documentation for how to use configuration files. 
#' It is possible to do much more with configuration files than with arguments specified during the
#' function call.
#| 
#| ### Main options
#| 
#| There are no required arguments to `quilt` since all options have default values.
#| This is a design feature meant to encourage people to use configuration files to specify 
#| options rather than include them in the function call.
#| It also allows for beginers to use the function with limited understanding of its capibilities.
#| However, there are a core set of options that govern input and output.
#| 
#| #### Input paths 
#| 
#| The most important input for `quilt` is paths to _folders_ in which to look for input files.
#| Although `quilt` ultimatly works only on file content, it is meant to represent folders by doing so.
#| By default, this input path is the current working folder, so that even this central argument can
#| usually be excluded from function calls.
#| This option has a unique relationship with configuration files, which are intorduced with the next option.
#| Although `path` is used to specify where to look for configuration files, those same files can modify `path`,
#| thus allowing more configuration files to be found and more resulting modification of path, making the
#| the implementation of this option more subtle and complex than it might first appear.
#| More information on this important characteristic is provided with it implementation. `r # add explicit reference`
#| 
#| NOTE: add global/local stuff here
#|
#' @param path (\code{character} of length 1) [not path-specific or output-specific]
#' A path a to folder in which to look for files to
#' display in the output. This also determines where to look for configuration files.
#' A configuration file in \code{path} can modify the path before is it used to find content 
#' and configuration files for the rest of the options. 
#' Although only one path can be specified in the function call, it can be modified to multiple
#' paths in configuration files. 
#|
#| #### Configuration files
#|
#' @param config_name (\code{character} of length 1) The name of configuration files to use.
#' They can be anywhere in directories under \code{path}.
#' Configuration files are in YAML format and specify the values of options for this function.
#' Options that are file-path-specific (e.g. \code{type}) can take the form of named lists,
#' where names are file paths relative to the location of the configuration file 
#' (possibly with \code{*} or \code{**} wildcards) and values are the option values relevant
#' to the paths specified in the names.
#' File-path-specific options can be specified by configuraion files anywhere in the target 
#' directory, not just the root specified by \code{path}.
#' Only some of this function's options are file-path-specific; those that are not
#' (e.g. \code{output}) can be specified by values in a configuration file in \code{path}.
#' This is the only option that cannot be specified by a configuration file.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.

#' @param output (\code{character} of length 1) Location to write the output directory. The website
#' will be made in a directory called "website" in this location. If \code{NULL} or \code{NA}, the
#' the website will be made in a temporary directory.
#' @param name (\code{character} of length 1) The name on the link to the homepage of the website. 
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param overwrite (\code{logical} of length 1) If \code{TRUE}, an existing directory with the 
#' same name as the output directory will be overwritten. 
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param clean (\code{logical} of length 1) If \code{TRUE}, intermediate files are deleted after
#' use.
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param output_dir_name (\code{character} of length 1) The name of the output directory.
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param partial_copy (\code{logical} of length 1) If \code{FALSE}, The entire target directory 
#' of the notes will be copied instead of just the notes and their dependencies. It is possible that more than
#' just the target directory will be copied if there are files in the target directory with dependencies outside
#' it. Enough of the file structure will be copied to included all dependencies. 
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param open (\code{logical} of length 1) If \code{TRUE}, open the newly created website in an 
#' internet browser or the RStudio viewer window.
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param theme (\code{character} of length 1) The bootstrap theme used in the website. For current
#' options, see \url{http://rmarkdown.rstudio.com/html_document_format.html#appearance-and-style}
#  At the time of writing, valid options are:
#' \itemize{
#'   \item "default"
#'   \item "cerulean"
#'   \item "journal"
#'   \item "flatly"
#'   \item "readable"
#'   \item "spacelab"
#'   \item "united"
#'   \item "cosmo"
#' }
#' The value of this option can be set by a configuration file at the location specified by 
#' \code{path}, with name specified by \code{config_name}.
#' @param type (\code{character}) One or more file types to include in the output, specified using 
#' file extensions without the leading dot (e.g. \code{type = c("rmd", "html")}). The order file
#' types are given in indicates which file is used in the case that files have the same name
#' but different extensions. For example, if \code{type = c("html", "rmd")} and there is a note.html
#' and a note.Rmd in the same directory, the note.html will be used and the note.Rmd will be
#' ignored. NOTE: The file type precedence might be specified by a different option in the future.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' This might change in the future; see issue
#' \href{https://github.com/grunwaldlab/quiltr/issues/58}{#58}
#' @param apply_theme (\code{logical} of length 1) If \code{TRUE}, apply notebook CSS to 
#' input file content. This might not always work well depending on content and browser.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param use_file_names (\code{logical} of length 1) If \code{TRUE}, The names of files will be
#' be used to determine the hierarchy.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param use_dir_names (\code{logical} of length 1) If \code{TRUE}, The names of directories will
#' be used to determine the hierarchy.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param name_sep (\code{character} of length 1) A character to split file/directory names by when
#' using them for parts of the hierarchy.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param use_file_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of a file
#' name when split by the \code{name_sep} option.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param use_dir_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of directory
#' names when split by the \code{name_sep} option.
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param menu_name_parser (\code{function}) Defines a function to apply to each name in the menu hierarchy
#' to chenge it somehow. The function must take a single \code{character} input and output a single
#' \code{character}. 
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param placement (named \code{list} of \code{character}) Custom content placement rules. Custom
#' website menu heirarchy names can be specified.
#' These (usually; see \code{.} documentation below) override hierarchy inferred from file/directory
#' names.
#' The first element in the hierarchy can use the folling special values: 
#' \describe{
#'   \item{.}{Add to the hierarchy inferred from file/directory names instead of overriding it.}
#'   \item{..}{Add to the hierarchy inferred from file/directory names instead of overriding it, 
#'     but replace the last file/directory name value.}
#'   \item{""}{Do not include in website.} 
#' }
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param cumulative (\code{logical} of length 1) If \code{TRUE}, all of the intermendiate hierarchy
#' levels will be returned. 
#' The value of this option can be file-path-specific; see \code{config_name} documentation.
#' @param config_name (\code{character} of length 1) The name of configuration files to use.
#' They can be anywhere in directories under \code{path}.
#' Configuration files are in YAML format and specify the values of options for this function.
#' Options that are file-path-specific (e.g. \code{type}) can take the form of named lists,
#' where names are file paths relative to the location of the configuration file 
#' (possibly with \code{*} or \code{**} wildcards) and values are the option values relevant
#' to the paths specified in the names.
#' File-path-specific options can be specified by configuraion files anywhere in the target 
#' directory, not just the root specified by \code{path}.
#' Only some of this function's options are file-path-specific; those that are not
#' (e.g. \code{output}) can be specified by values in a configuration file in \code{path}.
#' This is the only option that cannot be specified by a configuration file.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' @details 
#' To see the list of file types that can be rendered, execute \code{formats_quilt_can_render()}.
#' All files types specified by \code{type} in \code{path} will be included.
#' Code is displayed using syntax highlighting if pandoc is installed. 
#' @return (\code{character} of length 1) The file path to the created website's home page 
#' (\code{index.html})
#' 
#' @examples
#' \dontrun{
#' 
#' # Make website out of the current working directory
#' quilt()
#' 
#' # Create a template directory in the current
#' # working directory and make a website from it
#' make_quiltr_template(getwd(), "default")
#' quilt("default")
#' }
#' 
#' @export
quilt <- function(path = getwd(), output_formats = "website", config_name = "quilt_config", config_path = path,
                  overwrite = FALSE, output_dir_name = "website",
                  partial_copy = TRUE, open = TRUE, theme = "journal",
                  type = formats_quilt_can_render(), apply_theme = FALSE,
                  use_file_names = FALSE, use_dir_names = TRUE,
                  name_sep = NULL, use_file_suffix = TRUE, use_dir_suffix = TRUE,
                  menu_name_parser = function(x) {x}, placement = character(0), cumulative = FALSE) {
  
  #| ### Validate input ############################################################################
  validate_quilt_input()
  
  #| ### Define output types #######################################################################
  #| The `quilt` function itself mostly deals with intrepreting options for rendering functions, each of which correspond to an output type.
  #| To make it easy to add output types, the rendering functions are identified by their name. 
  #| Simply creating a function with the appropriate name and parameters, even outside of the quiltr package source, will effectivly add a new output type.
  #| The function `get_quilt_renderes` searchs the current namespaces for functions named `quilt_[output type]`, where `[output type]` is the one-word name for an output type.
  #| It returns the list of renderer functions named by their output type. 
  renderers <- get_quilt_renderes()
  
  #| ### Get global option values from configuration files #########################################
  #| Apply any global option values in root configuration file.
  #| These options are determined before the local options since many of them effect the way local options are determined.
  #| Note that `config_path` is used instead of `path`; however, the default for `config_path` is `path`. 
  #| The function `get_global_options` should return a named list of named lists, representing the options for each output type. 
  #| The first dimension groups options by output type, and the second is the lists of options.
  global_option_names <- c("path", "output_formats", "config_name", "config_path")
  global_options <- get_global_options(main_function = "quilt", sub_functions = names(renderers),
                                       options = global_option_names, config_path, config_name)
  
  #| ### Define function to process each output format #############################################
  process_format <- function(global_options, output_format) {
    
    #| #### Find configuration files for local options #############################################
    config_paths <- find_config_files(global_options$path, global_options$config_name)
    
    #| #### Find all target file paths #############################################################
    target_paths <- get_target_paths(global_options$path)
    
    #| #### Get local option values from configuration files #######################################
    local_options <- get_path_specific_options(functions = c("quilt", renderers),
                                                paths = target_paths, 
                                                config_paths = config_paths,
                                                global_options = global_option_names)
    
    #| #### Get organizational hierarchy for target file paths #####################################
    hierarchy <- get_hierarchy(target_paths, ...)
    
    #| #### Render output ##########################################################################
    do.call(renderers[[output_format]],
            c(list(file_paths = target_paths, hierarchy = hierarchy), local_options))
  }
  
  #| ### Process each output format and return results #############################################
  mapply(process_format, global_options, names(global_options), SIMPLIFY = TRUE)
}
#|










quilt <- function(path = getwd(), output = NULL, name = "Home", 
                  overwrite = FALSE, clean = TRUE, output_dir_name = "website",
                  partial_copy = TRUE, open = TRUE, theme = "journal",
                  type = formats_quilt_can_render(), apply_theme = FALSE,
                  use_file_names = FALSE, use_dir_names = TRUE,
                  name_sep = NULL, use_file_suffix = TRUE, use_dir_suffix = TRUE,
                  menu_name_parser = function(x) {x}, placement = character(0), cumulative = FALSE,
                  config_name = "quilt_config.yml") {
  # Set up function to get option values from config files -----------------------------------------
  argument_names <- names(as.list(args(quilt)))
  argument_names <- argument_names[-length(argument_names)]
  arg_missing <- eval(c(missing(path), missing(output), missing(name), missing(overwrite),
                        missing(clean), missing(output_dir_name), missing(partial_copy), 
                        missing(open), missing(theme), missing(type), missing(apply_theme),
                        missing(use_file_names), missing(use_dir_names),
                        missing(name_sep), missing(use_file_suffix), missing(use_dir_suffix),
                        missing(menu_name_parser), missing(placement), missing(cumulative),
                        missing(config_name)))
  names(arg_missing) <- argument_names
  q_opt_raw <- function(context, option) {
    eval(get_option(path = context, option = option, func_arg_value = get(option), root = path,
                    config_name = config_name, is_missing = arg_missing[[option]]))
  }
  q_opt <- memoise::memoise(q_opt_raw)
  # Parse arguments --------------------------------------------------------------------------------
  path <- normalizePath(q_opt(NULL, "path"))
  output <- q_opt(NULL, "output")
  if (is.null(output) || is.na(output)) {
    output <- tempfile()
    dir.create(output)
  }
  output <- normalizePath(output)
  # Detect/delete old website ----------------------------------------------------------------------
  output_path <- file.path(output, output_dir_name)
  content_path <- file.path(output_path, "content")
  if (file.exists(output_path)) {
    if (overwrite)
      unlink(output_path, recursive = TRUE)
    else
      stop("Website exsits at ", output_path, ". Use `overwrite = TRUE` to replace.")
  }
  # Find content files -----------------------------------------------------------------------------
  target_paths <- get_content_files(path, q_opt)
  if (length(target_paths) == 0) stop(paste0("No content files found in '", path, "'"))
  # Filter for notes in hirearchy ------------------------------------------------------------------
  classification <- get_hierarchy(target_paths, root = path, q_opt)
  name_parsers <- lapply(target_paths, q_opt, option = "menu_name_parser")
  for (index in seq_along(target_paths)) {
    classification[index] <- rapply(classification[index],
                                    q_opt(target_paths[index], "menu_name_parser"),
                                    how = "list")
  }
  classification_paths <- rep(target_paths, vapply(classification, length, integer(1)))
  ul_classification <- unlist(classification, recursive = FALSE)
  hierarchy_class <- unique(ul_classification)
  if (!0 %in% sapply(hierarchy_class, length))
    hierarchy_class <- c(list(character(0)), hierarchy_class)
  hierarchy <- lapply(hierarchy_class,
                      function(x) classification_paths[vapply(ul_classification, 
                                                              identical, y = x, logical(1))])
  target_paths <- unique(unlist(hierarchy))
  classification <- get_hierarchy(target_paths, root = path, q_opt)
  for (index in seq_along(target_paths)) {
    classification[index] <- rapply(classification[index],
                                    q_opt(target_paths[index], "menu_name_parser"),
                                    how = "list")
  }
  # Make output directory --------------------------------------------------------------------------
  dir.create(output_path)
  dir.create(content_path)  
  # Copy content directory -------------------------------------------------------------------------
  content_copy_path <- render_files_to_html(target_paths, content_path,
                                            partial_copy = q_opt(NULL, "partial_copy"))
  # Make website menu ------------------------------------------------------------------------------
  page_names <- vapply(hierarchy_class, paste, character(1), collapse = "-")
  page_names[page_names == ""] <- "index"
  page_names <- gsub(" ", "_", page_names)
  page_rmd_names <- paste0(page_names, ".Rmd")
  page_html_names <- paste0(page_names, ".html")
  page_html_paths <- file.path(output_path, page_html_names)
  pre_body_html_path <- file.path(output_path, "before_body.html")
  cat(make_hierarchy_html(hierarchy_class, page_html_names, site_name = q_opt(NULL, "name")),
      file = pre_body_html_path)
  # Make other dependencies ------------------------------------------------------------------------
  dependencies <- vapply(c("in_header.html", "after_body.html"),
                         function(x) system.file("website_parts", x, package = "quiltr"), 
                         character(1))
  file.copy(from = dependencies, to = output_path)
  output_yaml_path <- file.path(output_path, "_output.yaml")
  cat(make_output_yaml(theme = q_opt(NULL, "theme")), file = output_yaml_path)
  # Step up clean up -------------------------------------------------------------------------------
  if (clean) {
    files_to_remove <- file.path(output_path, c("in_header.html", "after_body.html",
                                                "before_body.html","_output.yaml",
                                                page_rmd_names))
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  }  
  # Make website pages -----------------------------------------------------------------------------
  relative_copy_path <- gsub(pattern = paste0("^", output_path, .Platform$file.sep), "",
                             content_copy_path)
  relative_copy_class_path <- rep(relative_copy_path, vapply(classification, length, integer(1)))
  copy_hierarchy <- lapply(hierarchy_class,
                           function(x) relative_copy_class_path[vapply(ul_classification, 
                                                                       identical, y = x, logical(1))])
  for (index in seq_along(copy_hierarchy)) {
    names(copy_hierarchy[[index]]) <- hierarchy[[index]]
  }
  home_path <- mapply(make_master_rmd, page_rmd_names, copy_hierarchy,
                      MoreArgs = list(location = output_path, q_opt = q_opt))[["index.Rmd"]]
  # Open new website -------------------------------------------------------------------------------
  if (rstudioapi::isAvailable() && q_opt(NULL, "open")) { rstudioapi::viewer(home_path) }
  return(home_path)
}
#|