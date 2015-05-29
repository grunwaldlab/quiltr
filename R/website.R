#===================================================================================================
#' Generate _output.yaml for website
#' 
#' Make the content of the file that contols knitting of the website page Rmds
#' 
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
#'  
#' @return (\code{character} of length 1)
make_output_yaml <- function(theme = "journal") {
  yaml::as.yaml(list(html_document = list(self_contained = FALSE,
                                          theme = theme,
                                          highlight = "textmate",
                                          toc = FALSE,
                                          lib_dir = "libs",
                                          includes = list(in_header = "in_header.html",
                                                          before_body = "before_body.html",
                                                          after_body = "after_body.html"))))
}


#===================================================================================================
#' Make Rmd code to display HTML pages
#' 
#' Uses iframes to display content from other html files.
#' 
#' @param files (\code{character}) One or more files to display
#' @param titles (\code{character} same length as \code{files}) Titles to display above embedded
#'   content
#' @param rmd_header (\code{list}) YAML header data from Rmarkdown.
#' @param apply_theme (\code{logical} of length 1) If \code{TRUE}, apply parent window CSS to iframes
#' 
#' @return (\code{character} of length 1) The Rmd code to display the content provided. 
make_parent_html <- function(files, titles = NA, rmd_header = NULL, apply_theme = FALSE) {
  # Validate arguments -----------------------------------------------------------------------------
  if (!is.na(titles) && length(files) != length(titles)) {
    stop("Arguments `files` and `title` must the same length")
  }
  if (any(! tools::file_ext(files) %in% c("Rmd", "rmd", "md", "html"))) {
    stop("Only files with `Rmd` or `html` extensions can be used.")
  }
  # Define functions to make components ------------------------------------------------------------
  make_rmd_header <- function(rmd_header) {
    paste0('---\n', yaml::as.yaml(rmd_header), "---\n\n")
  }
  make_iframe_code <- function(file, count) {
    on_load <- paste0('autoResize(\'iframe', count, '\');')
    if (apply_theme)  on_load <- paste0('apply_theme(\'iframe', count, '\'); ', on_load)
    iframe_att <- paste0('width="100%" height="200px" id="iframe', count,
    '" marginheight="0" frameborder="0" onLoad="', on_load, '"')
    paste0('<iframe src="', file, '" ', iframe_att, '></iframe>\n\n')
  }
  make_child_rmd_code <- function(file) {
    paste0("```{r child = '", file, "'}\n```\n\n")
  }
  make_code <- function(file, title, count) {
    if (!is.na(title)) title_code <- paste0("## ", title, "\n\n") else title_code <- ""
    if (tools::file_ext(file) %in% c("Rmd", "rmd", "md")) {
      return(paste0(title_code, make_child_rmd_code(file)))
    } else if (tools::file_ext(file) %in% c("html")) {
      return(paste0(title_code, make_iframe_code(file, count)))
    } 
  }
  # Generate Rmd document ---------------------------------------------------------------------------
  paste0(make_rmd_header(rmd_header),
         paste0(mapply(make_code, file = files, title = titles, count = 1:length(files)), collapse = ""))
}


#===================================================================================================
#' Creates hierarchical menu html from note names
#' 
#' Parses the names of note directories to create html for a hierarchical menu. 
#' 
#' @param hierarchy (\code{list} of \code{character}) Locations in the menu hierarchy.
#' @param page_paths (\code{character}) The path to website site pages (.html files) corresponding 
#' argument \code{hierarchy}.
#' @param notebook_name (\code{character} of length 1) The name of the notebook. This is displayed 
#' as the link to the home page.
#' 
#' @return (\code{character} of length 1) The html code to make hierarchical menu
make_hierarchy_html <- function(hierarchy, page_paths, notebook_name = "Notebook") {
  # Parse note directory names ---------------------------------------------------------------------
  expand <- function(char) lapply(seq_along(char), function(i) char[1:i])
  full_hierarchy <- unique(unlist(lapply(hierarchy, expand), recursive = FALSE))
  depth <- vapply(full_hierarchy, length, numeric(1))
  # Recursive function to make menu html -----------------------------------------------------------
  make_nav <- function(index) {
    current <- full_hierarchy[[index]]
    children <- which(vapply(full_hierarchy, function(y) all(y[seq_along(current)] == current) & length(current) + 1 == length(y), logical(1)))
    page_path_index <- which(vapply(hierarchy, identical, y = current, logical(1)))
    if (length(page_path_index) == 0)
      path <- "#"
    else
      path <-  page_paths[page_path_index]
    name <- gsub("_", " ", current[length(current)], fixed = TRUE)
    out <- ""
    if (length(current) == 1) {
      if  (length(children) == 0) {
        out <- paste0(out, '<li><a href="', path,'">', name, '</a></li>')
      } else {
        out <- paste(sep = "\n",
                     out,
                     '<li>',
                     paste0('<a href="', path,
                            '" class="dropdown-toggle" data-toggle="dropdown">', name,
                            '<b class="caret"></b></a>'),
                     '<ul class="dropdown-menu multi-level">')
        child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
        out <- paste0(out, child_html)
        out <- paste0(out, "</ul></li>")
      }
    } else if (length(children) == 0) {
      out <- paste0(out, '<li><a href="', path, '">', name, '</a></li>')
    } else {
      out <- paste(sep = "\n",
                   out,
                   '<li class="dropdown dropdown-submenu">',
                   paste0('<a href="', path,
                          '" class="dropdown-toggle" data-toggle="dropdown">', name,
                          '</a>'),
                   '<ul class="dropdown-menu">')
      child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
      out <- paste0(out, child_html)
      out <- paste0(out, "</ul></li>")
    }
    return(out)
  }
  
  # Make menu --------------------------------------------------------------------------------------
  home <- page_paths[vapply(hierarchy, length, numeric(1)) == 0]
  notebook_name_html <- paste(sep = "\n",
                              '<div class="navbar-header">',
                              '<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">',
                              '<span class="sr-only">Toggle navigation</span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '</button>',
                              paste0('<a class="navbar-brand" href="', home, '">', notebook_name, '</a>'),
                              '</div>')
  menu_html <- paste(sep = "\n",
                     '<div class="collapse navbar-collapse">',
                     '<ul class="nav navbar-nav">',
                     paste0(lapply(which(depth == 1), make_nav), collapse = ""),
                     '</ul></div>')
  nav_bar_html <- paste( sep = "\n",
                         '<div class="navbar navbar-default" role="navigation">',
                         '<div class="container">',
                         notebook_name_html,
                         menu_html,
                         '</div>',
                         '</div>')
  return(nav_bar_html)
}


#===================================================================================================
#' Get paths to notes
#' 
#' Return the absolute paths to note files in a given directory.
#' 
#' @param path (\code{character}) One or more directories in which to look for note files.
#' @param type (\code{character}) One or more note file extensions to search for.
#' @param full_names (\code{logical} of length 1) See \code{\link{list.files}} help for option
#'   \code{full.names}.
#' @param simplify (\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
#' with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
#' single \code{character} vector is returned. 
#' 
#' @return Depends on the \code{simplify} option.
get_note_files <- function(path, type = c("html"), full_names = TRUE, simplify = TRUE) {
  # If nothing is given, return the same ----------------------------------------------------------
  if (length(path) == 0) return(path)
  # Make paths absolute ----------------------------------------------------------------------------
  path <- normalizePath(path)
  # Make regular expression for file extensions ----------------------------------------------------
  note_regex <- paste0(paste("\\.", type, collapse = "|", sep = ""), "$")
  # Search for files with matching extension -------------------------------------------------------
  note_paths <- lapply(path, list.files, note_regex, all.files = TRUE, recursive = TRUE,
                           ignore.case = TRUE, full.names = full_names)
  # Simplify if specified --------------------------------------------------------------------------
  if (simplify) note_paths <- unlist(note_paths)
  return(note_paths)
}


#===================================================================================================
#' Get html file dependencies
#' 
#' Return the absolute paths of file referneced by one or more html files.
#' 
#' @param path (\code{character}) One or more html files in which to look for references to
#'   dependencies.
#'   
#' @return \code{list} of paths are returned with elements corresponding to input \code{path}
get_html_dependencies <- function(path) {
  # define attributes of html tags to get the content of -------------------------------------------          
  xpath_tags <- c("//@src", "//@href")
  # define regular expressions to filter results ---------------------------------------------------
  excluded_dependencies <- c("^data:", "^https:", "^http:")
  # define function to process a single html file --------------------------------------------------
  get_dependency <- function(path) {
    # Extract values of tag attributes - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    html <- XML::htmlParse(path)
    output <- unlist(lapply(xpath_tags, XML::xpathSApply, doc = html))
    # Remove values that are note local file paths - - - - - - - - - - - - - - - - - - - - - - - - -
    for (pattern in excluded_dependencies) output <- output[!grepl(pattern, output)]
    if (is.null(output)) output <- character(0)
    return(output)
  }
  # process all html files -------------------------------------------------------------------------
  lapply(path, get_dependency)
}


#===================================================================================================
#' Get note dependencies
#' 
#' Return the absolute paths of note dependencies files. Currently, only \code{.html} files are
#' implemented.
#' 
#' @param path (\code{character}) One or more note files in which to look for references to
#'   other files.
#' @param simplify (\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
#' with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
#' single \code{character} vector is returned. 
#' 
#' @return Depends on the \code{simplify} option.
get_file_dependencies <- function(path, simplify = FALSE) {
  # If nothing is given, return the same ----------------------------------------------------------
  if (length(path) == 0) return(path)
  # Define parsers for each file type supported ----------------------------------------------------
  parsers <- list("html" = get_html_dependencies)
  # Check for unsupported file types ---------------------------------------------------------------
  extension <- tools::file_ext(path)
  unsupported_ext <- unique(extension[!extension %in% names(parsers)])
  if (length(unsupported_ext) > 0) 
    stop(paste("Unsupported file type(s) encountered: ", paste(unsupported_ext, collapse = ", ")))
  # Check for non-existant input files -------------------------------------------------------------
  absent_input <- path[!file.exists(path)]
  if (length(absent_input) > 0) stop(paste0("The following input files do not exist:\n",
                                            paste0("\t", absent_input, collapse = "\n")))
  # Call parser functions to get dependencies ------------------------------------------------------
  output <- lapply(names(parsers), function(p) parsers[[p]](path[extension == p]))
  output <- unlist(output, recursive = FALSE)
  # Standardize file paths -------------------------------------------------------------------------
  standardize_path <- function(path, context) {
    from_root <- grepl(paste0("^", .Platform$file.sep), path)
    path[!from_root] <- file.path(dirname(context), path[!from_root])
    suppressWarnings(normalizePath(path))
  }
  output <- mapply(standardize_path, output, path, SIMPLIFY = FALSE)
  # Remove any files that do not exist -------------------------------------------------------------
  absent_files <- lapply(output, function(x) x[!file.exists(x)])
  warning_text <- vapply(which(sapply(absent_files, length) > 0),
                         function(i) paste0("\t\t\t", path[i], ":\n",
                                            paste0("\t\t\t\t", absent_files[[i]], collapse = "\n")),
                         character(1))
  if (any(sapply(absent_files, length) > 0)) warning("The following dependencies do not exist:\n", 
                                                     paste0(warning_text, collapse = "\n"))
  output <- lapply(output, function(x) x[file.exists(x)])
  # Simplify if specified --------------------------------------------------------------------------
  if (simplify) output <- unlist(output)
  # Make dependencies unique -----------------------------------------------------------------------
  if (simplify) output <- unique(output) else  output <- lapply(output, unique)
  return(output)
}


#===================================================================================================
#' Copy notes and their dependencies
#' 
#' Copy note files and any files they reference to a new location while preserving relative
#' directory locations. 
#' Enough of the directory structure will be copied to contain all the files copies in their
#' original configuration. 
#' 
#' @param from (\code{character}) The paths to notes to copy.
#' @param to (\code{character} of length 1) The path to where the notes and their dependencies 
#' will be copied.
#' @param copy_depend (\code{logical} of length 1) If \code{FALSE}, dependencies will not be 
#' copied.
#' 
#' @return (\code{character}) Paths of where the notes were copied to.
copy_notes <- function(from, to, copy_depend = TRUE) {
  # Make input file paths absolute -----------------------------------------------------------------
  from_path <- normalizePath(from)
  to <- normalizePath(to)
  # Get dependencies of input files ----------------------------------------------------------------
  if (copy_depend) {
    depend_from <- get_file_dependencies(from_path, simplify = TRUE)
    from_path <- c(from_path, depend_from)
  }
  # Determine location of file copies --------------------------------------------------------------
  from_root <- get_common_dir(from_path)
  to_path <- file.path(to, gsub(paste0("^", dirname(from_root), .Platform$file.sep), "", from_path))
  # Copy files and directory structure -------------------------------------------------------------
  for (dir_to_make in unique(dirname(to_path))) 
    if (!file.exists(dir_to_make)) dir.create(dir_to_make, recursive = TRUE)
  invisible(file.copy(from = from_path, to = to_path, overwrite = TRUE))
  # Return the locations of input file copies ------------------------------------------------------
  to_path[1:length(from)]
}


#===================================================================================================
#' Get notebook note hierarchy
#' 
#' Return a list of all the locations in the notebook note classification hierarchy.
#' 
#' @param path (\code{character}) The paths to notes to assign hirearchical classifications to.
#' @param root (\code{character} of length 1) The path to the root directory of the notebook.
#' @param cumulative (\code{logical} of length 1) If \code{TRUE}, all of the intermendiate hierarchy
#' levels will be returned. 
#' @param use_file_names (\code{logical} of length 1) If \code{TRUE}, The names of files will be
#' be used to determine the hierarchy.
#' @param use_dir_names (\code{logical} of length 1) If \code{TRUE}, The names of directories will
#' be used to determine the hierarchy.
#' @param use_config_files (\code{logical} of length 1) If \code{TRUE}, configuration
#' files along the notes' file path will be used to determine the hierarchy. The name of 
#' configuration files is specified by the \code{note_config_name} option.
#' @param name_sep (\code{character} of length 1) A character to split file/directory names by when
#' using them for parts of the hierarchy.
#' @param use_file_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of a file
#' name when split by the \code{name_sep} option.
#' @param use_dir_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of directory
#' names when split by the \code{name_sep} option.
#' @param note_config_name (\code{character} of length 1) The name of configuration files.
#'   
#' @return (\code{list} of \code{character}) A list of locations in the notebook hierarchy 
#' corresponding to the input argument \code{path}.
get_note_hierarchy <- function(path, root, cumulative = TRUE, use_file_names = TRUE,
                               use_dir_names = TRUE, use_config_files = TRUE, name_sep = "-",
                               use_file_suffix = FALSE, use_dir_suffix = TRUE,
                               note_config_name = ".note.yml") {
  # Make input file paths absolute -----------------------------------------------------------------
  path <- normalizePath(path)
  root <- normalizePath(root)
  process_one <- function(path) {
    # Get directory path from root -----------------------------------------------------------------
    rel_path <- gsub(paste0("^", root, .Platform$file.sep), "", path)
    path_hierarchy <- strsplit(rel_path, .Platform$file.sep, fixed = TRUE)[[1]]
    # For each level in the directory, record effects on hierarchy --------------------------------- 
    hierarchy <- list(character(0))
    for (index in 1:length(path_hierarchy)) {
      addition <- NULL
      # Get path to current directory level  - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      current_path <- do.call(file.path, as.list(c(root, path_hierarchy[0:index])))
      # Apply directory name effects - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (index != length(path_hierarchy) && use_dir_names) {
        addition <- basename(current_path)
        if (!is.na(name_sep) && !is.null(name_sep) && length(addition) > 0) 
          addition <- unlist(strsplit(addition, name_sep, fixed = TRUE))
        if (!use_dir_suffix)
          addition <- addition[seq(1, length.out = length(addition) - 1)]
      }
      # Apply file name effects  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (index == length(path_hierarchy) && use_file_names) {
        addition <- tools::file_path_sans_ext(basename(current_path))
        if (!is.na(name_sep) && !is.null(name_sep) && length(addition) > 0) 
          addition <- unlist(strsplit(addition, name_sep, fixed = TRUE))
        if (!use_file_suffix)
          addition <- addition[seq(1, length.out = length(addition) - 1)]
      }
      # Apply configuration file effects - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      config_path <- file.path(dirname(current_path), note_config_name)
      if (use_config_files && file.exists(config_path)) {
        config <- yaml::yaml.load_file(config_path)
        for (pattern in names(config)) {
          matches <- Sys.glob(file.path(dirname(current_path), pattern))
          if (current_path %in% matches) {
            if (is.null(config[[pattern]][1])) {
              hierarchy <- list()
              addition <- NULL
            } else if (config[[pattern]][1] == ".") {
              if (length(config[[pattern]]) > 1)
                addition <- config[[pattern]][2:length(config[[pattern]])]
            } else {
              hierarchy <-  list(character(0))
              addition <- config[[pattern]]
            }
          }
        }
      }
      # Save resulting addition  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (cumulative)
      {
        hierarchy <-  c(hierarchy,
                        lapply(seq_along(addition),
                               function(i) c(hierarchy[[length(hierarchy)]], addition[1:i])))
      } else {
        if (length(hierarchy) > 0) 
          hierarchy[[length(hierarchy)]] <- c(hierarchy[[length(hierarchy)]], addition)
        else 
          hierarch <- list(addition)
      }  
    }
  return(hierarchy)
  }
  lapply(path, process_one)
}


#===================================================================================================
#' Make composite webpage
#' 
#' Make webpage out of iframes.
#'
#' @param name (\code{character}) Name of Rmd files to use
#' @param files (\code{list} of \code{character}) The file in each page, corresponding to argument
#' \code{name}
#' @param location (\code{character} of length 1) Where to make the webpages
#' @param clean (\code{logical} of length 1) If \code{TRUE}, intermediate files are deleted after
#' use.
#' @param apply_theme (\code{logical} of length 1) If \code{TRUE}, apply parent window CSS to iframes
#' 
#' @return (Named \code{character}) The file paths to created .html files named by their source Rmd
#' files.
make_master_rmd <- function(name, files, location, clean = FALSE, apply_theme = TRUE) {
  master_rmd_path <- file.path(location, name)
  if (clean) files_to_remove <- master_rmd_path
  if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
  note_title <-  rev(strsplit(tools::file_path_sans_ext(name)[1], '-')[[1]])[1]
  note_yaml <- list(title = note_title)
  parent_html <- make_parent_html(files = files, titles = NA,
                                  rmd_header = note_yaml, apply_theme = apply_theme)
  cat(parent_html, file = master_rmd_path, append = FALSE)
  rmarkdown::render(master_rmd_path, quiet = TRUE)
}


#===================================================================================================
#' Make a website from notes
#' 
#' Makes a website from a directory containing correctly formatted notes. 
#' Each note must be a directory with one or more Rmd files. 
#' All of the notes will be copied and built to make the website. 
#' 
#' 
#' @param path (\code{character}) One or more directories in which to look for note files.
#' @param output (\code{character} of length 1) Location to write the output directory. The website
#' will be made in a directory called "website" in this location.
#' @param clean (\code{logical} of length 1) If \code{TRUE}, intermediate files are deleted after
#' use.
#' @param overwrite (\code{logical} of length 1) If \code{TRUE}, an existing directory with the 
#' same name as the output directory will be overwritten. 
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
#' @param apply_theme (\code{logical} of length 1) If \code{TRUE}, apply notebook CSS to 
#' note content.
#' @param cumulative (\code{logical} of length 1) If \code{TRUE}, all of the intermendiate hierarchy
#' levels will be returned. 
#' @param use_file_names (\code{logical} of length 1) If \code{TRUE}, The names of files will be
#' be used to determine the hierarchy.
#' @param use_dir_names (\code{logical} of length 1) If \code{TRUE}, The names of directories will
#' be used to determine the hierarchy.
#' @param use_config_files (\code{logical} of length 1) If \code{TRUE}, configuration
#' files along the notes' file path will be used to determine the hierarchy. The name of 
#' configuration files is specified by the \code{note_config_name} option.
#' @param name_sep (\code{character} of length 1) A character to split file/directory names by when
#' using them for parts of the hierarchy.
#' @param use_file_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of a file
#' name when split by the \code{name_sep} option.
#' @param use_dir_suffix (\code{logical} of length 1) If \code{TRUE}, use the last part of directory
#' names when split by the \code{name_sep} option.
#' @param note_config_name (\code{character} of length 1) The name of note placement configuration files.
#' @param site_config_name (\code{character} of length 1) The name of the website building configuration file.
#' It does not need to exist, but if it does in a directory specified by \code{site_config_file}, it is used
#' automatically. To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' @param site_config_file (\code{character} of length 1) The path to a configuration file specifing
#'  this function's option values or to the directory is is located in. The file should be in YAML format.
#'  To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' @param output_dir_name (\code{character} of length 1) The name of the output directory. 
#'   
#' @return (\code{character} of length 1) The file path to the created websites home page 
#' (\code{index.html})
#' 
#' TODO: make option to accept Rmd and possible other note types.
#' TODO: let notes occur  in multiple places in the hierarchy
#' 
#' @export
make_website <- function(path = getwd(), output = path, clean = TRUE, overwrite = FALSE, theme = "journal",
                         apply_theme = TRUE, cumulative = TRUE, use_file_names = TRUE,
                         use_dir_names = TRUE, use_config_files = TRUE, name_sep = "-",
                         use_file_suffix = FALSE, use_dir_suffix = TRUE,
                         note_config_name = ".note.yml", site_config_name = ".website_config.yml", 
                         site_config_file = path, output_dir_name = "website") {
  # Parse arguments --------------------------------------------------------------------------------
  path <- normalizePath(path)
  output <- normalizePath(output)
  # Read any configuration files -------------------------------------------------------------------
  if (!is.na(site_config_name) && !is.na(site_config_file) &&
      !is.null(site_config_name) && !is.null(site_config_file)) {
    if (!file.exists(site_config_file)) stop(paste0("Cannot find website configuration file at '",
                                                    site_config_file, "'. Path does not exist."))
    if (file.info(site_config_file)$isdir) 
      config_path <- file.path(site_config_file, site_config_name)
    else
      config_path <- site_config_file
    if (file.exists(config_path)) {
      message(paste0("Using configuration file found at ", config_path,
                     ". Set option 'site_config_name' or 'site_config_file' to NA or NULL to ",
                     "ignore this configuration file."))
      config_data <- yaml::yaml.load_file(config_path)
      for(i in seq(from = 1, length.out = length(config_data)))
        assign(x = names(config_data)[i], value = config_data[[i]])
    }
  }
  # Get note files ---------------------------------------------------------------------------------
  note_paths <- get_note_files(path)
  # Make output directory --------------------------------------------------------------------------
  output <- file.path(output, output_dir_name)
  content <- file.path(output, "content")
  if (file.exists(output)) {
    if (overwrite)
      unlink(output, recursive = TRUE) else
        stop("Website exsits at ", output, ". Use `overwrite = TRUE` to replace.")
  }
  dir.create(output)
  dir.create(content)  
  # Filter for notes in hirearchy ------------------------------------------------------------------
  classification <- get_note_hierarchy(note_paths, root = path, cumulative = cumulative, 
                                       use_file_names = use_file_names, 
                                       use_dir_names = use_dir_names, 
                                       use_config_files = use_config_files, name_sep = name_sep,
                                       use_file_suffix = use_file_suffix, 
                                       use_dir_suffix = use_dir_suffix,
                                       note_config_name = note_config_name)
  classification_paths <- rep(note_paths, vapply(classification, length, integer(1)))
  ul_classification <- unlist(classification, recursive = FALSE)
  hierarchy_class <- unique(ul_classification)
  hierarchy <- lapply(hierarchy_class,
                      function(x) classification_paths[vapply(ul_classification, 
                                                         identical, y = x, logical(1))])
  note_paths <- unique(unlist(hierarchy))
  classification <- get_note_hierarchy(note_paths, root = path, cumulative = cumulative, 
                                       use_file_names = use_file_names, 
                                       use_dir_names = use_dir_names, 
                                       use_config_files = use_config_files, name_sep = name_sep,
                                       use_file_suffix = use_file_suffix, 
                                       use_dir_suffix = use_dir_suffix,
                                       note_config_name = note_config_name)
  # Copy note directory ----------------------------------------------------------------------------
  note_copy_path <- copy_notes(note_paths, content)
  # Make website menu ------------------------------------------------------------------------------
  page_names <- vapply(hierarchy_class, paste, character(1), collapse = "-")
  page_names[page_names == ""] <- "index"
  page_names <- gsub(" ", "_", page_names)
  page_rmd_names <- paste0(page_names, ".Rmd")
  page_html_names <- paste0(page_names, ".html")
  page_html_paths <- file.path(output, page_html_names)
  pre_body_html_path <- file.path(output, "before_body.html")
  cat(make_hierarchy_html(hierarchy_class, page_html_names), file = pre_body_html_path)
  # Make other dependencies ------------------------------------------------------------------------
  dependencies <- vapply(c("in_header.html", "after_body.html"),
                         function(x) system.file("file_templates", x, package = "labtools"), 
                         character(1))
  file.copy(from = dependencies, to = output)
  output_yaml_path <- file.path(output, "_output.yaml")
  cat(make_output_yaml(theme = theme), file = output_yaml_path)
  # Step up clean up -------------------------------------------------------------------------------
  if (clean) {
    files_to_remove <- file.path(output, c("in_header.html", "after_body.html",
                                           "before_body.html","_output.yaml",
                                           page_rmd_names))
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  }  
  # Make website pages -----------------------------------------------------------------------------
  relative_copy_path <- gsub(pattern = paste0("^", output, .Platform$file.sep), "", note_copy_path)
  relative_copy_class_path <-rep(relative_copy_path, vapply(classification, length, integer(1)))
  hierarchy <- lapply(hierarchy_class,
                      function(x) relative_copy_class_path[vapply(ul_classification, 
                                                                  identical, y = x, logical(1))])
  home_path <- mapply(make_master_rmd, page_rmd_names, hierarchy, location = output)[["index.Rmd"]]
  if (rstudioapi::isAvailable()) rstudioapi::viewer(home_path)
  return(home_path)
}
