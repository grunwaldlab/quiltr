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
    if (length(rmd_header > 0))
      paste0('---\n', yaml::as.yaml(rmd_header), "---\n\n")
    else
      ""
  }
  make_iframe_code <- function(file, count) {
    on_load <- paste0('autoResize(\'iframe', count, '\'); export_links(\'iframe', count, '\');')
    if (apply_theme)  on_load <- paste0('apply_theme(\'iframe', count, '\'); ', on_load)
    iframe_att <- paste0('width="100%" height="0px" id="iframe', count,
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
  if (length(files) > 0)
    iframe_code <- paste0(mapply(make_code, file = files, title = titles, count = 1:length(files)),
                          collapse = "")
  else 
    iframe_code <-""
  paste0(make_rmd_header(rmd_header), iframe_code)
}


#===================================================================================================
#' Creates hierarchical menu html from file names
#' 
#' Parses file paths to create html for a hierarchical menu. 
#' 
#' @param hierarchy (\code{list} of \code{character}) Locations in the menu hierarchy.
#' @param page_paths (\code{character}) The path to website site pages (.html files) corresponding 
#' argument \code{hierarchy}.
#' @param site_name (\code{character} of length 1) The name of the website. This is displayed 
#' as the link to the home page.
#' 
#' @return (\code{character} of length 1) The html code to make hierarchical menu
make_hierarchy_html <- function(hierarchy, page_paths, site_name = "Home") {
  # Parse directory names ---------------------------------------------------------------------
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
  site_name_html <- paste(sep = "\n",
                              '<div class="navbar-header">',
                              '<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">',
                              '<span class="sr-only">Toggle navigation</span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '</button>',
                              paste0('<a class="navbar-brand" href="', home, '">', site_name, '</a>'),
                              '</div>')
  menu_html <- paste(sep = "\n",
                     '<div class="collapse navbar-collapse">',
                     '<ul class="nav navbar-nav">',
                     paste0(lapply(which(depth == 1), make_nav), collapse = ""),
                     '</ul></div>')
  nav_bar_html <- paste( sep = "\n",
                         '<div class="navbar navbar-default" role="navigation">',
                         '<div class="container">',
                         site_name_html,
                         menu_html,
                         '</div>',
                         '</div>')
  return(nav_bar_html)
}


#===================================================================================================
#' Get paths to content
#' 
#' Return the absolute paths to content files in a given directory.
#' 
#' @param path (\code{character}) One or more directories in which to look for content files.
#' @param type (\code{character}) One or more content file extensions to search for.
#' @param full_names (\code{logical} of length 1) See \code{\link{list.files}} help for option
#'   \code{full.names}.
#' @param simplify (\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
#' with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
#' single \code{character} vector is returned. 
#' 
#' @return Depends on the \code{simplify} option.
get_content_files <- function(path, type = c("html"), full_names = TRUE, simplify = TRUE) {
  # If nothing is given, return the same ----------------------------------------------------------
  if (length(path) == 0) return(path)
  process_one <- function(path) {
    # Make paths absolute ----------------------------------------------------------------------------
    path <- normalizePath(path)
    # Make regular expression for file extensions ----------------------------------------------------
    file_regex <- paste0(paste("\\.", type, collapse = "|", sep = ""), "$")
    # Search for files with matching extension -------------------------------------------------------
    content_paths <- list.files(path, pattern = file_regex, all.files = TRUE, recursive = TRUE,
                            ignore.case = TRUE, full.names = full_names)
    # Remove files with same name but different extensions -------------------------------------------
    another_has_precedence <- function(a_path) {
      get_precedence <- function(another_path) { 
        which(tolower(tools::file_ext(another_path)) == tolower(type))
      }
      this_precedence <- get_precedence(a_path)
      other_precedence <- vapply(content_paths, get_precedence, numeric(1))
      any(this_precedence > other_precedence &
            tools::file_path_sans_ext(a_path) == tools::file_path_sans_ext(content_paths))
    }
    content_paths <- content_paths[!vapply(content_paths, another_has_precedence, logical(1))]
    
  }
  content_paths <- lapply(path, process_one)
   # Simplify if specified --------------------------------------------------------------------------
  if (simplify) { content_paths <- unlist(content_paths) }
  return(content_paths)
}






#===================================================================================================
#' Get content hierarchy
#' 
#' Return a list of all the locations in the classification hierarchy.
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
get_hierarchy <- function(path, root, cumulative = TRUE, use_file_names = TRUE,
                               use_dir_names = TRUE, use_config_files = TRUE, name_sep = "-",
                               use_file_suffix = FALSE, use_dir_suffix = TRUE,
                               note_config_name = "placement.yml") {
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
      if (length(hierarchy) != 0) { #If it has not been removed from the hierarchy via config file
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
      }
      # Apply configuration file effects - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (use_config_files) {
        rel_path <- strsplit(gsub(paste0("^", root), "", dirname(current_path)), .Platform$file.sep)[[1]]
        if (root == dirname(current_path)) rel_path <- ""
        config_locations <- lapply(1:length(rel_path), function(i) rel_path[1:i])
        config_locations <- sapply(config_locations, function(x) do.call(file.path, as.list(x)))
        config_locations <- gsub(paste0("^", .Platform$file.sep), "", config_locations)
        for (config_location in config_locations) {
          if (config_location != "") 
            config_path <- file.path(root, config_location, note_config_name)
          else 
            config_path <- file.path(root, note_config_name)
          if (file.exists(config_path)) {
            config <- yaml::yaml.load_file(config_path)
            for (pattern in names(config)) {
              if (config_location != "") 
                matches <- Sys.glob(file.path(root, config_location, pattern))
              else 
                matches <- Sys.glob(file.path(root, pattern))
              if (current_path %in% matches || path %in% matches) {
                if (is.null(config[[pattern]][1])) {
                  hierarchy <- list()
                  addition <- NULL
                } else if (config[[pattern]][1] == ".") {
                  if (length(config[[pattern]]) > 1)
                    addition <- config[[pattern]][2:length(config[[pattern]])]
                } else {
                  hierarchy <-  list(character(0))
                  addition <- config[[pattern]]
                  addition <- addition[addition != ""]
                }
              }
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
  parent_html <- make_parent_html(files = files, titles = NA,
                                  rmd_header = list(), apply_theme = apply_theme)
  cat(parent_html, file = master_rmd_path, append = FALSE)
  rmarkdown::render(master_rmd_path, quiet = TRUE)
}


#===================================================================================================
#' Make a website from a directory
#' 
#' Makes a website from the contents of a directory.
#' Currently, only html and Rmd files are included.
#' All of the content will be copied and converted to html to make the website. 
#' 
#' 
#' @param path (\code{character}) One or more directories in which to look for  files.
#' @param output (\code{character} of length 1) Location to write the output directory. The website
#' will be made in a directory called "website" in this location. If \code{NULL} or \code{NA}, make
#' the website in a temporary directory.
#' @param type (\code{character}) One or more file types to include in the output, specified using 
#' file extensions without the leading dot (e.g. \code{type = c("rmd", "html")}). The order file
#' types are given in indicates which file is used in the case that files have the same name
#' but different extensions. For example, if \code{type = c("html", "rmd")} and there is a note.html
#' and a note.Rmd in the same directory, the note.html will be used and the note.Rmd will be
#' ignored. 
#' @param name (\code{character} of length 1) The name on the link to the homepage of the website. 
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
#'  content.
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
#' @param menu_name_parser (\code{function}) Defines a function to apply to each name in the menu hierarchy
#' to chenge it somehow. The function must take a single \code{character} input and output a single
#' \code{character}. 
#' @param note_config_name (\code{character} of length 1) The name of content placement configuration files.
#' @param site_config_name (\code{character} of length 1) The name of the website building configuration file.
#' It does not need to exist, but if it does in a directory specified by \code{site_config_file}, it is used
#' automatically. To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' @param site_config_file (\code{character} of length 1) The path to a configuration file specifing
#'  this function's option values or to the directory is is located in. The file should be in YAML format.
#'  To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' @param output_dir_name (\code{character} of length 1) The name of the output directory.
#' @param partial_copy (\code{logical} of length 1) If \code{FALSE}, The entire target directory 
#' of the notes will be copied instead of just the notes and their dependencies. It is possible that more than
#' just the target directory will be copied if there are files in the target directory with dependencies outside
#' it. Enough of the file structure will be copied to included all dependencies. 
#' @param open (\code{logical} of length 1) If \code{TRUE}, open the newly created website in an 
#' internet browser.
#'   
#' @return (\code{character} of length 1) The file path to the created websites home page 
#' (\code{index.html})
#' 
#' @export
quilt <- function(path = getwd(), output = NULL, type = c("html", "rmd"), name = "Home",
                  clean = TRUE, overwrite = FALSE,
                  theme = "journal", apply_theme = TRUE, cumulative = FALSE, use_file_names = TRUE,
                  use_dir_names = TRUE, use_config_files = TRUE, name_sep = "-",
                  use_file_suffix = FALSE, use_dir_suffix = TRUE, menu_name_parser = NULL,
                  note_config_name = "placement.yml", site_config_name = "website_build_config.yml", 
                  site_config_file = path, output_dir_name = "website", partial_copy = TRUE,
                  open = TRUE) {
  # Read any configuration files -------------------------------------------------------------------
  argument_names <- names(as.list(args(quilt)))
  argument_names <- argument_names[-length(argument_names)]
  arg_missing <- eval(c(missing(path), missing(output), missing(type), missing(name), missing(clean),
                        missing(overwrite), missing(theme), missing(apply_theme), missing(cumulative),
                        missing(use_file_names), missing(use_dir_names), missing(use_config_files),
                        missing(name_sep), missing(use_file_suffix), missing(use_dir_suffix),
                        missing(menu_name_parser), missing(note_config_name), missing(site_config_name),
                        missing(site_config_file), missing(output_dir_name), missing(partial_copy),
                        missing(open)))
  names(arg_missing) <- argument_names
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
        if (arg_missing[names(config_data)[i]]) assign(x = names(config_data)[i], value = config_data[[i]])
    }
  }
 # Parse arguments --------------------------------------------------------------------------------
 path <- normalizePath(path)
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
  target_paths <- get_content_files(path, type = type)
  if (length(target_paths) == 0) stop(paste0("No HTML files found in '", path, "'"))
  # Filter for notes in hirearchy ------------------------------------------------------------------
  classification <- get_hierarchy(target_paths, root = path, cumulative = cumulative, 
                                       use_file_names = use_file_names, 
                                       use_dir_names = use_dir_names, 
                                       use_config_files = use_config_files, name_sep = name_sep,
                                       use_file_suffix = use_file_suffix, 
                                       use_dir_suffix = use_dir_suffix,
                                       note_config_name = note_config_name)
  if (!is.null(menu_name_parser)) classification <- rapply(classification, menu_name_parser, how = "list")
  classification_paths <- rep(target_paths, vapply(classification, length, integer(1)))
  ul_classification <- unlist(classification, recursive = FALSE)
  hierarchy_class <- unique(ul_classification)
  if (!0 %in% sapply(hierarchy_class, length))
    hierarchy_class <- c(list(character(0)), hierarchy_class)
  hierarchy <- lapply(hierarchy_class,
                      function(x) classification_paths[vapply(ul_classification, 
                                                         identical, y = x, logical(1))])
  target_paths <- unique(unlist(hierarchy))
  classification <- get_hierarchy(target_paths, root = path, cumulative = cumulative, 
                                       use_file_names = use_file_names, 
                                       use_dir_names = use_dir_names, 
                                       use_config_files = use_config_files, name_sep = name_sep,
                                       use_file_suffix = use_file_suffix, 
                                       use_dir_suffix = use_dir_suffix,
                                       note_config_name = note_config_name)
  if (!is.null(menu_name_parser)) {
    classification <- rapply(classification, menu_name_parser, how = "list")
  }
  # Make output directory --------------------------------------------------------------------------
  dir.create(output_path)
  dir.create(content_path)  
  # Copy content directory ----------------------------------------------------------------------------
  content_copy_path <- render_files_to_html(target_paths, content_path, partial_copy = partial_copy)
  # Make website menu ------------------------------------------------------------------------------
  page_names <- vapply(hierarchy_class, paste, character(1), collapse = "-")
  page_names[page_names == ""] <- "index"
  page_names <- gsub(" ", "_", page_names)
  page_rmd_names <- paste0(page_names, ".Rmd")
  page_html_names <- paste0(page_names, ".html")
  page_html_paths <- file.path(output_path, page_html_names)
  pre_body_html_path <- file.path(output_path, "before_body.html")
  cat(make_hierarchy_html(hierarchy_class, page_html_names, site_name = name), file = pre_body_html_path)
  # Make other dependencies ------------------------------------------------------------------------
  dependencies <- vapply(c("in_header.html", "after_body.html"),
                         function(x) system.file("file_templates", x, package = "quiltr"), 
                         character(1))
  file.copy(from = dependencies, to = output_path)
  output_yaml_path <- file.path(output_path, "_output.yaml")
  cat(make_output_yaml(theme = theme), file = output_yaml_path)
  # Step up clean up -------------------------------------------------------------------------------
  if (clean) {
    files_to_remove <- file.path(output_path, c("in_header.html", "after_body.html",
                                           "before_body.html","_output.yaml",
                                           page_rmd_names))
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  }  
  # Make website pages -----------------------------------------------------------------------------
  relative_copy_path <- gsub(pattern = paste0("^", output_path, .Platform$file.sep), "", content_copy_path)
  relative_copy_class_path <- rep(relative_copy_path, vapply(classification, length, integer(1)))
  hierarchy <- lapply(hierarchy_class,
                      function(x) relative_copy_class_path[vapply(ul_classification, 
                                                                  identical, y = x, logical(1))])
  home_path <- mapply(make_master_rmd, page_rmd_names, hierarchy, location = output_path, 
                      apply_theme = apply_theme, clean = clean)[["index.Rmd"]]
  # Make configuration file ------------------------------------------------------------------------
  new_config_path <- file.path(output_path, site_config_name)
  arguments <- mget(argument_names)
  cat(yaml::as.yaml(arguments), file = new_config_path)
  # Open new website -------------------------------------------------------------------------------
  if (rstudioapi::isAvailable() && open) rstudioapi::viewer(home_path)
  return(home_path)
}
