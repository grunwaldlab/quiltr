#===================================================================================================
#' Generate _output.yaml
#' 
#' Make the content of the file that contols knitting of the website page Rmds
#' 
#' @return (\code{character} of length 1)
make_output_yaml <- function() {
  yaml::as.yaml(list(html_document = list(self_contained = FALSE,
                                          theme = get_config("website_theme"),
                                          highlight = "textmate",
                                          toc = FALSE,
                                          lib_dir = "libs",
                                          includes = list(in_header = "in_header.html",
                                                          before_body = "before_body.html",
                                                          after_body = "after_body.html"))))
}



#===================================================================================================
#' Get note content file paths
#' 
#' Get the paths to note content that should be displayed
#' 
#' @param note_path (\code{character} of length 1) The path to a note.
#' 
get_note_content_files <- function(note_path) {
  list.files(path = note_path, pattern = "\\.html$", ignore.case = TRUE)
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
#' 
#' @return (\code{character} of length 1) The Rmd code to display the content provided. 
make_parent_html <- function(files, titles = NA, rmd_header = NULL) {
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
    if (get_config("theme_note_content"))  on_load <- paste0('apply_theme(\'iframe', count, '\'); ', on_load)
    iframe_att <- paste0('width="100%" height="200px" id="iframe', count,
    '" marginheight="0" frameborder="0" onLoad="', on_load, '"')
    paste0('<iframe src="', file, '" ', iframe_att, '></iframe>\n\n')
  }
  make_child_rmd_code <- function(file) {
    paste0("```{r child = '", rmd_names, "'}\n```\n\n")
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
#' Gets names of notes in a notebook
#' 
#' Returns the names/paths of notes in a given notebook.
#' 
#' @param notebook_path (\code{character} of length 1) The path to a notebook or one of its 
#'   subdirectories.
#' @param full_names (\code{logical} of length 1) If \code{TRUE}, return the full path to each note.
get_note_paths <- function(notebook_path = get_project_root(), full_names = FALSE) {
  notebook_path <- get_project_root(notebook_path)
  note_path <- file.path(notebook_path, "content")
  list.dirs(note_path, recursive = FALSE, full.names = full_names)  
}

#===================================================================================================
#' Get notebook note hierarchy
#' 
#' Return a list of all the locations in the notebook note classification hierarchy.
#' 
#' @param notebook_path (\code{character} of length 1) The path to a notebook or one of its 
#'   subdirectories.
#' @param implied (\code{logical} of length 1) If \code{TRUE}, return parts of the hierarchy that
#'   no notes are assigned to, but are implied to exist (e.g. the "roots" of the hierarchy).
#'   
#' @return (\code{list} of \code{character}) A list of locations in the notebook hierarchy.
get_note_hierarchy <- function(notebook_path = get_project_root(), implied = TRUE) {
  note_directories <- get_note_paths(notebook_path)
  hierarchy <- lapply(strsplit(note_directories, "-"), `[`, -1)
  if (implied) {
    hierarchy <- unlist(lapply(hierarchy, function(x) lapply(seq_along(x), function(i) x[1:i])),
                        recursive = FALSE)    
  }
  hierarchy <- unique(hierarchy)
  hierarchy <- hierarchy[order(sapply(hierarchy, `[`, 1))]
  return(hierarchy)
}




#===================================================================================================
#' Creates hierarchical menu html from note names
#' 
#' Parses the names of note directories to create html for a hierarchical menu. 
#' 
#' @param notebook_path (\code{character} of length 1) The path to a notebook or one of its 
#'   subdirectories.
make_menu_hierarchy <- function(notebook_path = get_project_root()) {
  # Parse note directory names ---------------------------------------------------------------------
  notebook_path <- get_project_root(notebook_path)
  notebook_name <- basename(notebook_path)
  hierarchy <- get_note_hierarchy(notebook_path)
  depth <- vapply(hierarchy, length, numeric(1))
  names <- vapply(get_note_hierarchy(notebook_path, implied = FALSE),
                  paste, character(1), collapse = "-")
  note_dir_names <- get_note_paths(notebook_path)
  
  # Recursive function to make menu html -----------------------------------------------------------
  make_nav <- function(index) {
    current <- hierarchy[[index]]
    children <- which(vapply(hierarchy, function(y) all(y[seq_along(current)] == current) & length(current) + 1 == length(y), logical(1)))
    if (paste(current, collapse = "-") %in% names) {
      dir_name <- note_dir_names[paste(current, collapse = "-") == names]
      path <- file.path("..", dir_name, paste0("master_parent", ".html"))
    } else {
      path <- "#"
    }
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
  notebook_name_html <- paste(sep = "\n",
                              '<div class="navbar-header">',
                              '<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">',
                              '<span class="sr-only">Toggle navigation</span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '<span class="icon-bar"></span>',
                              '</button>',
                              paste0('<a class="navbar-brand" href="#">', notebook_name, '</a>'),
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
#' Extract YAML attribute
#' 
#' Gets a given attribute, based on the key, from a YAML file. 
#' 
#' @param path (\code{character}) The path to a YAML file.
#' @param attribute (\code{character} of length 1) The key of the attribute to get.
#' @param default (\code{character} of length 1) the default to return of the key is not found.
get_rmd_yaml <- function(path, attribute, default = "") {
  do_once <- function(a_path) {
    content <- readChar(a_path, nchars = 10000)
    parsed_yaml <- yaml::yaml.load(stringr::str_match(content, "---\\\n(.*)---\\\n")[2])
    if (attribute %in% names(parsed_yaml)) return(parsed_yaml[[attribute]])
    return(as.character(default))
  }
  vapply(path, do_once, character(1)) 
}


#===================================================================================================
#' Concatenate and build Rmd files in a directory
#' 
#' Combines and builds all Rmd files in a directory for the purpose of making a website page.
#' 
#' @param directory_path (\code{character} of length 1) The path to the directory containing Rmd
#'   files to render. 
#' @param master_rmd_name (\code{character} of length 1) The name of the Rmd/html output file for
#'   each page. Should be different from any Rmd/html file in the notes.
#' @param clean (\code{logical}) Remove intermediate files afterwards
render_rmd_contents <- function(directory_path, master_rmd_name = "master_parent.Rmd",
                                clean = get_config("clean_website")) {
  note_files <- get_note_content_files(directory_path)

  # Copy dependencies into the current directory ---------------------------------------------------
  dependencies <- vapply(c("in_header.html", "after_body.html"),
                         function(x) system.file("file_templates", x, package = "labtools"), 
                         character(1))
  file.copy(from = dependencies, to = directory_path)
  pre_body_html_path <- file.path(directory_path, "before_body.html")
  output_yaml_path <- file.path(directory_path, "_output.yaml")
  cat(make_menu_hierarchy(file.path(get_project_root(), "content")), file = pre_body_html_path)
  cat(make_output_yaml(), file = output_yaml_path)
  if (clean) {
    files_to_remove <- file.path(directory_path, c("in_header.html", "after_body.html",
                                                   "before_body.html","_output.yaml"))
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  }
  
  # Create master Rmd that references the original files -------------------------------------------
  master_rmd_path <- file.path(directory_path, master_rmd_name)
  if (clean) files_to_remove <- c(files_to_remove, master_rmd_path)
  if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
  note_name <- basename(directory_path)
  date <- strsplit(note_name[1], '-')[[1]][1]
  date <- gsub("_", "/", date)
  note_title <-  rev(strsplit(note_name[1], '-')[[1]])[1]
  
  note_yaml <- list(title = note_title)
  parent_html <- make_parent_html(files = note_files, titles = NA,
                                  rmd_header = note_yaml)
  
  cat(parent_html, file = master_rmd_path, append = FALSE)
  rmarkdown::render(master_rmd_path)
}


#===================================================================================================
#' Make a website from notes
#' 
#' Makes a website from a directory containing correctly formatted notes. 
#' Each note must be a directory with one or more Rmd files. 
#' All of the notes will be copied and built to make the website. 
#' The hierarchy of the website menu will be made using the note directory names. 
#' 
#' @param notes_location (\code{character} of length 1) The path to the directory containing note directories. 
#' @param site_location (\code{character} of length 1) The path to where the website will be made.
#' @param master_rmd_name (\code{character} of length 1) The name of the Rmd/html output file for
#'   each page. Should be different from any Rmd/html file in the notes.
#'   
#' @export
make_website <- function(notes_location =  file.path(get_project_root(), "content"),
                         site_location = file.path(get_project_root(), "doc"),
                         master_rmd_name = "master_parent.Rmd") {
  set.seed(100)
  
  # Copy notes to website location -----------------------------------------------------------------
  site_path <- file.path(site_location, "website")
  note_directories <- list.dirs(notes_location, recursive = FALSE)
  if (!file.exists(site_location)) dir.create(site_location, recursive = TRUE)
  if (file.exists(site_path)) unlink(site_path, recursive = TRUE)
  dir.create(site_path, recursive = TRUE)
  file.copy(note_directories, site_path, overwrite = TRUE, recursive = TRUE)
  website_directories <- list.dirs(site_path, recursive = FALSE)
  
  # Render website ---------------------------------------------------------------------------------
  lapply(website_directories, render_rmd_contents, master_rmd_name = master_rmd_name)
}