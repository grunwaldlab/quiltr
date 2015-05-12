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
    iframe_att <- paste0('width="100%" height="200px" id="iframe', count,
    '" marginheight="0" frameborder="0" onLoad="autoResize(\'iframe', count, '\');"')
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
#' @param header_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put in the header of each page of the website.
#' @param pre_body_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put before the body of each page of the website.
#' @param post_body_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put after the body of each page of the website.
#' @param output_yaml (\code{character} of length 1) Rmarkdown settings for building Rmd files in 
#'   YAML format. Can be a file path. 
#' @param master_rmd_name (\code{character} of length 1) The name of the Rmd/html output file for
#'   each page. Should be different from any Rmd/html file in the notes.
#' @param clean (\code{logical}) Remove intermediate files afterwards
render_rmd_contents <- function(directory_path, header_html = NULL, pre_body_html = NULL,
                                post_body_html = NULL, output_yaml = NULL,
                                master_rmd_name = "master_parent.Rmd", clean = FALSE) {
  note_files <- get_note_content_files(directory_path)

  # Copy dependencies into the current directory ---------------------------------------------------
  copy_file_or_text <- function(input, output_path) {
    if (is.null(input)) input <- ""
    if (file.exists(input)) {
      file.copy(from = input, output_path)  
    } else {
      writeChar(input, con = output_path)
    }
  }
  dependencies <- list("in_header.html" = header_html,
                       "before_body.html" = pre_body_html,
                       "after_body.html" = post_body_html,
                       "_output.yaml" = output_yaml)
  names(dependencies) <- file.path(directory_path, names(dependencies))
  mapply(copy_file_or_text, dependencies, names(dependencies))
  if (clean) {
    files_to_remove <- names(dependencies)
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
#' @param header_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put in the header of each page of the website.
#' @param pre_body_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put before the body of each page of the website.
#' @param post_body_html (\code{character} of length 1) Html code or a file path to html code that will
#'   put after the body of each page of the website.
#' @param output_yaml (\code{character} of length 1) Rmarkdown settings for building Rmd files in 
#'   YAML format. Can be a file path. 
#' @param master_rmd_name (\code{character} of length 1) The name of the Rmd/html output file for
#'   each page. Should be different from any Rmd/html file in the notes.
#'   
#' @export
make_website <- function(notes_location, site_location, header_html = NULL, pre_body_html = NULL,
                         post_body_html = NULL, output_yaml = NULL,
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
  if (is.null(pre_body_html)) pre_body_html <- make_menu_hierarchy(notes_location)
  lapply(website_directories, render_rmd_contents,
         header_html = header_html,
         pre_body_html = pre_body_html, 
         post_body_html = post_body_html,
         output_yaml = output_yaml,
         master_rmd_name = master_rmd_name)
}