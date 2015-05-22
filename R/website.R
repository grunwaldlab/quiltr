#===================================================================================================
#' Generate _output.yaml
#' 
#' Make the content of the file that contols knitting of the website page Rmds
#' 
#' @return (\code{character} of length 1)
make_output_yaml <- function() {
  yaml::as.yaml(list(html_document = list(self_contained = FALSE,
                                          theme = "journal",
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
    if (FALSE)  on_load <- paste0('apply_theme(\'iframe', count, '\'); ', on_load)
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
make_hierarchy_html <- function(hierarchy, page_paths, notebook_name = "Notebook") {
  # Parse note directory names ---------------------------------------------------------------------
  depth <- vapply(hierarchy, length, numeric(1))
  # Recursive function to make menu html -----------------------------------------------------------
  make_nav <- function(index) {
    current <- hierarchy[[index]]
    children <- which(vapply(hierarchy, function(y) all(y[seq_along(current)] == current) & length(current) + 1 == length(y), logical(1)))
    path <-  page_paths[index]
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
  home <- page_paths[depth == 0]
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
                                clean = FALSE) {
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




get_dependencies <- function(paths, simplify = TRUE) {
  get_dependency <- function(path) {
    original_wd <- getwd()
    on.exit(setwd(original_wd))
    setwd(dirname(path))
    html <- XML::htmlParse(path)
    output <- c(XML::xpathSApply(html,  "//@src"),
                XML::xpathSApply(html,  "//@href"))
    output <- unlist(output)
    output <- output[!grepl("^data:", output)] #remove embedded data, e.g. images
    output <- output[!grepl("^https:", output)] #remove links to webpages
    output <- output[!grepl("^http:", output)] #remove links to webpages
    if (length(output) > 0) output <- normalizePath(output[file.exists(output)])
    return(output)
  }
  output <- lapply(paths, get_dependency)
  if (simplify) output <- unlist(output)
  return(output)
}


#` http://rosettacode.org/wiki/Find_common_directory_path
get_common_dir <- function(paths, delim = .Platform$file.sep)
{
  path_chunks <- strsplit(paths, delim)
  
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)
  
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
    return(path)
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
  return(output)
}


#===================================================================================================
#' Copy notes and their dependencies
#' 
#' Copy note files and any files they reference to a new location. 
#' Enough of the directory structure will be copied to contain all the files copies in their
#' original configuration. 
#' 
#' @param path (\code{character}) The paths to notes to copy.
#' @param output (\code{character} of length 1) The path to where the notes and their dependencies 
#' will be copied.
#' 
#' @return (\code{character}) Paths of where the notes were copied to.
copy_note_directory <- function(path, output) {
  
}


#===================================================================================================
#' Get notebook note hierarchy
#' 
#' Return a list of all the locations in the notebook note classification hierarchy.
#' 
#' @param path (\code{character}) The paths to notes to assign hirearchical classifications to.
#' @param root (\code{character} of length 1) The path to the root directory of the notebook.
#'   
#' @return (\code{list} of \code{character}) A list of locations in the notebook hierarchy 
#' corresponding to the input argument \code{path}.
get_note_hierarchy <- function(path, root) {
  
}

#===================================================================================================
#' Make a website from notes
#' 
#' Makes a website from a directory containing correctly formatted notes. 
#' Each note must be a directory with one or more Rmd files. 
#' All of the notes will be copied and built to make the website. 
#' 
#'   
#' TODO: make option to accept Rmd and possible other note types.
#' TODO: let notes occur  in multiple places in the hierarchy
#' @export
make_website <- function(target, output, use_file_names = TRUE, use_dir_names = TRUE, use_config_files = TRUE, 
                             overwrite = FALSE, name_sep = "-", config_name = ".notebook", clean = FALSE) {
  # Parse arguments --------------------------------------------------------------------------------
  target <- normalizePath(target)
  output <- normalizePath(output)
  # Get note files ---------------------------------------------------------------------------------
  note_paths <- get_note_files(target)
  # Get note dependencies
  note_dependencies <- get_dependencies(note_paths)
  config_regex <- gsub(".", "\\.", paste0(config_name, "$"), fixed = TRUE)
  config_files <- list.files(target, config_regex,  all.files = TRUE, recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  note_dependencies <- c(note_dependencies, config_files)
  # Copy note directory
  dependency_root <- get_common_dir(note_dependencies)
  if (nchar(dependency_root) > nchar(target)) dependency_root <- target
  output <- file.path(output, "website")
  content <- file.path(output, "content")
  note_destinations <- file.path(content,
                                 gsub(paste0("^", dirname(dependency_root), .Platform$file.sep), "", note_paths))
  dep_destinations <- file.path(content,
                                gsub(paste0("^", dirname(dependency_root), .Platform$file.sep), "", note_dependencies))
  files_to_copy_from <- c(note_dependencies, note_paths)
  files_to_copy_to <- c(dep_destinations, note_destinations)
  if (file.exists(output)) {
    if (overwrite)
      unlink(output, recursive = TRUE) else
        stop("Website exsits at ", output, ". Use `overwrite = TRUE` to replace.")
  }
  for (dir_to_make in unique(dirname(files_to_copy_to))) 
    if (!file.exists(dir_to_make)) dir.create(dir_to_make, recursive = TRUE)
  invisible(file.copy(from = files_to_copy_from, to = files_to_copy_to, overwrite = TRUE))
  # Get website hierarchy
  get_hierarchy <- function(path, root) {
    path_hierarchy <- strsplit(gsub(paste0("^", root, .Platform$file.sep), "", path),
                          .Platform$file.sep,
                          fixed = TRUE)[[1]]
    hierarchy <- character(0)
    for (index in 1:length(path_hierarchy)) {
      addition <- character(0)
      current_path <- do.call(file.path, as.list(c(root, path_hierarchy[0:index])))
      if (index != length(path_hierarchy) && use_dir_names)
        addition <- basename(current_path)
      if (index == length(path_hierarchy) && use_file_names)
        addition <- tools::file_path_sans_ext(basename(current_path))
      if (!is.na(name_sep) && !is.null(name_sep) && length(addition) > 0) 
        addition <- unlist(strsplit(addition, name_sep, fixed = TRUE))
      config_path <- file.path(dirname(current_path), config_name)
      if (use_config_files && file.exists(config_path)) {
        config <- yaml::yaml.load_file(config_path)
        for (pattern in names(config)) {
          matches <- Sys.glob(file.path(dirname(current_path), pattern))
          if (current_path %in% matches) {
            if (config[[pattern]][1] == ".") {
              if (length(config[[pattern]]) > 1) addition <- config[[pattern]][2:length(config[[pattern]])]
            } else {
              hierarchy <-  config[[pattern]]
              addition <- character(0)
            }
          }
        }
      }
      hierarchy <- c(hierarchy, addition)
    }
    return(hierarchy)
  }
  
  hierarchy_root <- file.path(content, gsub(paste0("^", dirname(dependency_root), .Platform$file.sep), "", target))
  note_placement <- lapply(note_destinations, get_hierarchy, root = hierarchy_root)
  hierarchy <- unique(unlist(lapply(note_placement, function(x) lapply(seq_along(x), function(i) x[1:i])),
                      recursive = FALSE))
  hierarchy <- c(list(character(0)), hierarchy)
  assign_notes <- function(class) {
    if (length(class) == 0) return(note_destinations)
    bool <- (length(class) <= sapply(note_placement, length) &
               sapply(note_placement, function(x) all(x[1:length(class)] == class)))
    note_destinations[bool]
  }
  notes <- lapply(hierarchy, assign_notes)
  # Make website pages
  page_dir <- output
  if (!file.exists(page_dir)) dir.create(page_dir)
  page_names <- vapply(hierarchy, paste, character(1), collapse = "-")
  page_names[page_names == ""] <- "index"
  page_names <- gsub(" ", "_", page_names)
  page_rmd_names <- paste0(page_names, ".Rmd")
  page_rmd_paths <- file.path(page_dir, page_rmd_names)
  page_html_names <- paste0(page_names, ".html")
  page_html_paths <- file.path(page_dir, page_html_names)
  
  
  dependencies <- vapply(c("in_header.html", "after_body.html"),
                         function(x) system.file("file_templates", x, package = "labtools"), 
                         character(1))
  file.copy(from = dependencies, to = page_dir)
  pre_body_html_path <- file.path(page_dir, "before_body.html")
  output_yaml_path <- file.path(page_dir, "_output.yaml")
  cat(make_hierarchy_html(hierarchy, page_html_names), file = pre_body_html_path)
  cat(make_output_yaml(), file = output_yaml_path)
  if (clean) {
    files_to_remove <- file.path(page_dir, c("in_header.html", "after_body.html",
                                             "before_body.html","_output.yaml"))
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  }

  
  mapply(make_master_rmd, page_rmd_names, notes, location = page_dir)
  
}


make_master_rmd <- function(name, files, location, clean = FALSE) {
  master_rmd_path <- file.path(location, name)
  if (clean) files_to_remove <- master_rmd_path
  if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
  note_title <-  rev(strsplit(tools::file_path_sans_ext(name)[1], '-')[[1]])[1]
  note_yaml <- list(title = note_title)
  parent_html <- make_parent_html(files = files, titles = NA,
                                  rmd_header = note_yaml)
  cat(parent_html, file = master_rmd_path, append = FALSE)
  rmarkdown::render(master_rmd_path)
}
