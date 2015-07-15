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
#' @param q_opt (\code{function}) The function used to get context-specific option values
#' 
#' @return (\code{character} of length 1) The Rmd code to display the content provided. 
make_parent_html <- function(files, titles = NA, rmd_header = NULL, q_opt) {
  # Validate arguments -----------------------------------------------------------------------------
  if (!is.na(titles) && length(files) != length(titles)) {
    stop("Arguments `files` and `title` must the same length")
  }
  if (any(!tools::file_ext(files) %in% c("Rmd", "rmd", "md", "html"))) {
    stop("Only files with `Rmd` or `html` extensions can be used.")
  }
  # Define functions to make components ------------------------------------------------------------
  make_rmd_header <- function(rmd_header) {
    if (length(rmd_header > 0))
      paste0('---\n', yaml::as.yaml(rmd_header), "---\n\n")
    else
      ""
  }
  make_iframe_code <- function(file, original, count) {
    on_load <- paste0('autoResize(\'iframe', count, '\'); export_links(\'iframe', count, '\');')
    if (q_opt(original, "apply_theme")) {
      on_load <- paste0('apply_theme(\'iframe', count, '\'); ', on_load)
    }
    iframe_att <- paste0('width="100%" height="0px" id="iframe', count,
                         '" marginheight="0" frameborder="0" onLoad="', on_load, '"')
    paste0('<iframe src="', file, '" ', iframe_att, '></iframe>\n\n')
  }
  make_child_rmd_code <- function(file) {
    paste0("```{r child = '", file, "'}\n```\n\n")
  }
  make_code <- function(file, original, title, count) {
    if (!is.na(title)) title_code <- paste0("## ", title, "\n\n") else title_code <- ""
    if (tools::file_ext(file) %in% c("Rmd", "rmd", "md")) {
      return(paste0(title_code, make_child_rmd_code(file)))
    } else if (tools::file_ext(file) %in% c("html")) {
      return(paste0(title_code, make_iframe_code(file, original, count)))
    } 
  }
  # Generate Rmd document ---------------------------------------------------------------------------
  if (length(files) > 0)
    iframe_code <- paste0(mapply(make_code, file = files, original = names(files), title = titles, count = 1:length(files)),
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
#' @param q_opt (\code{function}) The function used to get context-specific option values
#' @param full_names (\code{logical} of length 1) See \code{\link{list.files}} help for option
#'   \code{full.names}.
#' @param simplify (\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
#' with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
#' single \code{character} vector is returned. 
#' 
#' @return Depends on the \code{simplify} option.
get_content_files <- function(path, q_opt, full_names = TRUE, simplify = TRUE) {
  # If nothing is given, return the same ----------------------------------------------------------
  if (length(path) == 0) return(path)
  process_one <- function(path) {
    # Make paths absolute ----------------------------------------------------------------------------
    path <- normalizePath(path)
    # Search for files with matching extension -------------------------------------------------------
    content_paths <- list.files(path, all.files = TRUE, recursive = TRUE, full.names = full_names)
    is_included <- function(a_path) {
      file_regex <- paste0(paste("\\.", q_opt(a_path, "type"), "$", collapse = "|", sep = ""))
      grepl(pattern = file_regex, a_path, ignore.case = TRUE)
    }
    content_paths <- content_paths[vapply(content_paths, is_included, logical(1))]
    # Remove files with same name but different extensions -------------------------------------------
    another_has_precedence <- function(a_path) {
      get_precedence <- function(another_path) {
        precedence <- which(tolower(tools::file_ext(another_path)) == tolower(q_opt(a_path, "type")))
        if (length(precedence) == 0) { precedence <- 1000000 }
        return(precedence)
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
#' @param q_opt (\code{function}) The function used to get context-specific option values
#' @return (\code{list} of \code{character}) A list of locations in the notebook hierarchy 
#' corresponding to the input argument \code{path}.
get_hierarchy <- function(path, root, q_opt) {
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
        name_sep <- q_opt(current_path, "name_sep")
        # Apply directory name effects - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        if (index != length(path_hierarchy) && q_opt(current_path, "use_dir_names")) {
          addition <- basename(current_path)
          if (!is.null(name_sep) && !is.na(name_sep) && length(addition) > 0) 
            addition <- unlist(strsplit(addition, name_sep, fixed = TRUE))
          if (!q_opt(current_path, "use_dir_suffix"))
            addition <- addition[seq(1, length.out = length(addition) - 1)]
        }
        # Apply file name effects  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        if (index == length(path_hierarchy) && q_opt(current_path, "use_file_names")) {
          addition <- tools::file_path_sans_ext(basename(current_path))
          if (!is.null(name_sep) && !is.na(name_sep) && length(addition) > 0) 
            addition <- unlist(strsplit(addition, name_sep, fixed = TRUE))
          if (!q_opt(current_path, "use_file_suffix"))
            addition <- addition[seq(1, length.out = length(addition) - 1)]
        }
      }
      # Apply configuration file effects - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      config <- q_opt(current_path, "placement")
      if (is.null(config)) {
        hierarchy <- list()
        addition <- NULL
      } else if (length(config) > 0) {
        if (config[1] == ".") {
          if (length(config) > 1) {
            addition <- c(addition, config[2:length(config)])
          }
        } else if (config[1] == "..") {
          if (length(config) > 1) {
            addition <- config[2:length(config)]
          } else {
            addition <- NULL
          }
        } else {
          hierarchy <-  list(character(0))
          addition <- config
          addition <- addition[addition != ""]
        }          
      } 
      # Save resulting addition  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (q_opt(NULL, "cumulative"))
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
#' @param q_opt (\code{function}) The function used to get context-specific option values
#' 
#' @return (Named \code{character}) The file paths to created .html files named by their source Rmd
#' files.
make_master_rmd <- function(name, files, location, q_opt) {
  master_rmd_path <- file.path(location, name)
  if (q_opt(NULL, "clean")) files_to_remove <- master_rmd_path
  if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
  parent_html <- make_parent_html(files = files, titles = NA, rmd_header = list(), q_opt)
  cat(parent_html, file = master_rmd_path, append = FALSE)
  rmarkdown::render(master_rmd_path, quiet = TRUE)
}

