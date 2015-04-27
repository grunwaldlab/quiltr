#===================================================================================================
#' Creates hierarchical menu html from note names
#' 
#' Parses the names of note directories to create html for a hierarchical menu. 
#' 
#' @param notes_location (\code{character} of length 1) File path to the directory containing note
#'   directories. 
make_menu_hierarchy <- function(notes_location) {
  # Parse note directory names ---------------------------------------------------------------------
  note_directories <- Sys.glob(file.path(notes_location,"*"))
  note_names <- basename(note_directories) 
  hierarchy <- lapply(strsplit(note_names, "-"), `[`, -1)
  names <- vapply(hierarchy, paste, character(1), collapse = "-")
  hierarchy <- unique(unlist(lapply(hierarchy, function(x) lapply(seq_along(x), function(i) x[1:i])), recursive = FALSE))
  hierarchy <- hierarchy[order(sapply(hierarchy, `[`, 1))]
  depth <- vapply(hierarchy, length, numeric(1))
  
  # Recursive function to make menu html -----------------------------------------------------------
  make_nav <- function(index) {
    current <- hierarchy[[index]]
    children <- which(vapply(hierarchy, function(y) all(y[seq_along(current)] == current) & length(current) + 1 == length(y), logical(1)))
    if (paste(current, collapse = "-") %in% names) {
      dir_name <- note_names[paste(current, collapse = "-") == names]
      path <- file.path("..", dir_name, paste0("master_parent", ".html"))
    } else {
      path <- "#"
    }
    name <- gsub("_", " ", current[length(current)], fixed = TRUE)
    out <- ""
    if (length(current) == 1) {
      if  (length(children) == 0) {
        out <- paste0(out, '<div class="dropdown" style="position:relative;float:left"><a href="', path,'" class="btn btn-default">', name, '</a>\n<ul class="dropdown-menu">')
      } else {
        out <- paste0(out, '<div class="dropdown" style="position:relative;float:left"><a href="', path,'" class="btn btn-default dropdown-toggle" data-toggle="dropdown">', name, '<span class="caret"></span></a>\n<ul class="dropdown-menu">')
        child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
        out <- paste0(out, child_html)
      }
      out <- paste0(out, "</ul></div>")
    } else if (length(children) == 0) {
      out <- paste0(out, '<li><a href="', path, '">', name, '</a></li>')
    } else {
      out <- paste0(out, '<li><a class="trigger right-caret">', name, '</a>\n<ul class="dropdown-menu sub-menu">')
      child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
      out <- paste0(out, child_html)
      out <- paste0(out, "</ul></li>")
    }
    return(out)
  }
  
  # Make menu --------------------------------------------------------------------------------------
  menu_html <- paste0(paste0(lapply(which(depth == 1), make_nav), collapse = ""),
                      '<div style="clear:both"></div>')
  # home_button <- '<div class="dropdown" style="position:relative;float:left"><a href="../index/index.html" class="btn btn-default">Home</a></div>'
  return(menu_html)
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
render_rmd_contents <- function(directory_path, header_html = NULL, pre_body_html = NULL,
                                      post_body_html = NULL, output_yaml = NULL,
                                      master_rmd_name = "master_parent.Rmd") {
  # Copy dependencies into the current directory - - - - - - - - - - - - - - - - - - - - - - - - - -
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
  files_to_remove <- names(dependencies)
  on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
  
  # Create master Rmd that references the original files - - - - - - - - - - - - - - - - - - - - - -
  master_rmd_path <- file.path(directory_path, master_rmd_name)
  files_to_remove <- c(files_to_remove, master_rmd_path)
  if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
  note_name <- basename(directory_path)
  date <- strsplit(note_name[1], '-')[[1]][1]
  date <- gsub("_", "/", date)
  rmd_paths <- Sys.glob(file.path(directory_path,"*.Rmd"))
  rmd_names <- basename(rmd_paths)
  rmd_titles <- get_rmd_yaml(rmd_paths, "title", default = NA)
  rmd_titles[is.na(rmd_titles)] <- rmd_names[is.na(rmd_titles)]
  chunks <- paste0("```{r child = '", rmd_names, "'}\n```\n\n")
  if (length(rmd_paths) > 1) {
    chunks <- paste0("# ", rmd_titles, "\n\n", chunks)
  }
  if (length(rmd_paths) > 1 || (length(rmd_paths) == 1 && is.na(rmd_titles[1]))) {
    title <- rev(strsplit(note_name[1], '-')[[1]])[1]
    title <- gsub("_", " ", title)
#     title <- Hmisc::capitalize(title)
  } else {
    title <- rmd_titles[1]
  }
  header <- paste0('---\ntitle: "', title, '"\ndate: "', date, '"\n---\n\n')
  chunks <- paste0(chunks, collapse = "")
  cat(paste0(header, chunks), file = master_rmd_path, append = FALSE)
  html_paths <- gsub("\\.Rmd$", ".html", rmd_paths)
  files_to_remove <- c(files_to_remove, rmd_paths, html_paths)
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
  note_directories <- Sys.glob(file.path(notes_location,"*"))
  if (!file.exists(site_location)) dir.create(site_location, recursive = TRUE)
  if (file.exists(site_path)) unlink(site_path, recursive = TRUE)
  dir.create(site_path, recursive = TRUE)
  file.copy(note_directories, site_path, overwrite = TRUE, recursive = TRUE)
  website_directories <- Sys.glob(file.path(site_path,"*"))
  website_directories <- website_directories[file.info(website_directories)$isdir] #only use directories
  
  # Render website ---------------------------------------------------------------------------------
  if (is.null(pre_body_html)) pre_body_html <- make_menu_hierarchy(notes_location)
  lapply(website_directories, render_rmd_contents,
         header_html = header_html,
         pre_body_html = pre_body_html, 
         post_body_html = post_body_html,
         output_yaml = output_yaml,
         master_rmd_name = master_rmd_name)
}