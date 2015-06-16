#===================================================================================================
#' Convert a set of files to html
#' 
#' Render a selection of files into HTML, preserving their relative directory structure.
#' Target files will be converted to HTML if they are not already HTML. 
#' The original files and any files the HTML representations reference will be copied to a new
#' location.
#' Enough of the directory structure will be copied to allow files to have to same paths relative
#' to eachother as the original files.
#' 
#' @param from (\code{character}) The paths to content files to copy.
#' @param to (\code{character} of length 1) The path to where the content files and their dependencies 
#' will be copied.
#' @param copy_depend (\code{logical} of length 1) If \code{FALSE}, dependencies will not be 
#' copied.
#' @param partial_copy (\code{logical} of length 1) If \code{FALSE}, The entire root directory 
#' of the content files will be copied instead of just the content files and their dependencies.
#' 
#' @return (\code{character}) Paths of where the content files were copied to.
render_files_to_html <- function(from, to, copy_depend = TRUE, partial_copy = TRUE) {
  # Make input file paths absolute -----------------------------------------------------------------
  from_path <- normalizePath(from)
  to <- normalizePath(to)
  # Convert files to html --------------------------------------------------------------------------
  get_converters <- function() {
    function_names <- get_function("quiltr", "^quiltr_convert_.*_to_html$")
    converters <- mget(function_names, inherits = TRUE)
    names(converters) <- tolower(stringr::str_match(function_names,
                                                    "^quiltr_convert_(.*)_to_html$")[, 2])
    return(converters)
  }
  converters <- get_converters()
  extensions <- unique(tolower(tools::file_ext(from)))
  unsupported_extensions <- extensions[!extensions %in% names(converters)]
  if (length(unsupported_extensions) > 0) {
    stop(paste0("The following file types are not supported by quiltr: ",
                paste(unsupported_extensions, collapse = ", "), "\n", 
                "Change the value of the 'type' option of 'quilt' or remove unsupported files."))
  }
  convert <- function(input) { converters[[tolower(tools::file_ext(input))]](input) }
  converted_paths <- vapply(from, convert, character(1))
  on.exit(file.remove(converted_paths))
  # Get dependencies of input files ----------------------------------------------------------------
  if (copy_depend) {
    depend_from <- get_file_dependencies(converted_paths, context = from, simplify = TRUE)
    from_path <- c(from_path, depend_from)
  }
  # Determine location to copy files to ------------------------------------------------------------
  from_root <- get_common_dir(from_path)
  to_path <- file.path(to, gsub(paste0("^", dirname(from_root), .Platform$file.sep), "", from_path))
  html_to_path <- file.path(to, gsub(paste0("^", dirname(from_root), .Platform$file.sep), "", normalizePath(from)))
  html_to_path <- paste0(tools::file_path_sans_ext(html_to_path), ".html")
  # Copy directory structure -----------------------------------------------------------------------
  if (partial_copy) {
    for (dir_to_make in unique(dirname(to_path))) 
      if (!file.exists(dir_to_make)) dir.create(dir_to_make, recursive = TRUE)
  } else {
    file.copy(from_root, to, recursive = TRUE)
  }
  # Copy html file renderings ----------------------------------------------------------------------
  invisible(file.copy(from = converted_paths, to = html_to_path, overwrite = TRUE))
  # Copy original files and dependencies -----------------------------------------------------------
  invisible(file.copy(from = from_path, to = to_path, overwrite = TRUE))
  # Return the locations of input file copies ------------------------------------------------------
  html_to_path[1:length(from)]
}



#===================================================================================================
#' Get content file dependencies
#' 
#' Return the absolute paths of content file dependencies files. Currently, only \code{.html} files are
#' implemented.
#' 
#' @param path (\code{character}) One or more content file files in which to look for references to
#'   other files.
#' @param context (\code{character}) Working directory used when inferring relative dependency
#' paths. Corresponds to \code{path}.
#' @param simplify (\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
#' with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
#' single \code{character} vector is returned. 
#' 
#' @return Depends on the \code{simplify} option.
get_file_dependencies <- function(path, context = path, simplify = FALSE) {
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
  output <- mapply(standardize_path, output, context, SIMPLIFY = FALSE)
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
  excluded_dependencies <- c("^data:", "^https:", "^http:", "^mailto:", "^#")
  # define function to process a single html file --------------------------------------------------
  get_dependency <- function(path) {
    # Extract values of tag attributes - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    html <- XML::htmlParse(path)
    output <- unlist(lapply(xpath_tags, XML::xpathSApply, doc = html))
    # Remove values that are content local file paths - - - - - - - - - - - - - - - - - - - - - - - - -
    for (pattern in excluded_dependencies) output <- output[!grepl(pattern, output)]
    if (is.null(output)) output <- character(0)
    return(output)
  }
  # process all html files -------------------------------------------------------------------------
  lapply(path, get_dependency)
}


