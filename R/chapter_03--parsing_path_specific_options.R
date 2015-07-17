#| ## Option Parsing
#|
#| Perhaps the most important goal of `quiltr` is to place the fewest restrictions possible on input folder structure. 
#| This can be partly accomplished by having numerous options to change how folders are interpreted, such as an option to ignore file names.
#| However, any one set of option values might not be optimal for all parts of a complex folder.
#| Such "global" options only allow users to choose their restrictions rather than allowing true freedom of folder structure.
#| Users should ask "how can I configure quiltr to represent this folder?" rather than "how can I make this folder usable with quiltr?".
#|
#| ### Path-specific options
#| 
#| To maximize the flexibility of `quiltr`, most option values should be file-path-specific.
#| Some options will only apply to files, such as an option specifying which file types to include, whereas others will only apply to folders, such as an option specifying where to look for files.
#| If a folder has been organized well, such file-path-specific options should be intiutive to apply.
#| For example, a folder might contain python scripts that the user wants to display, so an option like the following might be used:
#| 
#| ```{r, eval = FALSE}
#| display = c("py")
#| ```
#|
#| However, an other part of the folder, lets say `src/common_lib`, might contain a standard library of python code that the script uses, but the user does not want to display. 
#| Therefore, there needs to be a way to specify which parts of the folder use which options. 
#| One way this could be done is to make each option accpet a list of named values, with names correspoonsing to file paths.
#| For example, the option might specified by:
#|
#| ```{r, eval = FALSE}
#| display = list('**' = c('py'), 'src/**' = c())
#| ```
#|
#| The example above uses [recursive filename expansion](https://en.wikipedia.org/wiki/Glob_%28programming%29#Recursive_Globbing) (a.k.a. globbing) to specify that all python files will be displayed except those in `src`.
#| Note that order of option values matters; the more specific value for `src/**` overwrites the more general value for `**`.
#| If the order was reversed,  `src/**` would have no effect. 
#| The options path could also be specified using [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) instead of filename expansion.
#| In that case, the above example would be: 
#|
#| ```{r, eval = FALSE}
#| display = list('.*' = c('py'), 'src/.*' = c())
#| ```
#| 
#| Although this syntax increases flexibility, it can easily result in excessivly long commands.
#| This can be ameliorated in two ways:
#|
#| * logical defaults for paths if they are not specified
#| * configuration files containing option values for each folder. 
#|
#| The following sections will deal with these two ideas
#|
#|
#| ### Default paths for path-specific options
#|
#| If a path for an option value is not specified, the most logical default would be to match all paths under the current folder (i.e. `**`).
#| When non-absolute file paths are used, this brings up the question of what the "current folder" is. 
#| In this case, it would be the value of the `path` option given to the `quilt` function, but this will become more complex and important when we consider configuration files.
#| If we assume that the default should match all files under the current path, a simplified version of the above example could be:
#|
#| ```{r, eval = FALSE}
#| display = list(c('py'), 'src/.*' = c())
#| ```
#|
#| If we assume further that all option values should be coerced into lists, the first example in this section: 
#|
#| ```{r, eval = FALSE}
#| display = c("py")
#| ```
#|
#| would be interpreted by as:
#|
#| ```{r, eval = FALSE}
#| display = list('**' = c('py'))
#| ```
#|
#| The intuitive usage that we started with is now a special case of the more flexible usage. 
#| This behavior is desireable because it allows novice users to use simple commands while still allowing for more advanced usage.
#| 
#| ### Configuration files


















#===================================================================================================
#' Get paths to configuration files 
#' 
#' @param path (\code{character} of length 1) The path along which configuration files are searched.
#' @param name (\code{character} of length 1) The name of configuration files. 
#' @param root (\code{character} of length 1) Where along \code{path} to start seaching. Must be a 
#' parent directory of \code{path}.
#' @param must_exist (\code{logical} of length 1) If \code{FALSE}, return the paths where
#' configuration could be even if they dont exist.
#' 
#' @return \code{character} of absolute paths.
get_config_paths <- function(path, name, root, must_exist = TRUE) {
  # Validate input ---------------------------------------------------------------------------------
  path <- normalizePath(path)
  root <- normalizePath(root)
  if (!grepl(pattern = paste0("^", root), path)) {
    stop("'root' is not a parent directory of 'path'.")
  }
  # Get directories in search path -----------------------------------------------------------------
  if (root == dirname(path)) {
    split_rel_path <- ""
  } else {
    if (root == .Platform$file.sep) { root = "" }
    rel_path <- gsub(paste0("^", root), "", dirname(path))
    split_rel_path <- strsplit(rel_path, .Platform$file.sep)[[1]]
  }
  config_locations <- lapply(1:length(split_rel_path), function(i) split_rel_path[1:i])
  config_locations <- sapply(config_locations, function(x) do.call(file.path, as.list(x)))
  config_locations <- gsub(paste0("^", .Platform$file.sep), "", config_locations)
  search_path <- vapply(config_locations, FUN.VALUE = character(1),
                        function(x) ifelse(x == "", file.path(root, name), file.path(root, x, name)))
  # Check for configuration files in search path ---------------------------------------------------
  if (must_exist) { search_path <- search_path[file.exists(search_path)] }
  unname(search_path)
}


#===================================================================================================
#' Get value of option in config file
#' 
#' Get the value of a specific option in a specific configuration file.
#' 
#' @param path (\code{character} of length 1)
#' @param option (\code{character} of length 1)
#' 
#' @return depends on content of config file. Returns \code{NA} if the option cannot be found.
get_config_value <- function(path, option) {
  content <- yaml::yaml.load_file(input = path)
  if (option %in% names(content)) {
    return(unlist(content[[option]], recursive = FALSE))
  } else {
    return(NA)
  }
}



#===================================================================================================
#' Wildcard expansion
#' 
#' Like \code{\link{Sys.glob}}, but also understands double wildcards for recursive matching using
#' double wildcards (\code{**})
#' 
#' @param path (\code{character} of length 1) The path to expand.
#' @param max_search_depth (\code{integer} of length 1) How deep to search.
#' 
#' @return \code{character}
sys_glob <- function(path, max_search_depth = 50) {
  # Find location of double wildcard ---------------------------------------------------------------
  split_path <- strsplit(path, .Platform$file.sep)[[1]]
  pair_index <- which(grepl(pattern = "\\*\\*", split_path))
  if (length(pair_index) > 1) {
    stop(paste0("Currently, Quiltr only supports one double wildcard (**) per path. ",
                "The following path has more than one:\n\t", path))
  }
  if (length(pair_index) == 0) { return(Sys.glob(path)) }
  # List possible paths using single wildcards -----------------------------------------------------
  possibilities <- lapply(0:max_search_depth, rep, x = "*")
  
  split_path[pair_index] <- gsub(pattern = "\\*\\*", replacement = "*",
                                 split_path[pair_index])
  possible_paths <- lapply(possibilities,
                           function(x) c(split_path[seq(from = 1, length.out = pair_index - 1)],
                                         x,
                                         split_path[seq(from = pair_index, to = length(split_path))]))
  possible_paths <- lapply(possible_paths, function(x) do.call(file.path, as.list(x)))
  # Search all possible paths ----------------------------------------------------------------------
  unique(unlist(lapply(possible_paths, Sys.glob)))
}


#===================================================================================================
#' Get option value for a given context
#' 
#' Get option value for a given context
#' 
#' @param path (\code{character} of length 1) The path to a file that might have option values in
#' configuration files apply to it. Configuration files will be looked for in its path and any 
#' options that apply to the file will be used. If \code{NULL}, look for values in a configuration
#' file in root.
#' @param option (\code{character} of length 1) The name of the option to get a value for
#' @param func_arg_value The value for the option passed into thj
#' @param root (\code{character} of length 1) A parent directory of \code{path} in which to start 
#' looking ofr configuration files
#' @param config_name (\code{character} of length 1) The name of configuration files.
#' @param is_missing (\code{logical} of length 1) If \code{TRUE}, \code{default} is ignored.
#' @param inherit (\code{logical} of length 1) If \code{FALSE}
#' 
get_option <- function(path, option, func_arg_value, root, config_name, is_missing, inherit = TRUE) {
  search_paths <- function(value, path, context) {
    if (!is.list(value)) { value <- list(value) }
    if (is.null(names(value))) { names(value) <- rep("", length(value)) }
    for (index in seq_along(value)) {
      if (names(value)[index] == "") { # If patterns are not specified...
        names(value)[index] <- ifelse(inherit, "**", "*")
      }
    }
    for (index in seq_along(value)) {
      if (path %in% mem_sys_glob(file.path(context, names(value)[index]))) {
        output_value <<- value[[index]]
      }
    }
  }
  mem_get_config_value <- memoise:: memoise(get_config_value)
  mem_sys_glob <- memoise:: memoise(sys_glob)
  # Validate inputs --------------------------------------------------------------------------------
  if (!is.null(path)) { path = normalizePath(path) }
  root = normalizePath(root)
  # Assign default value as defined in quilt -------------------------------------------------------
  output_value <- as.list(args(quilt))[[option]]
  # Return value from config file in root directory if path is NULL --------------------------------
  if (is.null(path)) {
    if (!is.null(config_name) && !is.na(config_name)) {
      root_config_path <-  file.path(root, config_name)
      if (file.exists(root_config_path)) {
        value <- mem_get_config_value(path = root_config_path, option = option)
        if (length(value) > 1 || !is.na(value)) { output_value <- value }
      }
    }
    if (!is_missing) { output_value <- func_arg_value }
  } else {
    if (!is.null(config_name) && !is.na(config_name)) {
      # Get relevant configuration file paths ------------------------------------------------------
      config_paths <- get_config_paths(path = path, name = config_name, root = root,
                                       must_exist = TRUE)
      # Look for options that apply to the path given in each configuration file -------------------
      for (config_path in config_paths) {
        value <- mem_get_config_value(path = config_path, option = option)
        if (is.function(value) || length(value) > 1 || !is.na(value)) { # If the option is found in the config file...
          search_paths(value, path, dirname(config_path))     
        }
      }
    }
    if (!is_missing) { search_paths(func_arg_value, path, root) }    
  }
  return(output_value)
}