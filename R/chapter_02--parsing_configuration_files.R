#| ## Parsing Configuration Files
#|
#| Perhaps the most important goal of `quiltr` is to place the fewest restrictions possible on input folder structure. 
#| This can be partly accomplished by having numerous options to change how folders are interpreted, such as an option to ignore file names.
#| However, any one set of option values might not be optimal for all parts of a heterogenous folder structure.
#| Such "global" options only allow users to choose their restrictions rather than accomidating diverse folder.
#| Users should ask "how can I configure quiltr to represent this folder?" rather than "how can I make this folder usable with quiltr?".
#| The accomplish this, we will use configuration files that accept path-specific values of options.
#| The following criteria should be used:
#|
#| * __path-specificity__: 
#| Most `quilt` options should have _consitent_ syntax for specifying file-path-specific values.
#| This allows for a relativly small set of `quilt` options to adapt to heterogenous folder structures. For example, 
#| A directory might have a few python scripts written by a user that should be quilted, but other python dependencies in
#| a library folder that should not.
#| * __simplicity__: 
#| It should be possible to exclude the path information when applying a function globally. 
#| This will make it easier for novice users to assign global options and make simple configuration files easier to read.
#| * __maintainability__: 
#| It should be simple to add options or modify their input.
#| In other words, the "path-specific value" functionality should be separted from the option functionality.
#|
#| ### Path-specific option syntax
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
#| However, an other part of the folder, lets say `src`, might contain a standard library of python code that the script uses, but the user does not want to display. 
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
#| Although this syntax is intuitive, it poses problems for parsing if excluding path information is supported.
#| If a given option takes a named list as input, there is no  easy way of telling if that named list is supposed to define the path-specific values or if it is a value for the function with the path inforamtion excluded.
#| For example, lets imagin a theroetical option 'parsers' that defines custom file parsers.
#| This options takes a named list of functions, with names corresponding to parsers:
#|
#| ```{r, eval = FALSE}
#| parsers = list("py" = a_function)
#| ```
#|
#| The path-specific syntax would then be:
#|
#| ```{r, eval = FALSE}
#| parsers = list('some_path/*' = list("py" = a_function))
#| ```
#|
#| The problem with this is that both the default and path-specific version are named lists, so it is hard to tell the two apart without storing information on the input format of each option.
#| One solution to this is to change the syntax for defining path-specific options from...
#|
#| ```{r, eval = FALSE}
#| parsers = list("some_path/*" = list("py" = a_function))
#| display = list("some_path/*" ="py")
#| ```
#|
#| to...
#|
#| ```{r, eval = FALSE}
#| options = list("some_path/*" = list("parsers" = list("py" = a_function),
#|                                     "display" = "py"))
#| ```
#|
#| Now we can leave off the path information  for `display` like so:
#|
#| ```{r, eval = FALSE}
#| options = list("some_path/*" = list("parsers" = list("py" = a_function)),
#|                "display" = "py")
#| ```
#|
#| Now we can use the knowlege of the option names to distinguish between option names and paths.
#| For example, `"display"` is the name of an option, whereas `"some_path/*"` clearly is not.
#| In the case that an option name and a path are the same (e.g. a folder named "display" exists) we can issue a warning telling the user to append a "./" to the name if it was supposed to be a path.
#| This syntax works well for configuration files, but is counter-intuiitve to use in the function call; this is not the way function parameters are typically used in R.
#| It also prevents `quilt` options from being explicitly documented by roxygen2 comments.
#| Therefore, as a comprimise, we will use this sytax in configration files and not include support for path-specific option values in the function call.
#| Options specified in the function call (rather than in configuration files) can be interpreted as if they were specified in a configuraiton file located in `path` without path-specific information.
#| This has the desireable side-effect of encouarging people to use configuration files instead of function arguments for complex uses.
#|
#| We will define the parsers and format of the configuration files in the next section, but lets write a function to apply defaults for path-sepcific values now.
#| Lets assume that parsing configuration files result in R lists with the syntax described above.
#| This function should take that parser output and convert it into a standard format with the default values applied.
#| For reasons described in the next section, some options will not support path-specific values.
#| If a user attempts to set path-specific values on these, an error should be thrown.
##==================================================================================================
#' @title Standardize configuration content
#' 
#' @description 
#' Standardize the raw output of parsed configuration file content by applying defaults for
#' path-specific options. Also checks for path-spcific values being applied to global options
#' inappropriatly.
#' 
#' @param content (\code{list})
#' The parsed content of a configuration file
#' 
#' @param global_options (\code{character})
#' The names of global options, which should not be given path-specific values.
#' 
#' @param current_folder (\code{character} of length 1)
#' The current working directory from which relative paths originate.
#' 
#' @param config_file (\code{character} of length 1)
#' The name of the config file the content was from. 
#' This is only used for printing error messages.
#' 
#' @param default_path (\code{character} of length 1)
#' The default path applied to options that accept path specific values but dont have a path
#' specified.
#' 
#' @return (\code{list})
standardize_config_data <- function(content, global_options, current_folder, config_file,
                                    default_path = "**") {
  ## Get quilt options and check that `global_options` are valid -----------------------------------
  option_names <- names(formals(quilt))
  unknown_options <- global_options[!global_options %in% option_names]
  if (length(unknown_options) > 0) {
    stop(paste0("The following options are not known `quilt` options: ", 
                paste(unknown_options, collapse = ", ")))
  }
  ## Find which content names match option names ---------------------------------------------------
  matches_option_names <- names(content) %in% option_names
  ## Check which content names are valid paths -----------------------------------------------------
  putative_paths <- ifelse(R.utils::isAbsolutePath(names(content)), 
                           names(content), 
                           file.path(current_folder, names(content)))
  are_valid_paths <- file.exists(putative_paths)
  ## Check for ambiguous content names -------------------------------------------------------------
  ambiguous_names <- names(content)[are_valid_paths & matches_option_names]
  if (length(ambiguous_names) > 0) {
    stop(paste0("The following option names are specified in the configuration file '", config_path,
                "' but also are valid paths: ", paste(ambiguous_names, collapse = ", "),
                " Add '.", .Platform$file.sep, "' to indicate that they are paths if they are."))
  }
  ## Verify path-specific values -------------------------------------------------------------------
  for (index in which(!matches_option_names)) {
    value <- content[[index]]
    if (class(value) != "list" || is.null(names(value))) {
      stop("Invalid format for path specific value in '", config_path, "'.\n",
           "Must be in the form of a named list")
    }
    invalid_names <- names(content)[ ! names(content) %in% option_names ]
    if (length(invalid_names) > 0 ) {
      stop(paste0("The following option names are not recgnoized in configuration file '", 
                  config_file, "': ", 
                  paste(invalid_names, collapse = ", ")))
    }
    invalid_global_names <- names(content)[ names(content) %in% global_options ]
    if (length(invalid_global_names) > 0 ) {
      stop(paste0("The following global options cannot be given path-specific values in configuration file '", 
                  config_file, "': ", 
                  paste(invalid_global_names, collapse = ", ")))
    }
  }
  ## Apply default paths ---------------------------------------------------------------------------
  is_local_but_pathless <- matches_option_names & (! names(content) %in% global_options)
  lapply(content[is_local_but_pathless], function(x) setNames(list(x), nm = default_path))
}














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