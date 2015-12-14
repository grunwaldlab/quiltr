#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Parsing Configuration Files
#|
#| In order to determine local and global options, we first need to define the format of configuration files and their format.
#|
#| ## The configuration file parsing function
#|
#| We will start by making a high level function that takes one or more folders and returns a list of option settings specified by configuration files therein.
#| This function should also do some basic checks of configuration file structure and content.
#| Since the reading of configuration files is a rather general goal, it might be a good idea to make to function reusable for other programs. 
#|
#| ### Function citeria
#|
#| * The input should be one or more configuration file paths or folders containing configuration files with a specified name. 
#| Alternativly, if a named list is given, it should be interpreted as equivalent to the parsed content of configuration files with names corresponding to the folder they apply to.
#| This second input type could be useful for adding configuration data for read-only folders and should not be difficult to implement. 
#| * The output should be a list of option settings for individual paths patterns (i.e. regex or file path wildcards).
#| Each item in the list should have the following information:
#|     + option name
#|     + option value
#|     + path-specific pattern
#|     + path to source configuration file
#|     + optional grouping defined by a set of valid prefixes to option names
#| * The structure and content of configuration files should be verified and error messages should mention the path of the offending configuration file.
#| * Relative paths for path-specific options should be made absolute by assuming the current working directory is the location of the configuration file.
#|
#| ### Documentation for `parse_configuration` #####################################################
#' @title  Get configuration file data
#' 
#' @description 
#' Parse the data in configuration files for one or more folders.
#' 
#' @param paths (\code{character} or named \code{list})
#' If \code{character}, one or more folders in which to look for configuration files to parse.
#' If named \code{list}, one or more R lists representing parsed configuration data.
#' 
#' @param config_name (\code{character} of length 1)
#' The file name of configuration files without the file extension.
#' 
#' @param valid_options (\code{character} of length 1) 
#' The set of vaild options that can be specified.
#' This is used to tell the differnece between options with and without path specificity
#' 
#' @param global_options (\code{character})
#' The names of global options, which should not be given path-specific values.
#' This is only used for error checking. 
#' By default, no global options are assumed.
#' 
#' @param default_path (\code{character} of length 1)
#' The default path applied to options that accept path specific values but dont have a path
#' specified.
#' 
#' @return \code{list} of \code{list} with the following items:
#' \describe{
#'   \item{option}{The name of an option for \code{function}}
#'   \item{value}{The value of the option}
#'   \item{path}{The path pattern for the option-value pair}
#'   \item{config_path}{The full path to the configuration file the setting was derived from}
#' }
parse_configuration <- function(paths, config_name, valid_options, global_options = NULL,
                                default_path = "**") {
  
  #| ### Input vaildation ##########################################################################
  #| Since this is a rather high-level function, lets do some argument validation.
  #|
  #| All folder paths should exist and point to actual folders unless a named list is given.
  #| If a named list is given, then these checks do not apply;
  #| However, the content of the named list will still be checked later in the function.
  is_named_list <- function(x) { is.list(x) && !is.null(names(x)) }
  if ( ! is_named_list(paths) ) {
    do_not_exist <- paths[ ! file.exists(paths) ]
    if ( length(do_not_exist) > 0 ) {
      stop( paste0("The following paths do not exist: ", paste(do_not_exist, collapse = ", ")) )
    }
  }
  #| There should only be one config name...
  if ( length(config_name) != 1 ) {
    stop( paste0("Incorrect length of 'config_name' (", length(config_name), ").") )
  }
  # Global options should be a subset of `valid_options`...
  unknown_options <- global_options[!global_options %in% valid_options]
  if (length(unknown_options) > 0) {
    stop(paste0("The following options are not known options: ", 
                paste(unknown_options, collapse = ", ")))
  }
  # There should be only one default path...
  if ( length(default_path) != 1 ) {
    stop( paste0("Incorrect length of 'default_path' (", length(default_path), ").") )
  }
  
  #| ### Get raw configuration content #############################################################
  #| This is the step where configuration files are read and a list of raw content is obtained.
  #| The names of the list should correspond to configuration file paths.
  #| If a named list is given for `paths`, then it is treated as if it was the raw content.
  #| In that case, the names of the list are the folder paths the settings apply to.
  #| All folders might not have configuration files, so `raw_content` could have less items than `paths`
  raw_content <- lapply(paths, function(x) {
    if (is_named_list(x)) {
      return(list(x))
    } else {
      return(read_configuration_files(x, config_name))
    }
  })
  raw_content <- unlist(raw_content, recursive = FALSE)

  #| ### Convert content to output format ##########################################################
  #| Next we need to convert `raw_content` into the output format described in the function documentation.
  #| This can be thought of as an operation similar to `reshape2::melt`, where the dimentionailty of the data is reduced.
  #| For options that are not given path-specific values, the path pattern returned should be `NA`
  settings <- reformat_configuration(raw_content, valid_options)
  
  #| ### Verify content ############################################################################
  #| We should vaildate the content of the settings now that it is in a form that is easy to parse. 
  #|
  #| Lets for global options being given path-specific values.
  #| For options that are not given path-specific values, the path pattern returned by `reformat_configuration` should be `NA`.
  invalid_rows <- which(settings[, "option"] %in% global_options & !is.na(settings[, "path"]))
  if (length(invalid_rows) > 0) {
    stop(paste0('Attempt to set global option "', settings[invalid_rows[1], "option"],
                '" to a path-specific value in configuration file "', settings[invalid_rows[1], "path"], '".'))
  }
  
  #| ### Apply default paths #######################################################################
  #| Finally, lets replace the `NA`s introduced by `reformat_configuration` with the default path value
  settings[is.na(settings[, "path"]), "path"] <- default_path
  
  #| ### Make paths absolute #######################################################################
  settings[ , "config_path"] <- R.utils::getAbsolutePath(settings[ , "config_path"])
  settings[ , "path"] <- mapply(R.utils::getAbsolutePath, settings[ , "path"],
                                workDirectory = dirname(unlist(settings[ , "config_path"])))
  
  #| ### Return result #############################################################################
  return(settings)
}
#|
#| ## Reading configuration files
#|
#| This function should not attempt to validate or standardize the content, besides any changes that are specific to the input file format.
#| The output should be an R data structure representing the raw content of the configuration files. 
#| Since configuration files can be thought of an attribute of a folder, a folder path should be the input of the function.
#| By using folders as input instead of configuration file paths explicitly, it restricts the specification of which file types are accepted to this function.
#| This will make it easier to accept other formats of configuration files in the future should it become needed, since only this function would need to change. 
#| Another input needed is the value of `config_name`. 
#| 
#| Although this function is not exported, it would still be good to add some basic documentaion:
#|
#' @title Parse configuration files
#' 
#' @description 
#' Parse configuration files for one or more folders and return a list of their content. 
#' 
#' @param folder_paths (\code{character})
#' The path to one or more folders from which to extract configuration file data.
#' This can also include the paths to configuration files themselves
#' 
#' @param config_name (\code{character} of length 1)
#' The file name of configuration files minus the file extension
#' 
#' @return \code{list}
#' A \code{list} representing the raw content of the configuration files
#' Returns \code{NA} for folders with no configuration files.
#' Returns \code{NULL} for empty configuration files.
read_configuration_files <- function(folder_paths, config_name) {
  #| Within this function we should define a function to parse each file type.
  #| Each function should take a single file path and should assume the file type is correct.
  #| We can use a named list of functions to associate the file type with its parser.
  #| Since it is possible for some file types to have multiple accepted extensions (e.g. "yml" and "yaml"), the parsers should be defined independently of the list.
  #| If a file is empty, the parser functions should return `NULL`.
  #| ### Define YAML parser ########################################################################
  #| Lets define the YAML parser first. 
  #| We can just reference `yaml::yaml.load_file` for now, but it might need to be more complicated eventually.
  #| `yaml::yaml.load_file` returns `NULL` when the file is empty.
  parse_yaml <- function(path) {
    yaml::yaml.load_file(path)
  }

  #| ### Define R parser ###########################################################################
  #| Next we should define the R file parser
  #| There are more ways to do this than YAML, so there is some room for preference. 
  #| It should be possible for their to be an arbitrary amount of R code before the configuration content so that code can be used to customize the configuration values.
  #| I can think of two general ways to go about this:
  #| 
  #| * Variables with names corresponding to option names can be defined as varaibles.
  #| * A single named list not assigned to a variable can be defined at the end of the file.
  #|
  #| I will use the second option since it is more explicit and easier to parse. 
  #| It also reduces the potential for users to accendentally assign option values when they meant to assign temporary variables.
  #| 
  parse_r <- function(path) {
    # `parse` loads the expressions from an R file without parsing them, resulting in a list of expressions.
    content <- parse(path)
    # In the file is empty, return `NULL`.
    if (length(content) == 0) { return(NULL) }
    # Filter out expressions that are assigned to a variable
    content <- content[ vapply(content, class, character(1)) != "=" ]
    # Filter out expressions that are not lists
    content <- content[ vapply(content, function(x) class(eval(x)), character(1)) == "list" ]
    # Return the last list not assigned to a variable
    return( eval(content[length(content)]) )
  }

  #| ### Consolidate parsers into a list ###########################################################
  #| Now that the parsers are defined, lets put them in a list with names corresponding to file extensions.
  #| The extensions will be in lower case. 
  #| We will add one entry for each type of extention accepted.
  parsers <- list("yaml" = parse_yaml, 
                  "yml"  = parse_yaml,
                  "r"    = parse_r)

  #| ### Get configuration file paths ##############################################################
  #| Lets find the configuration files in each folder
  #| There could be multiple valid configuration files in a given folder.
  #| We could append the content of multiple files together, but I cant think of a reason that this would be useful, so lets throw an error when this happens for simplicity.
  ext_regex <- paste0("\\.", "(", paste(names(parsers), collapse = "|"), ")" )
  find_file <- function(folder_or_file) {
    if (grepl(pattern = ext_regex, folder_or_file, ignore.case = TRUE)) {
      path <- folder_or_file
    } else {
      path <- list.files(folder_or_file, pattern = ext_regex,
                         ignore.case = TRUE, full.names = TRUE)
      if (length(path) == 0) { return(NA) }
      if (length(path) > 1) {
        stop(paste0('Multiple configuration files found in "', folder_or_file, '".' ))
      }
    }
    path <- R.utils::getAbsolutePath(path)
    return(path)
  }
  file_paths <- lapply(folder_paths, find_file)
  
  #| ### Determine file extensions #################################################################
  #| Next we need to determine the file type of input folder
  #| They will also need to be converted to lowercase to be compatible with the format of `parsers`.
  extensions <- tolower(tools::file_ext(file_paths))

  #| ### Parse configuration files #################################################################
  #| Finally, we can execute the appropriate parser for each folder
  contents <- mapply(file_paths, extensions, SIMPLIFY = FALSE,
                     FUN = function(path, ext) {
                       if (is.na(path)) { return(NA) }
                       parsers[[ext]](path)
                     })
  
  #| ### Name by input and return ##################################################################
  names(contents) <- file_paths
  return(contents)
}
#|
#| ## Path-specific option syntax
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
#| ## The reformating function
#|
#| The two types of path-specific syntax described above present two ways of storing configuration file information.
#| The first type stores settings in a nested list with the first level corresponding to option names and the second to path-specific values:
#|
#| ```{r}
#| options = list(option_1 = list(path_1 = value_1,
#|                                path_2 = value_2),
#|                option_2 = list(path_1 = value_2),
#|                option_3 = list(path_2 = value_3))
#| ```
#|
#| The second type described, which is used in configuration files, has the same information except the first dimension is the paths and the second is options:
#|
#| ```{r}
#| options = list(path_1 = list(option_1 = value_1,
#|                              option_2 = value_2),
#|                path_2 = list(option_3 = value_3,
#|                              option_1 = value_2))
#| ```
#|
#| These two dimensional data structures allow compact representation of shared attributes (paths or options) so are useful as configuration files formats.
#| However, they are harder to parse since they require two nested loops to get at the fundamental path-option-value combination.
#| A better data structure for computers to read is a list where each item is every path-option-value combination:
#|
#| ```{r}
#| options = list(list(path_1, option_1, value_1),
#|                list(path_1, option_2, value_2),
#|                list(path_2, option_3, value_3),
#|                list(path_2, option_1, value_2))
#| ```
#|
#| This is effectivly a one-dimensional data structure that can be iterated over with a single loop. 
#| It also makes it easier to store additional information like configuration file path. 
#| To make code more informative, we can add names to the values of each combination:
#|
#| ```{r}
#| options = list(list(path = path_1, option = option_1, value = value_1, config_path = config_path_1),
#|                ...)
#| ```
#|
#| The goal of the configuration reformating function is to convert the format of a parsed configuration file (that of the second described) to the one-dimensional format shown above.
#| This function will also need to tell when a path has been left off, as is done for global options and can be done for local options.
#|
#| ### Documentation of `reformat_configuration` ###################################################
#' @title Standardize configuration content
#' 
#' @description 
#' Standardize the raw output of parsed configuration file content by applying defaults for
#' path-specific options. Also checks for path-spcific values being applied to global options
#' inappropriatly.
#' 
#' @param raw_content (\code{list})
#' The parsed content one or more configuration files
#' 
#' @param option_names (\code{character} of length 1) 
#' The set of vaild options that can be specified.
#' This is used to tell the differnece between options with and without path specificity
#' 
#' @param group_regex (\code{character} of length 1)
#' Fragment of regular expression that matches a group name at the start of the option name. 
#' 
#' @param group_sep (\code{character} of length 1)
#' Fragment of regular expression that matches what separates group names from option names. 
#' 
#' @return (\code{list})
reformat_configuration <- function(raw_content, option_names,
                                   group_regex = "[a-zA-Z0-9_.]+", group_sep = "\\.") {
  
  #| ### Define valid option names
  #| We use the knowledge of valid option names to identify when path-specific information is left off.
  #| We must take into account unknown group prefixes.
  valid_regex <- paste0("((^", group_regex, ")",
                        group_sep, ")?",
                        "(", paste(option_names, collapse = "|"), "$)")
  
  #| ### Define function to process one configuration file data ####################################
  #| The function can take the data from multiple configuration files. 
  #| To simplify the logic of the function, lets define a child function that process the data of a single file.
  #| We can then interate over the input and concatenate the output of the child function.
  #|
  #| This function will use two nested for loops to iterate over the data from one configuration file and append each path-option-value combination to an output list.
  #| Growing lists like this in R can be inefficient when the list becomes large, but I expect that most configuration files will be relativly short.
  process_one <- function(data, config_path) {
    output <- list()
    add_row <- function(option, value, path) {
      group <- stringr::str_match(option, valid_regex)[1, 3] # extract group from option name
      option <- stringr::str_match(option, valid_regex)[1, 4] # extract option name
      output <<- c(output, list(option, value, path, config_path, group))
    }
    for ( index_1 in seq_along(data) ) { # Iterate over first dimension
      dim_1_name <- names(data)[index_1]
      dim_1_value <- data[[index_1]]
      if ( grepl(valid_regex, dim_1_name) ) {
        if ( ( ! is.null(names(dim_1_value)) ) && all(names(dim_1_value) %in% option_names) ) {
          stop(paste0('It appears that you are trying to specify a path-specific value for a path',
                      ' with the same name as an option. This is ambiguous since paths can be left',
                      ' off to make options global. If this is indeed a path-specific option,',
                      ' the prefix the path with ".', .Platform$file.sep, '" in configuration', 
                      ' file "', config_path, '".'))
        }
        add_row(option = dim_1_name, value = dim_1_value, path = NA)
      } else {
        for ( index_2 in seq_along(dim_1_value) ) { # Iterate over second dimension
          dim_2_name <- names(dim_1_value)[index_2]
          dim_2_value <- dim_1_value[[index_2]]
          if ( grepl(valid_regex, dim_2_name) ) {
            add_row(option = dim_2_name, value = dim_2_value, path = dim_1_name)
          } else {
            stop(paste0('Invalid configutation file format in "', config_path,
                        '". No option name in first or second dimension'))
          }
        }
      }
    }
    return(output)
  }
  
  #| ### Run function for each piece and combine ###################################################
  output <- unlist(mapply(FUN = process_one, raw_content,  names(raw_content), SIMPLIFY = FALSE), 
                   recursive = FALSE, use.names = FALSE)
  
  #| ### Define dimensions and names
  #| At this point, `output` is a simple list of values with option, value, path, config_path, and group data all together.
  #| We need to apply named dimensions to identify what each value is.
  dim(output) <- c(5, length(output) / 5)
  rownames(output) <- c("option", "value", "path", "config_path", "group")
  output <- t(output)
  return(output)
}
#|
#|