#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| ## Configuration files
#| 
#| The use of configuration files are central to how `quilt` is designed.
#| Therefore, finding configuration files will be the first task described. 
#| The implementation of configuration file finding and parseing should fufill the following citeria: 
#|
#| * __embeddability__: It should be possible to set _every option_ using configuration files,
#| so that all settings used can be stored in the target directory. This saves time and effort for the user since they can run quilt
#| with no parameters (except perhaps `path`) and not have to remember `quilt`'s many options for every project. 
#| * __adaptability__: It should require minimal changes in configuration files to lump target folders together or split them apart.
#| In other words, no "root" folder should be relied on too heavly and configuration files should be distributed thourought a directory structure.
#| Combining two projects or splitting one into two should require minimal changes to configuration files. 
#| * __portability__: Moving or renaming a target folder should not effect the output of `quilt` _using default options_.
#| * __explainability__: It should be easy to explain how configuration files are found _using default options_ to avoid confusing novice users. 
#| The default options should make using configuration files as intuitive as possible.
#| 
#| This is a more subtle problem to implement might be expected. 
#| It might not be possible to accomplish all these criteria with any one set of parameter values.
#| For example, there are some options (e.g. `path`, `config_name`) that can modify which configuration files are found yet
#| these same options must be specifiable in configuration files according to the
#| __embeddability__ criteria, creating the potential for a circular dependency. 
#| One solution to this is to split the options of `quilt` into two categories: 
#|
#| * __"global"__ options that can only be set by configuration files in `path` (before `path` itself has the potential to be modified by confguration files.) and do not have path-specific values.
#| * __"local"__ options that be set in configuration files regardless of location and can have path-specific values. 
#|
#| This means the first thing that should be done is determining global option values.
#| After that, the rest of the configuration files can be found that influence local options and their path-specific values can be inferred.
#| `path` is unique in this system in that is used to find the "global" configuration options, but should also be settable in those same files.
#| To solve this contridiction, the value of `path` given to the function will be used to find configuration files in which global options
#| (including itself) can be set; after `path` is potentially changed by these files, this new value of `path` can be used to find the rest of
#| the configuration files in which local options can be set. 
#|
#| 1. Apply global option settings in configuration files in `path`
#| 2. Apply any modifications to `path`
#| 3. Find configuration files in the potentially modified `path` that will affect local options. 
#| 4. Inferr the path-specific values of all local options for every file/folder under `path`
#| 
#| Since the first step in this process requires reading configuration files, we need to define the structure of a configuration file.
#|
#| ### The format of configuration files
#| 
#| The structure of the configuration files should have the following properties:
#| 
#| * It should use a pre-existing syntax with pre-existing parsers in R. 
#| * Simple configuration files should be immediatly understandanble to those with minimal R experience.
#| * It should allow for specifying all types of R data structures, including functions. 
#| 
#| Two solutions seem to fit all these criteria more or less. 
#| Perhaps the simplest and most flexible solution is to write the configuration files in R code. 
#| R can obviously parse R code quite easily using the `source` function and there should be no problem defining complex R data structures. 
#| However, a nested lists defined in R could intimidate novice users; this is a concern becasue quilt should be usuable with minimal programming experience.
#| R code has significant advantanges for advanced usage since value for options can be computed. 
#| Using R code introduces some ambiguity in regards to how to format/parse the option values;
#| For example, each option could be a variable with the options value or they could be items in a named list. 
#| Another solution is to use [YAML](https://en.wikipedia.org/wiki/YAML) to specify a R data structure and the `yaml` package to parse it into R.
#| YAML has an intuitive layout that novice users should be able to understand.
#| Since YAML is designed to represent data structures it does not have the minor issue of format ambiguity that pure R code does.
#| However, advanced usage of YAML (e.g. defining R functions) is potenttially more complex then just using R code.
#| In summary, R code is best for advanced usage by programmers and YAML is best for simple usage.
#| 
#| Since both are easy to parse, the best solution might be to support for both formats. 
#| For this reason, the `config_name` option should accept a file name without an extension.
#| Files of that name with either a R or YAML extension would be used, allowing users to mix the two according to their tastes. 
#|
#| #### Configuration file parser 
#|
#| Lets start by writing the code for the configuration file parser.
#| This function should not attempt to validate or standardize the content, besides any changes that are specific to the input file format.
#| The output should be an R data structure representing the raw content of the configuration files. 
#| Since configuration files can be thought of an attribute of a folder, a folder path should be the input of the function.
#| By using folders as input instead of configuration file paths explicitly, it restricts the specification of which file types are accepted to this function.
#| This will make it easier to accept other formats of configuration files in the future should it become needed, since only this function would need to change. 
#| Another input needed is the value of `config_name`. 
#| 
#| Although this function is not exported, it would still be good to add some basic documentaion:
#|
#===================================================================================================
#' @title Parse configuration files
#' 
#' @description 
#' Parse configuration files for one or more folders and return a list of their content. 
#' 
#' @param folder_paths (\code{character})
#' The path to one or more folders from which to extract configuration file data.
#' 
#' @param config_name (\code{character} of length 1)
#' The file name of configuration files minus the file extension
#' 
#' @return \code{list}
#' Returns \code{NA} for folders with no configuration files.
#' Returns \code{NULL} for empty configuration files.
read_configuration_files <- function(folder_paths, config_name) {
  #|
  #| Within this function we should define a function to parse each file type.
  #| Each function should take a single file path and should assume the file type is correct.
  #| We can use a named list of functions to associate the file type with its parser.
  #| Since it is possible for some file types to have multiple accepted extensions (e.g. "yml" and "yaml"), the parsers should be defined independently of the list.
  #| If a file is empty, the parser functions should return `NULL`.
  #|
  #| Lets define the YAML parser first. 
  #| We can just reference `yaml::yaml.load_file` for now, but it might need to be more complicated eventually.
  #| `yaml::yaml.load_file` returns `NULL` when the file is empty.
  #|
  ## Define YAML parser ----------------------------------------------------------------------------
  parse_yaml <- function(path) {
    yaml::yaml.load_file(path)
  }
  #|
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
  ## Define R parser -------------------------------------------------------------------------------
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
  #|
  #| Now that the parsers are defined, lets put them in a list with names corresponding to file extensions.
  #| The extensions will be in lower case. 
  #| We will add one entry for each type of extention accepted.
  #| 
  ## Consolidate parsers into a list ---------------------------------------------------------------
  parsers <- list("yaml" = parse_yaml, 
                  "yml"  = parse_yaml,
                  "r"    = parse_r)
  #|
  #| Let find the configuration files in each folder
  #| There could be multiple valid configuration files in a given folder.
  #| We could append the content of multiple files together, but I cant think of a reason that this would be useful, so lets throw an error when this happens for simplicity.
  #|
  ## Get configuration file paths ------------------------------------------------------------------
  ext_regex <- paste(names(parsers), collapse = "|")
  file_paths <- lapply(folder_paths, 
                       function (x) {
                         path <- list.files(x, pattern = paste0(config_name, ".", 
                                                                "(", ext_regex, ")"))
                         if (length(path) == 0) { return(NA) }
                         if (length(path) > 1) {
                           stop(paste0('Multiple configuration files found in "', x, '".' ))
                         }
                         return(path)
                       })
  #|
  #| Next we need to determine the file type of input folder
  #| They will also need to be converted to lowercase to be compatible with the format of `parsers`
  #|
  ## Determine file extensions ---------------------------------------------------------------------
  extensions <- tolower(tools::file_ext(folder_paths))
  #|
  #| Finally, we can execute the appropriate parser for each folder
  #|
  ## Parse configuration files ---------------------------------------------------------------------
  contents <- mapply(file_paths, extensions, SIMPLIFY = FALSE,
                     FUN = function(path, ext) {
                       if (is.na(path)) { return(NA) }
                       parsers[[ext]](path)
                     })
  #| 
  #| The last step will be to verify/standardize the data structures returned by the parsers
  #| To do this, we first need to define what the acceptable syntax of configuration files are.
  #| Since the syntax of configuration files deals heavily with the concept of path-specific options,
  #| this will be described in the next chapter
  #| The next section will do this and define the following function.
  #|
  standardize_config_data(contents)
}
#|
#|


