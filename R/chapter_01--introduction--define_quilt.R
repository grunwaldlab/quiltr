#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # The quilt function
#| 
#| <img src="
#|```{r, eval = TRUE, results = "asis", echo = FALSE}
#|cat(file.path("..", "figures", "knitr_metaphore_figure.png"))
#|```
#| ">
#|
#| ## Introduction
#|
#| This is the central function of the quiltr package.
#| The function's goal is to make a sharable representation of file content in a *target folder*.
#| Examples of possible representations include a static html website and book-style PDF/DOCX.
#| The organization of file content in the output can be inferred from file names and folder structure.
#| Scripts/programs in the target folder can be executed and their code and results integrated into the output.
#| This is especially useful for literate programming documents (e.g. Rmarkdown), but plain code files can also be executed.
#|
#| > **Target folder**:
#| > The primary "input" of the `quilt` function; the folder that a sharable representation will be made from.
#|
#| ### Configuration files
#|
#| There are no required arguments to `quilt` since all have default values.
#| Therefore, all inputs to the `quilt` function will be referred to as "options". 
#| All options can be specified via *configration files* making it unnecessary to set options in the function call.
#| Configuration files store the values of options relevant to the folders they are saved in.
#| Much more complex uses of `quilt` are possible with configuration files than with options specified in the function call. 
#| This is a design feature meant to encourage people to use configuration files to specify options.
#| Using configuration files makes it easier to re-use a complex configuration of `quilt`.
#| It also allows for beginers to use the function with limited understanding of its capibilities.
#|
#| > **Configuration file**: 
#| > A file that stores option values for `quilt` in the folder they apply to. 
#|
#| ### Path-specific option values
#|
#| The `quilt` function must be able to work intuitivly on complex folder structures.
#| It is not likly that all parts of a complex folder will be optimally represented by a single set of option values.
#| Therefore, *path-specific option values* can be specified in configuration files for applicable options.
#| This allows for different parts of a target folder to be treated diffrently. 
#| Paths associated with option values can be absolute or relative and can include wildcards.
#| How path-specific options are implemented is covered in chapter 2. 
#|
#| > **Path-specific option**: 
#| > An option setting that only applies to files/folders whose path/name matches a given pattern. 
#|
#| ### Output-specific option values
#|
#| Since `quilt` supports multiple output types and uses configuration files for option settings, it allows for *output-specific option* values. 
#| This is useful when a given folder is represented in multiple output formats.
#| Some options are shared amoung differnet output *renderer* functions and some are unique to a subset.
#| Using output-specific option values allows a given folder to be represented in multiple independent output formats.
#| Each output format can have its own settings during a single execution of `quilt`.
#| Therefore, configurations files do not need to be constantly changes to accomidate multiple output formats. 
#| 
#| > **Output-specific option**: 
#| > An option setting that applies to a single output format (i.e. renderer).
#|
#| > **Renderer**: 
#| > A function used to make a specific output type (e.g. website). 
#| > They are called by `quilt`.
#|
#| ### Global vs local options
#|
#| There are some options that can effect how configuration files are found, yet these same options can be set by configuration files.
#| This has the possiblity of introducing some circular logic. 
#| To get around this, options are split up into two classifications: *global options* and *local options*.
#| Global options tend to apply to general input/output and how option values are determined from configuration files.
#| Local options tend to apply to settings specific to output formats and file paths.
#| Since the values of local options can be influenced by those of global options, global options are parsed first. 
#| 
#| > **Global options**: 
#| > Options defined in the `quilt` function itself.
#| > They can only be set by configuration files in `config_path` and can not have path-specific values.
#|
#| > **Local options**: 
#| > Options defined in renderer functions.
#| > They can be set by configuration files in `path` and can have path-specific values.
#|
#| ## Design criteria
#|
#| * __embeddability__:
#|   It should be possible to specify all options using configuration files.
#|   This allows quilt to be run with no parameters (except perhaps `path`) so`quilt`'s many options do not need to be remembered. 
#|   No "root" folder should be relied on too heavly and configuration files should be distributed thourought a directory structure.
#|   Options in configuration files of sub directories should override options specified in parent directories.
#|   Combining two projects or splitting one into two should require minimal changes to configuration files. 
#| * __contextual options__: 
#|   There should be a consistent way to set *path-specific* and *output-specific* option values. 
#|   The implementation of this should be independent from the implementation of the options themselves.
#|   This allows `quilt` to adapt to heterogenous folder structures.
#| * __simplicity__: 
#|   The default behavior of all options should be intuitive and simple to explain yet still conform to a flexible conceptual model.
#|   In other words, intuitive default behavior should be a special case of more complicated and flexible behavior.
#|   It should be possible to leave off path and output information when the **contextual options** behavior is not needed. 
#| * __modularity__:
#|   It should be possible to extend `quilt` by defineing new functions with appropriate inputs/outputs and names. 
#| * __delegation__"
#|   The `quilt` function allow for multiple outputs per execution by calling other self-contained renderer functions.
#|   Each renderer function should corresponding to an output type.
#|   It should be possible to set all options of renderer functions using `quilt`. 
#| 
#| ## The function documentation
#| 
#| Below is the source of the documentation that is accessed by executing `?quilt` in an R consol.
#|
#| ### The "Title" and "Description"
#|
#| The following text is used for the "Title" and "Description" sections in the help menu.
#| They should contain the minimal amount of detail necessary to give a general impression;
#| more in-depth documentation appears in sections (i.e. `@section` tags) after the option documentation.
#|
#' @title 
#' Renders the contents of folder(s) into a sharable form.
#' 
#' @description 
#' Currently, the only output type is a static website.
#' The content of files in target folder(s) can be displayed in the output or accessed by links.
#' Rmarkdown and some programming languages can be executed and their output included.
#' The organization of the output website is derived from file/folder names or configuration files. 
#' This function's options are best specified with configuration files rather than passing values
#' to the function itself.
#' See the \code{config_name} option documentation for how to use configuration files. 
#' It is possible to do much more with configuration files than with arguments specified during the
#' function call.
#| 
#| ### The input 
#| 
#| The most important input for `quilt` is a path to a folder in which to look for input files.
#| Although `quilt` ultimatly works only on file content, it is meant to represent folders by doing so.
#| By default, this input path is the current working folder, so even this central argument can usually be unspecified.
#| This option provides the default value for the `config_path` option described below. 
#| Although only a single path can be specified in the function call, configuration files can specify multiple paths.
#| This behavior avoids the ambiguity of multiple configuration files specifying global options. 
#|
#' @param path (\code{character} of length 1)  [not path-specific or output-specific]
#' A path a to folder in which to look for files to display in the output.
#' This also determines the default place to look for configuration files.
#' A configuration file in \code{path} can modify the path before is it used to find content 
#' and configuration files for local options. 
#' Although only one path can be specified in the function call, it can be modified to multiple
#' paths in configuration files. 
#|
#| ### Output options
#|
#' @param output_format (\code{character}) [not path-specific or output-specific]
#' The type of output to generate. 
#' Each output type is associated with a renderer function.
#' The following output types are supported:
#' \describe{
#'   \item{\code{"website"}}{
#'     A set of HTML files linked together by a shared hierarchical menu.
#'     File content is rendered as HTML and displayed using iframes.  
#'   }
#'   \item{\code{"book"}}{
#'     IN DEVLOPMENT
#'     A single book-style pdf document containing file content rendered as pdf.
#'   }
#'   \item{\code{"figure"}}{
#'     IN DEVLOPMENT
#'     A single image file containing a scematic of input folder structure. 
#'   }
#' }
#' 
#' Output-type-specific option values can be specified in configuration files.
#' To make multiple outputs of the same type, with potentailly differnt options, supply a named character vector.
#' The names are used to identify corresponding output-specific options values in configuration files.
#' For example, \code{output_format = c("pubic" = "website", "private" = "website")} would create two websites;
#' Option names prefixed with \code{"public."} in configuration files, would only apply to the "public" website.
#'
#' @param output_path (\code{character} of length 1) [not path-specific]
#' Path to folder in which to write the output file/folder. 
#' If \code{NULL} or \code{NA}, a temporary directory will be used.
#' 
#' @param  output_name (\code{character} of length 1) [not path-specific]
#' The name of the output file/folder.
#' The name of the \code{output_format} will be appended onto this name to allow for multiple outputs at once.
#' 
#' @param overwrite (\code{logical} of length 1) If \code{TRUE}, an existing directory with the 
#' same name as an output directory will be overwritten. 
#|
#| ### Configuration file options
#|
#| Perhaps the most important goal of `quiltr` is to place the fewest restrictions possible on input folder structure. 
#| This can be partly accomplished by having numerous options to change how folders are interpreted, such as an option to ignore file names.
#| However, any one set of option values might not be optimal for all parts of a heterogenous folder structure.
#| Such "global" options only allow users to choose their restrictions rather than accomidating diverse folder stuctures.
#| Users should ask "how can I configure quiltr to represent this folder?" rather than "how can I make this folder usable with quiltr?".
#| The accomplish this, we can use configuration files that accept path-specific values of options.
#| Similar to `knitr::knit`'s ability to read options embedded in input files (via `knitr::opts_chunk` or YAML front matter), `quilt` uses configuration files to define options for input folders.
#| Since the fundamental input unit of `quilt` is a folder, configuration files are best thought of as an aspect of the folder they are in.
#|
#' @param config_name (\code{character} of length 1) [not path-specific]
#' The name of configuration files to use.
#' They can be anywhere in directories under \code{config_path}.
#' Configuration files are in YAML or R format and specify the values of options for this function.
#' Options that are file-path-specific (e.g. \code{type}) can take the form of named lists,
#' where names are file paths relative to the location of the configuration file 
#' (possibly with \code{*} or \code{**} wildcards) and values are the option values relevant
#' to the paths specified in the names.
#' Path-specific options can be specified by configuraion files anywhere in the target 
#' directory, not just the root specified by \code{path}.
#' Only some of this function's options are file-path-specific; those that are not
#' (e.g. \code{output}) can be specified by values in a configuration file in \code{path}.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' 
#' @param config_path (\code{character} of length 1) [not path-specific]
#' Path to a folder in which to look for a configuration file with name specified by \code{config_name}.
#' The default is the value of \code{path} given during the function call. 
#' This only applies to global options is not effected by \code{config_search_type}.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' 
#' @param config_search_type (\code{character}) [not path-specific]
#' Where to look for configuration files relative to \code{path}.
#' Only applies to local options and is not affected by \code{config_path}.
#' 
#' Accepts one or more of the following values:
#' 
#' \describe{
#'   \item{parents}{Files in parent folders of \code{path}}
#'   \item{root}{Files in \code{path}}
#'   \item{children}{Files in child folders of \code{path}}
#' }
#' 
#' By default, this option is set to c("root", "children").
#' This means that all configuration files in \code{path} and any subfolder will be used when determining values for local options.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' 
#' @section Configuration files:
#' Configuration files are used to store option values in the folders they apply to. 
#' They can be anywhere in directories under \code{config_path} (\code{path} by default).
#' Configuration files are in YAML or R format and specify the values of options for this function.
#' Options that are file-path-specific (e.g. \code{type}) can take the form of named lists,
#' where names are file paths relative to the location of the configuration file 
#' (possibly with \code{*} or \code{**} wildcards) and values are the option values relevant
#' to the paths specified in the names.
#' File-path-specific options can be specified by configuraion files anywhere in the target 
#' directory, not just the root specified by \code{path}.
#' Only some of this function's options are file-path-specific; those that are not
#' (e.g. \code{output}) can be specified by values in a configuration file in \code{path}.
#' This is the only option that cannot be specified by a configuration file.
#' To ignore website configuartion files, set this option to \code{NA} or \code{NULL}.
#' 
#|
#| ### Accepted input file formats
#|
#' 
#' @section Renderable file types: 
#' To see the list of file types that can be rendered, execute \code{formats_quilt_can_render()}.
#' All files types specified by \code{type} in \code{path} will be included.
#' Code is displayed using syntax highlighting if pandoc is installed. 
#' 
#|
#| ### The function return value
#|
#' 
#' @return (\code{character} of same length as \code{output_format})
#' The file path to the output file(s)/folder(s).
#' 
#|
#| ### Examples
#|
#' @examples
#' \dontrun{
#' 
#' # Make website out of the current working directory
#' quilt()
#' 
#' # Create a template directory in the current
#' # working directory and make a website from it
#' make_quiltr_template(getwd(), "default")
#' quilt("default")
#' }
#' 
#' @export
#|
#| ## The code
#|
quilt <- function(path = getwd(), output_format = "website", output_path = NULL, output_name = NULL,
                  overwrite = FALSE, config_name = "quilt_config", config_path = path,
                  config_search_type = c("root", "children"), file_search_type = c("root", "children")) {
  
  #| ### Validate input ############################################################################
  validate_quilt_input()
  
  #| ### Define output types #######################################################################
  #| The `quilt` function itself mostly deals with intrepreting options for rendering functions, each of which correspond to an output type.
  #| To make it easy to add output types, the rendering functions are identified by their name. 
  #| Simply creating a function with the appropriate name and parameters, even outside of the quiltr package source, will effectivly add a new output type.
  #| The function `get_quilt_renderes` searchs the current namespaces for functions named `quilt_[output type]`, where `[output type]` is the one-word name for an output type.
  #| It returns the list of renderer functions named by their output type. 
  renderers <- get_quilt_renderes()
  
  #| ### Get global option values from configuration files #########################################
  #| Apply any global option values in root configuration file.
  #| These options are determined before the local options since many of them effect the way local options are determined.
  #| Note that `config_path` is used instead of `path`; however, the default for `config_path` is `path`. 
  #| The function `get_global_options` should return a named list of named lists, representing the options for each output type. 
  #| The first dimension groups options by output type, and the second is the lists of options.
  global_option_names <- names(formals(quilt))
  global_options <- get_global_options(main_function = "quilt",
                                       sub_functions = renderers,
                                       config_path   = config_path,
                                       config_name   = config_name)
  
  #| ### Define function to process each output format #############################################
  process_format <- function(global_options, renderer) {
    
    ## Find configuration files for local options
    config_paths <- find_config_files(paths = global_options$path,
                                      config_name = global_options$config_name,
                                      search_type = global_options$config_search_type)
    
    ## Find all input files
    target_paths <- get_target_paths(paths = global_options$path,
                                     search_type = global_options$file_search_type)
    
    ## Get local option values from configuration files
    local_options <- get_path_specific_options(main_function  = "quilt",
                                               sub_function   = renderer,
                                               target_paths   = target_paths, 
                                               config_paths   = config_paths,
                                               global_options = global_option_names)
    
    ## Call renderer functions with path-specific options
    local_options$file_paths <- target_paths
    do.call(renderer, local_options)
  }
  
  #| ### Process each output format and return results #############################################
  mapply(process_format, global_options, names(global_options), SIMPLIFY = TRUE)
}
#|