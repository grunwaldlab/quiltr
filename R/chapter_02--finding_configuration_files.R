#| ## Configuration files
#| 
#| The use of configuration files are central to how `quilt` is designed.
#| The __embeddedability__ citeria described below states that all options should be settable via configuration files. 
#| Therefore, finding configuration files will be the first task described and executed. 
#| The implementation of configuration file finding and parseing should fufill the following citeria: 
#|
#| * __path-specificity__: Most `quilt` options should have _consitent_ syntax for specifying file-path-specific values.
#| This allows for a relativly small set of `quilt` options to adapt to heterogenous folder structures. For example, 
#| A directory might have a few python scripts written by a user that should be quilted, but other python dependencies in
#| a library folder that should not.
#| * __embeddedability__: It should be possible to set _every option_ using configuration files,
#| so that all settings used can be stored in the target directory. This saves time and effort for the user since they can run quilt
#| with no parameters (except perhaps `path`) and not have to remember `quilt`'s many options for every project. 
#| * __adaptability__: It should require minimal changes in configuration files to lump target folders together or split them apart.
#| In other words, no "root" folder should be assumed and configuration files should be distributed thourought a directory structure.
#| * __modularity__: Moving or renaming a target folder should not effect the output of `quilt`.
#| * __explainability__: It should be easy to explain how configuration files are found by default to avoid confusing novice users. 
#|
#| This is a more subtle problem to implement might be expected. 
#| It might not be possible to accomplish all these criteria with any one set of parameter values.
#| For example, there are some options (e.g. `path`, `config_name`) that can modify which configuration files are found yet
#| these same options must be specifiable in configuration files according to the
#| __embeddedability__ criteria, creating the potential for a circular dependency. 
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
#| ### The structure of configuration files
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

