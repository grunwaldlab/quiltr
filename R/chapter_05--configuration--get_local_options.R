#+ echo = FALSE
knitr::opts_chunk$set(eval = FALSE)
#|
#| # Getting local options
#|
#|


get_path_specific_options <- function(sub_function, target_paths, config_paths, global_options) {
  main_function <- "quilt"
  
  #| ### Define default output structure
  #| The first thing we will do is make the two-dimensional list output structure.
  #| It will be populated with default values of `sub_function` (`quilt` in this case).
  #| To make the structure we need the list of target_paths and the list of `main_funciton` options.
  default_options <- as.list(formals(sub_function))
  output <- t(vapply(target_paths,
                     USE.NAMES = TRUE, FUN.VALUE = default_options, 
                     FUN = function(x) default_options))
  
  
  #| ### Order `config_paths` by folder depth
  #| Options defined in parent directories should be overwritten by options defined in their children. 
  #| Therefore, configurations files in folders closer to root should be applied first.
  
  
  #| ### Parse configuration files
  #| All of the configuration files are parsed at the same time and consolidated into the same 2-dimentional list.
  
  
  #| ### Define function to apply a single option
  #| This function will be run on each row of the parsed configuration file data and apply the changes specficied to the ouptut data. 
  apply_config_file <- function(config_path) {
    # Determine which file paths the option setting applies to
    
    # Apply the setting to the relevant files
    
  }
}