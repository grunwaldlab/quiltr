#+ echo = FALSE
do.call(knitr::opts_chunk$set, source("knit_settings.R")$value)
#|
#| # Tests for getting local options
#|
library(quiltr)
library(testthat)
#|
#| ## Finding global option values
context("Finding local option values")
#|
#| ### Typical usage
#| 
#| We need a folder structure to test typical usage.
#| A single configuration file will be used that specifies some options for each output type and some for options for all output type.
#| Other options are left as the defaults.
#|
#| #### Create config data
data_a <- list("config_search_type" = "parents",
               "website.config_search_type" = "root",
               "folder_a/folder_b" = list("execute" = TRUE, 
                                          "display" = FALSE),
               "**.txt" = list("execute" = FALSE, 
                               "link" = TRUE),
               "*.txt" = list("link" = FALSE),
               "**/folder_b/*.R" = list("placement" = "x"))
#| 
#| #### Create folders
root_path <- tempfile()
folder_path_a <- file.path(root_path, "folder_a")
folder_path_b <- file.path(folder_path_a, "folder_b")
dir.create(folder_path_b, recursive = TRUE)
#|
#| #### Create configuration file
config_path <- file.path(root_path, "config.R")
dput(data_a, file = config_path)
#|
#| #### Create empty test files
files <- c(file.path(root_path, "111.txt"),
           file.path(folder_path_a, "222.txt"),
           file.path(folder_path_b, "333.txt"))
file.create(files)
#|
#| #### Call function 
#|
output <- quiltr:::get_path_specific_options(sub_function = quiltr:::get_quilt_renderers()$website,
                                             target_paths = files,
                                             config_paths = config_path,
                                             config_name = "config")