#+ echo = FALSE
do.call(knitr::opts_chunk$set, source("knit_settings.R")$value)
#|
#| # Tests for getting global options
#|
library(quiltr)
library(testthat)
#|
#| ## Finding global option values
context("Finding global option values")
#|
#| ### Typical usage
#| 
#| We need a folder structure to test typical usage.
#| Lets make two nested folders and put an R config file in one and a YAML file in the other. 
#| We will attempt to read both test that the output and the input are the same.
#|
#| #### Create config data
data_a <- list("config_search_type" = "parents",
               "website.config_search_type" = "root",
               "folder_b" = list("execute" = TRUE, 
                                 "display" = FALSE),
               "output_name" = "a_name")
data_b <- list("path_2" = list("group2.display" = NULL),
               "other_option" = list("x" = c))
#| 
#| #### Create folders
root_path = tempfile()
folder_path_a <- file.path(root_path, "folder_a")
folder_path_b <- file.path(folder_path_a, "folder_b")
dir.create(folder_path_b, recursive = TRUE)
#|
#| #### Create files
file_path_a <- file.path(folder_path_a, "config.R")
dput(data_a, file = file_path_a)
file_path_b <- file.path(folder_path_b, "config.yaml")
cat(yaml::as.yaml(data_b), file = file_path_b)
#|
#| #### Call function 
#|
global_options <- quiltr:::get_global_options(main_function = "quilt",
                                              renderers = quiltr:::get_quilt_renderers(),
                                              config_path   = folder_path_a,
                                              config_name   = "config")
