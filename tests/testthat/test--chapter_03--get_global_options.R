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
#| A single configuration file will be used that specifies some options for each output type and some for options for all output type.
#| Other options are left as the defaults.
#|
#| #### Create config data
data_a <- list("config_search_type" = "parents",
               "website.config_search_type" = "root",
               "folder_b" = list("execute" = TRUE, 
                                 "display" = FALSE),
               "output_name" = "a_name",
               "book.overwrite" = TRUE)
#| 
#| #### Create folder
root_path = tempfile()
folder_path_a <- file.path(root_path, "folder_a")
dir.create(folder_path_a, recursive = TRUE)
#|
#| #### Create configuration file
file_path_a <- file.path(folder_path_a, "config.R")
dput(data_a, file = file_path_a)
#|
#| #### Call function 
#|
global_options <- quiltr:::get_global_options(main_function = "quilt",
                                              renderers = quiltr:::get_quilt_renderers(),
                                              config_path   = folder_path_a,
                                              config_name   = "config")
test_that("Output types have the correct global option values", {
  #| Lets test that settings without an output type specified are applied.
  expect_equal(global_options[["website", "output_name"]], "a_name")
  expect_equal(global_options[["book", "output_name"]], "a_name")
  expect_equal(global_options[["book", "config_search_type"]], "parents")
  #| Those not-output-type-specific settings should be overwritten by output-speicific settings.
  expect_equal(global_options[["website", "config_search_type"]], "root")
  #| Output-specific settings should also overwrite defaults.
  expect_equal(global_options[["book", "overwrite"]], TRUE)  
  #| Options not set should have default values as defined by the quilt function..
  expect_equal(global_options[["website", "overwrite"]], formals(quilt)$overwrite)
})
#|
#|
#| ### Redirecting using `config_path`
#|
#| A `config_path` setting in a configuration file should be able to specify a different configuration files to use.
#|
#| #### Create config data
data_a <- list("output_name" = "name_a",
               "config_path" = c(".",
                                 "folder_b"))
data_b <- list("output_name" = "name_b",
               "website.config_path" = "../folder_c")
data_c <- list("output_name" = "name_c")
#| 
#| #### Create folder
root_path = tempfile()
folder_path_a <- file.path(root_path, "folder_a")
folder_path_b <- file.path(folder_path_a, "folder_b")
folder_path_c <- file.path(root_path, "folder_c")
dir.create(folder_path_b, recursive = TRUE)
dir.create(folder_path_c, recursive = TRUE)
#|
#| #### Create configuration file
file_path_a <- file.path(folder_path_a, "config.R")
dput(data_a, file = file_path_a)
file_path_b <- file.path(folder_path_b, "config.yaml")
cat(yaml::as.yaml(data_b), file = file_path_b)
file_path_c <- file.path(folder_path_c, "config.R")
dput(data_c, file = file_path_c)
#|
#| #### Call function 
#|
global_options <- quiltr:::get_global_options(main_function = "quilt",
                                              renderers = quiltr:::get_quilt_renderers(),
                                              config_path   = folder_path_a,
                                              config_name   = "config")
