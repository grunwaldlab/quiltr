#+ echo = FALSE
do.call(knitr::opts_chunk$set, source("knit_settings.R")$value)
#|
#| # Tests for parsing configuration files 
#|
library(quiltr)
library(testthat)
#|
#| ## Reading configuration files
context("Reading configuration files")
#|
#| ### Typical usage
#| 
#| We need a folder structure to test typical usage.
#| Lets make two nested folders and put an R config file in one and a YAML file in the other. 
#| We will attempt to read both test that the output and the input are the same.
#|
#| #### Create config data
data_a <- list("theme" = "example",
               "folder_b" = list(theme = "value",
                             include = FALSE),
               "other_option" = NA)
data_b <- list("path_2" = list(include = NULL),
               "other_option" = list(x = c))
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
#| #### Check if input is same as output
raw_output <- quiltr:::read_configuration_files(folder_paths = c(folder_path_a, folder_path_b),
                                                config_name = "config")
expected <- list(data_a, data_b)
raw_output
test_that("Configuration files can be read", {
  expect_identical(raw_output, list(data_a, data_b))
})
#|
#| ## Reformating configuration file data
context("Reformating configuration file data")
#|
#| ## Parsing configuration files
context("Parsing configuration files")
#|
