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
data_a <- list("group1.theme" = "example",
               "folder_b" = list("theme" = "value",
                                 "include" = FALSE),
               "other_option" = NA)
data_b <- list("path_2" = list("group2.include" = NULL),
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
#| #### Check if input is same as output
raw_config <- quiltr:::read_configuration_files(folder_paths = c(folder_path_a, folder_path_b),
                                                config_name = "config")
test_that("Configuration files can be read", {
  expect_identical(raw_config,
                   setNames(list(data_a, data_b), c(file_path_a, file_path_b)))
  expect_identical(quiltr:::read_configuration_files(folder_paths = folder_path_a,
                                                     config_name = "config"),
                   setNames(list(data_a), file_path_a))
  
})
#|
#| ## Reformating configuration file data
context("Reformating configuration file data")
#|
#| ### Typical usage
#|
#| We can use the test data generated above to test the reformating function.
option_names <- c("theme", "other_option", "include")
groups <- c("group1", "group2")
reformated <- quiltr:::reformat_configuration(raw_config, option_names, groups)
reformated
test_that("Configuration data can be reformatted", {
  expect_equal(reformated[[1, "option"]], "theme")
  expect_equal(reformated[[1, "value"]], "example")
  expect_equal(reformated[[1, "path"]], NA)
  expect_false(reformated[[3, "value"]])
  expect_equal(reformated[[4, "value"]], NA)
  expect_equal(reformated[[5, "option"]], "include")
  expect_null(reformated[[5, "value"]])
  expect_equal(reformated[[5, "group"]], "group2")
  expect_equal(reformated[[6, "value"]], list(x = c))
})
#|
#| ## Parsing configuration files
context("Parsing configuration files")
#|
#| ### Typical usage
#|
parsed <- quiltr:::parse_configuration(paths = c(folder_path_a, folder_path_b),
                                       valid_options = option_names,
                                       config_name = "config", 
                                       group_prefixes = groups)
parsed
test_that("Configuration files can be parsed", {
  expect_equal(parsed[[1, "option"]], "theme")
  expect_equal(parsed[[1, "value"]], "example")
  expect_false(parsed[[3, "value"]])
  expect_equal(parsed[[4, "value"]], NA)
  expect_equal(parsed[[5, "option"]], "include")
  expect_null(parsed[[5, "value"]])
  expect_equal(parsed[[5, "group"]], "group2")
  expect_equal(parsed[[6, "value"]], list(x = c))
})
#|
#| ### Using file paths instead of folder/config_name
#|
parsed_file <- quiltr:::parse_configuration(paths = c(file_path_a, file_path_b),
                                            valid_options = option_names,
                                            config_name = "config", 
                                            group_prefixes = groups)
parsed_mixed <- quiltr:::parse_configuration(paths = c(file_path_a, folder_path_b),
                                             valid_options = option_names,
                                             config_name = "config", 
                                             group_prefixes = groups)
test_that("Reference configuration file path explicitly", {
  expect_equal(parsed, parsed_file)
  expect_equal(parsed, parsed_mixed)
})

