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
data_a <- list("book.config_search_type" = "parents",
               "website.config_search_type" = "root",
               "open" = FALSE,
               "111.txt" = list("book.overwrite" = TRUE),
               "**.txt" = list("execute" = FALSE, 
                               "link" = TRUE),
               "*.txt" = list("link" = FALSE),
               "**/folder_b/*.R" = list("placement" = "x"),
               "**.R" = list("theme" = "test_theme"))
data_b <- list("555.R" = list("placement" = "y"),
               "../../111.txt" = list(theme = "test_theme_2"))
#| 
#| #### Create folders
root_path <- tempfile()
folder_path_a <- file.path(root_path, "folder_a")
folder_path_b <- file.path(folder_path_a, "folder_b")
dir.create(folder_path_b, recursive = TRUE)
#|
#| #### Create configuration file
config_paths <- c(file.path(root_path, "config.R"),
                  file.path(folder_path_b, "config.R"))
dput(data_a, file = config_paths[1])
dput(data_b, file = config_paths[2])
#|
#| #### Create empty test files
files <- c(file.path(root_path, "111.txt"),
           file.path(folder_path_a, "222.txt"),
           file.path(folder_path_b, "333.txt"),
           file.path(folder_path_b, "444.R"),
           file.path(folder_path_b, "555.R"))
file.create(files)
#|
#| #### Call function 
#|
output <- quiltr:::get_path_specific_options(sub_function = "website",
                                             target_paths = files,
                                             config_paths = config_paths,
                                             config_name = "config")

test_that("options for other output types are not used", {
  expect_equal(output[[files[1], "overwrite"]], FALSE)
})

test_that("options for undefined output types are used", {
  expect_true(all(output[, "open"] == FALSE))
})

test_that("setting in the same file can be overwritten", {
  expect_equal(output[[files[1], "link"]], FALSE)
})

test_that("setting in a differnet file can be overwritten", {
  expect_equal(output[[files[5], "placement"]], "y")
})

test_that("double wildcards work", {
  expect_equal(output[[files[4], "theme"]], "test_theme")
  expect_equal(output[[files[5], "theme"]], "test_theme")
})

test_that("Moving up the folder structure with '..' works", {
  expect_equal(output[[files[1], "theme"]], "test_theme_2")
})