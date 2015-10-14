#+ echo = FALSE
do.call(knitr::opts_chunk$set, source("knit_settings.R")$value)
#|
#| # Tests for finding configuration files
#|
library(quiltr)
library(testthat)
context("Finding configuration files")
#|
#| 
#|
#| ## Create folder structure for testing
root_path = tempfile()
folder_path_a <- file.path(root_path, "folder_a")
folder_path_b <- file.path(folder_path_a, "folder_b")
folder_path_c <- file.path(folder_path_b, "folder_c")
folder_path_d <- file.path(folder_path_c, "folder_d")
folder_path_e <- file.path(folder_path_c, "folder_e")
dir.create(folder_path_d, recursive = TRUE)
dir.create(folder_path_e, recursive = TRUE)
#| ## Test if current folder is found
result <- quiltr:::find_config_folders(folder_path_a, search_type = "root")
test_that("current folder is found", {
  expect_equal(result, folder_path_a)
})
#|
#| ## Test if parent folders are found
result <- quiltr:::find_config_folders(folder_path_a, search_type = "parent")
test_that("parent folder(s) are found", {
  expect_false(folder_path_a %in% result)
  expect_false(folder_path_b %in% result)
  expect_true(.Platform$file.sep %in% result)
  expect_true(root_path %in% result)
})
#|
#| ## Test if children are found
result <- quiltr:::find_config_folders(folder_path_a, search_type = "children")
test_that("child folder(s) are found", {
  expect_true(all(c(folder_path_b, folder_path_c, folder_path_d, folder_path_e) %in% result))
  expect_false(folder_path_a %in% result)
  expect_false(root_path %in% result)
})
#|
#| ## Test that more than one search type can be used
result <- quiltr:::find_config_folders(folder_path_a, search_type = c("root", "parents", "children"))
test_that("all folder(s) are found", {
  expect_true(all(c(root_path, folder_path_a, folder_path_b, folder_path_c, folder_path_d, folder_path_e) %in% result))
})

