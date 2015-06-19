library(quiltr)
library(testthat)
context("Getting path-specific option values")

test_that("Correct paths are searched", {
  test_dir <- system.file("templates", "default", package = "quiltr")
  config_name <- "quiltr_config.yml"
  file.copy(from = test_dir, ".", recursive = TRUE, overwrite = TRUE)
  on.exit(unlink("./default", recursive = TRUE))
  expect_error(quiltr:::get_config_paths(path = file.path(test_dir, "home.Rmd"),
                                         name = config_name,
                                         root = file.path(test_dir, "markdown")), 
               "'root' is not a parent directory of 'path'")
  expect_equal(quiltr:::get_config_paths(path = file.path(test_dir, "input_types", "code", "c.c"),
                                         name = config_name,
                                         root = test_dir), 
               c(file.path(test_dir, config_name),
                 file.path(test_dir, "input_types", "code", config_name)))
  expect_equal(quiltr:::get_config_paths(path = test_dir,
                                         name = config_name,
                                         root = test_dir), 
               file.path(test_dir, config_name))
  expect_equal(quiltr:::get_config_paths(path = file.path(test_dir, "input_types", "code", "c.c"),
                                         name = config_name,
                                         root = test_dir, 
                                         must_exist = FALSE), 
               c(file.path(test_dir, config_name),
                 file.path(test_dir, "input_types", config_name),
                 file.path(test_dir, "input_types", "code", config_name)))
  expect_true(all(c(file.path(test_dir, config_name),
                    file.path(test_dir, "input_types", "code", config_name)) %in%
                    quiltr:::get_config_paths(path = file.path(test_dir, "input_types", "code", "c.c"),
                                              name = config_name,
                                              root = .Platform$file.sep)))
})