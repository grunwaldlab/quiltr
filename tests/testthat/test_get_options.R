library(quiltr)
library(testthat)
context("Getting path-specific option values")

test_that("Correct paths are searched", {
  test_dir <- system.file("templates", "default", package = "quiltr")
  config_name <- "quilt_config.yml"
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




test_that("Option values are read correctly", {
  test_dir <- system.file("templates", "default", package = "quiltr")
  config_name <- "quilt_config.yml"
  file.copy(from = test_dir, ".", recursive = TRUE, overwrite = TRUE)
  on.exit(unlink("./default", recursive = TRUE))
  config_path <- file.path(test_dir, config_name)
  
  expect_equal(quiltr:::get_config_value(path = config_path, option = "name"),
               "Example")
  expect_equal(quiltr:::get_config_value(path = config_path, option = "placement"),
               list("home.Rmd" = ""))
  expect_equal(quiltr:::get_config_value(path = config_path, option = "does_not_exist"), NA)
})


test_that("Wildcards are expanded correctly", {
  test_dir <- system.file("templates", "default", package = "quiltr")
  config_name <- "quilt_config.yml"
  file.copy(from = test_dir, ".", recursive = TRUE, overwrite = TRUE)
  on.exit(unlink("./default", recursive = TRUE))

  expect_true(file.path("default", "input_types", "code", config_name) %in% quiltr:::sys_glob("**.yml"))
  expect_error(quiltr:::sys_glob("**/**.yml"), "Currently, Quiltr only supports one double wildcard")
})
