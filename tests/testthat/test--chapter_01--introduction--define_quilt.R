#+ echo = FALSE
do.call(knitr::opts_chunk$set, source("knit_settings.R")$value)
#|
#| # Tests executing the quilt function
#|
library(quiltr)
library(testthat)
#|
#| ## Finding global option values
context("Running quilt")
#|
#| ### Typical usage
#| 
#|
#| #### Create config data
data_a <- list("output_format" = c("website", "book"))
data_b <- list("placement" = "data_b_placement")
config_data <- list(data_a, data_b)
#| 
#| #### Create folders
root_path <- tempfile()
folder_paths <- c(file.path(root_path, "folder_a"),
                  file.path(root_path, "folder_a", "folder_b"))
sapply(folder_paths, dir.create, recursive = TRUE)
#|
#| #### Create configuration file
config_name <- "quilt_conifg.R"
config_paths <- c(file.path(root_path, config_name),
                  file.path(folder_paths[2], config_name))
mapply(dput, config_data, file = config_paths)
#|
#| #### Create empty test files


#|
#| #### Call function 
#|
# output <- quilt(root_path)