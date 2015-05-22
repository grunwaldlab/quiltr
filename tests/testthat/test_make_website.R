library(labtools)
context("Website creation")

test_that("note files are found", {
  test_path <- normalizePath("tests/testthat/test_notebook/")
  html_paths <- c("dir_1/note_b.html",
                  "dir_2/dir_2.1/note_e.html",
                  "dir_2/note_c.html", 
                  "dir_2/note_g.html",
                  "dir_3-submenu_3.1/submenu_3.1.1-submenu_3.1.1.1-note_d.html", 
                  "note_a.html",
                  "note_f.html")
  expect_equal(labtools:::get_note_files(test_path, full_names = FALSE),
               html_paths)
  expect_equal(labtools:::get_note_files(test_path),
               file.path(test_path, html_paths))
  
})