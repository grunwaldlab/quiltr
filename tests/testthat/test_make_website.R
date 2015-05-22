library(labtools)
context("Website creation")

test_that("note files are found", {
  test_path <- normalizePath("test_notebook/")
  html_paths <- c("dir_1/note_b.html",
                  "dir_2/dir_2.1/note_e.html",
                  "dir_2/note_c.html", 
                  "dir_2/note_g.html",
                  "dir_3-submenu_3.1/submenu_3.1.1-submenu_3.1.1.1-note_d.html", 
                  "note_a.html",
                  "note_f.html")
  expect_equal(labtools:::get_note_files(test_path, full_names = FALSE), html_paths)
  expect_equal(labtools:::get_note_files(test_path), file.path(test_path, html_paths))
  expect_equal(labtools:::get_note_files(character(0)), character(0))
})

test_that("dependencies can be found", {
  paths <- c("test_notebook/note_a.html", "test_notebook/dir_2/note_c.html")
  expect_is(get_file_dependencies(paths), "list")
  expect_is(get_file_dependencies(paths, simplify = TRUE), "character")
  expect_warning(get_file_dependencies("test_notebook/note_f.html"), "dependencies do not exist")
  expect_error(get_file_dependencies("test_notebook/r_logo.jpg"), "Unsupported file type")
  expect_error(get_file_dependencies("this_file_does_not_exist.html"), "The following input files do not exist")
})
