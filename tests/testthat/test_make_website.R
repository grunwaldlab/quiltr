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
  expect_equal(get_note_files(test_path, full_names = FALSE), html_paths)
  expect_equal(get_note_files(test_path), file.path(test_path, html_paths))
  expect_equal(get_note_files(character(0)), character(0))
})

test_that("dependencies can be found", {
  paths <- c("test_notebook/note_a.html", "test_notebook/dir_2/note_c.html")
  expect_equal(length(get_file_dependencies("test_notebook/dir_1/note_b.html", simplify = TRUE)), 1)
  expect_is(get_file_dependencies(paths), "list")
  expect_is(get_file_dependencies(paths, simplify = TRUE), "character")
  expect_warning(get_file_dependencies("test_notebook/note_f.html"), "dependencies do not exist")
  expect_error(get_file_dependencies("test_notebook/r_logo.jpg"), "Unsupported file type")
  expect_error(get_file_dependencies("this_file_does_not_exist.html"), "The following input files do not exist")
})



test_that("Hierarchy is made correctly", {
  paths <- c("test_notebook/dir_3-submenu_3.1/submenu_3.1.1-submenu_3.1.1.1-note_d.html",
             "test_notebook/note_f.html",
             "test_notebook/dir_2/note_g.html")
  expect_equal(get_note_hierarchy(paths, root = "test_notebook"),
               list(list(c("dir_3"),
                         c("dir_3", "submenu_3.1"),
                         c("dir_3", "submenu_3.1", "submenu_3.1.1"),
                         c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1")),
                    list(c("custom"),
                         c("custom", "note"),
                         c("custom", "note", "placement")),
                    list(c("dir_2"),
                         c("dir_2", "relative"),
                         c("dir_2", "relative", "note"),
                         c("dir_2", "relative", "note", "placement"))))
  expect_equal(get_note_hierarchy(path = paths, root = "test_notebook", cumulative = FALSE),
               list(list(c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1")),
                    list(c("custom", "note", "placement")),
                    list(c("dir_2", "relative", "note", "placement"))))
  expect_equal(get_note_hierarchy(path = paths, root = "test_notebook", use_file_names = FALSE),
               list(list(c("dir_3"),
                         c("dir_3", "submenu_3.1")),
                    list(c("custom"),
                         c("custom", "note"),
                         c("custom", "note", "placement")),
                    list(c("dir_2"),
                         c("dir_2", "relative"),
                         c("dir_2", "relative", "note"),
                         c("dir_2", "relative", "note", "placement"))))
  expect_equal(get_note_hierarchy(paths, root = "test_notebook", use_dir_names = FALSE),
               list(list(c("submenu_3.1.1"),
                         c("submenu_3.1.1", "submenu_3.1.1.1")),
                    list(c("custom"),
                         c("custom", "note"),
                         c("custom", "note", "placement")),
                    list(c("relative"),
                         c("relative", "note"),
                         c("relative", "note", "placement"))))
  expect_equal(get_note_hierarchy(paths, root = "test_notebook", use_file_suffix = TRUE),
               list(list(c("dir_3"),
                         c("dir_3", "submenu_3.1"),
                         c("dir_3", "submenu_3.1", "submenu_3.1.1"),
                         c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1"),
                         c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1", "note_d")),
                    list(c("custom"),
                         c("custom", "note"),
                         c("custom", "note", "placement")),
                    list(c("dir_2"),
                         c("dir_2", "relative"),
                         c("dir_2", "relative", "note"),
                         c("dir_2", "relative", "note", "placement"))))
})