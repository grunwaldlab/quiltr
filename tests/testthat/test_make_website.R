# library(quiltr)
# context("Website creation")
# 
# 
# test_that("content files are found", {
#   test_path <- normalizePath("test_notebook/")
#   html_paths <- c("dir_1/note_b.html",
#                   "dir_2/dir_2.1/note_e.html",
#                   "dir_2/note_c.html", 
#                   "dir_2/note_g.html",
#                   "dir_3-submenu_3.1/submenu_3.1.1-submenu_3.1.1.1-note_d.html", 
#                   "note_a.html",
#                   "note_f.html",
#                   "note_h.html")
#   expect_equal(get_content_files(test_path, full_names = FALSE), html_paths)
#   expect_equal(get_content_files(test_path), file.path(test_path, html_paths))
#   expect_equal(get_content_files(character(0)), character(0))
# })
# 
# 
# test_that("dependencies can be found", {
#   print(getwd())
#   paths <- c("test_notebook/note_a.html", "test_notebook/dir_2/note_c.html")
#   if (!grepl(".Rcheck", getwd(), fixed = TRUE)) {
#     # The following tests if links and their targets are collapsed to a single dependency,
#     #    but the links are clobbered when copying the test directory during R CMD CHECK
#     expect_equal(length(get_file_dependencies("test_notebook/dir_1/note_b.html", simplify = TRUE)), 1)    
#   }
#   expect_is(get_file_dependencies(paths), "list")
#   expect_is(get_file_dependencies(paths, simplify = TRUE), "character")
#   expect_warning(get_file_dependencies("test_notebook/note_f.html"), "dependencies do not exist")
#   expect_error(get_file_dependencies("test_notebook/r_logo.jpg"), "Unsupported file type")
#   expect_error(get_file_dependencies("this_file_does_not_exist.html"), "The following input files do not exist")
# })
# 
# 
# test_that("Hierarchy is made correctly", {
#   paths <- c("test_notebook/dir_3-submenu_3.1/submenu_3.1.1-submenu_3.1.1.1-note_d.html",
#              "test_notebook/note_f.html",
#              "test_notebook/dir_2/note_g.html")
#   expect_equal(get_hierarchy(paths, root = "test_notebook"),
#                list(list(character(0),
#                          c("dir_3"),
#                          c("dir_3", "submenu_3.1"),
#                          c("dir_3", "submenu_3.1", "submenu_3.1.1"),
#                          c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1")),
#                     list(character(0),
#                          c("custom"),
#                          c("custom", "note"),
#                          c("custom", "note", "placement")),
#                     list(character(0),
#                          c("dir_2"),
#                          c("dir_2", "relative"),
#                          c("dir_2", "relative", "note"),
#                          c("dir_2", "relative", "note", "placement"))))
#   expect_equal(get_hierarchy(path = paths, root = "test_notebook", cumulative = FALSE),
#                list(list(c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1")),
#                     list(c("custom", "note", "placement")),
#                     list(c("dir_2", "relative", "note", "placement"))))
#   expect_equal(get_hierarchy(path = paths, root = "test_notebook", use_file_names = FALSE),
#                list(list(character(0),
#                          c("dir_3"),
#                          c("dir_3", "submenu_3.1")),
#                     list(character(0),
#                          c("custom"),
#                          c("custom", "note"),
#                          c("custom", "note", "placement")),
#                     list(character(0),
#                          c("dir_2"),
#                          c("dir_2", "relative"),
#                          c("dir_2", "relative", "note"),
#                          c("dir_2", "relative", "note", "placement"))))
#   expect_equal(get_hierarchy(paths, root = "test_notebook", use_dir_names = FALSE),
#                list(list(character(0),
#                          c("submenu_3.1.1"),
#                          c("submenu_3.1.1", "submenu_3.1.1.1")),
#                     list(character(0),
#                          c("custom"),
#                          c("custom", "note"),
#                          c("custom", "note", "placement")),
#                     list(character(0),
#                          c("relative"),
#                          c("relative", "note"),
#                          c("relative", "note", "placement"))))
#   expect_equal(get_hierarchy(paths, root = "test_notebook", use_file_suffix = TRUE),
#                list(list(character(0),
#                          c("dir_3"),
#                          c("dir_3", "submenu_3.1"),
#                          c("dir_3", "submenu_3.1", "submenu_3.1.1"),
#                          c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1"),
#                          c("dir_3", "submenu_3.1", "submenu_3.1.1", "submenu_3.1.1.1", "note_d")),
#                     list(character(0),
#                          c("custom"),
#                          c("custom", "note"),
#                          c("custom", "note", "placement")),
#                     list(character(0),
#                          c("dir_2"),
#                          c("dir_2", "relative"),
#                          c("dir_2", "relative", "note"),
#                          c("dir_2", "relative", "note", "placement"))))
#   expect_equal(get_hierarchy(path = paths, root = "test_notebook",
#                                   use_file_names = FALSE,
#                                   use_dir_names = FALSE,
#                                   use_config_files = FALSE), 
#                list(list(character(0)),
#                     list(character(0)),
#                     list(character(0))))
#   expect_equal(get_hierarchy(path = "test_notebook/note_h.html", root = "test_notebook"), 
#                list(list()))
#   expect_equal(get_hierarchy(path = "test_notebook/note_h.html", root = "test_notebook", cumulative = FALSE), 
#                list(list()))
# })