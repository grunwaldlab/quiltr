## ---- echo=FALSE, results='hide'-----------------------------------------
doc_path <- "../inst/doc"
if (file.exists(doc_path)) unlink(doc_path, recursive = TRUE)
dir.create(doc_path)
file.copy(from = "test_website_1", to = doc_path, recursive = TRUE, overwrite = TRUE)
file.copy(from = "test_website_2", to = doc_path, recursive = TRUE, overwrite = TRUE)
file.copy(from = "custom.css", to = doc_path, overwrite = TRUE)
file.copy(from = "placement.yml", to = doc_path, overwrite = TRUE)

## ----make_test_website_1, echo=FALSE, results='hide', eval=FALSE---------
#  # This is not run automatically due to rmarkdown issue #248, but is used to make the website example
#  quiltr::make_website(path = "vignette_examples/test_website_1", output = "website_source", overwrite = TRUE, output_dir_name = "test_website_1", theme = "cerulean")

## ---- echo=FALSE, comment=""---------------------------------------------
cat(readChar("../vignette_examples/test_website_2/old/note_config.yml", nchars = 10000))

## ---- echo=FALSE, comment=""---------------------------------------------
cat(readChar("../vignette_examples/test_website_2/old/sub_dir/note_config.yml", nchars = 10000))

## ----make_test_website_2, echo=FALSE, results='hide', eval=FALSE---------
#  # This is not run automatically due to rmarkdown issue #248, but is used to make the website example
#  quiltr::make_website(path = "vignette_examples/test_website_2", output = "website_source", overwrite = TRUE, output_dir_name = "test_website_2", note_config_name = "note_config.yml", theme = "cerulean")

