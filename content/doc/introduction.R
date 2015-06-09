## ---- echo=FALSE, results='hide'-----------------------------------------
if (!file.exists("../inst/doc")) dir.create("../inst/doc")
file.copy(from = "test_website_1", to = "../inst/doc", recursive = TRUE, overwrite = TRUE)
file.copy(from = "test_website_2", to = "../inst/doc", recursive = TRUE, overwrite = TRUE)
file.copy(from = "custom.css", to = "../inst/doc", overwrite = TRUE)
file.copy(from = "placement.yml", to = "../inst/doc", overwrite = TRUE)

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

