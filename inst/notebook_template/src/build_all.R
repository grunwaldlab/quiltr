#!/usr/bin/env Rscript
project_root <- labtools::get_project_root()
setwd(project_root)
source(file.path(project_root, ".Rprofile"))

# Make the website =================================================================================
labtools::make_website(notes_location = file.path(project_root, "results"),
             site_location = file.path(project_root, "doc"),
             header_html = file.path(project_root, "src", "in_header.html"), 
             pre_body_html = NULL, #default is to make the menu
             post_body_html = file.path(project_root, "src", "after_body.html"), 
             output_yaml = file.path(project_root, "src", "_output.yaml")
)