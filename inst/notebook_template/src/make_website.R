library(yaml)
library(stringr)
library(Hmisc)

# Make website menu that will be included in each page =============================================

# Get menu heirarchy -------------------------------------------------------------------------------
note_directories <- Sys.glob(file.path("results","*"))
note_names <- basename(note_directories) 
dates <-  vapply(strsplit(note_names, "-"), `[`, character(1), 1)
hierarchy <- lapply(strsplit(note_names, "-"), `[`, -1)
names <- vapply(hierarchy, paste, character(1), collapse = "-")
hierarchy <- unique(unlist(lapply(hierarchy, function(x) lapply(seq_along(x), function(i) x[1:i])), recursive = FALSE))
hierarchy <- hierarchy[order(sapply(hierarchy, `[`, 1))]
depth <- vapply(hierarchy, length, numeric(1))


# Recursive function to make menu html -------------------------------------------------------------
make_nav <- function(index) {
  current <- hierarchy[[index]]
  children <- which(vapply(hierarchy, function(y) all(y[seq_along(current)] == current) & length(current) + 1 == length(y), logical(1)))
  if (paste(current, collapse = "-") %in% names) {
    dir_name <- note_names[paste(current, collapse = "-") == names]
    path <- file.path("..", dir_name, paste0("master_parent", ".html"))
  } else {
    path <- "#"
  }
  name <- gsub("_", " ", current[length(current)], fixed = TRUE)
  out <- ""
  if (length(current) == 1) {
    if  (length(children) == 0) {
      out <- paste0(out, '<div class="dropdown" style="position:relative;float:left"><a href="', path,'" class="btn btn-default">', name, '</a>\n<ul class="dropdown-menu">')
    } else {
      out <- paste0(out, '<div class="dropdown" style="position:relative;float:left"><a href="', path,'" class="btn btn-default dropdown-toggle" data-toggle="dropdown">', name, '<span class="caret"></span></a>\n<ul class="dropdown-menu">')
      child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
      out <- paste0(out, child_html)
    }
    out <- paste0(out, "</ul></div>")
  } else if (length(children) == 0) {
    out <- paste0(out, '<li><a href="', path, '">', name, '</a></li>')
  } else {
    out <- paste0(out, '<li><a class="trigger right-caret">', name, '</a>\n<ul class="dropdown-menu sub-menu">')
    child_html <- paste0(vapply(children, make_nav, character(1)), collapse = "")
    out <- paste0(out, child_html)
    out <- paste0(out, "</ul></li>")
  }
  return(out)
}

# Make menu ----------------------------------------------------------------------------------------
sink(file.path("src", "before_body.html"), append = FALSE)
# cat('<div class="dropdown" style="position:relative;float:left"><a href="../index/index.html" class="btn btn-default">Home</a></div>')
cat(paste0(lapply(which(depth == 1), make_nav), collapse = ""))
cat('<div style="clear:both"></div>')
sink()


# Render notes =====================================================================================


# Copy "results" to "doc/website" ------------------------------------------------------------------
website_path <- file.path(project_root, "doc", "website")
if (!file.exists(website_path)) dir.create(website_path, recursive = TRUE)
file.copy(note_directories, website_path, overwrite = TRUE, recursive = TRUE)
website_directories <- Sys.glob(file.path(website_path,"*"))
website_directories <- website_directories[file.info(website_directories)$isdir]

# Render notes -------------------------------------------------------------------------------------
get_rmd_yaml <- function(path, attribute, default = "") {
  do_once <- function(a_path) {
    content <- readChar(a_path, nchars = 10000)
    parsed_yaml <- yaml.load(str_match(content, "---\\\n(.*)---\\\n")[2])
    if (attribute %in% names(parsed_yaml)) return(parsed_yaml[[attribute]])
    return(as.character(default))
  }
  vapply(path, do_once, character(1)) 
}

render_directory_contents <- function(directory_path) {
  set.seed(100)
  
  render_rmd <- function() {
    master_rmd_name <- "master_parent.Rmd"
    master_rmd_path <- file.path(directory_path, master_rmd_name)
    website_root <- dirname(directory_path)
    files_to_remove <- master_rmd_path
    on.exit(lapply(files_to_remove[file.exists(files_to_remove)], file.remove))
    
    # Copy dependencies into the current directory - - - - - - - - - - - - - - - - - - - - - - - - -
    dependencies <- file.path(project_root, "src", c("after_body.html", 
                                                     "in_header.html",
                                                     "before_body.html",
                                                     "_output.yaml"))
    files_to_remove <- c(files_to_remove, file.path(directory_path, basename(dependencies)))
    file.copy(dependencies, directory_path, overwrite = TRUE)
    
    # Create master Rmd that references the original files - - - - - - - - - - - - - - - - - - - - - 
    if (file.exists(master_rmd_path)) file.remove(master_rmd_path)
    note_name <- basename(directory_path)
    date <- strsplit(note_name[1], '-')[[1]][1]
    date <- gsub("_", "/", date)
    rmd_paths <- Sys.glob(file.path(directory_path,"*.Rmd"))
    rmd_names <- basename(rmd_paths)
    rmd_titles <- get_rmd_yaml(rmd_paths, "title", default = NA)
    rmd_titles[is.na(rmd_titles)] <- rmd_names[is.na(rmd_titles)]
    chunks <- paste0("```{r child = '", rmd_names, "'}\n```\n\n")
    if (length(rmd_paths) > 1) {
      chunks <- paste0("# ", rmd_titles, "\n\n", chunks)
    }
    if (length(rmd_paths) > 1 || (length(rmd_paths) == 1 && is.na(rmd_titles[1]))) {
      title <- rev(strsplit(note_name[1], '-')[[1]])[1]
      title <- gsub("_", " ", title)
      title <- capitalize(title)
    } else {
      title <- rmd_titles[1]
    }
    header <- paste0('---\ntitle: "', title, '"\ndate: "', date, '"\n---\n\n')
    chunks <- paste0(chunks, collapse = "")
    cat(paste0(header, chunks), file = master_rmd_path, append = FALSE)
    html_paths <- gsub("\\.Rmd$", ".html", rmd_paths)
    files_to_remove <- c(files_to_remove, rmd_paths, html_paths)
    rmarkdown::render(master_rmd_path)
  }
  
  
  render_rmd()
}

# render_directory_contents(website_directories[1])

lapply(website_directories, render_directory_contents)


