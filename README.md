
# Quiltr

[![Build Status](https://travis-ci.org/grunwaldlab/quiltr.png?branch=master)](https://travis-ci.org/grunwaldlab/quiltr?branch=master)
[![codecov.io](https://codecov.io/github/grunwaldlab/quiltr/coverage.svg?branch=master)](https://codecov.io/github/grunwaldlab/quiltr?branch=master)



## UNDER DEVELOPMENT

Quiltr is still under active development and might not always work correctly until this notice is removed.
It is being developed on the Ubuntu operating system, so it might not work on Windows yet.
It does work on Mac OS X. 


## What it is

Quiltr is an R package used display the content of files scattered throughout a file system into a single website organized by a hierarchical drop-down menu.
It is designed for compiling computational science project directories into a shareable form and making research notebooks.
For this reason, quiltr is most appropriate for creating websites to document projects, where ease of use, transparency, and reliability are the priority.
When used with RMarkdown, Quiltr extends the concept of [literate programming](https://en.wikipedia.org/?title=Literate_programming) to whole directories.


## What it is not

Making simple static websites for other purposes is also possible, but quiltr is not meant to be an all-purpose website generator such as [Jekyll](http://jekyllrb.com/) or [Drupal](https://www.drupal.org/).
Quiltr is designed to be very flexible in regard to input; it is has less flexibility for customizing output. 


## What it does

Quiltr makes a website representation of a directory.
Quiltr consolidates most types of files used in computational science in any directory structure into a website.
All of the raw content of the website, besides the drop down menu and other basic site structure elements, is derived from file content.
The organization of the website and where file content is displayed can be determined by the names of the files/directories or customized using configuration files scattered throughout the target directory.
There is also a wrapper function that makes it easy to upload and modify websites on [GitHub Pages](https://pages.github.com/) (where this site is hosted).


## Why use it

The benefit of this system is that there are no restrictions on the layout of the file structure and no knowledge of web design needed.
[RMarkdown](http://rmarkdown.rstudio.com/) is particularly suited to be used with quiltr since it is automatically knitted to HTML when the website is made.
Together, Quiltr and Markdown can be used to create websites without any knowledge of web design or HTML for free on any operating system (NOTE: not yet tested in Windows, but it should be compatible in theory).
It is so flexible that a website can be made from almost any directory, regardless of whether the directory was made for this purpose or not.


## An example

In the case of documenting computer science projects, the choice of directory structure can be governed by the needs of the project rather than the documentation.
R packages are a good example, in that they require a very specific directory structure, yet should have integrated, easily accessible documentation, such as vignettes. 
Quiltr can be used to consolidate vignettes into a website. 
This website you are on now is the result of running `quilt()` while in the Quiltr package source directory and each page is one of the vignettes you access individually by typing `browseVignettes(package = "quiltr")` into an R terminal. 
A single function call rebuilds the website and uploads it to GitHub pages each time changes are made. 


## Installation

[![Build Status](https://travis-ci.org/grunwaldlab/quiltr.png?branch=master)](https://travis-ci.org/grunwaldlab/quiltr?branch=master)

quiltr is an R package available on GitHub, so it can be installed from the RStudio console or an R command line session using the `devtools` command `install_github`.
First, make sure `devtools` is installed:


```r
install.packages("devtools")
```

Next, install the package from GitHub by entering:


```r
devtools::install_github("grunwaldlab/quiltr")
```

The package should now be installed and can be loaded using:


```r
library(quiltr)
```

Note that `quilt` uses [Pandoc](http://pandoc.org/) for syntax highlighting. 
If you want to use syntax highlighting for code files, [install Pandoc](http://pandoc.org/installing.html). 


## Quick start example

To get an idea of how Quiltr works you can start by making a website from one of Quiltr's included template directories. 
First you need to make a template directory.
Choose a location on your computer where you want a new directory to be made and use `make_quiltr_template` to create a template directory there.
`make_quiltr_template` will return the path to the new template directory. 


```r
make_quiltr_template("/where/to/make/template", template = "default") # Change this path
```

```
[1] "/where/to/make/template/default"
```

Then use `quilt` on the new template directory:


```r
quilt("/where/to/make/template/default")
```

```
[1] "/tmp/RtmpO6tA6y/file6ca11557aca6/website/index.html"
```

`quilt` will return the path to the homepage of the new website.
By default, the website is created in a temporary directory; use the `output` option to change this.
If you are using RStudio, the website should appear in your viewer pane when made.
Compare the resulting website to the structure of the template directory to get an idea of how Quiltr works.

## Documentation

For more information, visit our website at http://grunwaldlab.github.io/quiltr, which also happens to be an example of Quiltr output. 
