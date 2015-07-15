#| ## The quiltr source code
#|
#| ### Literate programming and the package's name
#|
#| The name the package and its primary function `quilt` is inspired an extended metaphor that first appeared in Donald Knuth's
#| _[Literate Programming](https://en.wikipedia.org/wiki/Literate_programming)_ and has since been expanded on in the documentation of 
#| [Sweave](https://www.statistik.lmu.de/~leisch/Sweave/) and [knitr](http://yihui.name/knitr/).
#|
#| > **Literate programming** integrates code and expository explanations/documentation into the same file.
#| > The focus of literate programs is to explain to _people_ how to make a computer do a given task
#| > rather than telling a _computer_ how to do it, although that is also done in the process.
#| > For example, the source of this document you are reading is a literate programming file
#| > that is part of the quiltr source code. 
#|
#| This name and purpose is best understood in the context of an extension of Knuth's metaphor used in the knitr package documentation.
#| The knitr function `spin` convertes code with comments writen in a
#| markup language ("goat's hair") into a literate programming document ("wool").
#| The markup portion of a literate programming document is typically [markdown](https://en.wikipedia.org/wiki/Markdown) or [latex](https://en.wikipedia.org/wiki/LaTeX) (rendered into html and pdf
#| respectivly) and the code is often [R](https://en.wikipedia.org/wiki/R_%28programming_language%29) or [python](https://en.wikipedia.org/wiki/Python_%28programming_language%29), although many other languages can also be used. 
#| The `knit` function can be used to execute the code in a literate programming document
#| ("wool") and integrate it and its results into the rendered markdown document.
#| The output of `knit` is prusumably some kind of textile if the metaphor holds ([mohair](https://en.wikipedia.org/wiki/Mohair)?).
#| This function is called `quilt` because it was originally designed to execute and consolidate
#| the output of many literate programming documents in specified folders into a portable website.
#|
#| <img src="`r file.path("..", "figures", "knitr_metaphore_figure.png")`">
#|
#| Each "patch" or "block" of the "quilt" is the rendering of a specific file in this metaphor.
#| Since quiltr is itself a literate program it can be run on its own source code to create
#| the documentation you are reading now.
#| 
#| ### The structure of quiltr
#| 
#| This document and the quiltr source code are the same set of files. 
#| If you look at the "source" of this document ("quiltr/R/chapter_00--the_quiltr_package.R") and other quiltr source files, 
#| you will see a plain text file with three distinct types of content:
#| 
#| * __Commented RMarkdown:__ These lines that start with `#|` and are rendered into HTML using `knitr::spin` and `knitr::knit`.
#| All formatted text, headers, and images are encoded in Rmarkdown.
#| Since these lines are commented out (i.e. start with `#`), they do not affect how the intermixed R code is executed. 
#| This is the "literate" part of a literate program and is used to explain the ideas behind the code and its implementation.
#| * __Roxygen2 Documentation:__ These lines start with `#`r "'"`` and are used to generate the documentation embedded into
#| the quiltr functions each time the package is built.
#| Their syntax is similar to and inspired by Latex.
#| The embedded documentation can be accessed by entering a function's name prefixed with `?` in an R console after the package is loaded.
#| It is used to remind users of parameters and basic function use rather than explain concepts and therefore writen in a terse and technical style
#| Like the commented RMarkdown, these lines have no effect on how the code actually executes.
#| Throughout this document, there might be some things explained in the Roxygen2 documentation that I will not explain the main text, 
#| in order to conform to the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) (don't repeat yourself) programming prinicipal. 
#| * __R code__: These lines have no prefix and constitue the quiltr source code that computers use.
#| It is the "program" part of a literate program.
#|
#| This section is mostly RMarkdown since it is the introduction, but examples of Roxygen2 documentation and R code appear below.
#| 
#| ### A description of the package
#| 
#| Below is the first example we will encounter of Roxygen2 documentation.
#| Typically, Roxygen2 appear before the code for functions, but in this case, it is used too describe the package as a whole.
#| The `NULL` after the Roxygen2 comments is a placeholder for where a function would normally be and is the first example of R code in the package.
#| This documentation can be accessed by typing `?quiltr` into an R console after the package is loaded using `library(quiltr)`.
#| 
#+ package_documentation, eval=FALSE
#' @title
#' The quiltr package
#'
#' @description 
#' This package includes tools for consolidating the file content of specified folders into a sharable
#' form (e.g. a website) in such a way that the relative file paths of the input folders/files
#' determine the organization the output.
#' In the case of website output, there are also tools to help post output on remote servers for hosting,
#' such as \href{https://pages.github.com/}{GitHub Pages}.
#' The primary function of this package is \code{\link{quilt}}; most other functions are wrappers or
#' components of \code{\link{quilt}}.
#' 
#' @details
#' Quiltr relies heavly on other R packages, especially \code{\link{knitr}} and \code{\link{rmarkdown}}.
#' The source of the package is available on GitHub at \url{https://github.com/grunwaldlab/quiltr} and more
#' documentation is available on the GitHub Pages site at \url{http://grunwaldlab.github.io/quiltr/}.
#' For error reports and requests for new/changed functionality, start an 
#' \href{https://guides.github.com/features/issues/}{issue} on the
#' \href{https://github.com/grunwaldlab/quiltr}{GitHub repository}.
#' Contributions are welcome and can be done by
#' \href{https://help.github.com/articles/fork-a-repo/}{forking}
#' the repository, making changes locally, and submitting a
#' \href{https://help.github.com/articles/using-pull-requests/}{pull request}. 
#' 
#' @author Zachary Foster and Niklaus Grunwald
#' 
#' @name quiltr
#' @docType package
NULL
