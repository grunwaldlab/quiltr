% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/website.R
\name{make_output_yaml}
\alias{make_output_yaml}
\title{Generate _output.yaml for website}
\usage{
make_output_yaml(theme = "journal")
}
\arguments{
\item{theme}{(\code{character} of length 1) The bootstrap theme used in the website. For current
options, see \url{http://rmarkdown.rstudio.com/html_document_format.html#appearance-and-style}
\itemize{
  \item "default"
  \item "cerulean"
  \item "journal"
  \item "flatly"
  \item "readable"
  \item "spacelab"
  \item "united"
  \item "cosmo"
}}
}
\value{
(\code{character} of length 1)
}
\description{
Make the content of the file that contols knitting of the website page Rmds
}

