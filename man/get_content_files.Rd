% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/website.R
\name{get_content_files}
\alias{get_content_files}
\title{Get paths to content}
\usage{
get_content_files(path, q_opt, full_names = TRUE, simplify = TRUE)
}
\arguments{
\item{path}{(\code{character}) One or more directories in which to look for content files.}

\item{q_opt}{(\code{function}) The function used to get context-specific option values}

\item{full_names}{(\code{logical} of length 1) See \code{\link{list.files}} help for option
\code{full.names}.}

\item{simplify}{(\code{logical} of length 1) If \code{FALSE}, a \code{list} of paths are returned
with elements corresponding to input directories in the \code{path} argument. If \code{TRUE}, a
single \code{character} vector is returned.}
}
\value{
Depends on the \code{simplify} option.
}
\description{
Return the absolute paths to content files in a given directory.
}

