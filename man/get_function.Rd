% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{get_function}
\alias{get_function}
\title{Get functions in package}
\usage{
get_function(package, pattern)
}
\arguments{
\item{package}{(\code{character} of length 1) The name of the package to search for functions
in.}

\item{pattern}{(\code{character} of length 1) A regular expression that functions' name must 
match.}
}
\value{
\code{list} of \code{function}
}
\description{
Get functions in package that match a regular expression. 
Private functions are returned as well.
}
\details{
Inspired by:
http://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package
}

