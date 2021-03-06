% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{make_quiltr_template}
\alias{make_quiltr_template}
\title{Make a template directory}
\usage{
make_quiltr_template(path, template = "default")
}
\arguments{
\item{path}{(\code{character} of length 1) Where to make the template directory.}

\item{template}{(\code{character} of length 1) That name of the template to use.
To see a list of available templates, use \code{\link{list_quiltr_templates}}.}
}
\value{
(\code{character} of length 1) The path to the template directory that was made.
}
\description{
Generates a template directory for use with \code{\link{quilt}} in the specified location.
}
\examples{
\dontrun{

# The following would make the directory "/home/my_name/documents/default"
make_quiltr_template(path = "/home/my_name/documents", template = "default")
}

}

