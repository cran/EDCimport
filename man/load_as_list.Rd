% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{load_as_list}
\alias{load_as_list}
\title{Load a \code{.RData} file as a list}
\usage{
load_as_list(filename)
}
\arguments{
\item{filename}{the filename, with the \code{.RData} extension.}
}
\value{
a list
}
\description{
Instead of loading a \code{.RData} file in the global environment, extract every object into a list.
}
\examples{
x = list(a=1, b=mtcars)
save_list(x, "test.RData")
y = load_as_list("test.RData")
print(y$a)
}
