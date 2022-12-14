% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7zip.R
\name{extract_7z}
\alias{extract_7z}
\title{Extract an archive using 7zip}
\usage{
extract_7z(archive, target_dir, password = NULL, path_7zip = NULL)
}
\arguments{
\item{archive}{the archive file}

\item{target_dir}{the target directory}

\item{password}{the password of the archive, if any}

\item{path_7zip}{See section below. Default to \code{\link[=get_7z_dir]{get_7z_dir()}}.}
}
\value{
the success/error message. Mainly used for its side effect of extracting the archive.
}
\description{
Use 7zip to extract an archive. Obviously, 7zip should be installed beforehand.
}
\section{Install 7-zip}{


If 7-zip is not installed on your computer, the easiest way to get it is to run \code{\link[installr:install.7zip]{installr::install.7zip()}}.
}

\section{Add 7zip to the \code{PATH}}{


For this function to work, 7-zip should be registered as a \code{PATH} environment variable. There are many resources online to help you register if, e.g. https://www.java.com/en/download/help/path.html.
If you cannot change the \code{PATH} on your computer, then you can add the path to 7zip executable directory in the parameter \code{path_7zip} or globally using \code{options(path_7zip="path/to/7zip")}.
This will add 7zip to the \code{PATH} for the current session.
}

\seealso{
https://info.nrao.edu/computing/guide/file-access-and-archiving/7zip/7z-7za-command-line-guide#section-17
}
