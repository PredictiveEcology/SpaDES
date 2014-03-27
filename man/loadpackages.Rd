\docType{methods}
\name{load.packages}
\alias{load.packages}
\alias{load.packages,list,ANY-method}
\alias{load.packages,list,logical-method}
\title{Load packages.}
\usage{
load.packages(package.list, install)

\S4method{load.packages}{list,logical}(package.list, install)

\S4method{load.packages}{list,ANY}(package.list)
}
\arguments{
  \item{package.list}{A list of character strings
  specifying the names of packages to be loaded.}

  \item{install}{Logical flag. If required packages are not
  already installed, should they be installed?}
}
\value{
Nothing is returned. Specified packages are loaded and
attached using \code{library()}.
}
\description{
Load and optionally install additional packages.
}
\seealso{
\code{\link{library}}.
}

