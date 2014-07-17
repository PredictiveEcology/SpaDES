\docType{methods}
\name{check.path}
\alias{check.path}
\title{Check filepath.}
\usage{
check.path(path, create = FALSE)
}
\arguments{
  \item{path}{A character string corresponding to a
  filepath.}

  \item{create}{A logical indicating whether the path
  should be created if it doesn't exist. Default is
  \code{FALSE}.}
}
\value{
Character string denoting the cleaned up filepath.
}
\description{
Checks the specified filepath for formatting consistencies,
such as trailing slashes, etc.
}
\seealso{
\code{\link{file.exists}}, \code{\link{dir.create}}.
}

