\docType{methods}
\name{reloadModuleLater}
\alias{reloadModuleLater}
\alias{reloadModuleLater,ANY,character-method}
\title{Load modules for simulation.}
\usage{
reloadModuleLater(sim, depends)

\S4method{reloadModuleLater}{ANY,character}(sim, depends)
}
\arguments{
  \item{sim}{An object of class \code{simList}.}

  \item{depends}{A list of character strings specifying the
  names of modules upon which the current module depends.}
}
\value{
\code{Logical}.
}
\description{
Checks the dependencies of the current module on other
modules. These dependencies need to be loaded first, so if
they are not already loaded, hold off loading the current
module until after dependencies are loaded.
}
\author{
Alex Chubaty
}
\seealso{
\code{\link{library}}.
}

