\docType{methods}
\name{reload.module.later}
\alias{reload.module.later}
\alias{reload.module.later,character-method}
\title{Load modules for simulation.}
\usage{
reload.module.later(depends, ...)

\S4method{reload.module.later}{character}(depends, ...)
}
\arguments{
  \item{depends}{A list of character strings specifying the
  names of modules upon which the current module depends.}
}
\value{
\code{TRUE}/\code{FALSE}.
}
\description{
Checks the dependencies of the current module on other
modules. These dependencies need to be loaded first, so if
they are not already loaded, hold off loading the cuurent
module until after dependencies are loaded.
}
\seealso{
\code{\link{library}}.
}

