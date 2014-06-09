\docType{methods}
\name{initialize,SimList-method}
\alias{initialize,SimList-method}
\title{initialize SimList}
\usage{
\S4method{initialize}{SimList}(.Object, ..., times = list(start = 0, stop =
  NA_real_))
}
\arguments{
  \item{times}{A named list of simulation start and stop
  times (e.g., \code{times=list(start=0.00, stop=10.00)}).}

  \item{.Object}{ An object: see the Details section.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
initialize SimList
}

