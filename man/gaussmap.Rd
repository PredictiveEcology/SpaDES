\docType{methods}
\name{GaussMap}
\alias{GaussMap}
\title{GaussMap}
\usage{
GaussMap(ext, scale = 10, var = 1, speedup = 10)
}
\arguments{
  \item{ext}{An object of class \code{extent} giving the
  dimensions of output map.}

  \item{scale}{The spatial scale in map units of the
  Gaussian pattern.}

  \item{var}{Spatial variance.}

  \item{speedup}{An index of how much faster than normal to
  generate maps.}
}
\value{
A map of extent \code{ext} with a Gaussian random pattern.

#
}
\description{
Produces a raster of a random gaussian process.
}
\details{
This is a wrapper for #' \code{RFsimulate} function in the
RandomFields package. The main addition is the
\code{speedup} argument which allows for faster map
generation. A \code{speedup} of 1 is normal and will get
progressively faster as the number increases, at the
expense of coarser pixel resolution of the pattern
generated
}
\seealso{
\code{\link{RandomFields::RFsimulate}} and
\code{\link{raster::extent}}
}

