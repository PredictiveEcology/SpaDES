\docType{methods}
\name{SpreadEvents}
\alias{SpreadEvents}
\title{Simulate a spread process on a landscape.}
\usage{
SpreadEvents(landscape, loci = NULL, spreadProb = 0.1, persistance = NULL,
  mask = NULL, maxSize = NULL, directions = 8)
}
\arguments{
  \item{landscape}{A \code{RasterLayer} object.}

  \item{loci}{A list(?) of locations in \code{landscape}}

  \item{spreadProb}{The probability of spreading.}

  \item{persistance}{DOCUMENTATION NEEDED}

  \item{mask}{non-NULL, a \code{RasterLayer} object
  congruent with \code{landscape} whose elements are
  \code{0,1}, where 1 indicates "cannot spread to".}

  \item{maxSize}{DOCUMENTATION NEEDED}

  \item{directions}{The number adjacent cells in which to
  look; default is 8 (Queen case).}
}
\value{
A \code{RasterLayer} indicating the spread of the process
in the landscape.
}
\description{
More detailed description needed here.
}
\examples{
\dontrun{tmp <- raster(nrows=10, ncols=10, vals=0)}
 \dontrun{plot(tmp)}
 \dontrun{tmp <- SpreadEvents(tmp, spreadProb=0.225)}
 \dontrun{plot(tmp)}
}
\author{
Steve Cumming <Steve.Cumming
}

