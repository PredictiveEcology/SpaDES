\docType{methods}
\name{spread.adj}
\alias{spread.adj}
\alias{spread.adj,RasterLayer,ANY,numeric,ANY,RasterLayer-method}
\title{Simulate a spread process on a landscape.}
\usage{
spread.adj(landscape, loci, spreadProb, persistance, mask, maxSize, directions,
  iterations, ...)

\S4method{spread.adj}{RasterLayer,ANY,numeric,ANY,RasterLayer}(landscape, loci,
  spreadProb, persistance, mask, maxSize, directions, iterations,
  plot.it = FALSE)
}
\arguments{
  \item{landscape}{A \code{RasterLayer} object.}

  \item{loci}{A vector of locations in \code{landscape}}

  \item{spreadProb}{The probability of spreading.}

  \item{persistance}{DOCUMENTATION NEEDED}

  \item{mask}{non-NULL, a \code{RasterLayer} object
  congruent with \code{landscape} whose elements are
  \code{0,1}, where 1 indicates "cannot spread to".}

  \item{maxSize}{The maximum number of pixels for a fire.
  This is currently only a single number, not one for each
  spread event}

  \item{directions}{The number adjacent cells in which to
  look; default is 8 (Queen case).}

  \item{iterations}{Number of iterations to spread. Leaving
  this \code{NULL} allows the spread to continue until
  stops spreading itself (i.e., exhausts itself).}

  \item{plot.it}{If TRUE, then plot the raster at every
  iteraction, so one can watch the spread event grow.}
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
 \dontrun{tmp <- spread(tmp, spreadProb=0.225)}
 \dontrun{plot(tmp)}
}
\author{
Steve Cumming \email{Steve.Cumming@sbf.ulaval.ca}
}

