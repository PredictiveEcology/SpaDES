\docType{methods}
\name{spread}
\alias{spread}
\alias{spread,RasterLayer,integer,numeric,numeric,RasterLayer,numeric,integer,integer-method}
\title{Simulate a spread process on a landscape.}
\usage{
spread()


  \S4method{spread}{RasterLayer,integer,numeric,numeric,RasterLayer,numeric,integer,integer}(landscape,
  loci, spreadProb, persistance, mask, maxSize, directions, iterations)
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

  \item{iterations}{Number of iterations to spread. Leaving
  this \code{NULL} allows the spread to continue until
  stops spreading itself (i.e., exhausts itself).}
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

