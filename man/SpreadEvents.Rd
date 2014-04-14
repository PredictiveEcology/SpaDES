\docType{methods}
\name{SpreadEvents}
\alias{SpreadEvents}
\title{Simulate a spread process on a landscape.}
\usage{
SpreadEvents(Landscape, Loci = NULL, SpreadProb = 0.1, Persistance = NULL,
  Mask = NULL, MaxSize = NULL, directions = 8)
}
\arguments{
  \item{Landscape}{A RasterLayer}

  \item{Loci}{A list(?) of locations in \code{Landscape}}

  \item{Mask}{non null, a RasterLayer congruent with
  Landscape whose elements are \code{0,1}, where 1
  indicates can not spread to.}
}
\value{
A \code{RasterLayer} indicating the spread of the process
in the landscape.
}
\description{
More detailed description needed here.
}
\examples{
\dontrun{tst <- raster(nrows=10, ncols=10, vals=0)}
}
\author{
Steve Cumming
}

