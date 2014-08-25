\docType{methods}
\name{drawArrows}
\alias{drawArrows}
\alias{drawArrows,SpatialPoints,SpatialPoints-method}
\title{Plots arrows showing direction of agent movement.}
\usage{
drawArrows(from, to, on.which.to.plot = 1, ...)

\S4method{drawArrows}{SpatialPoints,SpatialPoints}(from, to,
  on.which.to.plot = 1, ..., length = 0.1)
}
\arguments{
  \item{from}{Starting spatial coordinates
  (\code{SpatialPointsDataFrame}).}

  \item{to}{Ending spatial coordinates
  (\code{SpatialPointsDataFrame})..}

  \item{on.which.to.plot}{The name of a map layer on which
  to draw the arrows.}

  \item{...}{Additional plotting parameters.}

  \item{length}{The length of the arrows to draw (defaults
  to 0.1).}
}
\value{
Plots the vectors representing agent movement on the
specified map.
}
\description{
Plots arrows showing direction of agent movement.

Plot arrows showing direction of mobileAgent movement
}

