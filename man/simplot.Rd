\docType{methods}
\name{simplot}
\alias{simplot}
\alias{simplot,RasterStack-method}
\title{Ploting methods}
\usage{
simplot(x, ...)

\S4method{simplot}{RasterStack}(x, which.to.plot = "all", speedup = 100,
  axes = "L", add = F, ...)
}
\arguments{
  \item{x}{rasterStack object.}

  \item{which.to.plot}{numeric or character vector
  identifying which rasters in \code{rasterStack} to plot.}

  \item{speedup}{scalar indicating how much faster than
  normal to make plots (see Details).}

  \item{axes}{string either "all", "L", or "none" (see
  Details). Default is "L".}

  \item{add}{logical indicating whether to plot new maps
  (F) or update exising maps (T). Default is F.}
}
\value{
Creates a plot within the active plotting device.

#
}
\description{
Plotting for RasterStack, Agents and other classes used in
simulations.
}
\details{
For class \code{RasterStack}, this method plots many
rasters. This method automatically plots individual rasters
in with an arrangement on the plotting window that is
optimal for the size and shape of the window.
\code{Speedup} will make downsample the number of pixels,
allowing for greater plotting speed, at a cost of more
fuzzy plots.
}
\seealso{
\code{\link{grid.raster}}
}

