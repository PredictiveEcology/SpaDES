\docType{methods}
\name{plot,rasterList,ANY-method}
\alias{plot,rasterList,ANY-method}
\title{Plot \code{rasterList} Class}
\usage{
\S4method{plot}{rasterList,ANY}(x, ..., which.to.plot = "all",
  speedup = 100, axes = "L", add = F)
}
\arguments{
  \item{x}{rasterList object.}

  \item{which.to.plot}{numeric or character vector
  identifying which rasters in \code{rasterList} to plot.}

  \item{speedup}{scalar indicating how much faster than
  normal to make plots (see Details).}

  \item{axes}{string either "all", "L", or "none" (see
  Details). Default is "L".}

  \item{add}{logical indicating whether to plot new maps
  (F) or update exising maps (T). Default is F.}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\value{
Creates a plot within the active plotting device.

#
}
\description{
Extends \code{plot} function for use in simulations.
}
\details{
This method is designed to plot many rasters that are
organized into a \code{rasterList}, which is simply a list
of rasters with a specified class. This method
automatically plots individual rasters in with an
arrangement on the plotting window that is optimal for the
size and shape of the window. \code{Speedup} will make
downsample the number of pixels, allowing for greater
plotting speed, at a cost of more fuzzy plots.
}
\seealso{
\code{\link{plot}} and \code{\link{rasterList}}
}

