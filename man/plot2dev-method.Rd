\docType{methods}
\name{plot2dev}
\alias{newPlot}
\alias{plot2dev}
\title{Specify where to plot}
\usage{
plot2dev(x, ...)

newPlot(...)
}
\arguments{
  \item{...}{Additional arguments passed to quartz(),
  x11(), or windows().}

  \item{...}{Additional arguments passed to quartz(),
  x11(), or windows().}
}
\value{
Opens a new plot device on the screen.

Opens a new plot device on the screen.
}
\description{
Switch to an existing plot device, or if not already open,
launch a new graphics device based on operating system
used.

Launch a new graphics device based on operating system
used. Mac OS: open device with \code{quartz()}. Linux: open
device with \code{x11()}. Windows: open device with
\code{windows()}.
}
\details{
For example, \code{plot2(6)} switches the active plot
device to device #6. If it doesn't exist, it opens it.
NOTE: if devices 1-5 don't exist they will be opened too.
}

