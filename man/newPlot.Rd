\docType{methods}
\name{newPlot}
\alias{newPlot}
\title{Open a new plotting window}
\usage{
newPlot()
}
\value{
Opens a new plot device on the screen.
}
\description{
Launch a new graphics device based on operating system
used. Mac OS: open device with \code{quartz()}. Linux: open
device with \code{x11()}. Windows: open device with
\code{windows()}.
}

