\docType{methods}
\name{doSim}
\alias{doSim}
\alias{doSim,simList,logical-method}
\alias{doSim,simList,missing-method}
\title{Process a simulation event}
\usage{
doSim(sim, debug)

\S4method{doSim}{simList,logical}(sim, debug)

\S4method{doSim}{simList,missing}(sim)
}
\arguments{
  \item{sim}{Character string for the \code{simList}
  simulation object.}

  \item{debug}{Optional logical flag determines whether sim
  debug info will be printed (default is
  \code{debug=FALSE}).}
}
\value{
Invisibly returns the modified \code{simList} object.
}
\description{
Internal function called from \code{doSim}.
}
\details{
Calls the module corresponding to the event call, and
executes the event.

Based on code from chapter 7.8.3 of Matloff (2011):
"Discrete event simulation". Here, we implement a
simulation in a more modular fashion so it's easier to add
submodules to the simulation. We use S4 classes and
methods, and use `data.table` instead of `data.frame` to
implement the event queue (because it is much faster).
}
\note{
The debug option is primarily intended to facilitate
building simulation models by the user. Setting
\code{debug=TRUE} allows the user to toggle debugging
statements in their own modules.
}
\examples{
\dontrun{
mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
modules=list("habitat", "caribou"), path="/path/to/my/modules/)
}
\dontrun{doSim{mySim}}
}
\author{
Alex Chubaty
}
\references{
Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
San Fransisco, CA: No Starch Press, Inc.. Retrieved from
\url{http://www.nostarch.com/artofr.htm}
}
\seealso{
\code{\link{simInit}}.
}

