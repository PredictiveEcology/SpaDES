\docType{methods}
\name{doEvent}
\alias{doEvent}
\title{Process a simulation event}
\usage{
doEvent(sim, debug)
}
\arguments{
  \item{sim}{A \code{simList} simulation object.}

  \item{debug}{Optional logical flag determines whether sim
  debug info will be printed (default is
  \code{debug=FALSE}).}
}
\value{
Returns the modified \code{simList} object.
}
\description{
Internal function called from \code{doSim}.
}
\details{
Calls the module corresponding to the event call, and
executes the event.

Based on code from Matloff (2011) ch. 7.8.3 on discrete
event simulation: - implemented using S4 classes and
methods; - uses `data.table` instead of `data.frame` (which
is much faster); - implemented in a more modular fashion so
it's easier to add submodules to the simulation.
}
\author{
Alex Chubaty
}
\references{
Matloff, N. (2011). The Art of R Programming (373 pp.). San
Fransisco, CA: No Starch Press, Inc.. Retrieved from
\url{http://www.nostarch.com/artofr.htm}
}

