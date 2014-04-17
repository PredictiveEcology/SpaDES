\docType{methods}
\name{dosim}
\alias{dosim}
\alias{dosim,SimList,missing-method}
\title{Process a simulation event}
\usage{
dosim(sim, debug)

\S4method{dosim}{SimList,missing}(sim)
}
\arguments{
  \item{sim}{A \code{SimList} simulation object.}

  \item{debug}{Optional logical flag determines whether sim
  debug info will be printed. If not specified, the default
  is \code{debug=FALSE}.}
}
\value{
Returns the modified \code{SimList} object.
}
\description{
Internal function called from \code{dosim}.

dosim
}
\details{
Calls the module corresponding to the event call, and
executes the event.
}
\note{
The debug option is primarily intended to facilitate
building simulation models by the user. Setting
\code{debug=TRUE} allows the user to toggle debugging
statements in their own modules.
}
\examples{
\dontrun{mySim <- sim.init(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
modules=list("habitat", "caribou"), path="/path/to/my/modules/)}
\dontrun{dosim{mySim}}
}
\seealso{
\code{\link{sim.init}}.
}

