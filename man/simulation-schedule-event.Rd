\docType{methods}
\name{schedule.event}
\alias{schedule.event}
\alias{schedule.event,SimList,numeric,character,character-method}
\title{Schedule a simulation event}
\usage{
schedule.event(sim, event.time, module.name, event.type)

\S4method{schedule.event}{SimList,numeric,character,character}(sim, event.time,
  module.name, event.type)
}
\arguments{
  \item{sim}{A \code{SimList} simulation object.}

  \item{event.time}{A numeric specifying the time of the
  next event.}

  \item{module.name}{A character string specifying the
  module from which to call the event.}

  \item{event.type}{A character string specifying the type
  of event from within the module.}
}
\value{
Returns the modified \code{SimList} object.
}
\description{
Adds a new event to the simulation's event queue, updating
the simulation object.

schedule event
}
\examples{
\dontrun{}
}

