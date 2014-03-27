\docType{class}
\name{SimList}
\alias{SimList}
\alias{SimList-class}
\title{The SimList class}
\description{
This class contains the minimum components of a simulation.
}
\details{
An object of class SimList contains: events: the events
data.table (see below); .loaded: list of already loaded
modules; modules: list of required modules to load; params:
list of simulation parameters; simtime: the current
simulated time; debug: indicates debugging mode.  Each
event is represented by a data.table row consisting of:
event.time: the time the event is to occur; module.name:
the module from which the event is taken; event.type: a
character string for the programmer-defined event type; :
optional application-specific components.
}
\note{
You can still add notes
}
\section{Slots}{
  \describe{ \item{\code{.loaded}:}{List of character names
  specifying which modules are currently loaded.}
  \item{\code{modules}:}{List of character names specifying
  which modules to load.} \item{\code{params}:}{Named list
  of potentially other lists specifying simulation
  parameters.} \item{\code{events}:}{The list of scheduled
  events, as a data.table class.}
  \item{\code{simtime}:}{Numerical value describing the
  current simulation time.} \item{\code{debug}:}{Logical
  value specifying whether to run simulation in debugging
  mode.} }
}
\author{
Alex Chubaty
}

