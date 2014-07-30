\docType{methods}
\name{simLoaded}
\alias{<-}
\alias{simLoaded}
\alias{simLoaded,simList-method}
\alias{simLoaded<-}
\title{Accessor methods for \code{simList} object slots}
\usage{
simLoaded(object)

\S4method{simLoaded}{simList}(object)

simLoaded(object) <- value
}
\arguments{
  \item{object}{A \code{simList} simulation object.}

  \item{value}{The object to be stored at the slot.}
}
\value{
Returns or sets the value of the slot from the
\code{simList} object.
}
\description{
Currently, only get and set methods are defined. Subset
methods are not.

get list of loaded simulation modules

set list of loaded simulation modules

set list of loaded simulation modules
}
\details{
Additonal methods are provided to access the current,
start, and stop times of the simulation:
\code{simCurrentTime(sim)}, \code{simStartTime(sim)},
\code{simStopTime(sim)}.
}
\author{
Alex Chubaty
}

