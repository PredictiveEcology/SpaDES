\docType{methods}
\name{simStopTime}
\alias{<-}
\alias{simStopTime}
\alias{simStopTime,simList-method}
\alias{simStopTime<-}
\title{Accessor methods for \code{simList} object slots}
\usage{
simStopTime(object)

\S4method{simStopTime}{simList}(object)

simStopTime(object) <- value
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

get the simulation stop time

set the simulation stop time

set the simulation stop time
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

