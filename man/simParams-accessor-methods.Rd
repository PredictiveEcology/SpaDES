\docType{methods}
\name{simParams}
\alias{<-}
\alias{simParams}
\alias{simParams,simList-method}
\alias{simParams<-}
\alias{simTimes}
\title{Accessor methods for \code{simList} object slots}
\usage{
simParams(object)

\S4method{simParams}{simList}(object)

simParams(object) <- value

simTimes(object)
}
\arguments{
  \item{object}{A \code{simList} simulation object.}

  \item{value}{The object to be stored at the slot.}

  \item{object}{A \code{simList} simulation object.}

  \item{value}{The object to be stored at the slot.}
}
\value{
Returns or sets the value of the slot from the
\code{simList} object.

Returns or sets the value of the slot from the
\code{simList} object.
}
\description{
Currently, only get and set methods are defined. Subset
methods are not.

get list of simulation parameters

set list of simulation parameters

set list of simulation parameters

Currently, only get and set methods are defined. Subset
methods are not.
}
\details{
Additonal methods are provided to access the current,
start, and stop times of the simulation:
\code{simCurrentTime(sim)}, \code{simStartTime(sim)},
\code{simStopTime(sim)}.

Additonal methods are provided to access the current,
start, and stop times of the simulation:
\code{simCurrentTime(sim)}, \code{simStartTime(sim)},
\code{simStopTime(sim)}.
}
\author{
Alex Chubaty

Alex Chubaty
}

