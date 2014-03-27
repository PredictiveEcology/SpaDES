\docType{class}
\name{pointAgent-class}
\alias{pointAgent-class}
\title{The \code{pointAgent} class.}
\description{
Extends the \code{vectorAgent} class. Describes spatial
agents whose spatial components are characterized by points
(i.e., in contrast to a polygon).

Extends the \code{pointAgent} class. Describes
non-stationary spatial agents whose spatial #' components
are characterized by points (i.e., in contrast to a
polygon).
}
\details{
\describe{ \item{ID}{A character}

\item{spatial}{A \code{SpatialPoints} object.}

\item{other}{A list for storing other (user-specific) agent
attributes.} }

\describe{ \item{ID}{A character}

\item{spatial}{A \code{SpatialPoints} object.}

\item{heading}{A \code{numeric} describing the agent's
heading (in degrees) from it's previous position.}

\item{distance}{A \code{numeric} describing the agent's
distance from previous position.}

\item{other}{A list for storing other (user-specific) agent
attributes.} }
}

