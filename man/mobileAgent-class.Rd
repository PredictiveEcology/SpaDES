\docType{class}
\name{mobileAgent-class}
\alias{mobileAgent-class}
\title{The \code{mobileAgent} class.}
\description{
Extends the \code{pointAgent} class. Describes
non-stationary spatial agents whose spatial #' components
are characterized by points (i.e., in contrast to a
polygon).
}
\section{Slots}{

\describe{
\item{\code{ID}}{A character string that identifies the individual agent.
             By default, the numbers 1...n are used.}

\item{\code{spatial}}{A \code{SpatialPoints} object.}

\item{\code{distance}}{A numeric describing the agent's heading (in degrees) from it's previous position.}

\item{\code{heading}}{A numeric describing the agent's distance from previous position.}

\item{\code{other}}{A list for storing other (user-specific) agent attributes.}
}}

