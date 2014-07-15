\docType{class}
\name{polygonAgent}
\alias{polygonAgent}
\alias{polygonAgent-class}
\title{The \code{polygonAgent} class.}
\description{
Extends the \code{vectorAgent} class. Describes spatial
agents whose spatial components are characterized by
polygons (i.e., in contrast to points or a raster).
}
\section{Slots}{

\describe{
\item{\code{ID}}{A character string that identifies the individual agent.
             By default, the numbers 1...n are used.}

\item{\code{spatial}}{A \code{SpatialPolygons} object.}

\item{\code{other}}{A list for storing other (user-specific) agent attributes.}
}}
\note{
add additional notes here.
}

