\name{agent-class}
\Rdversion{1.1}
\docType{class}
\alias{agent-class}

\title{Class \code{"agent"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("agent", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{pos}:}{Object of class \code{"SpatialPoints"} ~~ }
    \item{\code{last.pos}:}{Object of class \code{"SpatialPoints"} ~~ }
    \item{\code{direction}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{distance}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ids}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{other}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{length}:}{Returns number of agents}
    \item{\code{coordinates}:}{Returns the coordinates of the agents, as a \code{SpatialPoints} object}
    \item{\code{arrow}:}{Plots the vectors from previous location to current location}
    \item{\code{points}:}{Plots the agent as points on a pre-existing plots}
  }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("agent")
}
\keyword{classes}
