\docType{methods}
\name{crw}
\alias{crw}
\title{Simple Correlated Random Walk}
\usage{
crw(agent, step.len, dir.sd)
}
\arguments{
  \item{agent}{An object of class mobileAgent.}

  \item{step.len}{Numeric vector of length 1 or number of
  agents describing step length.}

  \item{dir.sd}{Numeric vector of length 1 or number of
  agents describing sd of wrapped normal turn angles.}
}
\value{
An agent object with updated spatial position defined by a
single occurrence of step length(s) and turn angle(s).
}
\description{
This version uses just turn angles and step lengths to
define the correlated random walk.
}
\details{
This simple version of a correlated random walk is largely
the version that was presented in Turchin 1998, but it was
also used with bias modifications in McIntire, Schultz,
Crone 2007.
}
\references{
Turchin, P. 1998. Quantitative analysis of movement:
measuring and modeling population redistribution in animals
and plants. Sinauer Associates, Sunderland, MA.

McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007.
Designing a network for butterfly habitat restoration:
where individuals, populations and landscapes interact.
Journal of Applied Ecology 44:725-736.
}
\seealso{
\code{\link{mobileAgent}}
}

