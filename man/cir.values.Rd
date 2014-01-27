\name{cir.values}
\alias{cir.values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine values within the circle around an \code{agent} object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cir.values(agent, rings, hab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
%%     ~~Describe \code{agent} here~~
}
  \item{rings}{
%%     ~~Describe \code{rings} here~~
}
  \item{hab}{
%%     ~~Describe \code{hab} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (agent, rings, hab) 
{
    alive = which(!is.na(agent@direction))
    id = rep(caribou@ids, times = sapply(rings, length))
    ring.hab.val = list()
    coords1 = do.call(rbind, lapply(rings, coordinates))
    ring.hab.val = split(extract(hab, coords1), id)
    if (!is.null(alive)) {
        ring.hab.val[!alive] = NA
    }
    return(ring.hab.val)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
