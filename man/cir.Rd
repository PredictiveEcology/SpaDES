\name{cir}
\alias{cir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine coordinates of points in a circle around points in an agent object
}
\description{
Takes an agent object, and for a given vector of radiuses and number of vertices, will return a SpatialPointsDataFrame with 
points around each agent at their respective radiuses
}
\usage{
cir(agent, radiuses, nv = 36)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
Object of class "agent"
}
  \item{radiuses}{
Numeric or numeric vector with same length as agent 
}
  \item{nv}{
Numeric or numeric vector with same length as agent
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a SpatialPointsDataFrame with coordinates for each point around the agents. These will have an index column with value
equal to the original agent row number
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
function (agent, radiuses, nv = 36) 
{
    n = length(agent@pos)
    if (length(radiuses) == 1) {
        radiuses = matrix(rep(radiuses, n), 1)
    }
    else {
        radiuses = matrix(radiuses, nrow = 1)
    }
    angle.inc <- 2 * pi/nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    xs = matrix(agent@pos@coords[, "x"], ncol = 1)
    xv = t(cos(angles) \%*\% radiuses)
    x = apply(xv, 2, function(j) j + xs)
    ys = matrix(agent@pos@coords[, "y"], ncol = 1)
    yv = t(sin(angles) \%*\% radiuses)
    y = apply(yv, 2, function(j) j + ys)
    est.circles = lapply(1:n, function(i) SpatialPoints(list(x = x[i, 
        ], y = y[i, ])))
    return(est.circles)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
