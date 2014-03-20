\name{crw}
\alias{crw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a 3 parameter correlated random walk on an object of class agent
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crw(agent, step.len, dist.sd, dir.sd, hab = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
An object of class agent
}
  \item{step.len}{
Numeric vector or number. If a vector, then it must be the same length of the number of agents in the object \code{agent}
}
  \item{dist.sd}{
%%     ~~Describe \code{dist.sd} here~~
}
  \item{dir.sd}{
%%     ~~Describe \code{dir.sd} here~~
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
function (agent, step.len, dist.sd, dir.sd, hab = NULL) 
{
    if (class(agent) != "agent") {
        stop("must be an agent class")
    }
    n = length(agent@pos)
    agent@last.pos = agent@pos
    rand.dir = (rnorm(n, agent@direction, dir.sd))
    rand.dir = ifelse(rand.dir > 180, rand.dir - 360, ifelse(rand.dir < 
        (-180), 360 + rand.dir, rand.dir))
    rand.len = rlnorm(n, step.len, dist.sd)
    agent@pos@coords[, "y"] = agent@last.pos@coords[, "y"] + 
        cos(rad(rand.dir)) * rand.len
    agent@pos@coords[, "x"] = agent@last.pos@coords[, "x"] + 
        sin(rad(rand.dir)) * rand.len
    agent@direction = deg(atan((agent@pos@coords[, "x"] - agent@last.pos@coords[, 
        "x"])/(agent@pos@coords[, "y"] - agent@last.pos@coords[, 
        "y"])))
    agent@direction = ifelse((agent@pos@coords[, "y"] - agent@last.pos@coords[, 
        "y"]) < 0, ifelse((agent@pos@coords[, "x"] - agent@last.pos@coords[, 
        "x"]) < 0, agent@direction + 180 - 360, agent@direction + 
        180), agent@direction)
    agent@distance = sqrt((agent@last.pos@coords[, "y"] - agent@pos@coords[, 
        "y"])^2 + (agent@last.pos@coords[, "x"] - agent@pos@coords[, 
        "x"])^2)
    return(agent)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
