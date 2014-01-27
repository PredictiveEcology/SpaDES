\name{agent.init}
\alias{agent.init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initiate the agent object
}
\description{
This is used when using ABM in a SELES-like mode. It is equivalent to AgentLocation, 
ProbInit, NumAgents and it leads to the TRANSITIONS which would be next.
}
\usage{
agent.init(agentlocation = al, numagents = na, probinit = pri)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agentlocation}{
%%     ~~Describe \code{agentlocation} here~~
}
  \item{numagents}{
%%     ~~Describe \code{numagents} here~~
}
  \item{probinit}{
%%     ~~Describe \code{probinit} here~~
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
function (agentlocation = al, numagents = na, probinit = pri) 
{
    agent = as.agent()
    if (!is.null(numagents)) {
        if (!is.null(probinit)) {
            nonNAs = !is.na(getValues(pri))
            wh.nonNAs = which(nonNAs)
            ProbInit.v = cumsum(getValues(pri)[nonNAs])
            ran = runif(numagents, 0, 1)
            fI = findInterval(ran, ProbInit.v) + 1
            fI2 = wh.nonNAs[fI]
            agent@pos = xyFromCell(hab, fI2, spatial = T)
            ran = runif(numagents, 0, 1)
            fI = findInterval(ran, ProbInit.v) + 1
            fI2 = wh.nonNAs[fI]
            agent@last.pos = xyFromCell(hab, fI2, spatial = T)
        }
        else {
            agent@pos = SpatialPoints(sampleRandom(al, na, xy = T, 
                sp = T))
            agent@last.pos = SpatialPoints(sampleRandom(al, na, 
                xy = T, sp = T))
        }
    }
    else {
        if (!is.null(probinit)) {
            stop("you must declare NumAgents")
        }
    }
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
    agent@ids = 1:N
    return(agent)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
