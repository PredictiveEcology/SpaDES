\name{AgentLocation}
\alias{AgentLocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AgentLocation (SELES property)
}
\description{
Specify a function that will create a RasterLayer of 1s where the agents can be. All
non-available locations are given NA. This can then be used to use functions like
clump that requires NAs
}
\usage{
AgentLocation(fn = fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{
A function of any maps that will result in some set of pixels in the Raster being
1.
}
}
\details{
This attempts to replicate the functionality of AGENTLOCATION in SELES
}
\value{
Returns a RasterLayer with 1s and NAs.
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
See Also \code{\link{ProbInit}}
}
\examples{


set.seed(123)
ny = 200
nx = 200
hab <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn = -ny/2, ymx = ny/2)
hab <- setValues(hab,round(runif(nx*ny)*2.7 +1 ))
pts <- SpatialPoints(xyFromCell(hab,cell=sampleRandom(hab,10,cells=T)[,"cell"]))
dfp = distanceFromPoints(hab,pts)

al = AgentLocation(Which(hab > 3 & dfp < 5))

al.clump = clump(al) # can't actually see the tiny pixels. This makes them coloured

layers = stack(hab,dfp,buffer(al.clump,2.99)) # adds extra cells around the random ones to be able to see them

plot(layers)
 

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
