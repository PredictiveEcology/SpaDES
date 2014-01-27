\name{ABM-package}
\alias{ABM-package}
\alias{ABM}
\docType{package}
\title{
What the package does (short line)
~~ package title ~~
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab ABM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-17\cr
License: \tab What license is it under?\cr
Depends: \tab methods\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
\dontrun{
N = 35
ntimesteps = 300#365 * 4

# SELES version
al = AgentLocation(Which(hab > 3 | hab < 1))
pri = ProbInit(hab*al)
na = NumAgents(N)
caribou = agent.init()

caribou = Transitions(hab[caribou@pos]>3, caribou)

sam = sample(N,min(500,N)) # for speed, this sample of the individuals is used for plotting (See below)
plot(hab)
lineat3 = min(rowFromCell(hab,Which(hab < 3,cells = T)))
black.line = raster(hab)
black.line[] = NA
black.line[cellFromRow(hab,lineat3)] = 1

plot(black.line, add = T, col = "black")
points(caribou@pos[sam,],pch=19,cex = 1, col = caribou@ids, xpd = NA)

for(i in 1:ntimesteps) {
  ex =  hab[caribou@pos] # found out what pixels the individuals are on now
  if (sum(is.na(ex))==N) stop(paste("all agents off map at time",i))
  ln = rlnorm(length(ex),-0.1,0.01) # log normal step length
  caribou = crw(caribou, step.len = ifelse(is.na(ex),1,ifelse(ex>1,ln, 0.02)) , dist.sd = ifelse(is.na(ex),1,ex), dir.sd = dir.sd)

  p = hab[caribou@pos] > 3
  points(caribou@pos[which(p==0)], cex = 2, pch = 19)
  caribou = Transitions(p, caribou)
#  caribou = crw(caribou, step.len = ln, dist.sd = 1, dir.sd = dir.sd)
  points(caribou@pos[sam,],pch=19,cex = 0.4, col = 1:length(caribou@pos), xpd = NA)
  dev.flush() # makes each time step visualized. Remove this for a slightly faster simulation
}
}
}
