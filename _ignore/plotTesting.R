devtools::load_all("~/GitHub/SpaDES")

dev()
clearPlot()
#library(SpaDES)
library(stringi)
times <- list(start=0, end=53, timeunit="year")
parameters <- list(.globals=list(stackName="landscape", burnStats="burnStats"),#
                   .progress=list(type="graphical", interval = 1),
                   randomLandscapes=list(.plotInitialTime=times$start),
                   fireSpread=list(.plotInitialTime=times$start,
                                   startTime=times$start),
                   caribouMovement=list(N=20, torus=TRUE,
                                        moveInitialTime=times$start,
                                        .plotInitialTime=times$start,
                                        .plotInterval=1))#, .checkpoint=list(interval=1, file="chkpnt.RData"))
modules <- list("fireSpread", "caribouMovement","randomLandscapes")
path <- system.file("sampleModules", package="SpaDES")
path <- file.path("~","GitHub","SpaDES","inst","sampleModules")

my <- simInit(times=times, params=parameters, modules=modules, path=path)
system.time(spades(my))

