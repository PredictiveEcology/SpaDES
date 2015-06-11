load_all()
clearPlot()
#library(SpaDES)
library(fpCompare)
times <- list(start=2005, stop=2008)
parameters <- list(.globals=list(stackName="landscape"))#, .checkpoint=list(interval=1, file="chkpnt.RData"))
modules <- list("randomLandscapes", "caribouMovement")
path <- system.file("sampleModules", package="SpaDES")
mySim6 <- simInit(times=times, params=parameters, modules=modules, path=path)

mySim6 <- spades(mySim6, debug=TRUE)
#clearPlot()
#times <- list(start=0, stop=6)
#mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
simStopTime(mySim6) <- 6
mySim6 <- spades(mySim6)
simStopTime(mySim6) <- 7
rePlot(5)
