require(dplyr, lib.loc="~/R-dev")
require(igraph, lib.loc="~/R-dev")

outputPath <- file.path(tempdir(), "simOutputs")
times <- list(start=0.0, stop=10.01)
parameters <- list(.globals=list(.stackName="landscape", .outputPath=outputPath,
                                 burnStats="nPixelsBurned"),
                   .progress=list(NA),
                   randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE),
                   fireSpread=list(nFires= 1e1, spreadprob=0.225, its=1e6,
                                   persistprob=0, returnInterval=10, startTime=0,
                                   .plotInitialTime=0.1, .plotInterval=10),
                   caribouMovement=list(N=1e2, moveInterval=1,
                                        .plotInitialTime=1.01, .plotInterval=1)
)
modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
path <- system.file("sampleModules", package="SpaDES")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)

# see what it looks like
simGraph = depsGraph(mySim, plot=TRUE) # the version returned to user for plotting
plot(simGraph)
