#' SpaDES simulation demo
#'
#' randomLandscapes, caribouMovement, fireSpread
#'
# @demoTitle spades-simulation
#

library("SpaDES")

fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="rasterToMemory",
                      packages="SpaDES",
                      stringsAsFactors=FALSE)

stackName = "landscape"

mySim <- simInit(times=list(start=0.0, stop=100.02),
                 params=list(
                   .loadFileList=fileList,
                   .progress=list(.graphical=FALSE, .progressInterval = 10),
                   .globals=list(.stackName=stackName, .outputPath=NA, burnStats="nPixelsBurned"),
                   randomLandscapes = list(nx=1e2, ny=1e2, .saveObjects=stackName,
                                           .plotInitialTime=NA, .plotInterval=NA,
                                           inRAM=TRUE),
                   caribouMovement=list(N=1e1, .saveObjects=c("caribou"),
                                        .plotInitialTime = 1.01, .plotInterval=1,
                                        moveInterval=1),
                   fireSpread=list(nFires = 1e1, spreadprob=0.225,
                                   persistprob=0, its=1e6,
                                   .plotInitialTime = 0.1, .plotInterval=10,
                                   returnInterval=10, startTime=0)
                 ),
                 modules=list("randomLandscapes", "fireSpread", "caribouMovement"),
                 path=system.file("sampleModules", package="SpaDES"))

dev(4)
mySim <- spades(mySim, debug=FALSE)
