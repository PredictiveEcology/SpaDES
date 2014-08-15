library("SpaDES")

fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="rasterToMemory",
                      packages="SpaDES",
                      stringsAsFactors=FALSE)

mapName = "landscape"
outputPath=file.path("~", "tmp", "simOutputs")

mySim <- simInit(times=list(start=0.0, stop=100.02),
                 params=list(
                   #.checkpoint=list(interval=1000,
                   #                 file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
                   .loadFileList=fileList,
                   .progress=list(.graphical=FALSE, .progressInterval = 10),
                   .globals=list(mapName=mapName, .outputPath=outputPath),
                   randomLandscapes = list(nx=1e2, ny=1e2, .saveObjects=c(mapName),
                                           .savePath=file.path("output", "randomLandscapes"),
                                           .plotInitialTime=NA, .plotInterval=NA,
                                           .saveInitialTime=3, .saveInterval=100,
                                           interval=0, startTime=0),
                   caribouMovement=list(N=1e2, .saveObjects=c("caribou"),
                                        .savePath=file.path("output","caribouMovement"),
                                        .saveInitialTime=3, .saveInterval=100,
                                        .plotInitialTime=NA, .plotInterval=NA,
                                        #.plotInitialTime = 1.01, .plotInterval=1,
                                        moveInterval=1, startTime=0),
                   fireSpread=list(nFires = 1e1, spreadprob=0.225,
                                   persistprob=0, its=1e6,
                                   .plotInitialTime=NA, .plotInterval=NA,
                                   #.plotInitialTime = 0.1, .plotInterval=10,
                                   .saveObjects=c("Fires"), .saveInterval=10,
                                   .savePath=file.path("output","fireSpread"),
                                   returnInterval=10, startTime=0)
                 ),
                 #modules=list("randomLandscapes", "fireSpread", "caribouMovement"),
                 #modules=list("stackFileList"),
                 modules=list("stackFileList", "fireSpread", "caribouMovement"),
                 #modules=list("caribouMovement", "fireSpread"),
                 path=system.file("sampleModules", package="SpaDES"))

dev(4)
mySim <- doSim(mySim, debug=FALSE)
