## ----checkpoints, echo=TRUE, eval=TRUE-----------------------------------
library("SpaDES")

# initialize a new simulation, setting the checkpoint interval and checkpoint filename.
times <- list(start=0, stop=100)
outputPath=file.path(tmpDir(), "simOutputs")
parameters <- list(.globals=list(.stackName="landscape", .outputPath=outputPath),
                   .checkpoint=list(interval=10, file="chkpnt.RData"),
                   randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE,
                                         .plotInitialTime=0, .plotInterval=1e3))
modules <- list("randomLandscapes")
path <- system.file("sampleModules", package="SpaDES")
#path <- file.path("~", "GitHub", "SpaDES", "inst", "sampleModules")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)

spades(mySim)

# retrieve the checkpoint params from the simulation object
simParams(mySim)$.checkpoint
simParams(mySim)$.checkpoint$interval
simParams(mySim)$.checkpoint$file

## ----progress, echo=TRUE, eval=FALSE-------------------------------------
#  # initialize a new simulation, setting the progress parameters
#  mySim <- simInit(times=list(start=0.0, stop=100),
#                   params=list(.progress=list(.graphical=FALSE, .progressInterval=10),
#                               randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE)),
#                   modules=list("randomLandscapes"),
#                   # path=system.file("sampleModules", package="SpaDES")
#                   path=file.path("~", "GitHub", "SpaDES", "inst", "sampleModules")
#  )
#  
#  # retrieve the checkpoint params from the simulation object
#  simParams(mySim)$.progress
#  simParams(mySim)$.progress$.graphical
#  simParams(mySim)$.progress$.progressInterval

## ----load-save, echo=TRUE, eval=FALSE------------------------------------
#  # initialize a new simulation, setting the load and save parameters
#  filelist <- file.path(find.package("SpaDES", quiet=FALSE),"maps")
#  mySim <- simInit(times=list(start=0.0, stop=100),
#                   params=list(
#                     .loadFileList=data.frame(files=filelist, stringsAsFactors=FALSE),
#                     randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE,
#                                  .saveObjects=c("habitat"),
#                                  .savePath=file.path("output", "randomLandscapes"),
#                                  .saveInitialTime=0, .saveInterval=10)
#                     ),
#                   modules=list("randomLandscapes"),
#                   # path=system.file("sampleModules", package="SpaDES")
#                   path=file.path("~", "GitHub", "SpaDES", "inst", "sampleModules")
#  )
#  
#  # retrieve the load and save params from the simulation object
#  simObjectsLoaded(mySim) # shows what's been loaded
#  simFileList(mySim) # returns empty if objects successfully loaded
#  
#  simParams(mySim)$randomLandscapes$.saveObjects
#  simParams(mySim)$randomLandscapes$.savePath
#  simParams(mySim)$randomLandscapes$.saveInitialTime
#  simParams(mySim)$randomLandscapes$.saveInterval
#  
#  # schedule a recurring save event [WITHIN A MODULE]
#  nextSave <- simCurrentTime(mySim) + simParams(mySim)$randomLandscapes$.saveInterval
#  sim <- scheduleEvent(mySim, nextSave, "randomLandscapes", "save")

## ----create-new-module, eval=FALSE, echo=TRUE----------------------------
#  # create a new module called "randomLandscape" in the "custom-modules" subdirectory
#  # and open the resulting file immediately for editing.
#  newModule(name="randomLandscapes", path="custom-modules", open=TRUE)

## ----plotting, echo=TRUE, eval=FALSE-------------------------------------
#  # initialize a new simulation, setting the load and save parameters
#  mySim <- simInit(times=list(start=0.0, stop=100),
#                   params=list(
#                     randomLandscapes=list(nx=1e2, ny=1e2,
#                                  .plotInitialTime=0, .plotInterval=1)
#                     ),
#                   modules=list("randomLandscapes"),
#                   # path=system.file("sampleModules", package="SpaDES")
#                   path=file.path("~", "GitHub", "SpaDES", "inst", "sampleModules")
#  )
#  
#  # retrieve the plotting params from the simulation object
#  simParams(mySim)$randomLandscapes$.plotInitialTime
#  simParams(mySim)$randomLandscapes$.plotInterval
#  
#  # schedule a recurring save event [WITHIN A MODULE]
#  nextPlot <- simCurrentTime(mySim) + simParams(mySim)$randomLandscapes$.plotInterval
#  mySim <- scheduleEvent(mySim, nextPlot, "randomLandscapes", "save")

## ----randomLandscapes, echo=FALSE, eval=TRUE-----------------------------
source(file.path(system.file("sampleModules", package="SpaDES"), "randomLandscapes.R"), echo=TRUE, max.deparse.length=10000)

## ----fireSpread, echo=FALSE, eval=TRUE-----------------------------------
source(file.path(system.file("sampleModules", package="SpaDES"), "fireSpread.R"), echo=TRUE, max.deparse.length=10000)

## ----caribouMovement, echo=FALSE, eval=TRUE------------------------------
source(file.path(system.file("sampleModules", package="SpaDES"), "caribouMovement.R"), echo=TRUE, max.deparse.length=10000)

