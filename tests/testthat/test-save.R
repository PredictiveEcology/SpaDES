test_that("saving files does not work correctly", {
  on.exit(rm(mySim))

  mapPath <- system.file("maps", package="SpaDES")

  print(paste("mapPath is",mapPath))
  savePath <- tempdir()
  print(paste("savePath is", savePath))

  filelist = data.table(files=dir(file.path(mapPath),
     full.names=TRUE, pattern="tif")[1:2], functions="rasterToMemory", package="SpaDES")
  #'
  times <- list(start=0, stop=2, "month")
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA,
                                          .saveObjects="caribou",
                                          .saveInitialTime=1,
                                          .saveInterval=1),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                outputPath=savePath)
  mySim <- simInit(times=times, params=parameters, modules=modules, paths=paths)
  mySim <- spades(mySim)
  expect_true(file.exists(file.path(savePath,"cariboumonth1.rds")))

})
