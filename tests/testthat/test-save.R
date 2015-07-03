test_that("saving files does not work correctly", {
  on.exit(rm(mySim))

  mapPath <- system.file("maps", package="SpaDES")

  print(paste("mapPath is",mapPath))
  savePath <- tempdir()
  print(paste("savePath is", savePath))

  filelist = data.table(files=dir(file.path(mapPath),
     full.names=TRUE, pattern="tif")[1:2], functions="rasterToMemory", package="SpaDES")
  #'
  times <- list(start=0, stop=6, "month")
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA,
                                          .saveObjects="caribou",
                                          .saveInitialTime=1,
                                          .saveInterval=2),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))

  outputs <- data.table(expand.grid(objectName=c("caribou", "vegMap"), saveTime=1:2))

  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                outputPath=savePath)
  mySim <- simInit(times=times, params=parameters, modules=modules, paths=paths,
                   outputs=outputs)

  mySim <- spades(mySim)

  # test spades-level mechanism
  expect_true(file.exists(file.path(savePath,"caribou_month1.rds")))
  expect_true(file.exists(file.path(savePath,"vegMap_month2.rds")))

  # test module-level mechanism
  expect_true(file.exists(file.path(savePath,"caribou_month3.rds")))
  expect_true(file.exists(file.path(savePath,"caribou_month5.rds")))


  outputs <- data.table(expand.grid(objectName=c("caribou", "vegMap")))
  times <- list(start=0, stop=7, "month")
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))
  mySim <- simInit(times=times, params=parameters, modules=modules, paths=paths,
                   outputs=outputs)

  mySim <- spades(mySim)

  # test that if no save times are stated, then it is at end time
  expect_true(file.exists(file.path(savePath,"caribou_month7.rds")))
  expect_true(file.exists(file.path(savePath,"vegMap_month7.rds")))

  })
