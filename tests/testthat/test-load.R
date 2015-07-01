test_that("loading inputs does not work correctly", {
  on.exit(rm(mySim, sim1, sim2))

  #mapPath <- if(interactive()) {"~/GitHub/SpaDES/inst"} else {find.package("SpaDES", quiet=FALSE)}
  #mapPath <- find.package("SpaDES", quiet=FALSE)
  mapPath <- system.file("maps", package="SpaDES")

  print(paste("mapPath is",mapPath))



  filelist = data.table(files=dir(file.path(mapPath),
     full.names=TRUE, pattern="tif")[1:2], functions="rasterToMemory", package="SpaDES")
  #'
  times <- list(start=0, stop=1)
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA),
                     randomLandscapes=list(.plotInitialTime=NA))
  inputs <- list(filelist=filelist)
  modules <- list("randomLandscapes", "caribouMovement")
  path <- system.file("sampleModules", package="SpaDES")
  mySim <- simInit(times=times, params=parameters, modules=modules, path=path, inputs=inputs)
  expect_true(all(c("DEM", "forestAge") %in% ls(mySim)))


  # use loadFiles directly
  sim1 <- loadFiles(filelist=filelist)
  expect_true(all(c("DEM", "forestAge") %in% ls(sim1)))

  # load at future time, i.e., nothing gets loaded
  inputs <- list(filelist=data.table(files=dir(file.path(mapPath),
                                  full.names=TRUE, pattern="tif")[1:2],
                        functions="rasterToMemory", package="SpaDES", loadTime=3))
  mySim <- simInit(times=times, params=parameters, modules=modules, path=path, inputs=inputs)
  expect_true(!any(c("DEM", "forestAge") %in% ls(mySim)))

  # load some at future time, i.e., only one gets loaded
  inputs <- list(filelist=data.table(files=dir(file.path(mapPath),
                                               full.names=TRUE, pattern="tif")[1:2],
                                     functions="rasterToMemory", package="SpaDES", loadTime=c(0,3)))
  mySim <- simInit(times=times, params=parameters, modules=modules, path=path, inputs=inputs)

  expect_true(c("DEM") %in% ls(mySim))
  expect_true(!any(c("forestAge") %in% ls(mySim)))




  # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
  #  at time = 10 and 20 (via "intervals").
  # Also, pass the single argument as a list to all functions...
  #  specifically, when add "native = TRUE" as an argument to the raster function
  arguments = list(native=TRUE)
  files = dir(file.path(mapPath),
       full.names=TRUE, pattern= "tif")[1:4]
  inputs=list(filelist = list(
     files = files,
     functions="raster::raster",
     objectName = NA,
     arguments = arguments,
     loadTime = c(0, 1, 1, 3),
     intervals = c(NA, 1, 2, NA)))
  #'
  times <- list(start=0, stop=1, timeunit="seconds")

  sim2 <- simInit(times=times, params=parameters, modules=modules, path=path, inputs=inputs)
  expect_true(c("DEM") %in% ls(sim2))
  expect_true(!any(c("forestCover", "forestAge", "habitatQuality") %in% ls(sim2)))

  sim2 <- spades(sim2, debug=FALSE)
  expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
  expect_true(!any(c("habitatQuality") %in% ls(sim2)))

  rm(forestAge, envir=simEnv(sim2))
  expect_true(!("forestAge" %in% ls(sim2)))

  end(sim2) <- 2
  sim2 <- spades(sim2, debug=FALSE)
  expect_true(all(c("forestAge") %in% ls(sim2)))

  #
  end(sim2) <- 3
  expect_message(spades(sim2, debug=FALSE), "forestAge")
  expect_message(spades(sim2, debug=FALSE), "habitatQuality")
  expect_message(spades(sim2, debug=FALSE), "forestCover")
  expect_message(sim2 <- spades(sim2, debug=FALSE), "forestAge")
  expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))



})
