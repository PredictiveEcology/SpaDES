test_that("test-load.R: loading inputs does not work correctly", {
  on.exit(rm(mySim, sim1, sim2, sim3))

  mapPath <- system.file("maps", package="SpaDES")

  filelist = data.frame(files=dir(file.path(mapPath),full.names = TRUE,
     pattern="tif")[1:2], functions="raster", package="raster", stringsAsFactors=FALSE)

  times <- list(start=0, stop=1)
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                inputPath=mapPath,
                outputPath=tempdir())

  mySim <- simInit(times=times, params=parameters, modules=modules, paths=paths)
  mySim <- spades(mySim)
  expect_true(all(c("DEM", "forestAge") %in% names(mySim$landscape)))
  inputs(mySim) <- data.frame(files=dir(file.path(mapPath),
                                        full.names=TRUE, pattern="tif")[1:2],
                               functions="raster", package="raster", loadTime=c(0,3),
                              stringsAsFactors=FALSE)


   # use loadFiles directly
   sim1 <- loadFiles(filelist=filelist,
                     paths=list(modulePath=system.file("sampleModules", package="SpaDES"),
                                   inputPath=mapPath,
                                   outputPath=tempdir())
   )
   expect_true(all(c("DEM", "forestAge") %in% ls(sim1)))

  # load at future time, i.e., nothing gets loaded
  inputs <- data.frame(files=dir(file.path(mapPath),
                                  full.names=TRUE, pattern="tif")[1:2],
                        functions="raster", package="raster", loadTime=3, stringsAsFactors=FALSE)
  mySim <- simInit(times=times, params=parameters, modules=modules, paths=paths,
                   inputs=inputs)
  expect_true(!any(c("DEM", "forestAge") %in% ls(mySim)))

  # load some at future time, i.e., only one gets loaded
  inputs <- data.frame(files=dir(file.path(mapPath),
                                     full.names=TRUE, pattern="tif")[1:2],
                                functions="raster", package="raster",
                       loadTime=c(0,3), stringsAsFactors=FALSE)
  mySim <- simInit(times=times, params=parameters, modules=modules,
                   paths=paths, inputs=inputs)

  expect_true(c("DEM") %in% ls(mySim))
  expect_true(!any(c("forestAge") %in% ls(mySim)))
  rm(mySim)




})

test_that("test-load.R: passing arguments to filelist in simInit does not work correctly", {
  # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
  #  at time = 10 and 20 (via "intervals").
  # Also, pass the single argument as a list to all functions...
  #  specifically, when add "native = TRUE" as an argument to the raster function
  mapPath <- system.file("maps", package="SpaDES")
  files = dir(file.path(mapPath),
       full.names=TRUE, pattern= "tif")[1:4]
  #mapPath <- file.path(find.package("SpaDES", quiet=FALSE), "inst/maps")
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                inputPath=mapPath,
                outputPath=tempdir())
  inputs <-
    data.frame(files = files,
               functions=rep("raster::raster",4),
               objectName = rep(NA,4),
               loadTime = c(0, 1, 1, 3),
               intervals = c(NA, 1, 2, NA),
               args=I(rep(list("native"=TRUE),4)),
               stringsAsFactors=FALSE)
  times <- list(start=0, stop=1, timeunit="seconds")

  sim2 <- simInit(times=times, params=parameters, modules=modules,
                  paths=paths, inputs=inputs)
  expect_true(c("DEM") %in% ls(sim2))
  expect_true(!any(c("forestCover", "forestAge", "habitatQuality") %in% ls(sim2)))

  sim2 <- spades(sim2)
  expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
  expect_true(!any(c("habitatQuality") %in% ls(sim2)))

  rm(forestAge, envir=envir(sim2))
  expect_true(!("forestAge" %in% ls(sim2)))

  end(sim2) <- 2
  sim2 <- spades(sim2)
  expect_true(all(c("forestAge") %in% names(sim2$landscape)))

  end(sim2) <- 3
  expect_message(spades(sim2), "habitatQuality read from")
  expect_message(spades(sim2), "forestCover")
  expect_message(spades(sim2), "forestAge")
  expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
  rm(sim2)

})

test_that("test-load.R: passing objects to simInit does not work correctly", {
  mapPath <- mapPath <- system.file("maps", package="SpaDES")

  # test object passing directly
  filelist = data.frame(files=dir(file.path(mapPath),full.names = TRUE,
                                  pattern="tif")[1:2], functions="raster", package="raster", stringsAsFactors=FALSE)
  layers <- lapply(filelist$files, raster)
  DEM <- layers[[1]]
  forestAge <- layers[[2]]



  times <- list(start=0, stop=1)
  parameters <- list(.globals=list(stackName="landscape"),
                     caribouMovement=list(.plotInitialTime=NA),
                     randomLandscapes=list(.plotInitialTime=NA,
                                           nx=20, ny=20))
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                inputPath=mapPath,
                outputPath=tempdir())

  # Pass as a named list
  objects <- list(DEM="DEM", forestAge="forestAge")
  sim3 <- simInit(times=times, params=parameters, modules=modules,
                  paths=paths, objects = objects)

  print(ls(sim3))
  expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
  rm(sim3)

  # pass as character vector
  objects <- c("DEM", "forestAge")
  sim4 <- simInit(times=times, params=parameters, modules=modules,
                  paths=paths, objects = objects)

  expect_true(all(c("DEM", "forestAge") %in% ls(sim4)))
  rm(sim4)

})

test_that("test-load.R: passing nearly empty file to simInit does not work correctly", {
  mapPath <- system.file("maps", package="SpaDES")
  #mapPath <- file.path(find.package("SpaDES", quiet=FALSE), "inst/maps")
  #mapPath <- file.path(find.package("SpaDES", quiet=FALSE), "maps")

  # test object passing directly
  filelist = data.frame(files=dir(file.path(mapPath),full.names = TRUE,
                                  pattern="tif")[1:2], functions="raster", package="raster", stringsAsFactors=FALSE)
  layers <- lapply(filelist$files, raster)
  DEM <- layers[[1]]
  forestAge <- layers[[2]]

  times <- list(start=0, stop=1)

  sim3 <- simInit(inputs=filelist)

  expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
  rm(sim3)

})
