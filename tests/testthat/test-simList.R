test_that("simList object initializes correctly", {
  defaults <- list("checkpoint", "save", "progress", "load")
  times <- list(start = 0.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape")
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES"))

  mySim <- simInit(times, params, modules, objects = list(), paths)

  expect_is(mySim, "simList")

  w <- getOption("width")
  options(width = 100L)
  out <- utils::capture.output(show(mySim))
  expect_equal(length(out), 78)
  options(width = w); rm(w)

  ### SLOT .envir
  expect_is(envir(mySim), "environment")
  expect_is(objs(mySim), "list")
  expect_equal(sort(names(objs(mySim))),
               sort(names(as(mySim, "simList_")@.list)))
  expect_equivalent(mySim, as(as(mySim, "simList_"), "simList"))
  expect_equal(ls(mySim), objects(mySim))
  expect_equal(ls(mySim), sort(names(objs(mySim))))
  expect_equivalent(ls.str(mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(pos = mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(name = mySim), ls.str(objs(mySim)))

  mySim$test1 <- TRUE
  mySim[["test2"]] <- TRUE
  objs(mySim) <- list(test3 = TRUE)

  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])
  expect_true(objs(mySim)$test3)
  expect_error(objs(mySim) <- "test4", "must provide a named list.")

  oldEnv <- envir(mySim)
  envir(mySim) <- new.env(parent=.GlobalEnv)

  expect_true(is.null(mySim$test1))
  expect_true(is.null(mySim[["test2"]]))
  expect_true(is.null(objs(mySim)$test3[[1]]))

  envir(mySim) <- oldEnv
  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])
  rm(oldEnv)

  ### SLOT modules
  expect_is(modules(mySim), "list")
  expect_equal(modules(mySim), as.list(c(defaults, modules)))

  ### SLOT params
  expect_is(params(mySim), "list")

  # globals
  outputPath(mySim) <- file.path(tempdir(), "outputs")
  expect_identical(outputPath(mySim), file.path(tempdir(), "outputs"))

  # checkpoint
  expect_true(is.null(checkpointFile(mySim)))
  checkpointFile(mySim) <- file.path(outputPath(mySim), "checkpoint.RData")
  expect_identical(checkpointFile(mySim),
                   file.path(outputPath(mySim), "checkpoint.RData"))

  expect_true(is.na(checkpointInterval(mySim)))
  checkpointInterval(mySim) <- 10
  expect_identical(checkpointInterval(mySim), 10)

  # progress
  expect_true(is.na(progressType(mySim)))
  progressType(mySim) <- "text"
  expect_identical(progressType(mySim), "text")

  expect_true(is.na(progressInterval(mySim)))
  progressInterval(mySim) <- 10
  expect_identical(progressInterval(mySim), 10)

  # load
  expect_equal(
    inputs(mySim),
    .fileTableIn()
  )
  expect_error(inputs(mySim) <- "something", "inputs must be a list")

  # need tests for inputs
  # See test-load.R

  ### SLOT events
  expect_is(events(mySim), "data.table")
  expect_equal(nrow(events(mySim)), length(modules(mySim)))

  ### SLOT current
  expect_is(current(mySim), "data.table")
  expect_equal(nrow(current(mySim)), 0)

  ### SLOT completed
  expect_is(completed(mySim), "data.table")
  expect_equal(nrow(completed(mySim)), 0)

  ### SLOT depends
  expect_is(depends(mySim), ".simDeps")
  expect_is(depends(mySim)@dependencies, "list")
  expect_is(depends(mySim)@dependencies[[3]], ".moduleDeps")
  expect_equal(depends(mySim)@dependencies[[3]]@name, modules[[3]])
  # not going to go though each level...object validity checking does types

  ### SLOT simtimes
  expect_equivalent(
    times(mySim),
    list(
      current = 0.0,
      start = 0.0,
      end = convertTimeunit(as.numeric(dmonth(10)), "month"),
      timeunit = "month")
  )
  expect_equivalent(end(mySim),  10)
  expect_equivalent(start(mySim), 0)
  expect_equivalent(time(mySim),  0)

  expect_equivalent(end(mySim)   <- 20, 20.0)
  expect_equivalent(start(mySim) <- 10, 10.0)
  expect_equivalent(time(mySim)  <- 10, 10.0)

  expect_equal(timeunit(mySim), attr(end(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(start(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(time(mySim), "unit"))

  expect_equal("second", attr(mySim@simtimes$start, "unit"))
  expect_equal("second", attr(mySim@simtimes$end, "unit"))
  expect_equal("second", attr(mySim@simtimes$current, "unit"))

  ### required packages
  pkgs <- c("grid", "methods", "RandomFields", "raster", "RColorBrewer", "sp",
            "SpaDES", "tkrplot")
  expect_equal(sort(packages(mySim)), sort(pkgs))
  rm(mySim)
})

test_that("simList test all signatures", {
  # times
  times <- list(start = 0.0, end = 10)

  # modules
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")

  # paths
  mapPath <- system.file("maps", package = "SpaDES")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES"),
    inputPath = mapPath,
    outputPath = tempdir()
  )

  # inputs
  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )

  # objects
  layers <- lapply(filelist$files, raster)
  DEM <- layers[[1]]
  forestAge <- layers[[2]]
  objects <- list(DEM = "DEM", forestAge = "forestAge")
  objectsChar <- c("DEM", "forestAge")

  # outputs
  outputs <- data.frame(
    expand.grid(objectName = c("caribou","landscape"),
                saveTime = 1:2,
                stringsAsFactors = FALSE)
  )

  # parameters
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
  )

  # loadOrder
  loadOrder <- c("randomLandscapes", "caribouMovement", "fireSpread")

  # In order in the simulation.R
  origWd <- getwd()
  setwd(system.file("sampleModules", package = "SpaDES"))

  errors <- logical()
  argsTested <- list()
  for (i in 1:256) {
    li <- list(
      {if (i %% 2 ^ 1 == 0) times = times},
      {if (ceiling(i/2) %% 2 == 0) params = parameters},
      {if (ceiling(i/4) %% 2 == 0) modules = modules},
      {if (ceiling(i/8) %% 2 == 0) objects = objects},
      {if (ceiling(i/16) %% 2 == 0) paths = paths},
      {if (ceiling(i/32) %% 2 == 0) inputs = filelist},
      {if (ceiling(i/64) %% 2 == 0) outputs = outputs},
      {if (ceiling(i/128) %% 2 == 0) loadOrder = loadOrder}
    )
    argNames <- c("times", "params", "modules", "objects", "paths", "inputs",
                  "outputs", "loadOrder")
    names(li) <- argNames
    li <- li[!sapply(li, is.null)]
    errors[i] <- tryCatch(is(do.call(simInit, args = li), "simList"),
                           error = function(x) { FALSE })
    argsTested[[i]] <- names(li)
  }
  expect_more_than(sum(errors, na.rm = TRUE), 27) # needs paths and params
  setwd(origWd)
})
