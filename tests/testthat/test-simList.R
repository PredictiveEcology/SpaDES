test_that("simList object initializes correctly", {
  on.exit(rm(mySim))

  defaults <- list("checkpoint", "save", "progress", "load")

  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  expect_is(mySim, "simList")

  w <- getOption("width")
  options(width=100L)
  out <- capture.output(show(mySim))
  expect_equal(length(out), 79)
  options(width=w); rm(w)

  ### SLOT .envir
  expect_is(envir(mySim), "environment")
  expect_is(objs(mySim), "list")
  expect_equal(sort(names(objs(mySim))),
               sort(names(as(mySim, "simList_")@.list)))
  expect_equivalent(mySim, as(as(mySim, "simList_"), "simList"))
  expect_equal(ls(mySim), sort(names(objs(mySim))))
  expect_equivalent(ls.str(mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(pos=mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(name=mySim), ls.str(objs(mySim)))

  mySim$test1 <- TRUE
  mySim[["test2"]] <- TRUE
  objs(mySim) <- list(test3=TRUE)

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
  expect_true(is.null(outputPath(mySim)))
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
  expect_identical(inputs(mySim), list())
  inputs(mySim) <- "something"
  expect_equal(inputs(mySim), "something")

  # need tests for inputs

  ### SLOT events
  expect_is(events(mySim), "data.table")
  expect_equal(nrow(events(mySim)), length(modules(mySim)))

  ### SLOT completed
  expect_is(completed(mySim), "data.table")
  expect_equal(nrow(completed(mySim)), 0)

  ### SLOT depends
  expect_is(depends(mySim), ".simDeps")
  expect_is(depends(mySim)@dependencies, "list")
  expect_is(depends(mySim)@dependencies[[3]], ".moduleDeps")
  expect_equal(depends(mySim)@dependencies[[3]]@name, "fireSpread")
  # not going to go though each level...object validity checking does types

  ### SLOT simtimes
  expect_equivalent(
    times(mySim),
    list(current=0.0, start=0.0, stop=as.numeric(dmonth(10)), timeunit="month")
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
  expect_equal("second", attr(mySim@simtimes$stop, "unit"))
  expect_equal("second", attr(mySim@simtimes$current, "unit"))

  ### required packages
  pkgs <- c("grid", "methods", "RandomFields", "raster", "RColorBrewer", "sp",
            "SpaDES", "tkrplot")
  expect_equal(sort(packages(mySim)), sort(pkgs))
})
