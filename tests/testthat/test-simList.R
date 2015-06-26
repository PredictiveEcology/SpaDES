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
  expect_is(simEnv(mySim), "environment")
  expect_is(simObjects(mySim), "list")
  expect_equal(sort(names(simObjects(mySim))),
               sort(names(as(mySim, "simList_")@.list)))
  expect_equivalent(mySim, as(as(mySim, "simList_"), "simList"))
  expect_equal(ls(mySim), sort(names(simObjects(mySim))))
  expect_equivalent(ls.str(mySim), ls.str(simObjects(mySim)))
  expect_equivalent(ls.str(pos=mySim), ls.str(simObjects(mySim)))
  expect_equivalent(ls.str(name=mySim), ls.str(simObjects(mySim)))

  mySim$test1 <- TRUE
  mySim[["test2"]] <- TRUE

  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])

  ### SLOT modules
  expect_is(modules(mySim), "list")
  expect_equal(modules(mySim), as.list(c(defaults, modules)))

  ### SLOT params
  expect_is(params(mySim), "list")

  # checkpoint
  expect_true(is.null(checkpointFile(mySim)))
  expect_true(is.na(checkpointInterval(mySim)))

  # progress
  expect_true(is.na(progressType(mySim)))
  expect_true(is.na(progressInterval(mySim)))

  ### SLOT events
  expect_is(events(mySim), "data.table")
  expect_equal(nrow(events(mySim)), length(modules(mySim)))

  ### SLOT completed
  expect_is(completed(mySim), "data.table")
  expect_equal(nrow(completed(mySim)), 0)

  ### SLOT depends
  expect_is(simDepends(mySim), ".simDeps")
  expect_is(simDepends(mySim)@dependencies, "list")
  expect_is(simDepends(mySim)@dependencies[[3]], ".moduleDeps")
  expect_equal(simDepends(mySim)@dependencies[[3]]@name, "fireSpread")
  # not going to go though each level...object validity checking does types

  ### SLOT simtimes
  expect_equivalent(
    times(mySim),
    list(current=0.0, start=0.0, stop=as.numeric(dmonth(10)), timestepUnit="month")
  )
  expect_equivalent(time(mySim), 0)
  expect_equivalent(start(mySim), 0.0)
  expect_equivalent(end(mySim), 10)

  expect_equal(timeunit(mySim), attr(end(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(start(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(time(mySim), "unit"))

  expect_equal("second", attr(mySim@simtimes$start, "unit"))
  expect_equal("second", attr(mySim@simtimes$stop, "unit"))
  expect_equal("second", attr(mySim@simtimes$current, "unit"))

  ### required packages
  pkgs <- c("grid", "methods", "RandomFields", "raster", "RColorBrewer", "sp",
            "SpaDES", "tkrplot")
  expect_equal(sort(simReqdPkgs(mySim)), sort(pkgs))
})
