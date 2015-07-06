test_that("paths file does not work correctly", {
  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")

  tempPath <- checkPath(tempdir())

  # test for mixture of named and unnamed
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                tempPath)
  mySim <- simInit(times, params, modules, objects=list(), paths)
  expect_equal(paths(mySim), list(modulePath=paths$modulePath,
                                  inputPath=paths[[2]], outputPath=getwd()))

  # test for non consecutive order, but named
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                outputPath=tempPath)
  mySim <- simInit(times, params, modules, objects=list(), paths)
  expect_equal(paths(mySim), list(modulePath=paths$modulePath, inputPath=getwd(),
                                  outputPath=path.expand(paths[[2]])))

  # test for all unnamed
  paths <- list(system.file("sampleModules", package="SpaDES"),
                tempPath,
                tempPath)
  mySim <- simInit(times, params, modules, objects=list(), paths)
  expect_equal(paths(mySim), list(modulePath=paths[[1]],
                                  inputPath=paths[[2]], outputPath=paths[[3]]))

  # test for all named, non consecutive, using accessors
  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
                outputPath=tempPath,
                inputPath=tempPath)
  mySim <- simInit(times, params, modules, objects=list(), paths)
  expect_equal(paths(mySim), list(modulePath=modulePath(mySim),
                                  inputPath=inputPath(mySim), outputPath=outputPath(mySim)))

  inputPath(mySim) <- tempPath
  expect_equal(inputPath(mySim), tempPath)

  outputPath(mySim) <- tempPath
  expect_equal(outputPath(mySim), tempPath)

  modulePath(mySim) <- tempPath
  expect_equal(modulePath(mySim), tempPath)

})
