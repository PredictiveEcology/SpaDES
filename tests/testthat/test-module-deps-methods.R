test_that("depsEdgeList and depsGraph work", {
  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"),
                 randomLandscapes=list(.plotInitialTime=NA, .plotInterval=NA),
                 caribouMovement=list(.plotInitialTime=NA, .plotInterval=NA),
                 fireSpread=list(.plotInitialTime=NA, .plotInterval=NA))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  # depsEdgeList
  el <- depsEdgeList(mySim)
  el_from <- c("caribouMovement", "caribouMovement", "fireSpread", "fireSpread",
               "fireSpread", "randomLandscapes", "randomLandscapes")
  el_to <- c("caribouMovement", "fireSpread", "caribouMovement", "fireSpread",
             "fireSpread", "caribouMovement", "fireSpread")
  el_objName <- c("landscape", "landscape", "landscape", "landscape", "npixelsburned",
                  "landscape", "landscape")
  el_objClass <- c("RasterStack", "RasterStack", "RasterStack", "RasterStack",
                   "numeric", "RasterStack", "RasterStack")

  expect_is(el, "data.table")
  expect_equal(names(el), c("from", "to", "objName", "objClass"))
  expect_equal(el$from, el_from)
  expect_equal(el$to, el_to)
  expect_equal(el$objName, el_objName)
  expect_equal(el$objClass, el_objClass)

  # .depsPruneEdges
  p <- .depsPruneEdges(el)
  p_from <- c("randomLandscapes", "randomLandscapes")
  p_to <- c("caribouMovement", "fireSpread")
  p_objName <- c("landscape", "landscape")
  p_objClass <- c("RasterStack", "RasterStack")
  p_ <- data.table(from=p_from, to=p_to, objName=p_objName, objClass=p_objClass)

  expect_is(p, "data.table")
  expect_equivalent(p, p_)

  # depsGraph
  expect_is(depsGraph(mySim), "igraph")
})
