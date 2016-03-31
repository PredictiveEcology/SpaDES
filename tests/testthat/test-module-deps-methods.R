test_that("defineModule correctly handles different inputs", {
  tmp <- simInit()

  # check empty metadata
  x0 <- list()
  expect_warning(defineModule(tmp, x0))
  expect_identical(suppressWarnings(defineModule(tmp, x0)),
                   suppressWarnings(defineModule(tmp, .emptyMetadata())))

  # check each element in metadata
  x1 <- list(
    name = "testModule",
    description = "this is a test.",
    keywords = c("test"),
    childModules = character(),
    authors = c(person(c("Alex", "M"), "Chubaty",
                       email = "alexander.chubaty@canada.ca",
                       role = c("aut", "cre"))),
    version = numeric_version("0.0.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = NA_character_,
    citation = list(),
    documentation = list(),
    reqdPkgs = list("grid", "raster", "sp"),
    parameters = rbind(
      defineParameter("dummyVal", "numeric", 1.0, NA, NA, "vague description")
    ),
    inputObjects = data.frame(
      objectName = "testInput", objectClass = "list", sourceURL = "",
      other = NA_character_, stringsAsFactors = FALSE
    ),
    outputObjects = data.frame(
      objectName = "testOutput", objectClass = "list", other = NA_character_,
      stringsAsFactors = FALSE
    )
  )

  ## check name
  x2 <- x1
  x2$name <- list("testModule") # not a character
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check description
  x2 <- x1
  x2$description <- list("this is a test.") # not a character vector
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check keywords
  x2 <- x1
  x2$keywords <- list("test") # not a character vector
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check authors
  x2 <- x1
  x2$authors <- "not a person class"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check version
  x2 <- x1
  x2$version <- "0.0.1"
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check spatialExtent
  x2 <- x1
  x2$spatialExtent <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check timeframe
  x2 <- x1
  x2$timeframe <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check timeunit
  x2 <- x1
  x2$timeunit <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check citation
  x2 <- x1
  x2$citation <- character() # not a list
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check reqdPkgs
  x2 <- x1
  x2$reqdPkgs <- c("grid", "raster", "sp") # not a list
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check parameters
  x2 <- x1
  x2$parameters <- "not a data.frame"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check inputObjects
  x2 <- x1
  x2$inputObjects <- "not a data.frame"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check authors
  x2 <- x1
  x2$outputObjects <- "not a person class"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed
})

test_that("depsEdgeList and depsGraph work", {
  times <- list(start = 0.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES"))

  mySim <- simInit(times, params, modules, paths = paths)

  # depsEdgeList
  el <- depsEdgeList(mySim)
  el_from <- c("caribouMovement", "caribouMovement", "fireSpread", "fireSpread",
               "fireSpread", "randomLandscapes", "randomLandscapes")
  el_to <- c("caribouMovement", "fireSpread", "caribouMovement", "fireSpread",
             "fireSpread", "caribouMovement", "fireSpread")
  el_objName <- c("landscape", "landscape", "landscape", "landscape",
                  "npixelsburned", "landscape", "landscape")
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
  p_ <- data.table::data.table(
    from = p_from, to = p_to, objName = p_objName, objClass = p_objClass
  )

  expect_is(p, "data.table")
  expect_equivalent(p, p_)

  # depsGraph
  expect_is(depsGraph(mySim), "igraph")
})
