test_that("test-load.R: loading inputs does not work correctly", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_load", rndstr()) %>% checkPath(create = TRUE)
  on.exit({
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  mapPath <- system.file("maps", package = "quickPlot")

  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    stringsAsFactors = FALSE
  )

  times <- list(start = 0, end = 1)
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES"),
    inputPath = mapPath,
    outputPath = tmpdir
  )

  mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  mySim <- spades(mySim)
  expect_true(all(c("DEM", "forestAge") %in% names(mySim$landscape)))

  # test overall inputs setReplaceMethod
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "fun", "package", "loadTime")], inputs)
  expect_equal(fileName(inputs(mySim)$file), inputs(mySim)$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  # test fill in objectName and function and package
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "loadTime")], inputs)
  expect_equal(fileName(inputs(mySim)$file), inputs(mySim)$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  # test override default object name
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    objectName = c("rasDEM", "rasForestAge"),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "objectName")], inputs)
  expect_equal(inputs(mySim)$objectName, inputs$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  rm(mySim)

  # use loadFiles directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    sim1 <- loadFiles(
      filelist = filelist,
      paths = list(
        modulePath = system.file("sampleModules", package = "SpaDES"),
        inputPath = mapPath,
        outputPath = file.path(tmpdir, rndstr()))
    )
    expect_true(all(c("DEM", "forestAge") %in% ls(sim1)))
    rm(sim1)

    # load at future time, i.e., nothing gets loaded
    inputs <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      loadTime = 3,
      stringsAsFactors = FALSE
    )
    mySim <- simInit(times = times, params = parameters, modules = modules,
                     paths = paths, inputs = inputs)
    expect_true(!any(c("DEM", "forestAge") %in% ls(mySim)))
    rm(mySim)

    # load some at future time, i.e., only one gets loaded
    inputs <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      loadTime = c(0, 3),
      stringsAsFactors = FALSE
    )
    mySim <- simInit(times = times, params = parameters, modules = modules,
                     paths = paths, inputs = inputs)

    expect_true(c("DEM") %in% ls(mySim))
    expect_true(!any(c("forestAge") %in% ls(mySim)))
    rm(mySim)
  }
})

test_that("test-load.R: passing arguments to filelist in simInit does not work correctly", {
  tmpdir <- file.path(tempdir(), "test_load", rndstr()) %>% checkPath(create = TRUE)
  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
  #  at time = 10 and 20 (via "intervals").
  # Also, pass the single argument as a list to all functions...
  #  specifically, when add "native = TRUE" as an argument to the raster function
  mapPath <- system.file("maps", package = "quickPlot")
  files <- dir(file.path(mapPath), full.names = TRUE, pattern =  "tif")[1:4]
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES"),
    inputPath = mapPath,
    outputPath = tmpdir
  )

  inputs <- data.frame(
    files = files,
    functions = rep("raster::raster", 4),
    objectName = rep(NA, 4),
    loadTime = c(0, 1, 1, 3),
    intervals = c(NA, 1, 2, NA),
    args = I(rep(list("native" = TRUE), 4)),
    stringsAsFactors = FALSE
  )
  times <- list(start = 0, end = 1, timeunit = "seconds")

  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    sim2 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, inputs = inputs)
    expect_true(c("DEM") %in% ls(sim2))

    # Test that arguments got passed in correctly
    expect_equal(inputs(sim2)$arguments, I(rep(list(native = TRUE), 4)))
    expect_true(!any(c("forestCover", "forestAge", "habitatQuality") %in% ls(sim2)))

    sim2 <- spades(sim2)
    expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
    expect_true(!any(c("habitatQuality") %in% ls(sim2)))

    rm(forestAge, envir = envir(sim2))
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
  }
})

test_that("test-load.R: passing objects to simInit does not work correctly", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_load", rndstr()) %>% checkPath(create = TRUE)
  on.exit({
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  mapPath <- mapPath <- system.file("maps", package = "quickPlot")

  # test object passing directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, raster)
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = system.file("sampleModules", package = "SpaDES"),
      inputPath = mapPath,
      outputPath = tmpdir
    )

    # Pass as a named list
    objects <- list(DEM = DEM, forestAge = forestAge)
    sim3 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects  =  objects)
    expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
    rm(sim3)

    # pass as character vector
    objects <- c("DEM", "forestAge")
    sim4 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects)
    expect_true(all(c("DEM", "forestAge") %in% ls(sim4)))
    rm(sim4)

    # pass both inputs and objects
    objects <- c("DEM")
    sim5 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects, inputs = filelist[-1, ])
    expect_true(all(c("DEM", "forestAge") %in% ls(sim5)))
    rm(sim5)

    # pass both inputs (at non-start time) and objects
    # test object passing directly
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[2],
      functions = "raster",
      package = "raster",
      loadTime = 1,
      stringsAsFactors = FALSE
    )
    objects <- c("DEM")
    sim6 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects, inputs = filelist)
    expect_true(all(c("DEM") %in% ls(sim6)))
    expect_false(all(c("forestAge") %in% ls(sim6)))
    sim6End <- spades(sim6) # run for 1 time, loading the file
    expect_true(all(c("forestAge") %in% ls(sim6End)))
    rm(sim6)
  }
})

test_that("test-load.R: passing nearly empty file to simInit does not work correctly", {
  mapPath <- system.file("maps", package = "quickPlot")

  # test object passing directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
      filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, raster)
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

    times <- list(start = 0, end = 1)

    sim3 <- simInit(inputs = filelist)

    expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
    rm(sim3)
  }
})

test_that("test-load.R: more tests", {
  tmpdir <- file.path(tempdir(), "test_load", rndstr()) %>% checkPath(create = TRUE)

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  sim <- simInit()
  test <- 1:10
  tmpFile <- file.path(tmpdir, "test.rds")
  saveRDS(test, file = tmpFile)

  # Test for data.frame being kept as data.frame even with only one column
  expect_silent(inputs(sim) <- data.frame(file = tmpFile))


  # Test for incremental loading via intervals
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)

    files <- dir(system.file("maps", package = "quickPlot"),
                full.names = TRUE, pattern = "tif")
    arguments <- I(rep(list(native = TRUE), length(files)))
    filelist <- data.frame(
       files = files,
       functions = "raster::raster",
       objectName = NA,
       arguments = arguments,
       loadTime = 0,
       intervals = c(rep(NA, length(files) - 1), 10)
    )
    expect_message(sim2 <- loadFiles(filelist = filelist), "DEM read from")
    expect_message(sim2 <- loadFiles(filelist = filelist), "forestAge read from")
    end(sim2) <- 20
    expect_message(sim3 <- spades(sim2), "time 10")
    expect_message(sim3 <- spades(sim2), "time 20")
  }
})
