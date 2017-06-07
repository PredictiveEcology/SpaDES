test_that("test cache", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  sims <- experiment(mySim, replicates = 2, cache = TRUE)
  out <- showCache(sims[[1]])
  expect_output(print(out), "cacheId")
  expect_true(NROW(out) == 16) # will become 15 with new experiment caching stuff
  clearCache(sims[[1]])
  out <- showCache(sims[[1]])
  expect_true(NROW(out) == 0)
})

test_that("test event-level cache", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  #expect_true(!grepl(pattern = "Using cached copy of init event in randomLandscapes module",
  #                   capture_messages(sims <- spades(Copy(mySim), notOlderThan = Sys.time()))))
  sims <- spades(Copy(mySim), notOlderThan = Sys.time()) ## TO DO: fix this test
  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires

  mess1 <- capture_messages(sims <- spades(Copy(mySim)))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))

  clearCache(sims)
})

test_that("test module-level cache", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  file.create(tmpfile)
  tmpfile <- normPath(tmpfile)
  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  times <- list(start = 0.0, end = 1.0, timeunit = "year")
  mySim <- simInit(
    times = times,
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = times$start, .useCache = TRUE)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  set.seed(1123)
  pdf(tmpfile)
  #expect_true(!grepl(pattern = "Using cached copy of init event in randomLandscapes module",
  #                   capture_messages(sims <- spades(Copy(mySim), notOlderThan = Sys.time()))))
  sims <- spades(Copy(mySim), notOlderThan = Sys.time())
  dev.off()

  expect_true(file.info(tmpfile)$size > 20000)
  unlink(tmpfile)

  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires

  # The cached version will be identical for both events (init and plot),
  # but will not actually complete the plot, because plotting isn't cacheable
  pdf(tmpfile)
  mess1 <- capture_messages(sims <- spades(Copy(mySim)))
  dev.off()

  expect_true(file.info(tmpfile)$size < 10000)
  unlink(tmpfile)

  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))

  clearCache(sims)
})

test_that("test file-backed raster caching", {
  library(igraph)
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpRasterfile <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  file.create(tmpRasterfile)
  tmpRasterfile <- normPath(tmpRasterfile)
  try(clearCache(tmpdir), silent = TRUE)

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpdir, tmpRasterfile) {
    r <- randomPolygons(numTypes = 30)
    writeRaster(r, tmpRasterfile, overwrite = TRUE)
    r <- raster(tmpRasterfile)
    r
  }

  a <- randomPolyToDisk(tmpdir, tmpRasterfile)
  # confirm that the raster has the given tmp filename
  expect_identical(strsplit(tmpRasterfile, split = "[\\/]"),
                   strsplit(a@file@name, split = "[\\/]"))
  aa <- Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")

  # Test clearCache by tags
  expect_equal(NROW(showCache(tmpdir)), 9)
  clearCache(tmpdir, userTags = "something$")
  expect_equal(NROW(showCache(tmpdir)), 9)
  clearCache(tmpdir, userTags = "something2")
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")
  expect_equal(NROW(showCache(tmpdir)), 9)
  clearCache(tmpdir, userTags = c("something$", "testing$"))
  expect_equal(NROW(showCache(tmpdir)), 9)
  clearCache(tmpdir, userTags = c("something2$", "testing$"))
  expect_equal(NROW(showCache(tmpdir)), 9)
  clearCache(tmpdir, userTags = c("something2$", "randomPolyToDisk$"))
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")

  # confirm that the raster has the new filename in the cachePath
  expect_false(identical(strsplit(tmpRasterfile, split = "[\\/]"),
                         strsplit(file.path(tmpdir, "rasters", basename(tmpRasterfile)), split = "[\\/]")))
  expect_true(any(grepl(pattern = basename(tmpRasterfile),
                        dir(file.path(tmpdir, "rasters")))))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time()
  for (i in 1:2) {
    assign(paste0("b", i), system.time(
      assign(paste0("a", i), Cache(rasterTobinary, aa, cacheRepo = tmpdir, notOlderThan = nOT))
    ))
    nOT <- Sys.time() - 100
  }

  # test that they are identical
  expect_equal(a1, a2)

  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])

  clearCache(tmpdir)

  # Check that Caching of rasters saves them to tif file instead of rdata
  randomPolyToMemory <- function(tmpdir) {
    r <- randomPolygons(numTypes = 30)
    dataType(r) <- "INT1U"
    r
  }

  bb <- Cache(randomPolyToMemory, tmpdir, cacheRepo = tmpdir)
  expect_true(filename(bb)=="")
  expect_true(inMemory(bb))

  bb <- Cache(randomPolyToMemory, tmpdir, cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir)) == 9)

  # Test that factors are saved correctly
  randomPolyToFactorInMemory <- function(tmpdir) {
    r <- randomPolygons(numTypes = 30)
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30,replace = TRUE),
                            vals2 <- sample(1:7, size = 30, replace = TRUE))
    dataType(r) <- "INT1U"
    r
  }
  bb <- Cache(randomPolyToFactorInMemory, tmpdir, cacheRepo = tmpdir)
  expect_true(dataType(bb) == "INT1U")
  expect_true(is.factor(bb))
  expect_true(is(levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(levels(bb)[[1]]) == 3)
  expect_true(NROW(levels(bb)[[1]]) == 30)

  randomPolyToFactorOnDisk <- function(tmpdir, tmpFile) {
    r <- randomPolygons(numTypes = 30)
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 = sample(1:7, size = 30, replace = TRUE))
    r <- writeRaster(r, tmpFile, overwrite = TRUE, datatype = "INT1U")
    #r <- raster(tmpRasterfile)
    r
  }
  tf <- tempfile(fileext = ".grd")
  file.create(tf)
  tf <- normPath(tf)

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tmpdir, tf)
  # bb has new one, inside of cache repository, with same basename
  bb <- Cache(randomPolyToFactorOnDisk, tmpdir, tmpFile = tf, cacheRepo = tmpdir)
  expect_true(dirname(filename(bb)) == file.path(tmpdir, "rasters"))
  expect_true(basename(filename(bb)) == basename(tf))
  expect_false(filename(bb) == tf)
  expect_true(dirname(filename(bb1)) == dirname(tf))
  expect_true(basename(filename(bb1)) == basename(tf))
  expect_true(dataType(bb) == "INT1U")
  expect_true(is.factor(bb))
  expect_true(is(levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(levels(bb)[[1]]) == 3)
  expect_true(NROW(levels(bb)[[1]]) == 30)

  clearCache(tmpdir)
})

test_that("test date-based cache removal", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  file.create(tmpfile)
  tmpfile <- normPath(tmpfile)
  try(clearCache(tmpdir), silent = TRUE)

  a <- Cache(runif, 1, cacheRepo = tmpdir)
  a1 <- showCache(tmpdir)
  expect_true(NROW(a1) > 0)
  b <- clearCache(tmpdir, before = Sys.Date() - 1)
  expect_true(NROW(b) == 0)
  expect_identical(a1, showCache(tmpdir))

  b <- clearCache(tmpdir, before = Sys.Date() + 1)
  expect_identical(b, a1)

  # Example of changing parameter values

  clearCache(tmpdir)



  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )
  clearCache(mySim)
  sims <- spades(Copy(mySim), notOlderThan = Sys.time())
  simsCacheID <- unlist(gsub(attr(sims, "tags"), pattern = "cacheId:", replacement = ""))
  ranNums <- Cache(runif, 4, cacheRepo=cachePath(mySim), userTags = "objectName:a")
  # access it again, but "later"
  Sys.sleep(1)
  sims <- spades(Copy(mySim)) # i.e., this is a "read" operation, does not create a new artifact
  wholeCache <- showCache(mySim)
  expect_true(length(unique(wholeCache, by="artifact")$artifact)==2)
  expect_true(sum(wholeCache$tagKey=="accessed")==3)
  # keep only items accessed "recently"
  onlyRecentlyAccessed <- showCache(mySim,
                                    userTags = max(wholeCache[tagKey=="accessed"]$tagValue))
  # inverse join with 2 data.tables ... using: a[!b]
      # i.e., return all of wholeCache that was not recently accessed
  toRemove <- unique(wholeCache[!onlyRecentlyAccessed], by="artifact")$artifact
  clearCache(mySim, toRemove) # remove ones not recently accessed
  expect_true(length(unique(showCache(mySim), by="artifact")$artifact)==1)
  expect_true(sum(showCache(mySim)$tagKey=="accessed")==2)
  # make sure it is the sims one
  expect_true(length(unique(showCache(mySim, simsCacheID)$artifact))==1)

})
