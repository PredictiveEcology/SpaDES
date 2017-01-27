test_that("test cache", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

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
  expect_true(NROW(out) == 10) # will become 15 with new experiment caching stuff
  clearCache(sims[[1]])
  out <- showCache(sims[[1]])
  expect_true(NROW(out) == 0)
})

test_that("test event-level cache", {
  #if((getRversion() > "3.3.2"))
  #skip("Not working on R devel")
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

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
  expect_true(!grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                     capture_messages(sims <- spades(Copy(mySim), notOlderThan = Sys.time()))))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo = "xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo = "xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash)
  expect_true("4e6e705cb7e50920" %in% firesHash)

  mess1 <- capture_messages(sims <- spades(Copy(mySim)))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                        mess1)))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo = "xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo = "xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash) # cached part is identical
  expect_false("4e6e705cb7e50920" %in% firesHash) # The non cached stuff goes ahead as normal

  clearCache(sims)
})

test_that("test module-level cache", {
  #if((getRversion() > "3.3.2"))
  #skip("Not working on R devel")
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpfile <- tempfile(fileext = ".pdf")

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
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  pdf(tmpfile)
  expect_true(!grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                     capture_messages(sims <- spades(Copy(mySim), notOlderThan = Sys.time()))))
  landscapeObjHash <- digest::digest(object = raster::dropLayer(sims$landscape, "Fires"), algo = "xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo = "xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash)
  expect_true("4e6e705cb7e50920" %in% firesHash)
  dev.off()
  expect_true(file.info(tmpfile)$size > 20000)
  unlink(tmpfile)

  # The cached version will be identical for both events (init and plot), but will not actually
  # complete the plot, because plotting isn't cacheable
  pdf(tmpfile)
  mess1 <- capture_messages(sims <- spades(Copy(mySim)))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                        mess1)))
  landscapeObjHash <- digest::digest(object = raster::dropLayer(sims$landscape, "Fires"), algo = "xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo = "xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash) # cached part is identical
  expect_false("4e6e705cb7e50920" %in% firesHash) # The non cached stuff goes ahead as normal
  dev.off()
  expect_true(file.info(tmpfile)$size < 10000)
  unlink(tmpfile)

  clearCache(sims)
})

test_that("test file-backed raster caching", {
  #if((getRversion() > "3.3.2"))
  library(igraph)
  library(raster)
  tmpdir <- file.path(tempdir(), "testCache", fsep = "\\")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpRasterfile <- tempfile(tmpdir = tempdir(), fileext = ".tif")

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpdir, tmpRasterfile) {
    r <- randomPolygons(numTypes = 30)
    writeRaster(r, tmpRasterfile, overwrite = TRUE)
    r <- raster(tmpRasterfile)
    r
  }

  a <- randomPolyToDisk(tmpdir, tmpRasterfile)
  # confirm that the raster has the given tmp filename
  expect_true(tmpRasterfile == a@file@name)
  aa <- Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir)
  # confirm that the raster has the new filename in the cachePath
  expect_false(tmpRasterfile == file.path(tmpdir, "rasters", basename(tmpRasterfile), fsep = "\\"))
  expect_true(any(grepl(pattern = basename(tmpRasterfile),
                    dir(file.path(tmpdir, "rasters", fsep = "\\")))))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time()
  for (i in 1:2) {
    assign(paste0("b", i), system.time(
      assign(paste0("a", i), Cache(rasterTobinary, aa, cacheRepo = tmpdir, notOlderThan = nOT))
    ))
    Sys.sleep(1.0)
  }
  # test that they are identical
  expect_equal(a1, a2)
  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])
})
