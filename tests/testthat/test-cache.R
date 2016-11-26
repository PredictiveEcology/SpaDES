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
  out <- print(showCache(sims[[1]]))
  expect_output(print(out), "cacheId")
  expect_true(NROW(out) == 10) # will become 15 with new experiment caching stuff
  clearCache(sims[[1]])
  out <- print(showCache(sims[[1]]))
  expect_true(NROW(out) == 0)
})

test_that("test event-level cache", {
  #if((getRversion() > "3.3.2"))
  skip("Not working on R devel")
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
                     capture_messages(sims <- spades(mySim, notOlderThan = Sys.time()))))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo="xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo="xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash)
  expect_true("4e6e705cb7e50920" %in% firesHash)

  mess1 <- capture_messages(sims <- spades(mySim))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                        mess1)))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo="xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo="xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash) # cached part is identical
  expect_false("4e6e705cb7e50920" %in% firesHash) # The non cached stuff goes ahead as normal

  clearCache(sims)
})

test_that("test module-level cache", {
  #if((getRversion() > "3.3.2"))
  skip("Not working on R devel")
  library(igraph)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpfile <- tempfile(fileext = ".pdf")

  # Example of changing parameter values
  times = list(start = 0.0, end = 1.0, timeunit = "year")
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
                     capture_messages(sims <- spades(mySim, notOlderThan = Sys.time()))))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo="xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo="xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash)
  expect_true("4e6e705cb7e50920" %in% firesHash)
  dev.off()
  expect_true(file.info(tmpfile)$size > 20000)
  unlink(tmpfile)

  # The cached version will be identical for both events (init and plot), but will not actually
  # complete the plot, because plotting isn't cacheable
  pdf(tmpfile)
  mess1 <- capture_messages(sims <- spades(mySim))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module",
                        mess1)))
  landscapeObjHash <- digest::digest(object = dropLayer(sims$landscape, "Fires"), algo="xxhash64")
  firesHash <- digest::digest(object = sims$landscape$Fires, algo="xxhash64")
  expect_identical("290afe2cf904d4f5", landscapeObjHash) # cached part is identical
  expect_false("4e6e705cb7e50920" %in% firesHash) # The non cached stuff goes ahead as normal
  dev.off()
  expect_true(file.info(tmpfile)$size < 10000)
  unlink(tmpfile)

  clearCache(sims)
})
