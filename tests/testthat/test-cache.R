test_that("test cache", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(igraph); on.exit(detach("package:igraph"), add = TRUE)
  library(dplyr); on.exit(detach("package:dplyr"), add = TRUE)

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
  expect_true(NROW(out)==10) # will become 15 with new experiment caching stuff
  clearCache(sims[[1]])
  out <- print(showCache(sims[[1]]))
  expect_true(NROW(out)==0)

})

