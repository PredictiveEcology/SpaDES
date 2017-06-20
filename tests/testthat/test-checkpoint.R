test_that("test checkpointing", {
  library(igraph)
  library(quickPlot)
  tmpdir <- file.path(tempdir(), "test_checkpoint") %>% checkPath(create = TRUE)
  file <- file.path("chkpnt.RData")
  on.exit({
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  ## save checkpoints; no load/restore
  set.seed(1234)
  times <- list(start = 0, end = 2, timeunit = "second")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    .checkpoint = list(interval = 1, file = file),
    randomLandscapes = list(.plotInitialTime = NA),
    caribouMovement = list(.plotInitialTime = NA, torus = TRUE)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES"),
    outputPath = tmpdir
  )
  simA <- simInit(times = times, params = parameters, modules = modules,
                  paths = paths)
  simA <- suppressWarnings(spades(simA))

  ## save checkpoints; with load/restore
  set.seed(1234)
  times <- list(start = 0, end = 2, timeunit = "second")
  simB <- simInit(times = times, params = parameters, modules = modules,
                  paths = paths)
  end(simB) <- 1
  simB <- suppressWarnings(spades(simB))
  rm(simB)

  checkpointLoad(file = file.path(paths$outputPath, file))
  end(simB) <- 2
  simB <- spades(simB)

  ## both versions above should yield identical results
  expect_true(all.equal(as(simA, "simList_"), as(simB, "simList_")))
})
