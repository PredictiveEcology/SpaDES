test_that("test checkpointing", {
  tmpdir <- tempdir()
  file <- file.path("chkpnt.RData")
  fobj <- file.path("chkpnt_objs.RData")
  on.exit(unlink(c(file, fobj, tmpdir)))

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
  sim1 <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  sim1 <- spades(sim1)

  ## save checkpoints; with load/restore
  set.seed(1234)
  times <- list(start = 0, end = 1, timeunit = "second")
  sim2 <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  sim2 <- spades(sim2)
  rm(sim2)

  checkpointLoad(file = file.path(paths$outputPath, file))
  end(sim2) <- 2
  sim2 <- spades(sim2)

  ## both versions above should yield identical results
  expect_true(all.equal(as(sim1, "simList_"), as(sim2, "simList_")))
})
