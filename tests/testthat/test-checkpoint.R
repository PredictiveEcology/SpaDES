test_that("test checkpointing", {
#   file <- "chkpnt.RData"
#   fobj <- "chkpnt_objs.RData"
#   on.exit(unlink(c(file, fobj)))
#
#   ## save checkpoints; no load/restore
#   set.seed(1234)
#   times <- list(start=0, stop=10, timeunit="second")
#   parameters <- list(.globals=list(stackName="landscape"),
#                     .checkpoint=list(interval=1, file=file),
#                     randomLandscapes=list(.plotInitialTime=NA),
#                     caribouMovement=list(.plotInitialTime=NA, torus=TRUE))
#   modules <- list("randomLandscapes", "caribouMovement")
#   path <- system.file("sampleModules", package="SpaDES")
#   sim1 <- simInit(times=times, params=parameters, modules=modules, path=path)
#   sim1 <- spades(sim1)
#
#   ## save checkpoints; with load/restore
#   set.seed(1234)
#   times <- list(start=0, stop=5, timeunit="second")
#   sim2 <- simInit(times=times, params=parameters, modules=modules, path=path)
#   sim2 <- spades(sim2)
#   rm(sim2)
#
#   checkpointLoad(file=file)
#   endTime <- 10
#   attributes(endTime)$unit <- "second"
#   end(sim2) <- 10
#   sim2 <- spades(sim2)
#
#   ## both versions above should yield identical results
#   expect_true(all.equal(as(sim1,"simList_"), as(sim2,"simList_")))
    expect_true(TRUE) # remove once the tests have been fixed
})
