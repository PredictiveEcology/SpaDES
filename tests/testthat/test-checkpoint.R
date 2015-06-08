test_that("testing checkpoint, passed", {

  ####################
  set.seed(1234)
  times <- list(start=0, stop=10)
  parameters <- list(.globals=list(stackName="landscape"),
                     .checkpoint=list(interval=1, file="chkpnt.rdata"),
                     randomLandscapes=list(.plotInitialTime=NA),
                     caribouMovement=list(.plotInitialTime=NA))
  modules <- list("randomLandscapes", "caribouMovement")
  path <- system.file("sampleModules", package="SpaDES")
  mySimFirst <- simInit(times=times, params=parameters, modules=modules, path=path)
  mySimFirst <- spades(mySimFirst)

  ####################
  set.seed(1234)
  times <- list(start=0, stop=5)
  mySimSecond <- simInit(times=times, params=parameters, modules=modules, path=path)
  mySimSecond <- spades(mySimSecond)
  rm(mySimSecond)

  checkpointLoad(file="chkpnt.rdata")
  simStopTime(mySimSecond) <- 10
  mySimSecond <- spades(mySimSecond)

expect_true(all.equal(as(mySimFirst,"simList_"), as(mySimSecond,"simList_")))

})
