test_that("timeunit works correctly", {
  times <- list(start=0.0, stop=10)
  params <- list(
    .globals=list(burnStats="npixelsburned", stackName="landscape"),
    randomLandscapes=list(.plotInitialTime=NA, .plotInterval=NA),
    caribouMovement=list(.plotInitialTime=NA, .plotInterval=NA),
    fireSpread=list(.plotInitialTime=NA, .plotInterval=NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  x1 <- list(
    name="testModule",
    description="this is a test.",
    keywords=c("test"),
    authors=c(person(c("Alex", "M"), "Chubaty", email="achubaty@nrcan.gc.ca", role=c("aut", "cre"))),
    version=numeric_version("0.0.1"),
    spatialExtent=raster::extent(rep(NA_real_, 4)),
    timeframe=as.POSIXlt(c(NA, NA)),
    timeunit=NA_character_,
    citation=list(),
    reqdPkgs=list("grid", "raster", "sp"),
    parameters=rbind(defineParameter("dummyVal", "numeric", 1.0, NA, NA, "vague description")),
    inputObjects=data.frame(objectName="testInput",
                            objectClass="list",
                            other=NA_character_,
                            stringsAsFactors=FALSE),
    outputObjects=data.frame(objectName="testOutput",
                             objectClass="list",
                             other=NA_character_,
                             stringsAsFactors=FALSE)
  )

  # Test for numerics, or character strings that are not recognized
  expect_message(timeunit(mySim) <- 1, "^unknown timeunit provided:")
  expect_message(timeunit(mySim) <- "LeapYear", "^unknown timeunit provided:")

  # test that NA_real_ gets coerced to NA_character_
  timeunit(mySim) <- NA_real_
  expect_identical(timeunit(mySim), NA_character_)

  # check that the minTimeunit captures one of the timestepUnits in the loaded modules
  expect_true(
    any(match(minTimeunit(mySim),
              sapply(depends(mySim)@dependencies, function(x) {
                x@timeunit
              })
             )
       )
  )

  # check that minTimeunit finds the smallest timeunit of the modules loaded
  whNotNA <- sapply(depends(mySim)@dependencies,
                    function(x) !is.na(x@timeunit))
  expect_equivalent(as.numeric(eval(parse(
    text=paste0("d", minTimeunit(mySim), "(1)")))),
    min(sapply(depends(mySim)@dependencies[whNotNA],
               function(x) {
                 eval(parse(text=paste0("d",x@timeunit,"(1)")))
               }
    )))
})
