test_that("experiment does not work correctly", {
  library(raster)
  beginCluster(8)
  startFiles <- dir(file.path(tempdir()), full.names=TRUE, recursive=TRUE)
  #'
  #' # Example of changing parameter values
  mySimFull <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime=NA),
      caribouMovement = list(.plotInitialTime=NA),
      randomLandscapes = list(.plotInitialTime=NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tempdir()),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
  )
  #'
  #'
  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)
  experimentParams <- list(fireSpread = list(spreadprob = c(0.2, 0.23),
                                              nFires = c(20, 10)),
                        caribouMovement = list(N = c(100, 1000)))
  #'
  sims <- experiment(mySimFull, params=experimentParams)
  exptDesign <- read.csv(file.path(tempdir(), "experiment.csv"))

    expect_equal(paths(mySim),
               list(cachePath = paths[[2]], modulePath = paths$modulePath,
                    inputPath = getwd(), outputPath = getwd())
              )

  # test for non consecutive order, but named
})
