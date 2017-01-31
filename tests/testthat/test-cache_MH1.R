test_that("test-cache", {
  library(tools)
  library(raster)

  on.exit({
    detach("package:tools")
    detach("package:raster")
  }, add = TRUE)

  outdir<-tempdir()
  sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"),
                         params = list(),
                         modules = list(),
                         paths = list(outputPath=outdir))

  sim$url<- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/image/landsat_7/geobase_ortho/geotiff/lcc/004026/004026_0100_010805_l7_6l_lcc00.tif.zip"
  sim$path <- tempdir()

  testcache <- function(sim) {
    myGreatFuncion <- function(sim) {
      data2 <- basename(sim$url)
      download.file(sim$url, file.path(sim$path, data2))
      unzip <- unzip(file.path(sim$path, data2), exdir = sim$path)
      sim$file<- list.files(sim$path, pattern="\\.tif$")
      return(sim)
    }
    myGreatFuncion1 <- function(sim) {
      sim$rast <-raster(file.path(sim$path,sim$file))
      return(sim)
    }
  sim <- myGreatFuncion(sim)
  sim <- myGreatFuncion1(sim)
  return(sim)
  }
  #testcache(Copy(sim))
  mySim<-SpaDES::Cache(cacheRepo= outdir, testcache, Copy(sim), sideEffect = TRUE, makeCopy = TRUE, quick = FALSE)

  expect_that(file.exists(file.path(mySim$path, basename(sim$url))), is_true())
  expect_that(is(mySim$rast, "RasterLayer"), is_true())
})
