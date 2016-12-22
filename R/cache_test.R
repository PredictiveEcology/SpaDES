#' To run cache
#'
#' This temp script is to try modification made on cache
#'
#' @param sim   bleblebleb
#'
#' @return Invisibly returns the cropped, projected, and masked raster.
#'
#' @importFrom raster raster
#' @importFrom utils download.file unzip
#' @export
#' @docType methods
#'
#'
testcache <- function(sim) {
  myGreatFuncion <- function(sim) {
    data2 <- basename(sim$url)
    download.file(sim$url, file.path(paths$outputPath, data2))
    unzip <- unzip(file.path(paths$outputPath, data2), exdir = file.path(paths$outputPath, file_path_sans_ext(data2)))
    sim$file<- list.files(file.path(outdir, file_path_sans_ext(data2)), pattern="\\.tif$")
    return(sim)
  }
  myGreatFuncion1 <- function(sim) {
    sim$rast <-raster(file.path(paths$outputPath,sim$file,sim$file))
    return(sim)
  }
  sim <- myGreatFuncion(sim)
  sim <- myGreatFuncion1(sim)

  return(sim)
}

outdir<-tempdir()
times <- list(start = 2010.0, end = 2020.0, timeunit = "year")
parameters <- list()
modules <- list()
paths <- list(outputPath=outdir)
sim <- SpaDES::simInit(times = times, params = parameters, modules = modules, paths = paths)
#sim$url<-"ftp://ftp.agrc.utah.gov/Imagery/LandForm/"
sim$url<- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/image/landsat_7/geobase_ortho/geotiff/lcc/004026/004026_0100_010805_l7_6l_lcc00.tif.zip"



#mySim<- testcache(sim)
mySim<-SpaDES::Cache(cacheRepo= outdir, testcache, Copy(sim), sideEffect = TRUE, makeCopy = TRUE)


#mySim<-system.time(SpaDES::Cache(cacheRepo=outdir, testcache, sim, sideEffect = FALSE))

#system.time(archivist::cache(cacheRepo, testcache(sim)))

