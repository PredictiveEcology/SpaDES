################################################################################
#' Use Pattern Oriented Modeling to fit unknown parameters
#'
#'
#' @inheritParams spades
#' @inheritParams splitRaster
#' @param objFn An objective function to be passed into
#'              \code{optimizer}
#' @param optimizer The function to use to optimize. Default is
#'                  DEoptim
#' @param ... All objects needed in objFn
#'
#' @return The values for parameters used in objFn that minimize
#' the objFn.
#'
#' @seealso \code{\link{spades}}, \code{\link[parallel]{makeCluster}},
#' \code{\link{simInit}}
#'
#' @include module-dependencies-class.R
#' @include helpers.R
#' @include simList-class.R
#' @include environment.R
#' @include priority.R
#' @export
#' @docType methods
#' @rdname POM
#'
#' @author Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  set.seed(52461)
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      fireSpread = list(nFires = 1),
#'      randomLandscapes = list(nx = 200, ny = 200)
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  out <- spades(copy(mySim), .plotInitialTime = NA)
#'  fireData <- out$landscape$Fires
#'  Plot(fireData, new=TRUE)
#'
#'
#'  library(parallel)
#'  library(DEoptim)
#'  objFn <- function(spreadProb, fireMap, simList) {
#'     params(mySim)$fireSpread$spreadprob = spreadProb
#'     out <- spades(copy(simList), .plotInitialTime = NA)
#'     abs(cellStats(fireMap - out$landscape$Fires, "sum"))
#'  }
#'  cl <- makeCluster(6)
#'  clusterExport(cl, c("mySim", "fireData"))
#'  clusterEvalQ(cl, {
#'  library(SpaDES)
#'  library(RColorBrewer)
#'  library(raster)
#'  })
#'  output <- DEoptim(objFn, lower = 0.10, upper = 0.26, simList = mySim, fireMap = fireData,
#'      control=list(parallelType=3, itermax = 10), cl = cl)
#'  stopCluster(cl)
#'
#'  )
#'  }
setGeneric(
  "POM",
  function(sim, objFn, cl, optimizer = "DEoptim", ...) {
    standardGeneric("POM")
  })

#' @rdname POM
setMethod(
  "POM",
  signature(sim = "simList", objFn = "function"),
  definition = function(sim, objFn, cl, optimizer, ...) {

    stop("this is a stub")
  })

