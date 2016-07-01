################################################################################
#' Use Pattern Oriented Modeling to fit unknown parameters
#'
#'
#' @inheritParams spades
#' @params objFn An objective function to be passed into DEoptim
#' @params cl A cluster object, likely from parallel::makeCluster
#'
#' @return A .
#'
#' @seealso \code{\link{spades}},
#' \code{\link{times}}, \code{\link{params}}, \code{\link{objs}}, \code{\link{paths}},
#' \code{\link{modules}}, \code{\link{inputs}}, \code{\link{outputs}}
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @include priority.R
#' @export
#' @docType methods
#' @rdname simInit
#'
#' @author Alex Chubaty and Eliot McIntire
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

#' @rdname simInit
setMethod(
  "POM",
  signature(sim = "simList", objFn = "function"),
  definition = function(simList, objFn, optimizer, cl, ...) {

    stop("this is a stub")
  })

