################################################################################
#' Move
#'
#' Wrapper for selecting different animal movement methods.
#'
#' @param hypothesis  Character vector, length one, indicating which movement
#'                    hypothesis/method to test/use. Currently defaults to
#'                    'crw' (correlated random walk) using \code{crw}.
#'
#' @param ... arguments passed to the function in \code{hypothesis}
#'
#' @export
#' @docType methods
#' @rdname crw
#'
#' @author Eliot McIntire
#'
move <- function(hypothesis = "crw", ...) {
     #if (hypothesis == "TwoDT") move <- "TwoDT"
     if (hypothesis == "crw") move <- "crw"
     if (is.null(hypothesis) ) stop("Must specify a movement hypothesis")
     get(move)(...)
 }

################################################################################
#' Simple Correlated Random Walk
#'
#' This version uses just turn angles and step lengths to define the correlated random walk.
#'
#' This simple version of a correlated random walk is largely the version that
#' was presented in Turchin 1998, but it was also used with bias modifications
#' in McIntire, Schultz, Crone 2007.
#'
#' @param agent       A \code{SpatialPoints*} object.
#'                    If a \code{SpatialPointsDataFrame}, 2 of the columns must
#'                    be \code{x1} and \code{y1}, indicating the previous location.
#'                    If a \code{SpatialPoints} object, then \code{x1} and
#'                    \code{y1} will be assigned randomly.
#'
#' @param stepLength  Numeric vector of length 1 or number of agents describing
#'                    step length.
#'
#' @param extent      An optional \code{Extent} object that will be used for \code{torus}.
#'
#' @param torus       Logical. Should the movement be wrapped to the opposite
#'                    side of the map, as determined by the \code{extent} argument.
#'                    Default \code{FALSE}.
#'
#' @param stddev      Numeric vector of length 1 or number of agents describing
#'                    standard deviation of wrapped normal turn angles.
#'
#' @param lonlat      Logical. If \code{TRUE}, coordinates should be in degrees.
#'                    If \code{FALSE} coordinates represent planar ('Euclidean')
#'                    space (e.g. units of meters)
#'
#' @return A SpatialPointsDataFrame object with updated spatial position defined
#'         by a single occurrence of step length(s) and turn angle(s).
#'
#' @seealso \code{\link{pointDistance}}
#'
#' @references Turchin, P. 1998. Quantitative analysis of movement: measuring and modeling population redistribution in animals and plants. Sinauer Associates, Sunderland, MA.
#'
#' @references McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007. Designing a network for butterfly habitat restoration: where individuals, populations and landscapes interact. Journal of Applied Ecology 44:725-736.
#'
#' @export
#' @importFrom CircStats rad
#' @importFrom stats rnorm
#' @docType methods
#' @rdname crw
#'
#' @author Eliot McIntire
#'
setGeneric("crw", function(agent, extent, stepLength, stddev, lonlat, torus = FALSE) {
  standardGeneric("crw")
})

#' @export
#' @rdname crw
setMethod(
  "crw",
  signature(agent = "SpatialPointsDataFrame"),
  definition = function(agent, extent, stepLength, stddev, lonlat, torus = FALSE) {
    if (is.null(lonlat) || !is.logical(lonlat)) {
      stop("you must provide a \"lonlat\" argument (TRUE/FALSE)")
    }
    hasNames <- names(agent) %in% c("x1", "y1")
    n <- length(agent)

    if (sum(hasNames) < 2) {
        stop("SpatialPointsDataFrame needs x1 and y1 columns with previous location")
    }

    agentHeading <- heading(cbind(x = agent$x1, y = agent$y1), agent)
    rndDir <- rnorm(n, agentHeading, stddev)
    rndDir[rndDir > 180] <- rndDir[rndDir > 180] - 360
    rndDir[rndDir <= 180 & rndDir < (-180)] <- 360 + rndDir[rndDir <= 180 & rndDir < (-180)]

    agent@data[, c("x1", "y1")] <- coordinates(agent)
    agent@coords <- cbind(
      x = coordinates(agent)[, 1] + sin(rad(rndDir)) * stepLength,
      y = coordinates(agent)[, 2] + cos(rad(rndDir)) * stepLength
    )

    if (torus) {
      return(wrap(X = agent, bounds = extent, withHeading = TRUE))
    } else {
      return(agent)
    }
})

#' @export
#' @importFrom sp SpatialPointsDataFrame
#' @rdname crw
setMethod(
  "crw",
  signature(agent = "SpatialPoints"),
  definition = function(agent, extent, stepLength, stddev, lonlat, torus = FALSE) {
    n <- length(agent)
    agent <- SpatialPointsDataFrame(agent, data = data.frame(
      x1 = runif(n, -180, 180), y1 = runif(n, -180, 180)
    ))
    names(agent) <- c("x1", "y1")
    agent <- crw(agent, extent = extent, stepLength = stepLength,
                 stddev = stddev, lonlat = lonlat, torus = torus)
    return(agent)
})
