##############################################################
#' Move
#'
#' Wrapper for selecting different animal movement methods.
#'
#' @param agent       A SpatialPoints*Named (created by SpatialPointsNamed or
#' SpatialPointsDataFrameNamed, of with name(obj) <- "character").
#'
#' @export
#' @docType methods
#' @rdname crw-method
#'
#' @author Eliot McIntire
#'
move <- function(hypothesis=NULL,...) {
     #if (hypothesis == "TwoDT") move <- "TwoDT"
     if (hypothesis == "crw") move <- "crw"
     if (is.null(hypothesis) ) stop("Must specify a movement hypothesis")
     get(move)(...)
 }


##############################################################
#' Simple Correlated Random Walk
#'
#' This version uses just turn angles and step lengths to define the correlated random walk.
#'
#' This simple version of a correlated random walk is largely the version that
#' was presented in Turchin 1998, but it was also used with bias modifications
#' in McIntire, Schultz, Crone 2007.
#'
#' @param agent       A SpatialPoints*Named (created by SpatialPointsNamed or
#' SpatialPointsDataFrameNamed, of with name(obj) <- "character").
#'
#' @param stepLength  Numeric vector of length 1 or number of agents describing
#'                    step length.
#'
#' @param stddev          Numeric vector of length 1 or number of agents describing
#'                    standard deviation of wrapped normal turn angles.
#'
#' @param lonlat      Logical. If \code{TRUE}, coordinates should be in degrees.
#'                    If \code{FALSE} coordinates represent planar ('Euclidean')
#'                    space (e.g. units of meters)
#'
#' @return An agent object with updated spatial position defined by a single
#'          occurence of step length(s) and turn angle(s).
#'
#' @seealso \code{\link{pointDistance}}
#'
#' @references Turchin, P. 1998. Quantitative analysis of movement: measuring and modeling population redistribution in animals and plants. Sinauer Associates, Sunderland, MA.
#'
#' @references McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007. Designing a network for butterfly habitat restoration: where individuals, populations and landscapes interact. Journal of Applied Ecology 44:725-736.
#'
#' @export
#' @docType methods
#' @rdname crw-method
#'
#' @author Eliot McIntire
#'
#@examples
#NEED EXAMPLES
crw = function(agent, stepLength, stddev, lonlat) {

  if (is.null(lonlat)) {
    stop("you must provide a \"lonlat\" argument (TRUE/FALSE)")
  }
  stopifnot(is.logical(lonlat))

  n <- length(agent)
  agentHeading <- heading(cbind(x=agent$x1, y=agent$y1), agent)
  rndDir <- rnorm(n, agentHeading, stddev)
  #rndDir <- ifelse(rndDir>180, rndDir-360, ifelse(rndDir<(-180), 360+rndDir, rndDir))
  rndDir[rndDir>180] <- rndDir[rndDir>180]-360
  rndDir[rndDir<=180 & rndDir<(-180)] <- 360+rndDir[rndDir<=180 & rndDir<(-180)]

  agent@data[,c("x1","y1")] <- coordinates(agent)
  agent@coords <- cbind(x=agent$x + sin(rad(rndDir)) * stepLength,
                        y=agent$y + cos(rad(rndDir)) * stepLength)


  return(agent)
}

crw3 = function(agent, stepLength, stddev, lonlat) {

  if (is.null(lonlat)) {
    stop("you must provide a \"lonlat\" argument (TRUE/FALSE)")
  }
  stopifnot(is.logical(lonlat))

  n <- length(agent)
  agentHeading <- heading(cbind(x=agent$x1, y=agent$y1), agent)
  rndDir <- rnorm(n, agentHeading, stddev)
  #rndDir <- ifelse(rndDir>180, rndDir-360, ifelse(rndDir<(-180), 360+rndDir, rndDir))
  rndDir[rndDir>180] <- rndDir[rndDir>180]-360
  rndDir[rndDir<=180 & rndDir<(-180)] <- 360+rndDir[rndDir<=180 & rndDir<(-180)]


  return(cbind(x=agent$x + sin(rad(rndDir)) * stepLength,
               y=agent$y + cos(rad(rndDir)) * stepLength,
               x1=agent@coords[,"x"],
               y1=agent@coords[,"y"]))
}
