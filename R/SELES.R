### SELES options LIBRARY
transitions <- function(p, agent) {
    coordinates(agent)[which(p==0),] = NA
    return(agent)
}

numAgents <- function(N) {
    if ((length(N) == 1) && (is.numeric(N))) numAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(numAgents)
}

# this is poorly named
agentLocation <- function(map) {
    if (length(grep(pattern = "Raster", class(map)))==1) {
        map[map==0] <- NA
    } else if (length(grep(pattern = "SpatialPoints", class(map)))==1) {
        map
    } else if (!is.na(pmatch("SpatialPolygons", class(map)))) {
        map
    } else {
        stop("only raster, Spatialpoints or SpatialPolygons implemented")
    }
    return(map)
}

##############################################################
#' probInit
#'
#' Details here.
#'
#' @param map Description of this.
#'
#' @param p Description of this.
#'
#' @param absolute Description of this.
#'
#' @return Decribe what it returns: fromto.
#'
#' @seealso \code{\link{print}} and \code{\link{cat}}
#'
#' @import raster sp
#' @include agent-methods.R
#' @include named-objects.R
#' @export
#' @docType methods
#' @rdname probInit
#'
#' @author Eliot McIntire
#'
# @examples
# NEED EXAMPLES
probInit = function(map, p=NULL, absolute=FALSE) {
  if (length(p) == 1) {
    probInit = raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
    probInit = setValues(probInit, rep(p,length(probInit)))
  } else if (is(p,"RasterLayer")) {
    probInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  } else if (is(map,"SpatialPolygonsDataFrame")) {
    probInit = p/sum(p)
  } else if (is(p,"NULL"))  {
    probInit = map/(cellStats(p,sum)*(1-absolute)+1*(absolute))
  } else {
    stop("Error initializing probability map: bad inputs") # temp err msg (general)
  }
  return(probInit)
}

###
patchSize = function(patches) {
  return(freq(patches))
}
