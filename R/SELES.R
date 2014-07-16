### SELES options LIBRARY

Transitions = function(p, agent) {
    agent@spatial@coords[which(p==0),] = NA
    return(agent)
}

NumAgents = function(N) {
    if ((length(N) == 1) && (is.numeric(N))) NumAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(NumAgents)
}

AgentLocation = function(map) {
    if (length(grep(pattern = "Raster", class(map)))==1) {
        map[map==0] = NA
    } else if (length(grep(pattern = "SpatialPoints", class(map)))==1) {
        map
    } else if (!is.na(pmatch("SpatialPolygons",class(map)))) {
        map
    } else {
        stop("only raster, Spatialpoints or SpatialPolygons implemented")
    }
    return(map)
}

##############################################################
#' ProbInit
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
#' #@seealso \code{\link{print}} and \code{\link{cat}}
#' 
#' @import raster
#' @import sp
#' @export
#' @docType methods
#' @rdname ring-probs
#'
# @examples
# NEED EXAMPLES
ProbInit = function(map, p=NULL, absolute=FALSE) {
  if (length(p) == 1) { 
    ProbInit = raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
    ProbInit = setValues(ProbInit, rep(p,length(ProbInit)))
  } else if (is(p,"RasterLayer")) {
    ProbInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  } else if (is(map,"SpatialPolygonsDataFrame")) {
    ProbInit = p/sum(p) 
  } else if (is(p,"NULL"))  {
    ProbInit = map/(cellStats(p,sum)*(1-absolute)+1*(absolute))
  } else {
    stop("Error initializing probability map: bad inputs") # temp err msg (general)
  }
  return(ProbInit)
}


patch.size = function(patches) {
  patch.size = freq(patches)
  return(patch.size)
}
