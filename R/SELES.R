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

##############################################################
#' spec.num.per.patch
#'
#' Instantiate a specific number of agents per patch.
#'
#' @param patches Description of this.
#'
#' @param num.per.patch.table Description of this.
#'
#' @param num.per.patch.map Description of this.
#'
#' @return Decribe what it returns: \code{al}.
#' 
#' #@seealso \code{\link{print}} and \code{\link{cat}}
#' 
#' @import data.table raster sp
#' @export
#' @docType methods
#' @rdname specnumperpatch-probs
#'
# @examples
# NEED EXAMPLES
# 
# To initialize with a specific number per patch, which may come from
#  data or have been derived from patch size. Options include a combination of either
#  a patchid map and a table with 2 columns, pops and num.in.pop,
#  or 2 maps, patchid and patchnumber. Returns a map with a single unique pixel
#  within each patch representing an agent to start. This means that the number
#  of pixels per patch must be greater than the number of agents per patch
spec.num.per.patch = function(patches, num.per.patch.table=NULL, num.per.patch.map=NULL) {
  patchids = as.numeric(na.omit(getValues(patches)))
  wh = Which(patches, cells = T)
  if (!is.null(num.per.patch.table)) {
    dt1 = data.table(wh, pops=patchids)
    setkey(dt1, pops)
    if (is(num.per.patch.table, "data.table")) {
      num.per.patch.table = data.table(num.per.patch.table)
    }
    setkey(num.per.patch.table, pops)
    dt2 = dt1[num.per.patch.table]
  } else if (!is.null(num.per.patch.map)) {
    num.per.patch.table = as.numeric(na.omit(getValues(num.per.patch.map)))
    dt2 = data.table(wh,pops = patchids, num.in.pop = num.per.patch.table)
  } else { stop("need num.per.patch.map or num.per.patch.table") }
  
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  dt3 = dt2[, list(cells=resample(wh, unique(num.in.pop))), by=pops]
  dt3$ids = rownames(dt3)
  
  al = raster(patches)
  al[dt3$cells] = 1
  
  return(al)
}




patch.size = function(patches) {
  patch.size = freq(patches)
  return(patch.size)
}
