##############################################################
#' GaussMap
#'
#' Produces a raster of a random gaussian process. 
#' 
#' This is a wrapper for the \code{RFsimulate} function in the RandomFields 
#' package. The main addition is the \code{speedup} argument which allows
#' for faster map generation. A \code{speedup} of 1 is normal and will get
#' progressively faster as the number increases, at the expense of coarser pixel
#' resolution of the pattern generated
#'
#' @param ext An object of class \code{extent} giving the dimensions of output map.
#'
#' @param scale The spatial scale in map units of the Gaussian pattern.
#'
#' @param var Spatial variance.
#'
#' @param speedup An index of how much faster than normal to generate maps.
#'
#' @return A map of extent \code{ext} with a Gaussian random pattern.
#' 
#' @seealso \code{\link{RFsimulate}} and \code{\link{extent}}
#' 
#' @import RandomFields
#' @import raster
#' @export
#' @docType methods
#' @rdname gaussmap-method
#'
#@examples
#EXAMPLES NEEDED
GaussMap = function(x, scale = 10, var = 1, speedup = 10) {#, fast = T, n.unique.pixels = 100) {
  RFoptions(spConform=FALSE)
  ext <- extent(x)
  resol <- res(x)
  xmn = ext@xmin
  xmx = ext@xmax
  ymn = ext@ymin
  ymx = ext@ymax
  nc = (xmx-xmn)/speedup # ifelse(fast, min(n.unique.pixels,xmx-xmn),xmx-xmn)
  nr = (ymx-ymn)/speedup # ifelse(fast, min(ymx-ymn,n.unique.pixels),ymx-ymn)
  xfact = (xmx-xmn)/nc
  yfact = (ymx-ymn)/nr
  
  model <- RMexp(scale=scale, var = var)
  x.seq = 1:nc
  y.seq = 1:nr
  sim <- raster(RFsimulate(model, x = x.seq, y = y.seq, grid = T))
  sim <- sim - cellStats(sim, "min")
  extent(sim) <- ext
  res(sim) <- resol
  
  if(speedup>1) {
    sim <- disaggregate(sim, c(xfact, yfact))
  } else {
    extent(sim) <- ext
  }
  return(sim)
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
