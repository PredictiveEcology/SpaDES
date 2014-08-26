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
#' @param x An spatial object (e.g., a raster).
#'
#' @param scale The spatial scale in map units of the Gaussian pattern.
#'
#' @param var Spatial variance.
#'
#' @param speedup An index of how much faster than normal to generate maps.
#'
#' @param inMemory Should the RasterLayer be forced to be in memory? Default \code{FALSE}.
#'
#' @param ... Additional arguments to \code{raster}.
#'
#' @return A map of extent \code{ext} with a Gaussian random pattern.
#'
#' @seealso \code{\link{RFsimulate}} and \code{\link{extent}}
#'
#' @import RandomFields raster
#' @export
#' @docType methods
#' @rdname gaussmap-method
#'
#@examples
#EXAMPLES NEEDED
GaussMap <- function(x, scale=10, var=1, speedup=10, inMemory=FALSE, ...) {#, fast=TRUE, n.unique.pixels=100) {
  RFoptions(spConform=FALSE)
  ext <- extent(x)
  resol <- res(x)
  nc <- (ext@xmax-ext@xmin)/speedup
  nr <- (ext@ymax-ext@ymin)/speedup

  model <- RMexp(scale=scale, var=var)
  if (inMemory) {
    sim <- rasterToMemory(RFsimulate(model, y=1:nc, x=1:nr, grid=TRUE, ...))
  } else {
    sim <- raster(RFsimulate(model, y=1:nc, x=1:nr, grid=TRUE, ...))
  }
  sim <- sim - cellStats(sim, "min")
  extent(sim) <- ext
  if(speedup>1)
    return(disaggregate(sim, c(speedup, speedup)))
  else
    return(invisible(sim))
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
#
spec.num.per.patch <- function(patches, num.per.patch.table=NULL, num.per.patch.map=NULL) {
  patchids <- as.numeric(na.omit(getValues(patches)))
  wh <- Which(patches, cells=TRUE)
  if (!is.null(num.per.patch.table)) {
    dt1 <- data.table(wh, pops=patchids)
    setkey(dt1, pops)
    if (is(num.per.patch.table, "data.table")) {
      num.per.patch.table <- data.table(num.per.patch.table)
    }
    setkey(num.per.patch.table, pops)
    dt2 <- dt1[num.per.patch.table]
  } else if (!is.null(num.per.patch.map)) {
    num.per.patch.table <- as.numeric(na.omit(getValues(num.per.patch.map)))
    dt2 <- data.table(wh,pops=patchids, num.in.pop=num.per.patch.table)
  } else {
    stop("need num.per.patch.map or num.per.patch.table")
  }

  resample <- function(x, ...) x[sample.int(length(x), ...)]
  dt3 <- dt2[, list(cells=resample(wh, unique(num.in.pop))), by=pops]
  dt3$ids <- rownames(dt3)

  al <- raster(patches)
  al[dt3$cells] <- 1

  return(al)
}


###
# ### INCORPORATE RELEVANT PARTS OF THIS OLD INIT FUNCTION INTO INITCOODRS()
# ###
# #' initialize mobileAgent
# #'
# #' @param agentlocation The initial positions of the agents
# #'                      (currently only \code{RasterLayer} or
# #'                      \code{SpatialPolygonsDataFrame}) accepted.
# #'
# #' @param numagents The number of agents to initialize.
# #'
# #' @param probinit The probability of placing an agent at a given initial position.
# #'
# #' @export
# setMethod("initialize", "mobileAgent", function(.Object, ..., agentlocation=NULL, numagents=NULL, probinit=NULL) {
#   if (is(agentlocation, "Raster")){
#     ext <- extent(agentlocation)
#     if (!is.null(probinit)) {
#       #            nonNAs <- !is.na(getvalue(probinit))
#       nonNAs <- !is.na(getValues(probinit))
#       wh.nonNAs <- which(nonNAs)
#       #            ProbInit.v <- cumsum(getvalue(probinit)[nonNAs])
#       ProbInit.v <- cumsum(getValues(probinit)[nonNAs])
#       if (!is.null(numagents)) {
#         ran <- runif(numagents,0,1)
#         fI <- findInterval(ran, ProbInit.v)+1
#         fI2 <- wh.nonNAs[fI]
#         last.ran <- runif(numagents,0,1)
#         last.fI <- findInterval(last.ran, ProbInit.v)+1
#         last.fI2 <- wh.nonNAs[last.fI]
#       } else {
#         #                va <- getvalue(probinit)[nonNAs]
#         va <- getValues(probinit)[nonNAs]
#         ran <- runif(length(va), 0, 1)
#         fI2 <- wh.nonNAs[ran<va]
#
#         last.ran <- runif(length(fI2), 0, 1)
#         last.fI <- findInterval(last.ran, ProbInit.v) + 1
#         last.fI2 <- wh.nonNAs[last.fI]
#
#         #                last.ran <- runif(length(fI2),0,1)
#         #                last.fI2 <- wh.nonNAs[last.ran<va]
#       }
#       if (length(grep(pattern="Raster",class(agentlocation)))==1) {
#         position <- xyFromCell(agentlocation,fI2,spatial=TRUE)
#       } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
#         position <- coordinates(agentlocation)
#       } else {
#         stop("need raster layer or Spatial Points object")
#       }
#       numagents <- length(position)
#     } else {
#       # probinit is NULL - start exactly the number of agents as there
#       # are pixels in agentlocation
#       if (!is.null(numagents)) {
#         if (is(agentlocation,"Raster")) {
#           xy=matrix(runif(numagents*2, c(xmin(ext), ymin(ext)), c(xmax(ext), ymax(ext))), ncol=2, byrow=TRUE)
#           colnames(xy)=c("x","y")
#           position <- SpatialPoints(xy)
#           #                    position <- SpatialPoints(sampleRandom(agentlocation, numagents, xy=TRUE, sp=TRUE))
#         } else if (is(agentlocation,"SpatialPoints")) {
#           sam <- sample(1:length(agentlocation),numagents)
#           position <- SpatialPoints(agentlocation[sam,])
#         } else {
#           stop("need raster layer or Spatial Points object")
#         }
#       } else { # for numagents also NULL
#         if (length(grep(pattern="Raster",class(agentlocation)))==1) {
#           position <- SpatialPoints(xyFromCell(agentlocation,Which(agentlocation,cells=T)))
#         } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
#           position <- SpatialPoints(agentlocation)
#         } else {
#           stop("need raster layer or Spatial Points object")
#         }
#         numagents <- length(position)
#       }
#     }
#   } else if (is(agentlocation,"SpatialPolygonsDataFrame")) {
#     if (!is.null(numagents)) {
#       if (!is.null(pri) ) {
#         position <- SpatialPoints(dotsInPolys(agentlocation,as.integer(round(numagents*pri,0))))
#         numagents <- length(position)
#       } else {stop("with SpatialPolygonsDataFrame, probinit is required")}
#     } else {stop("with SpatialPolygonsDataFrame, numagents is required")}
#   } else if (is.null(agentlocation)) { stop("Need to provide agentlocation, which can be a map layer")
#   }
#   heading1 <- runif(numagents, 0, 360)
#   distance <- runif(numagents, 0.1, 10)
#
#   .Object@ID <- as.character(1:numagents)
#   .Object@spatial <- position
#   .Object@heading <- heading1
#   .Object@distance <- distance
#
#   return(.Object)
# })
