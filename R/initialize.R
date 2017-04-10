if (getRversion() >= "3.1.0") {
  utils::globalVariables("num.in.pop")
}

###############################################################################
#' Produce a \code{raster} of a random Gaussian process.
#'
#' This is a wrapper for the \code{RFsimulate} function in the \code{RandomFields}
#' package. The main addition is the \code{speedup} argument which allows
#' for faster map generation. A \code{speedup} of 1 is normal and will get
#' progressively faster as the number increases, at the expense of coarser
#' pixel resolution of the pattern generated.
#'
#' @param x        A spatial object (e.g., a \code{RasterLayer}).
#'
#' @param scale    The spatial scale in map units of the Gaussian pattern.
#'
#' @param var      Spatial variance.
#'
#' @param speedup  An numeric value indicating how much faster than 'normal'
#'                 to generate maps. It may be necessary to give a value larger
#'                 than 1 for large maps. Default is 1.
#'
#' @param inMemory Should the RasterLayer be forced to be in memory?
#'                 Default \code{FALSE}.
#'
#' @param ... Additional arguments to \code{raster}.
#'
#' @return A raster map with same extent as \code{x}, with a Gaussian random pattern.
#'
#' @seealso \code{\link{RFsimulate}} and \code{\link{extent}}
#'
#' @importFrom RandomFields RFoptions RFsimulate RMexp round
#' @importFrom raster cellStats disaggregate extent 'extent<-' raster res
#' @export
#' @docType methods
#' @rdname gaussmap
#'
#' @examples
#' \dontrun{
#' library(RandomFields)
#' library(raster)
#' nx <- ny <- 100L
#' r <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2, ymn = -ny/2, ymx = ny/2)
#' speedup <- max(1, nx/5e2)
#' map1 <- gaussMap(r, scale = 300, var = 0.03, speedup = speedup, inMemory = TRUE)
#' Plot(map1)
#' }
#'
gaussMap <- function(x, scale = 10, var = 1, speedup = 1, inMemory = FALSE, ...) {
  RFoptions(spConform = FALSE)
  ext <- extent(x)
  resol <- res(x)
  nc <- (ext@xmax - ext@xmin) / resol[1]
  nr <- (ext@ymax - ext@ymin) / resol[2]
  wholeNumsCol <- .findFactors(nc)
  wholeNumsRow <- .findFactors(nr)
  ncSpeedup <- wholeNumsCol[which.min(abs(wholeNumsCol - nc / speedup))]
  nrSpeedup <- wholeNumsRow[which.min(abs(wholeNumsRow - nr / speedup))]
  speedupEffectiveCol <- nc / ncSpeedup
  speedupEffectiveRow <- nr / nrSpeedup

  model <- RMexp(scale = scale, var = var)
  if (inMemory) {
    map <- rasterToMemory(RFsimulate(model, y = 1:ncSpeedup, x = 1:nrSpeedup, grid = TRUE, ...))
  } else {
    map <- raster(RFsimulate(model, y = 1:ncSpeedup, x = 1:nrSpeedup, grid = TRUE, ...))
  }
  map <- map - cellStats(map, "min")
  extent(map) <- ext
  if (speedup > 1)
    return(disaggregate(map, c(speedupEffectiveCol, speedupEffectiveRow)))
  else
    return(invisible(map))
}

###############################################################################
#' Find factors
#'
#' Internal function (used in \code{link{gaussMap}}).
#' Finds the integer factors of an integer.
#'
#' @param x An integer to factorize
#'
#' @return A vector of integer factors
#'
#' @keywords internal
#' @rdname findFactors
#'
.findFactors <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  return(div[x %% div == 0L])
}

###############################################################################
#' randomPolygons
#'
#' Produces a raster of random polygons.
#' These are built with the \code{\link{spread}} function internally.
#'
#' @param ras A raster that whose extent will be used for the randomPolygons
#'
#' @param numTypes Numeric value. The number of unique polygon types to use.
#'
#' @param ...      Other arguments passed to spread. No known uses currently.
#'
#' @return A map of extent \code{ext} with random polygons.
#'
#' @seealso \code{\link{spread}} and \code{\link{raster}}
#'
#' @importFrom raster cellFromXY extent raster xmax xmin ymax ymin
#' @importFrom sp SpatialPoints
#'
#' @export
#' @docType methods
#' @rdname randomPolygons
#'
#' @examples
#' set.seed(1234)
#' Ras <- randomPolygons(numTypes = 5)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(Ras, cols = c("yellow", "dark green", "blue", "dark red"))
#' }
#'
#' library(raster)
#' # more complex patterning, with a range of patch sizes
#' a <- randomPolygons(numTypes = 400, raster(extent(0, 50, 0, 50), res = 1, vals = 0))
#' a[a<320] <- 0
#' a[a>=320] <- 1
#' suppressWarnings(clumped <- clump(a)) # (warning sometimes occurs, but not important)
#' aHist <- hist(table(getValues(clumped)), plot = FALSE)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(a)
#'   Plot(aHist)
#' }
#'
randomPolygons <- function(ras = raster(extent(0, 15, 0, 15), res = 1, vals = 0),
                           numTypes = 2, ...) {
  args <- list(...)
  if (any(c("p", "A", "speedup", "minpatch") %in% names(args))) {
    warning("Arguments p, A, speedup, and minpatch have been deprecated. See new function definition.")
  }

  starts <- SpatialPoints(coords = cbind(x = stats::runif(numTypes, xmin(ras), xmax(ras)),
                                         y = stats::runif(numTypes, xmin(ras), xmax(ras))))
  loci <- raster::cellFromXY(starts, object = ras)
  a <- spread(landscape = ras, spreadProb = 1, loci, allowOverlap = FALSE, id = TRUE, ...)
  return(a)
}

###############################################################################
#' Initiate a specific number of agents in a map of patches
#'
#' Instantiate a specific number of agents per patch.
#' The user can either supply a table of how many to initiate in each patch,
#' linked by a column in that table called \code{pops}.
#'
#' @param patches \code{RasterLayer} of patches, with some sort of a patch id.
#'
#' @param numPerPatchTable A \code{data.frame} or \code{data.table} with a
#'  column named \code{pops} that matches the \code{patches} patch ids, and a
#'  second column \code{num.in.pop} with population size in each patch.
#'
#' @param numPerPatchMap A \code{RasterLayer} exactly the same as \code{patches}
#' but with agent numbers rather than ids as the cell values per patch.
#'
#' @return A raster with 0s and 1s, where the 1s indicate starting locations of
#' agents following the numbers above.
#'
#' @importFrom data.table data.table setkey
#' @importFrom raster getValues raster Which
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @rdname specnumperpatch-probs
#' @examples
#' library(raster)
#' library(data.table)
#' set.seed(1234)
#' Ntypes <- 4
#' ras <- randomPolygons(numTypes = Ntypes)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(ras)
#' }
#'
#' # Use numPerPatchTable
#' patchDT <- data.table(pops = 1:Ntypes, num.in.pop = c(1, 3, 5, 7))
#' rasAgents <- specificNumPerPatch(ras, patchDT)
#' rasAgents[is.na(rasAgents)] <- 0
#'
#' library(testthat)
#' expect_true(all(unname(table(ras[rasAgents])) == patchDT$num.in.pop))
#'
#' # Use numPerPatchMap
#' rasPatches <- ras
#' for (i in 1:Ntypes) {
#'   rasPatches[rasPatches==i] <- patchDT$num.in.pop[i]
#' }
#' if (interactive()) {
#'   clearPlot()
#'   Plot(ras, rasPatches)
#' }
#' rasAgents <- specificNumPerPatch(ras, numPerPatchMap = rasPatches)
#' rasAgents[is.na(rasAgents)] <- 0
#' if (interactive()) {
#'   clearPlot()
#'   Plot(rasAgents)
#' }
specificNumPerPatch <- function(patches, numPerPatchTable = NULL, numPerPatchMap = NULL) {
  patchids <- as.numeric(na.omit(getValues(patches)))
  wh <- Which(patches, cells = TRUE)
  if (!is.null(numPerPatchTable)) {
    dt1 <- data.table(wh, pops = patchids)
    setkeyv(dt1, "pops")
    if (is(numPerPatchTable, "data.table")) {
      numPerPatchTable <- data.table(numPerPatchTable)
    }
    setkeyv(numPerPatchTable, "pops")
    dt2 <- dt1[numPerPatchTable]
  } else if (!is.null(numPerPatchMap)) {
    numPerPatchTable <- as.numeric(na.omit(getValues(numPerPatchMap)))
    dt2 <- data.table(wh, pops = patchids, num.in.pop = numPerPatchTable)
  } else {
    stop("need numPerPatchMap or numPerPatchTable")
  }

  dt3 <- dt2[, list(cells = resample(wh, unique(num.in.pop))), by = "pops"]
  dt3$ids <- rownames(dt3)

  al <- raster(extent(patches), res = res(patches), vals = 0)
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
# setMethod("initialize", "mobileAgent", function(.Object, ...,
#           agentlocation = NULL, numagents = NULL, probinit = NULL) {
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
#       if (length(grep(pattern = "Raster",class(agentlocation))) == 1) {
#         position <- xyFromCell(agentlocation,fI2,spatial = TRUE)
#       } else if (length(grep(pattern = "SpatialPoints",class(agentlocation))) == 1) {
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
#           xy = matrix(runif(numagents*2, c(xmin(ext), ymin(ext)), c(xmax(ext), ymax(ext))), ncol = 2, byrow = TRUE)
#           colnames(xy) = c("x", "y")
#           position <- SpatialPoints(xy)
#           #                    position <- SpatialPoints(sampleRandom(agentlocation, numagents, xy = TRUE, sp = TRUE))
#         } else if (is(agentlocation,"SpatialPoints")) {
#           sam <- sample(1:length(agentlocation),numagents)
#           position <- SpatialPoints(agentlocation[sam,])
#         } else {
#           stop("need raster layer or Spatial Points object")
#         }
#       } else { # for numagents also NULL
#         if (length(grep(pattern = "Raster",class(agentlocation))) == 1) {
#           position <- SpatialPoints(xyFromCell(agentlocation, Which(agentlocation, cells = TRUE)))
#         } else if (length(grep(pattern = "SpatialPoints", class(agentlocation))) == 1) {
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
