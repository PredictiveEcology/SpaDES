if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("angles", "indices", "x", "y", "rasterVal"))
}

##############################################################
#' Fast `adjacent` function, and Just In Time compiled version
#'
#' Faster function for determining the cells of the 4, 8 or bishop
#'  neighbours of the \code{cells}. This is a hybrid function that uses
#'  matrix for small numbers of loci (<1e4) and data.table for larger numbers of loci
#'
#' Between 4x (large number loci) to 200x (small number loci) speed gains over
#' \code{adjacent} in raster package. There is some extra speed gain if
#' \code{NumCol} and \code{NumCells} are passed rather than a raster.
#' Efficiency gains come from:
#'  1. use \code{data.table} internally
#'     - no need to remove NAs because wrapped or outside points are
#'       just removed directly with data.table
#'     - use data.table to sort and fast select (though not fastest possible)
#'  2. don't make intermediate objects; just put calculation into return statement
#'
#' The steps used in the algorithm are:
#' 1. Calculate indices of neighbouring cells
#' 2. Remove "to" cells that are
#'    - <1 or >numCells (i.e., they are above or below raster), using a single modulo calculation
#'    - where the modulo of "to" cells is equal to 1 if "from" cells are 0 (wrapped right to left)
#'    - or where the modulo of the "to" cells is equal to 0 if "from" cells are 1 (wrapped left to right)
#'
#' @param x Raster* object for which adjacency will be calculated.
#'
#' @param cells vector of cell numbers for which adjacent cells should be found. Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom
#'
#' @param directions the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), or 'bishop' to connect cells with one-cell diagonal moves. Or a neigborhood matrix (see Details)
#'
#' @param sort logical. Whether the outputs should be sorted or not, using Cell IDs of the
#'  from cells (and to cells, if \code{match.adjacent} is TRUE.
#'
#' @param pairs logical. If TRUE, a matrix of pairs of adjacent cells is returned. If FALSE, a vector of cells adjacent to cells is returned
#'
#' @param include logical. Should the focal cells be included in the result?
#'
#' @param target a vector of cells that can be spread to. This is the inverse of a mask.
#'
#' @param numCol numeric indicating number of columns in the raster. Using this with numCell is a bit faster execution time.
#'
#' @param numCell numeric indicating number of cells in the raster. Using this with numCol is a bit faster execution time.
#'
#' @param match.adjacent logical. Should the returned object be the same as the \code{adjacent}
#'          function in the raster package.
#' @param cutoff.for.data.table numeric. Above this value, the function uses data.table which is
#' faster with large numbers of cells.
#'
#' @param torus Logical. Should the spread event wrap around to the other side of the raster.
#' Default is FALSE.
#'
#' @param id numeric If not NULL, then function will return "id" column. Default NULL.
#'
#' @return a matrix of one or two columns, from and to.
#'
#' @seealso \code{\link[raster]{adjacent}}
#'
#' @importFrom data.table data.table key setcolorder setkey ':='
#' @importFrom raster ncell ncol nrow
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @rdname adj
#'
#' @author Eliot McIntire
#'
#' @examples
#' library(raster)
#' a <- raster(extent(0, 1000, 0, 1000), res = 1)
#' sam <- sample(1:length(a), 1e4)
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8,
#'   include = TRUE)
#' if (interactive()) print(head(adj.new))
#'
adj.raw <- function(x = NULL, cells, directions = 8, sort = FALSE, pairs = TRUE,
                    include = FALSE, target = NULL, numCol = NULL, numCell = NULL,
                    match.adjacent = FALSE, cutoff.for.data.table = 1e4,
                    torus = FALSE, id = NULL) {
  to = NULL
  J = NULL
  if ((length(cells)<cutoff.for.data.table)) {
    if (is.null(numCol) | is.null(numCell)) {
      if (is.null(x)) stop("must provide either numCol & numCell or a x")
      numCol = as.integer(ncol(x))
      numCell = as.integer(ncell(x))
    }

    if (directions == 8) {
      # determine the indices of the 8 surrounding cells of the cells cells
      topl <- as.integer(cells-numCol-1)
      top <- as.integer(cells-numCol)
      topr <- as.integer(cells-numCol+1)
      lef <- as.integer(cells-1)
      rig <- as.integer(cells+1)
      botl <- as.integer(cells+numCol-1)
      bot <- as.integer(cells+numCol)
      botr <- as.integer(cells+numCol+1)

      if (match.adjacent){
        if (include){
          adj <- cbind(from = rep.int(cells,times = 9),
                       to = c(as.integer(cells), topl, lef, botl,
                              topr, rig, botr, top, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 9))
        } else {
          adj = cbind(from = rep.int(cells, times = 8),
                    to = c(topl, lef, botl, topr, rig, botr, top, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 8))
        }
      } else {
        if (include){
          adj = cbind(from = rep.int(cells, times = 9),
                    to = c(topl, top, topr, lef, as.integer(cells), rig, botl, bot, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 9))
        }else{
          adj = cbind(from = rep.int(cells, times = 8),
                    to = c(topl, top, topr, lef, rig, botl, bot, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 8))
        }
      }
    } else if (directions == 4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top <- as.integer(cells-numCol)
      lef <- as.integer(cells-1)
      rig <- as.integer(cells+1)
      bot <- as.integer(cells+numCol)
      if (match.adjacent){
        if (include) {
          adj <- cbind(from = rep.int(cells, times = 5),
                       to = c(as.integer(cells), lef, rig, top, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 5))
        } else {
          adj <- cbind(from = rep.int(cells, times = 4),
                       to = c(lef, rig, top, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 4))
        }
      } else {
        if (include) {
          adj <- cbind(from = rep.int(cells, times = 5),
                       to = c(top, lef, as.integer(cells), rig, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 5))
        } else {
          adj <- cbind(from = rep.int(cells, times = 4),
                       to = c(top, lef, rig, bot))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 4))
        }
      }
    } else if (directions == "bishop") {
      topl <- as.integer(cells-numCol-1)
      topr <- as.integer(cells-numCol+1)
      botl <- as.integer(cells+numCol-1)
      botr <- as.integer(cells+numCol+1)
      if (match.adjacent) {
        if (include) {
          adj <- cbind(from = rep.int(cells, times = 5),
                       to = c(as.integer(cells), topl, botl, topr, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 5))
        } else {
          adj <- cbind(from = rep.int(cells, times = 4),
                       to = c(topl, botl, topr, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 4))
        }
      } else {
        if (include) {
          adj <- cbind(from = rep.int(cells, times = 5),
                       to = c(topl, topr, as.integer(cells), botl, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 5))
        } else {
          adj  <- cbind(from = rep.int(cells, times = 4),
                        to = c(topl, topr, botl, botr))
          if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = 4))
        }
      }
    } else {
      stop("directions must be 4 or 8 or \'bishop\'")
    }

    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      adj <- adj[na.omit(adj[, "to"] %in% target),]
    }

    if (sort){
      if (match.adjacent) {
        adj <- adj[order(adj[, "from"], adj[, "to"]), ]
      } else {
        adj <- adj[order(adj[, "from"]),]
      }
    }

    # Remove the "from" column if pairs is FALSE
    # Good time savings if no intermediate object is created
    if (!torus) {
      if (pairs) {
        return(adj[
          !((((adj[, "to"]-1)%%numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"]%%numCol + adj[, "to"]%%numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          ,, drop = FALSE])
      } else {
        return(adj[
          !((((adj[, "to"]-1)%%numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"]%%numCol + adj[, "to"]%%numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          , 2, drop = FALSE])
      }
    } else {
      whLefRig <- (adj[, "from"]%%numCol+adj[, "to"]%%numCol) == 1
      adj[whLefRig, "to"] <- adj[whLefRig, "to"]+numCol*(adj[whLefRig, "from"]-adj[whLefRig, "to"])
      whBotTop <- ((adj[, "to"]-1)%%numCell+1) != adj[, "to"]
      adj[whBotTop, "to"] <- adj[whBotTop, "to"]+sign(adj[whBotTop, "from"]-adj[whBotTop, "to"])*numCell
      if (pairs) {
        return(adj)
      } else {
        return(adj[, 2, drop = FALSE])
      }
    }
  } else {

    #### THIS IS FOR SITUATIONS WHERE length(cells) is > 1e4; using data.table
    if (is.null(numCol) | is.null(numCell)) {
      if (is.null(x)) stop("must provide either numCol & numCell or a x")
      numCol <- as.integer(ncol(x))
      numCell <- as.integer(ncell(x))
    }

    if (directions == 8) {
      # determine the indices of the 8 surrounding cells of the cells cells
      topl <- as.integer(cells-numCol-1)
      top <- as.integer(cells-numCol)
      topr <- as.integer(cells-numCol+1)
      lef <- as.integer(cells-1)
      rig <- as.integer(cells+1)
      botl <- as.integer(cells+numCol-1)
      bot <- as.integer(cells+numCol)
      botr <- as.integer(cells+numCol+1)
      if (match.adjacent) {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 9),
                            to = c(as.integer(cells), topl, lef, botl,
                                   topr, rig, botr, top, bot))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 9)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 8),
                            to = c(topl, lef, botl, topr, rig, botr, top, bot))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 8)]
        }
      } else {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 9),
                            to = c(topl, top, topr, lef, as.integer(cells),
                                   rig, botl, bot, botr),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 9)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 8),
                            to = c(topl, top, topr, lef, rig, botl, bot, botr),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 8)]
        }
      }
    } else if (directions == 4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top <- as.integer(cells-numCol)
      lef <- as.integer(cells-1)
      rig <- as.integer(cells+1)
      bot <- as.integer(cells+numCol)
      if (match.adjacent) {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 5),
                            to = c(as.integer(cells), lef, rig, top, bot))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 5)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 4),
                            to = c(lef, rig, top, bot))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 4)]
        }
      } else {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 5),
                            to = c(top, lef, as.integer(cells), rig, bot),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 5)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 4),
                            to = c(top, lef, rig, bot),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 4)]
        }
      }
    } else if (directions == "bishop") {
      topl <- as.integer(cells-numCol-1)
      topr <- as.integer(cells-numCol+1)
      botl <- as.integer(cells+numCol-1)
      botr <- as.integer(cells+numCol+1)
      if (match.adjacent) {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 5),
                            to = c(as.integer(cells), topl, botl, topr, botr))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 5)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 4),
                            to = c(topl, botl, topr, botr))
          if (!is.null(id)) adj[,id:=rep.int(id, times = 4)]
        }
      } else {
        if (include) {
          adj <- data.table(from = rep.int(cells, times = 5),
                            to = c(topl, topr, as.integer(cells), botl, botr),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 5)]
        } else {
          adj <- data.table(from = rep.int(cells, times = 4),
                            to = c(topl, topr, botl, botr),
                            key = "from")
          if (!is.null(id)) adj[,id:=rep.int(id, times = 4)]
        }
      }
    } else {
      stop("directions must be 4 or 8 or \'bishop\'")
    }

    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      setkey(adj, to)
      adj <- adj[J(target)]
      setkey(adj, from)
      setcolorder(adj, c("from", "to"))
    }

    # Remove the "from" column if pairs is FALSE
    if (!pairs) {
      from <- as.integer(adj$from)
      adj[, from:=NULL]
    }

    if (!torus) {
      return(as.matrix(adj[
        !((((to-1)%%numCell+1) != to) |  #top or bottom of raster
                ((from%%numCol+to%%numCol) == 1))# | #right & left edge cells, with neighbours wrapped
        ]))
    } else {
      whLefRig <- (from%%numCol + adj[, to]%%numCol) == 1
      adj[whLefRig, to:=to+numCol*(from[whLefRig]-to)]
      whBotTop <- ((adj[, to]-1)%%numCell+1) != adj[, to]
      adj[whBotTop, to:=to+as.integer(sign(from[whBotTop]-to)*numCell)]
      return(as.matrix(adj))
    }
  }
}

#' @importFrom compiler cmpfun
#' @docType methods
#' @export
#' @rdname adj
adj <- compiler::cmpfun(adj.raw)

##############################################################
#' Identify pixels in a circle or ring (donut) around an object.
#'
#' Identify the pixels and coordinates that are at
#'  a (set of) buffer distance(s) of the objects passed into \code{coords}. This is similar
#'  to \code\link[rgeos]{gBuffer} but much faster and without the georeferencing information.
#'  In other words, it can be used for similar problems, but where speed is important.
#'
#' @param landscape    Raster on which the circles are built.
#'
#' @param coords Either a matrix with 2 columns, x and y, representing the coordinates
#'               or a SpatialPoints* object around which to make circles. Must be same
#'               coordinate system as the \code{landscape} argument.
#'
#' @param loci   Numeric. An alternative to \code{coords}. These are the indices on
#'               \code{landscape} to initiate this function. See \code{coords}. Default
#'               NA meaning "use \code{coords}".
#'
#' @param maxRadius  Numeric vector of length 1 or same length as coords
#'
#' @param minRadius  Numeric vector of length 1 or same length as \code{coords}. Default is
#'                   \code{maxRadius}, meaning return all cells that are touched
#'                   by the narrow ring at that exact radius. If smaller than \code{maxRadius},
#'                   then this will create a buffer or donut or ring.
#'
#' @param allowOverlap Logical. Should duplicates across eventID be removed or kept. Default TRUE.
#'
#' @param includeBehavior Character string. Currently accepts only "includePixels", the default,
#'                        and "excludePixels". See details.
#'
#' @param returnDistances Logical. If TRUE, then a column will be added to the returned
#'                        data.table that reports the distance from \code{coords} to every
#'                        point that was in the circle/donut surrounding \code{coords}. Default
#'                        FALSE, which is faster.
#'
#' @param returnAngles Logical. If TRUE, then a column will be added to the returned
#'                        data.table that reports the angle from \code{coords} to every
#'                        point that was in the circle/donut surrounding \code{coords}. Default
#'                        FALSE.
#'
#' @param simplify logical. If TRUE, then all duplicate pixels are removed. This means
#' that some x, y combinations will disappear.
#'
#' @details This function identifies all the pixels as defined by a donut
#' with inner radius minRadius and outer radius of maxRadius. The includeBehavior defines
#' whether the cells that intersect the radii are included or not in the returned
#' pixels identified. If this is excludeBehavior, and if a minRadius and maxRadius
#' are equal, this will return no pixels.
#'
#'
#' @return A \code{matrix} with 4 columns, \code{eventID}, \code{indices},
#' \code{x}, \code{y}. The \code{x} and \code{y} indicate the
#' exact coordinates of
#' the \code{indices} (i.e., cell number) of the \code{landscape}
#' associated with the ring or circle being identified by this function.
#'
#' @import igraph
#' @importFrom data.table data.table set setkey ':='
#' @importFrom sp coordinates
#' @importFrom raster cellFromXY extract res
#' @export
#' @rdname cir
#' @seealso \code{\link{rings}} which uses \code{spread} under internally, but it
#' is generally slower. One difference
#' between the two functions is that \code{rings} takes the centre of the pixel
#' as the centre of a circle, whereas \code{cir} takes the exact coordinates.
#' See example.
#' \code\link[rgeos]{gBuffer}
#'
#'@examples
#' library(raster)
#' library(data.table)
#' library(sp)
#'
#' Ras <- raster(extent(0, 15, 0, 15), res = 1)
#' Ras <- randomPolygons(Ras, numTypes = 4)
#' N <- 2
#' caribou <- SpatialPoints(coords = cbind(x = stats::runif(N, xmin(Ras), xmax(Ras)),
#'                                         y = stats::runif(N, xmin(Ras), xmax(Ras))))
#' cirs <- cir(Ras, caribou, 3, simplify = TRUE)
#' cirsSP <- SpatialPoints(coords = cirs[, c("x", "y")])
#' cirsRas <- raster(Ras)
#' cirsRas[] <- 0
#' cirsRas[cirs[, "indices"]] <- 1
#' Plot(Ras, new = TRUE)
#' Plot(cirsRas, addTo = "Ras", cols = c("transparent", "#00000055"))
#' Plot(caribou, addTo = "Ras")
#' Plot(cirsSP, addTo = "Ras")
#'
#' # Example comparing rings and cir
#' a <- raster(extent(0,30,0,30), res = 1)
#' hab <- gaussMap(a,speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
#' radius <- 4
#' N = 2
#'
#' caribou <- SpatialPoints(coords = cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
#'                                         y = stats::runif(N, xmin(hab), xmax(hab))))
#'
#' # cirs
#' cirs <- cir(hab, caribou, rep(radius, length(caribou)), simplify = TRUE)
#'
#' # rings
#' loci <- cellFromXY(hab, coordinates(caribou))
#' cirs2 <- rings(hab, loci, maxRadius = radius, minRadius=radius-1)
#'
#' # Plot both
#' ras1 <- raster(hab)
#' ras1[] <- 0
#' ras1[cirs[,"indices"]] <- cirs[,"eventID"]
#' Plot(ras1, new=TRUE)
#'
#' ras2 <- raster(hab)
#' ras2[] <- 0
#' ras2[cirs2$indices] <- cirs2$eventID
#' Plot(ras2)
#'
#' a <- raster(extent(0,100,0,100), res = 1)
#' hab <- gaussMap(a,speedup = 1)
#' cirs <- cir(hab, caribou, maxRadius = 44, minRadius = 0)
#' ras1 <- raster(hab)
#' ras1[] <- 0
#' cirsOverlap <- data.table(cirs)[,list(sumIDs = sum(eventID)),by=indices]
#' ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
#' Plot(ras1, new=TRUE)
#'
#' # using loci instead of coords
#' cirs <- cir(hab, loci = 4550, maxRadius = 14, minRadius = 0)
#' ras1[] <- 0
#' ras1[cirs[,"indices"]] <- cirs[,"eventID"]
#' Plot(ras1, new=TRUE)
#'
setGeneric("cir", function(landscape, coords, loci, maxRadius, minRadius = maxRadius,
                           allowOverlap = TRUE,
                           includeBehavior = "includePixels", returnDistances = FALSE,
                           returnAngles = FALSE,
                           simplify = TRUE) {
  standardGeneric("cir")
})

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "SpatialPoints"),
  definition = function(landscape, coords, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        includeBehavior, returnDistances, returnAngles,
                        simplify) {
    coords <- coordinates(coords)
    cir(landscape, coords, loci, maxRadius, minRadius, allowOverlap, includeBehavior,
        returnDistances, returnAngles, simplify)
    })

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "ANY", loci = "numeric"),
  definition = function(landscape, coords, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        includeBehavior, returnDistances, returnAngles,
                        simplify) {
    coords <- xyFromCell(landscape, loci)
    cir(landscape, coords=coords, loci=NULL, maxRadius, minRadius, allowOverlap, includeBehavior,
        returnDistances, returnAngles, simplify)
  })

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "matrix"),
  definition = function(landscape, coords, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        includeBehavior, returnDistances,
                        returnAngles, simplify) {
    scaleRaster <- res(landscape)
  if(scaleRaster[1] != scaleRaster[2]) stop("cir function only accepts rasters with identical ",
                                            "resolution in x and y dimensions")

  if(!any(includeBehavior == c("includePixels", "excludePixels")))
    stop("includeBehavior can only be \"includePixels\" or \"excludePixels\"")

  scaleRaster <- scaleRaster[1]


  moreThanOne <- NROW(coords)>1
  if(moreThanOne) {
    # create an index sequence for the number of individuals
    seqNumInd <- seq_len(NROW(coords))

    # n = optimum number of points to create the circle for a given individual;
    #       gross estimation (checked that it seems to be enough so that pixels
    #       extracted are almost always duplicated, which means there is small
    #       chance that we missed some on the circle).
    if(length(maxRadius)==1) maxRadius <- rep(maxRadius, NROW(coords))
    if(length(minRadius)==1) minRadius <- rep(minRadius, NROW(coords))

    # The goal of maxRadius and numAngles is to identify every cell within the circle
    #  The 0.68 and 0.75 were found by trial and error to minimize the number of
    #  pixels selected that are duplicates of each other.
    if(any((minRadius != maxRadius))) {
      if(any(minRadius > maxRadius)) stop("minRadius must be less than or equal to maxRadius")
      maxRadius <- do.call(cbind, lapply(seqNumInd, function(x) {
                                                           ## 0.75 was the maximum that worked with 1e4 pixels, 1e2 maxRadius
        a <- seq(minRadius[x], maxRadius[x], by = max(0.68,0.75-maxRadius[x]/3e3)) ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
        if(a[length(a)]!=maxRadius[x]) a <- c(a, maxRadius[x])
        a
      }))
    }
  } else {
    seqNumInd <- 1
    if(any((minRadius != maxRadius))) {
      a <- seq(minRadius, maxRadius, by = max(0.68,0.75-maxRadius/3e3)) ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
      if(a[length(a)]!=maxRadius) a <- c(a, maxRadius)
      maxRadius <- a


    }
  }

  numAngles <- ( ceiling((maxRadius/scaleRaster)*2.6*pi) + 1 )

  if(moreThanOne) {
    if(is.matrix(numAngles)) {
      n.angles <- apply(numAngles, 2, sum)
    } else {
      n.angles <- numAngles
    }
  } else {
    n.angles <- sum(numAngles)
  }


  ### Eliot's code to replace the createCircle of the package PlotRegionHighlighter
  #coords <- coordinates(spatialPoints)

  # create individual IDs for the number of points that will be done for their circle
  eventID <- rep.int(seqNumInd, times = n.angles)

  # create vector of radius for the number of points that will be done for each individual circle
  rads <- rep.int(maxRadius, times = numAngles)

  # extract the individuals' current coords
  xs <- rep.int(coords[, 1], times = n.angles)
  ys <- rep.int(coords[, 2], times = n.angles)

  # calculate the angle increment that each individual needs to do to complete a circle (2 pi)
  angle.inc <- rep.int(2*pi, length(numAngles)) / numAngles

  # repeat this angle increment the number of times it needs to be done to complete the circles
  angs <- rep.int(angle.inc, times = numAngles)
  angles <- unlist(lapply(angle.inc, function(ai) seq(ai, pi*2, by=ai)))

  x <- cos(angles)*rads + xs
  y <- sin(angles)*rads + ys
  indices <- cellFromXY(landscape, cbind(x,y))

  if(moreThanOne) {
    DT <- data.table(eventID, indices, rads, angles, x, y)

    if(allowOverlap)
        setkey(DT, "eventID", "indices")
      else
        setkey(DT, "indices")
    DT <- unique(DT)
    DT <- na.omit(DT)
    MAT <- as.matrix(DT)

  } else {
    notDups <- !duplicated(indices)
    MAT <- cbind(eventID=1, rads, angles, x, y, indices)
    MAT <- MAT[notDups,,drop=FALSE]
    MAT <- na.omit(MAT)
  }

  if(includeBehavior=="excludePixels" | returnDistances ) {
    maxRad <- maxRadius[NROW(maxRadius)]
    minRad <- maxRadius[1]
    if(returnDistances) {
      MAT2 <- MAT
    } else {
      MAT2 <- MAT[MAT[,"rads"]>=(maxRad-0.71) | MAT[,"rads"]<=(minRad+0.71),] # 0.71 is the sqrt of 1, so get
    }
    xyC <- xyFromCell(landscape, MAT2[,"indices"]);
    a <- cbind(mapID=MAT2[,"eventID"], rads=MAT2[,"rads"], angles=MAT2[,"angles"], x=xyC[,"x"],
               y = xyC[,"y"], to=MAT2[,"indices"])

    b <- cbind(coords, mapID=1:NROW(coords))

    colnames(b)[1:2] <- c("x","y")
    d <- .matchedPointDistance(a,b)

    if(includeBehavior=="excludePixels")
      d <- d[d[, "dists"] %<=% maxRad & d[, "dists"] %>=% minRad,,drop=FALSE]

    colnames(d)[c(2,7)] <- c("eventID", "indices")
    if(!returnDistances)
      d <- d[,-which(colnames(d)=="dists")]

    if(!returnAngles) {
      d <- d[,-which(colnames(d)=="angles")]
      MAT <- MAT[,-which(colnames(MAT)=="angles")]
    }
    if(!returnDistances) {
      MATinterior <- MAT[MAT[,"rads"]<(maxRad-0.71) & MAT[,"rads"]>(minRad+0.71),,drop=FALSE]
      MAT <- rbind(d[,colnames(MATinterior),drop=FALSE], MATinterior)
      MAT <- MAT[,-which(colnames(MAT)=="rads"),drop=FALSE]
    } else {
      MAT <- d[,c("eventID","indices","dists", "x","y"),drop=FALSE]
    }

  } else {
    if(!returnAngles) {
      MAT <- MAT[,-which(colnames(MAT)=="angles")]
    }
    MAT <- MAT[,-which(colnames(MAT)=="rads"),drop=FALSE]
  }

  return(MAT)
}
)

###############################################################################
#' Wrap coordinates or pixels in a torus-like fashion
#'
#' Generally for model development purposes.
#'
#' If \code{withHeading} used, then X must be a \code{SpatialPointsDataFrame}
#' that contains two columns, x1 and y1, with the immediately previous agent
#' locations.
#'
#' @param X A SpatialPoints* object, or matrix of coordinates
#'
#' @param bounds Either a Raster*, Extent, or bbox object defining bounds to wrap around
#'
#' @param withHeading logical. If TRUE, then the previous points must be wrapped also
#' so that the subsequent heading calculation will work. Default FALSE. See details.
#'
#' @return Same class as X, but with coordinates updated to reflect the wrapping
#'
#' @export
#' @docType methods
#' @rdname wrap
#'
#' @author Eliot McIntire
#' @examples
#' library(raster)
#' xrange <- yrange <- c(-50, 50)
#' hab <- raster(extent(c(xrange, yrange)))
#' hab[] <- 0
#'
#' # initialize caribou agents
#' N <- 10
#'
#' # previous points
#' x1 <- rep(0, N)
#' y1 <- rep(0, N)
#' # initial points
#' starts <- cbind(x = stats::runif(N, xrange[1], xrange[2]),
#'                 y = stats::runif(N, yrange[1], yrange[2]))
#'
#' # create the caribou agent object
#' caribou <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
#'
#'
#' ln <- rlnorm(N, 1, 0.02) # log normal step length
#' sd <- 30 # could be specified globally in params
#'
#' Plot(hab, zero.color = "white", new = TRUE, axes = "L")
#' for(i in 1:10) {
#'   caribou <- SpaDES::crw(agent = caribou,
#'                          extent = extent(hab), stepLength = ln,
#'                          stddev = sd, lonlat = FALSE, torus = TRUE)
#'   Plot(caribou, addTo = "hab", axes = TRUE)
#' }
setGeneric("wrap", function(X, bounds, withHeading) {
  standardGeneric("wrap")
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Extent", withHeading = "missing"),
  definition = function(X, bounds) {
    if (identical(colnames(X), c("x", "y"))) {
      return(cbind(
        x = (X[, "x"] - bounds@xmin) %% (bounds@xmax - bounds@xmin) + bounds@xmin,
        y = (X[, "y"] - bounds@ymin) %% (bounds@ymax - bounds@ymin) + bounds@ymin
      ))
    } else {
      stop("When X is a matrix, it must have 2 columns, x and y,",
           "as from say, coordinates(SpatialPointsObj)")
    }
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPoints", bounds = "ANY", withHeading = "missing"),
  definition = function(X, bounds) {
    X@coords <- wrap(X@coords, bounds = bounds)
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
  definition = function(X, bounds) {
    X <- wrap(X, bounds = extent(bounds))
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
  definition = function(X, bounds) {
    X <- wrap(X, bounds = extent(bounds))
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "matrix", withHeading = "missing"),
  definition = function(X, bounds) {
    if (identical(colnames(bounds), c("min", "max")) &
         (identical(rownames(bounds), c("s1", "s2")))) {
      X <- wrap(X, bounds = extent(bounds))
      return(X)
    } else {
      stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
    }
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "Extent", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
    if (withHeading) {
      # This requires that previous points be "moved" as if they are
      #  off the bounds, so that the heading is correct
      X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] <-
        (X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] - bounds@xmin) %%
        (bounds@xmax - bounds@xmin) + bounds@xmax
      X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] <-
        (X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] - bounds@xmax) %%
        (bounds@xmin - bounds@xmax) + bounds@xmin
      X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] <-
        (X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] - bounds@ymin) %%
        (bounds@ymax - bounds@ymin) + bounds@ymax
      X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] <-
        (X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] - bounds@ymax) %%
        (bounds@ymin - bounds@ymax) + bounds@ymin
    }
    return(wrap(X, bounds = bounds))
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "Raster", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
      X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
      return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "matrix", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
    if ( identical(colnames(bounds), c("min", "max")) &
         identical(rownames(bounds), c("s1", "s2"))) {
      X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
      return(X)
    } else {
      stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
    }
})
