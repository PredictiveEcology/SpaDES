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
#'    - <1 or >numCells (i.e., they are above or below raster), using a single modulo
#'      calculation
#'    - where the modulo of "to" cells is equal to 1 if "from" cells are 0 (wrapped right
#'      to left)
#'    - or where the modulo of the "to" cells is equal to 0 if "from" cells are 1 (wrapped
#'      left to right)
#'
#' @param x Raster* object for which adjacency will be calculated.
#'
#' @param cells vector of cell numbers for which adjacent cells should be found. Cell
#'              numbers start with 1 in the upper-left corner and increase from left
#'              to right and from top to bottom
#'
#' @param directions the number of directions in which cells should be connected: 4
#'                   (rook's case), 8 (queen's case), or 'bishop' to connect cells
#'                   with one-cell diagonal moves. Or a neigborhood matrix (see Details)
#'
#' @param sort logical. Whether the outputs should be sorted or not, using Cell IDs of the
#'             from cells (and to cells, if \code{match.adjacent} is TRUE.
#'
#' @param pairs logical. If TRUE, a matrix of pairs of adjacent cells is returned.
#'              If FALSE, a vector of cells adjacent to cells is returned
#'
#' @param include logical. Should the focal cells be included in the result?
#'
#' @param target a vector of cells that can be spread to. This is the inverse of a mask.
#'
#' @param numCol numeric indicating number of columns in the raster. Using this with
#'               numCell is a bit faster execution time.
#'
#' @param numCell numeric indicating number of cells in the raster. Using this
#'                with numCol is a bit faster execution time.
#'
#' @param match.adjacent logical. Should the returned object be the same as the \code{adjacent}
#'                       function in the raster package.
#'
#' @param cutoff.for.data.table numeric. If the number of cells is above this value,
#'                              the function uses data.table which is
#'                              faster with large numbers of cells.
#'
#' @param torus Logical. Should the spread event wrap around to the other side of the raster.
#' Default is FALSE.
#'
#' @param id numeric If not NULL, then function will return "id" column. Default NULL.
#'
#' @param numNeighs A numeric scalar, indicating how many neighbours to return. Must be
#'                  less than or equal to \code{directions}; which neighbours are random
#'                  with equal probabilities.
#'
#' @return a matrix of one or two columns, from and to.
#'
#' @seealso \code{\link[raster]{adjacent}}
#'
#' @importFrom data.table data.table key set setcolorder setkeyv ':='
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
#'
adj.raw <- function(x = NULL, cells, directions = 8, sort = FALSE, pairs = TRUE,
                    include = FALSE, target = NULL, numCol = NULL, numCell = NULL,
                    match.adjacent = FALSE, cutoff.for.data.table = 1e4,
                    torus = FALSE, id = NULL, numNeighs = NULL) {
  to = NULL
  J = NULL
  cells <- as.integer(cells)

  if (is.null(numCol) | is.null(numCell)) {
    if (is.null(x)) stop("must provide either numCol & numCell or a x")
    numCol <- as.integer(ncol(x))
    numCell <- as.integer(ncell(x))
  }

  if (directions == "bishop")  {
    dirs <- 4
    needCorners <- TRUE
  } else {
    needCorners <- if (directions == 8) TRUE else FALSE
    dirs <- directions
  }

  numToCells <- dirs + include
  fromCells <- rep.int(cells, times = numToCells)

  if (is.numeric(directions)) {
    top <- cells - numCol
    lef <- cells - 1L
    rig <- cells + 1L
    bot <- cells + numCol
  }
  if (needCorners) {
    topl <- cells - numCol - 1L
    topr <- cells - numCol + 1L
    botl <- cells + numCol - 1L
    botr <- cells + numCol + 1L
  }

  toCells <- if (directions == 8) {

    if (match.adjacent)
      if (include)
        c(cells, topl, lef, botl, topr, rig, botr, top, bot)
      else
        c(topl, lef, botl, topr, rig, botr, top, bot)
    else
      if (include)
        c(topl, top, topr, lef, cells, rig, botl, bot, botr)
      else
        c(topl, top, topr, lef, rig, botl, bot, botr)
  } else if (directions == 4) {
    if (match.adjacent)
      if (include)
        c(cells, lef, rig, top, bot)
      else
        c(lef, rig, top, bot)
    else
      if (include)
        c(top, lef, cells, rig, bot)
      else
        c(top, lef, rig, bot)
  } else if (directions == "bishop") {
    if (match.adjacent)
      if (include)
        c(cells, topl, botl, topr, botr)
      else
        c(topl, botl, topr, botr)
    else
      if (include)
        c(topl, topr, cells, botl, botr)
      else
        c(topl, topr, botl, botr)
  } else {
    stop("directions must be 4 or 8 or \'bishop\'")
  }

  if (!is.null(numNeighs)) {
    lenCells <- length(cells)
    if (length(numNeighs) == 1) numNeighs <- rep(numNeighs, lenCells)
    ind <- unlist(sampleV(1:(directions + include), size = numNeighs))
    minusVal <- lenCells - rep.int(seq_along(cells), numNeighs)
    indFull2 <- ind * lenCells - minusVal

    toCells <- toCells[indFull2]
    fromCells <- fromCells[indFull2]
  }

  useMatrix <- (length(cells) < cutoff.for.data.table)
  if (useMatrix) {
    adj <- cbind(from = fromCells, to = toCells)
    if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = numToCells))
  } else {
    adj <- data.table(from = fromCells, to = toCells)
    if (!is.null(id)) set(adj, , "id", rep.int(id, times = numToCells))
  }

  if (useMatrix) {
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      adj <- adj[na.omit(adj[, "to"] %in% target), , drop = FALSE]
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          adj <- adj[order(adj[, "from"], adj[, "to"]), , drop = FALSE]
        } else {
          adj <- adj[order(adj[, "from"]), , drop = FALSE]
        }
      } else {
        adj <- adj[order(adj[, "to"]), , drop = FALSE]
      }
    }

    # Remove the "from" column if pairs is FALSE
    # Good time savings if no intermediate object is created
    keepCols <- if (is.null(id)) "to" else c("to", "id")
    if (!torus) {
      if (pairs) {
        return(adj[
          !((((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          , , drop = FALSE])
      } else {
        adj <- adj[
          !((((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          , keepCols, drop = FALSE]
        if (match.adjacent) {
          adj <- unique(adj[, "to"])
        }
        return(adj)
      }
    } else {
      whLefRig <- (adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1
      adj[whLefRig, "to"] <- adj[whLefRig, "to"] + numCol*(adj[whLefRig, "from"] - adj[whLefRig, "to"])
      whBotTop <- ((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]
      adj[whBotTop, "to"] <- adj[whBotTop, "to"] + sign(adj[whBotTop, "from"] - adj[whBotTop, "to"]) * numCell
      if (pairs) {
        return(adj)
      } else {
        if (match.adjacent) {
          adj <- unique(adj[, "to", drop = TRUE])
        } else {
          adj <- adj[, keepCols, drop = FALSE]
        }
        return(adj)
      }
    }
  } else {
    ## use data.table
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      set(adj, , "ord", seq_len(NROW(adj)))
      setkeyv(adj, "to")
      adj <- adj[J(target)]
      adj <- na.omit(adj)
      setkeyv(adj, "ord")
      set(adj, , "ord", NULL)
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          setkeyv(adj, c("from", "to"))
        } else {
          setkeyv(adj, "from")
        }
      } else {
        setkeyv(adj, "to")
      }
    }

    # Remove the "from" column if pairs is FALSE
    if (!pairs) {
      from <- as.integer(adj$from)
      set(adj, , "from", NULL)
    }

    if (!torus) {
      if (!pairs) {
        adj <- adj[
          !((((to - 1) %% numCell + 1) != to) |  #top or bottom of raster
              ((from %% numCol + to %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          ]
        if (match.adjacent) {
          return(unique(adj$to))
        }
        return(as.matrix(adj))
      } else {
        return(as.matrix(adj[
          !((((to - 1) %% numCell + 1) != to) | #top or bottom of raster
              ((from %% numCol + to %% numCol) == 1)) # | #right & left edge cells, with neighbours wrapped
          ]))
      }
    } else {
      if (!pairs) {
        whLefRig <- (from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol * (from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(from[whBotTop] - toWhBotTop) * numCell))

        if (match.adjacent) {
          adj <- unique(adj$to)
          return(adj)
        }
      } else {
        whLefRig <- (adj$from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol * (adj$from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(adj$from[whBotTop] - toWhBotTop) * numCell))
      }
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
#' Identify the pixels and coordinates that are at a (set of) buffer distance(s)
#' of the objects passed into \code{coords}.
#' This is similar to \code{rgeos::gBuffer} but much faster and without
#' the geo referencing information.
#' In other words, it can be used for similar problems, but where speed is important.
#' This code is substantially adapted from \code{PlotRegionHighlighter::createCircle}.
#'
#' @param landscape    Raster on which the circles are built.
#'
#' @param coords Either a matrix with 2 (or 3) columns, x and y (and id), representing the
#'               coordinates (and an associated id, like cell index),
#'               or a \code{SpatialPoints*} object around which to make circles. Must be same
#'               coordinate system as the \code{landscape} argument. Default is missing,
#'               meaning it uses the default to \code{loci}
#'
#' @param loci   Numeric. An alternative to \code{coords}. These are the indices on
#'               \code{landscape} to initiate this function. See \code{coords}. Default is one
#'               point in centre of \code{landscape}..
#'
#' @param maxRadius  Numeric vector of length 1 or same length as coords
#'
#' @param minRadius  Numeric vector of length 1 or same length as \code{coords}. Default is
#'                   \code{maxRadius}, meaning return all cells that are touched
#'                   by the narrow ring at that exact radius. If smaller than \code{maxRadius},
#'                   then this will create a buffer or donut or ring.
#'
#' @param allowOverlap Logical. Should duplicates across id be removed or kept. Default TRUE.
#'
#' @param allowDuplicates Logical. Should duplicates within id be removed or kept. Default FALSE.
#'                        This is useful if the actual x, y coordinates are desired, rather
#'                        than the cell indices. This will increase the size of the returned
#'                        object.
#'
#' @param includeBehavior Character string. Currently accepts only "includePixels", the default,
#'                        and "excludePixels". See details.
#'
#' @param returnDistances Logical. If TRUE, then a column will be added to the returned
#'                        data.table that reports the distance from \code{coords} to every
#'                        point that was in the circle/donut surrounding \code{coords}. Default
#'                        FALSE, which is faster.
#'
#' @param angles Numeric. Optional vector of angles, in radians, to use. This will create
#'               "spokes" outward from coords. Default is NA, meaning, use internally
#'               derived angles that will "fill" the circle.
#'
#' @param returnAngles Logical. If TRUE, then a column will be added to the returned
#'                        data.table that reports the angle from \code{coords} to every
#'                        point that was in the circle/donut surrounding \code{coords}. Default
#'                        FALSE.
#'
#' @param closest Logical. When determining non-overlapping circles, should the function
#'                give preference to the closest \code{loci} or the first one (much faster).
#'                Default is FALSE, meaning the faster, though maybe not desired behavior.
#'
#' @param simplify logical. If TRUE, then all duplicate pixels are removed. This means
#' that some x, y combinations will disappear.
#'
#' @inheritParams spread
#'
#' @details This function identifies all the pixels as defined by a donut
#' with inner radius minRadius and outer radius of maxRadius. The includeBehavior defines
#' whether the cells that intersect the radii but whose centres are not inside
#' the donut are included \code{includePixels} or not \code{excludePixels} in the returned
#' pixels identified. If this is \code{excludePixels}, and if a \code{minRadius} and
#' \code{maxRadius} are equal, this will return no pixels.
#'
#'
#' @return A \code{matrix} with 4 columns, \code{id}, \code{indices},
#' \code{x}, \code{y}. The \code{x} and \code{y} indicate the
#' exact coordinates of
#' the \code{indices} (i.e., cell number) of the \code{landscape}
#' associated with the ring or circle being identified by this function.
#'
#' @import igraph
#' @importFrom data.table data.table set setkeyv
#' @importFrom sp coordinates
#' @importFrom fpCompare %==%
#' @importFrom raster cellFromXY extract res xyFromCell ncell ncol
#' @export
#' @rdname cir
#' @seealso \code{\link{rings}} which uses \code{spread} internally.
#' \code{cir} tends to be faster when there are few starting points, \code{rings}
#' tends to be faster when there are many starting points. \code{cir} scales with
#' \code{maxRadius} ^ 2 and \code{coords}. Another difference
#' between the two functions is that \code{rings} takes the centre of the pixel
#' as the centre of a circle, whereas \code{cir} takes the exact coordinates.
#' See example. For the specific case of creating distance surfaces from specific
#' points, see \code{\link{distanceFromEachPoint}}, which is often faster.
#' For the more general GIS buffering, see \code{rgeos::gBuffer}.
#'
#'@example inst/examples/example_cir.R
#'
setGeneric("cir", function(landscape, coords, loci,
                           maxRadius = ncol(landscape) / 4, minRadius = maxRadius,
                           allowOverlap = TRUE, allowDuplicates = FALSE,
                           includeBehavior = "includePixels", returnDistances = FALSE,
                           angles = NA_real_,
                           returnAngles = FALSE, returnIndices = TRUE,
                           closest = FALSE, simplify = TRUE) {
  standardGeneric("cir")
})

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "SpatialPoints", loci = "missing"),
  definition = function(landscape, coords, maxRadius, minRadius = maxRadius, allowOverlap,
                        allowDuplicates, includeBehavior, returnDistances, angles,
                        returnAngles, returnIndices, closest, simplify) {
    coords <- coordinates(coords)

    cir(landscape, coords, maxRadius = maxRadius, minRadius = minRadius,
        allowOverlap = allowOverlap, allowDuplicates = allowDuplicates,
        includeBehavior = includeBehavior,
        returnDistances = returnDistances, angles = angles, returnAngles = returnAngles,
        returnIndices = returnIndices,
        closest = closest, simplify = simplify)
  })

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "missing", loci = "numeric"),
  definition = function(landscape, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        allowDuplicates, includeBehavior, returnDistances,
                        angles, returnAngles, returnIndices,
                        closest, simplify) {
    coords <- xyFromCell(landscape, loci)
    cir(landscape, coords = coords, maxRadius = maxRadius, minRadius = minRadius,
        allowOverlap = allowOverlap, allowDuplicates = allowDuplicates,
        includeBehavior = includeBehavior,
        returnDistances = returnDistances, angles = angles, returnAngles = returnAngles,
        returnIndices = returnIndices, closest = closest, simplify = simplify)
  })

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "missing", loci = "missing"),
  definition = function(landscape, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        allowDuplicates, includeBehavior, returnDistances, angles,
                        returnAngles, returnIndices,
                        closest, simplify) {
    ncells <- ncell(landscape)
    middleCell <- if (identical(ncells / 2, floor(ncells / 2))) {
      ncells / 2 - ncol(landscape) / 2
    } else {
      round(ncells / 2)
    }
    coords <- xyFromCell(landscape, middleCell)
    cir(landscape, coords = coords, maxRadius = maxRadius, minRadius = minRadius,
        allowOverlap = allowOverlap, allowDuplicates = allowDuplicates,
        includeBehavior = includeBehavior,
        returnDistances = returnDistances, angles = angles, returnAngles = returnAngles,
        returnIndices = returnIndices,
        closest = closest, simplify = simplify)
  })

#' @export
#' @rdname cir
setMethod(
  "cir",
  signature(landscape = "RasterLayer", coords = "matrix", loci = "missing"),
  definition = function(landscape, coords, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        allowDuplicates, includeBehavior, returnDistances, angles,
                        returnAngles, returnIndices, closest, simplify) {
    ### adapted from createCircle of the package PlotRegionHighlighter

    if (!all(c("x", "y") %in% colnames(coords) )) {
      stop("coords must have columns named x and y")
    }
    suppliedAngles <- if (all(!is.na(angles))) TRUE else FALSE

    scaleRaster <- res(landscape)
    if (scaleRaster[1] != scaleRaster[2]) {
      stop("cir function only accepts rasters with identical resolution in x and y dimensions")
    }

    if (!any(includeBehavior == c("includePixels", "excludePixels"))) {
      stop("includeBehavior can only be \"includePixels\" or \"excludePixels\"")
    }


    scaleRaster <- scaleRaster[1]

    moreThanOne <- NROW(coords) > 1

    equalRadii <- TRUE
    if (suppliedAngles) {
      # if provided with angles, then problem is easier
      seqNumInd <- seq_len(NROW(coords))
      maxRadius <- c(seq(minRadius, maxRadius, by = max(0.68, 0.75 - maxRadius / 3e3)), maxRadius)
      numAngles <- length(angles)
      rads <- rep(rep(maxRadius, each = numAngles), NROW(coords))
      x <- kronecker(coords[, "x"], c(cos(angles) %o% maxRadius), "+")
      y <- kronecker(coords[, "y"], c(sin(angles) %o% maxRadius), "+")
      id <- rep(rep(seqNumInd, each = numAngles), each = length(maxRadius))
    } else {
      if (moreThanOne) {
        # create an index sequence for the number of individuals
        seqNumInd <- seq_len(NROW(coords))

        if (length(maxRadius) == 1) maxRadius <- rep(maxRadius, NROW(coords))
        if (length(minRadius) == 1) minRadius <- rep(minRadius, NROW(coords))
        equalRadii <- sum(maxRadius - maxRadius[1]) %==% 0

        # The goal of maxRadius and numAngles is to identify every cell within the circle
        #  The 0.68 and 0.75 were found by trial and error to minimize the number of
        #  pixels selected that are duplicates of each other.
        if (any((minRadius != maxRadius))) {
          if (any(minRadius > maxRadius)) stop("minRadius must be less than or equal to maxRadius")
          maxRadiusList <- lapply(seqNumInd, function(x) {
            ## 0.75 was the maximum that worked with 1e4 pixels, 1e2 maxRadius
            ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
            a <- seq(minRadius[x], maxRadius[x], by = max(0.68, 0.75 - maxRadius[x] / 3e3))
            if (a[length(a)] != maxRadius[x]) a <- c(a, maxRadius[x])
            a
          })

          if (equalRadii) {
            maxRadius <- do.call(cbind, maxRadiusList)
          } else {
            lengths <- unlist(lapply(maxRadiusList, length))
            maxLen <- max(lengths)
            maxRadius <- do.call(cbind, lapply(seq_along(maxRadiusList), function(y) {
              c(maxRadiusList[[y]], rep(NA_real_, maxLen - lengths[y]))
            }))
          }
        }
      } else {
        seqNumInd <- 1
        if (any((minRadius != maxRadius))) {
          ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
          a <- seq(minRadius, maxRadius, by = max(0.68, 0.75 - maxRadius / 3e3))
          if (a[length(a)] != maxRadius) a <- c(a, maxRadius)
          maxRadius <- a
        }
      }

      numAngles <- ceiling((maxRadius / scaleRaster) * 2.6 * pi) + 1

      if (moreThanOne) {
        if (is.matrix(numAngles)) {
          nAngles <- apply(numAngles, 2, sum, na.rm = TRUE)
        } else {
          nAngles <- numAngles
        }
      } else {
        nAngles <- sum(numAngles)
      }

      # create individual IDs for the number of points that will be done for their circle
      if (!c("id") %in% colnames(coords) ) {
        if (moreThanOne) {
          id <- rep.int(seqNumInd, times = nAngles)
        } else {
          id <- 1L
        }
      } else {
        id <- as.integer(rep(coords[, "id"], times = nAngles))
      }

      # create vector of radius for the number of points that will be done for each individual circle
      if (equalRadii)
        rads <- rep.int(maxRadius, times = numAngles)
      else
        rads <- rep.int(na.omit(as.vector(maxRadius)), times = na.omit(as.vector(numAngles)))

      # extract the individuals' current coords
      xs <- rep.int(coords[, "x"], times = nAngles)
      ys <- rep.int(coords[, "y"], times = nAngles)

      angles <- if (all(is.na(angles))) {
        if (!is.null(dim(numAngles))) {
          if (equalRadii)
            rep(unlist(lapply(numAngles[, 1], function(na) seq_len(na) * (pi * 2 / na))), ncol(numAngles))
          else
            unlist(lapply(na.omit(as.vector(numAngles)), function(na) seq_len(na) * (pi * 2 / na)))
        } else {
          unlist(lapply(numAngles, function(na) seq.int(na) * (pi * 2 / na)))
        }
      } else {
        rep(angles, length(numAngles))
      }
      x <- cos(angles) * rads + xs
      y <- sin(angles) * rads + ys
    }

    indices <- as.integer(cellFromXY(landscape, cbind(x, y)))

    if (moreThanOne & allowOverlap & !closest) {
      MAT <- data.table(id, indices, rads, angles, x = x, y = y)
      setkeyv(MAT, c("id", "indices"))
      if (!equalRadii) {
        MAT[, maxRad := rep(apply(maxRadius, 2, max, na.rm = TRUE), nAngles)]
        MAT[, minRad := rep(apply(maxRadius, 2, min, na.rm = TRUE), nAngles)]
      }
      if (!allowDuplicates) {
        MAT <- unique(MAT)
      }
      MAT <- na.omit(MAT)
      MAT <- as.matrix(MAT)

    } else {
      MAT <- cbind(id, rads, angles, x, y, indices)
      if (!closest & !allowDuplicates) {
        notDups <- !duplicatedInt(indices)
        MAT <- MAT[notDups, , drop = FALSE]
      }
      MAT <- na.omit(MAT)
    }
    rm(id, indices, rads, x, y)

    # only need to calculate distances for these two cases
    if (includeBehavior == "excludePixels" | returnDistances | closest) {
      if (equalRadii) {
        maxRad <- maxRadius[NROW(maxRadius)]
        minRad <- maxRadius[1]
      }

      # if distances are not required, then only need the inner circle and outer
      # circle distances. Don't waste resources on calculating all distances.
      if (returnDistances | closest) {
        MAT2 <- MAT
      } else {
        if (equalRadii)
          # 0.71 is the sqrt of 1, so keep
          MAT2 <- MAT[MAT[, "rads"] >= (maxRad - 0.71) | MAT[, "rads"] <= (minRad + 0.71), , drop = FALSE]
        else {
          # 0.71 is the sqrt of 1, so keep
          MAT2 <- MAT[MAT[, "rads"] >= (MAT[, "maxRad"] - 0.71) | MAT[, "rads"] <= (MAT[, "minRad"] + 0.71), , drop = FALSE]
        }
      } #  only pixels that are in inner or outer ring of pixels

      if (suppliedAngles) {
        a <- cbind(id = MAT2[, "id"], rads = MAT2[, "rads"], angles = MAT2[, "angles"],
                   x = MAT2[, "x"], y = MAT2[, "y"], to = MAT2[, "indices"])

      } else {
        xyC <- xyFromCell(landscape, MAT2[, "indices"]);
        a <- cbind(id = MAT2[, "id"], rads = MAT2[, "rads"], angles = MAT2[, "angles"],
                   x = xyC[, "x"], y = xyC[, "y"], to = MAT2[, "indices"])
      }
      if (!equalRadii)
        a <- cbind(a, maxRad = MAT2[, "maxRad"], minRad = MAT2[, "minRad"])

      b <- cbind(coords, id = 1:NROW(coords))

      colnames(b)[1:2] <- c("x", "y")
      d <- distanceFromEachPoint(b, a)

      if (closest) {
        d <- d[order(d[, "rads"]),, drop = FALSE]
        dups <- duplicated(d[, "to", drop = FALSE])
        d <- d[!dups,, drop = FALSE]

      }

      if (includeBehavior == "excludePixels")
        if (equalRadii)
          d <- d[d[, "dists"] %<=% maxRad & d[, "dists"] %>=% minRad, , drop = FALSE]
      else
        d <- d[d[, "dists"] %<=% d[, "maxRad"] & d[, "dists"] %>=% d[, "minRad"], , drop = FALSE]

      colnames(d)[which(colnames(d) == "to")] <- "indices"
      if (!returnDistances)
        d <- d[, -which(colnames(d) == "dists"), drop = FALSE]

      if (!returnAngles) {
        d <- d[, -which(colnames(d) == "angles"), drop = FALSE]
        MAT <- MAT[, -which(colnames(MAT) == "angles"), drop = FALSE]
      } else {
        d[,"angles"] <- (pi / 2 - d[, "angles"]) %% (2 * pi)# convert to geographic
        MAT[,"angles"] <- pi / 2 -  MAT[, "angles", drop = FALSE] %% (2 * pi)# convert to geographic
      }

      if (returnDistances) {
        wh <- na.omit(match("rads", colnames(d)))
        if (length(wh) > 0) MAT <- d[, -wh, drop = FALSE]
      } else if (closest) {
        wh <- na.omit(match(c("rads", "dists"), colnames(d)))
        if (length(wh) > 0) MAT <- d[, -wh, drop = FALSE]
      } else {
        if (equalRadii)
          MATinterior <- MAT[MAT[, "rads"] < (maxRad - 0.71) & MAT[, "rads"] > (minRad + 0.71), , drop = FALSE]
        else
          MATinterior <- MAT[MAT[, "rads"] < (MAT[, "maxRad"] - 0.71) & MAT[, "rads"] > (MAT[, "minRad"] + 0.71), , drop = FALSE]

        MAT <- rbind(d[, colnames(MATinterior), drop = FALSE], MATinterior)
        MAT <- MAT[, -which(colnames(MAT) == "rads"), drop = FALSE]
      }
    } else {
      if (!returnAngles) {
        MAT <- MAT[, -which(colnames(MAT) == "angles"), drop = FALSE]
      }
      MAT <- MAT[, -which(colnames(MAT) == "rads"), drop = FALSE]
    }
    if (!returnIndices) {
      ras <- raster(landscape)
      ras[] <- 0
      if (!allowOverlap) {
        if (!returnDistances) {
          ras[MAT[, "indices"]] <- MAT[, "id"]
        } else {
          ras[MAT[, "indices"]] <- MAT[, "dists"]
        }
      } else {
        MAT <- data.table(MAT, key = "indices")
        if (!returnDistances) {
          MAT <- MAT[, sum(id), by = indices]
        } else {
          MAT <- MAT[, sum(1 / dists), by = indices]
        }
        ras[MAT$indices] <- MAT$V1
      }
      return(ras)
    }
    return(MAT)
  })

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
#' # initialize agents
#' N <- 10
#'
#' # previous points
#' x1 <- rep(0, N)
#' y1 <- rep(0, N)
#' # initial points
#' starts <- cbind(x = stats::runif(N, xrange[1], xrange[2]),
#'                 y = stats::runif(N, yrange[1], yrange[2]))
#'
#' # create the agent object
#' agent <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
#'
#'
#' ln <- rlnorm(N, 1, 0.02) # log normal step length
#' sd <- 30 # could be specified globally in params
#'
#' if (interactive()) {
#'   clearPlot()
#'   Plot(hab, zero.color = "white", axes = "L")
#' }
#' for(i in 1:10) {
#'   agent <- SpaDES::crw(agent = agent,
#'                          extent = extent(hab), stepLength = ln,
#'                          stddev = sd, lonlat = FALSE, torus = TRUE)
#'   if (interactive()) Plot(agent, addTo = "hab", axes = TRUE)
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


###############################################################################
#' Identify outward radiating spokes from initial points
#'
#' This is a generalized version of a notion of a viewshed. The main difference
#' is that there can be many "viewpoints".
#'
#' @inheritParams cir
#' @param stopRule A function. If the spokes are to stop. This can be a function
#'                 of \code{landscape}, fromCell, toCell, x (distance from coords cell),
#'                 or any other named argument passed into the ... of this function.
#'                 See examples.
#' @param nAngles Numeric, length one. Alternative to angles. If provided, the function
#'                will create a sequence of angles from 0 to 2*pi, with a length
#'                \code{nAngles}, and not including 2*pi. Will not be used if
#'                angles is provided, and will show warning of both are given.
#' @param ... Objects to be used by stopRule function. See examples.
#'
#' @return A matrix containing columns id (representing the row numbers of \code{coords}),
#' angles (from \code{coords} to each point along the spokes), x and y coordinates
#' of each point along the spokes, the corresponding indices on the \code{landscape}
#' Raster, dists (the distances between each \code{coords} and each point along the
#' spokes), and stop, indicating if it was a point that caused a spoke
#' to stop going outwards due to \code{stopRule}.
#'
#' @export
#' @docType methods
#' @rdname spokes
#' @importFrom fpCompare %<<%
#' @author Eliot McIntire
#' @example inst/examples/example_spokes.R
#'
setGeneric("spokes", function(landscape, coords, loci,
                           maxRadius = ncol(landscape)/4, minRadius = maxRadius,
                           allowOverlap = TRUE, stopRule = NULL,
                           includeBehavior = "includePixels", returnDistances = FALSE,
                           angles = NA_real_, nAngles = NA_real_,
                           returnAngles = FALSE, returnIndices = TRUE, ...) {
  standardGeneric("spokes")
})

#' @export
#' @rdname spokes
setMethod(
  "spokes",
  signature(landscape = "RasterLayer", coords = "SpatialPoints", loci = "missing"),
  definition = function(landscape, coords, loci, maxRadius, minRadius = maxRadius, allowOverlap,
                        stopRule,
                        includeBehavior, returnDistances, angles, nAngles,
                        returnAngles, returnIndices, ...
                        ) {
  if (!missing(nAngles)) {
    if (missing(angles)) {
    angles <- seq(0,pi*2,length.out = 17)
    angles <- angles[-length(angles)]
    } else {
      warning("Both angles and nAngles are provided. Using angles only.")
    }
  }

  aCir <- cir(landscape, coords = coords, minRadius = minRadius, maxRadius = maxRadius,
              returnAngles = TRUE, returnDistances = TRUE,
              allowOverlap = allowOverlap, allowDuplicates = TRUE,
              angles = angles, returnIndices = returnIndices)

  if (!is.null(stopRule)) {
    forms <- names(formals(stopRule))
    fromC <- "fromCell" %in% forms
    if (fromC) fromCell <- cellFromXY(landscape, coordinates(coords))
    toC <- "toCell" %in% forms
    if (toC) toCell <- cellFromXY(landscape, to[, c("x", "y")])
    land <- "landscape" %in% forms
    listArgs <- if (land) list(landscape = landscape[aCir[, "indices"]]) else NULL
    if (length(list(...)) > 0) listArgs <- append(listArgs, list(...))
    xDist <- "x" %in% forms

    #landscape <- landscape[aCir[,"indices"]]

    a <- cbind(aCir, stop = do.call(stopRule, args = listArgs))
    a <- cbind(a, stopDist = a[, "stop"] * a[, "dists"])
    a[a[, "stop"] %==% 0, "stopDist"] <- maxRadius #

    sortedUniqAngles <- sort(unique(a[, "angles"]))
    dxx <- lapply(sort(unique(a[, "id"])), function(id) {
      aID <- a[a[, "id"] == id, , drop = FALSE]
      b <- tapply(aID[, "stopDist"], aID[, "angles"], min, na.rm = TRUE)
      d1 <- lapply(sortedUniqAngles, function(x) {
        a1 <- aID[aID[, "angles"] %==% x, , drop = FALSE]
        if (includeBehavior == "excludePixels")
          a1[a1[, "dists"] %<<% b[as.numeric(names(b)) %==% x], , drop = FALSE]
        else
          a1[a1[, "dists"] %<=% b[as.numeric(names(b)) %==% x], , drop = FALSE]
      })
      do.call(rbind,d1)
    })
    d2xx <- do.call(rbind,dxx)
    whDrop <- match(c("stopDist"), colnames(d2xx))
    d2xx[, -whDrop, drop = FALSE]
  }
})


