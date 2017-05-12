if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("indices", "id", "initialLocus", "dists", "dup", ".I"))
}

###############################################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric landscape values (symmetric or asymmetric) and many other things.
#' Essentially, it starts from a collection of cells (\code{loci}) and spreads
#' to neighbours, according to the \code{directions} and \code{spreadProb} arguments.
#' This can become quite general, if \code{spreadProb} is 1 as it will expand
#' from every loci until all cells in the landscape have been covered.
#' With \code{id} set to \code{TRUE}, the resulting map will be classified
#' by the index of the cell where that event propagated from.
#' This can be used to examine things like fire size distributions.
#' \bold{NOTE:} See also \code{\link{spread2}}, which is more robust and can be
#' used to build custom functions.
#' However, under some conditions, this \code{spread} function is faster.
#' The two functions can accomplish many of the same things, and key differences
#' are internal.
#'
#' For large rasters, a combination of \code{lowMemory = TRUE} and
#' \code{returnIndices = TRUE} will use the least amount of memory.
#'
#' This function can be interrupted before all active cells are exhausted if
#' the \code{iterations} value is reached before there are no more active
#' cells to spread into. If this is desired, \code{returnIndices} should be
#' \code{TRUE} and the output of this call can be passed subsequently as an input
#' to this same function. This is intended to be used for situations where external
#' events happen during a spread event, or where one or more arguments to the spread
#' function change before a spread event is completed. For example, if it is
#' desired that the \code{spreadProb} change before a spread event is completed because,
#' for example, a fire is spreading, and a new set of conditions arise due to
#' a change in weather.
#'
#' \code{asymmetry} is currently used to modify the \code{spreadProb} in the following way.
#' First for each active cell, spreadProb is converted into a length 2 numeric of Low and High
#' spread probabilities for that cell:
#' \code{spreadProbsLH <- (spreadProb*2) // (asymmetry+1)*c(1,asymmetry)},
#' whose ratio is equal to
#' \code{asymmetry}.
#' Then, using \code{asymmetryAngle}, the angle between the
#' initial starting point of the event and all potential
#' cells is found. These are converted into a proportion of the angle from
#' \code{-asymmetryAngle}
#' to
#' \code{asymmetryAngle}
#' using:
#' \code{angleQuality <- (cos(angles - rad(asymmetryAngle))+1)/2}
#'
#' These are then converted to multiple spreadProbs by
#' \code{spreadProbs <- lowSpreadProb+(angleQuality * diff(spreadProbsLH))}
#' To maintain an expected \code{spreadProb} that is the same as the asymmetric
#' \code{spreadProbs}, these are then rescaled so that the mean of the
#' asymmetric spreadProbs is always equal to spreadProb at every iteration:
#' \code{spreadProbs <- spreadProbs - diff(c(spreadProb,mean(spreadProbs)))}
#'
#' @section Breaking out of spread events:
#'
#' There are 4 ways for the spread to "stop" spreading. Here, each "event" is defined as
#' all cells that are spawned from a single starting loci. So, one spread call can have
#' multiple spreading "events". The ways outlines below are all acting at all times,
#' i.e., they are not mutually exclusive. Therefore, it is the user's
#' responsibility to make sure the different rules are interacting with
#' each other correctly. Using \code{spreadProb} or \code{maxSize} are computationally
#' fastest, sometimes dramatically so.
#'
#' \tabular{ll}{
#'   \code{spreadProb} \tab Probabilistically, if spreadProb is low enough,
#'                          active spreading events will stop. In practice,
#'                          active spreading events will stop. In practice,
#'                          this number generally should be below 0.3 to actually
#'                          see an event stop\cr
#'   \code{maxSize} \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. This can be vectorized, one value
#'                       for each event   \cr
#'   \code{circleMaxRadius} \tab If \code{circle} is TRUE, then this will be the maximum
#'                       radius reached, and then the event will stop. This is
#'                       vectorized, and if length is >1, it will be matched
#'                       in the order of \code{loci}\cr
#'   \code{stopRule} \tab This is a function that can use "landscape", "id", "cells",
#'                       or any named vector passed into \code{spread} in the \code{...}.
#'                       This can take on relatively complex functions.
#'                       Passing in, say, a \code{RasterLayer} to \code{spread}
#'                       can access the individual values on that arbitrary
#'                       \code{RasterLayer} using "cells".
#'                       These will be calculated within all the cells of the individual
#'                       event (equivalent to a "group_by(event)" in \code{dplyr}.
#'                       So, \code{sum(arbitraryRaster[cells])} would sum up all
#'                       the raster values on the \code{arbitraryRaster} raster
#'                       that are overlaid by the individual event.
#'                       This can then be used in a logical statement. See examples.
#'                       To confirm the cause of stopping, the user can assess the values
#'                       after the function has finished.\cr
#' }
#'
#' The spread function does not return the result of this stopRule. If,
#' say, an event has both \code{circleMaxRadius} and \code{stopRule},
#' and it is
#' the \code{circleMaxRadius} that caused the event spreading to stop,
#' there will be no indicator returned from this function that indicates
#' which rule caused the stop.
#'
#' \code{stopRule} has many use cases. One common use case is evaluating
#' a neighbourhood around a focal set of points. This provides,
#' therefore, an alternative to the \code{\link[raster]{buffer}} function or
#' \code{\link[raster]{focal}} function.
#' In both of those cases, the window/buffer size must be an input to the function. Here,
#' the resulting size can be emergent based on the incremental growing and calculating
#' of the \code{landscape} values underlying the spreading event.
#'
#' @section \code{stopRuleBehavior}:
#' This determines how the \code{stopRule} should be implemented. Because
#' spreading occurs outwards in concentric circles or shapes, one cell width at a time, there
#' are 4 possible ways to interpret the logical inequality defined in \code{stopRule}.
#' In order of number of cells included in resulting events, from most cells to fewest cells:
#'
#' \tabular{ll}{
#'   \code{"includeRing"} \tab Will include the entire ring of cells that, as a group,
#'                             caused \code{stopRule} to be \code{TRUE}.\cr
#'   \code{"includePixel"} \tab Working backwards from the entire ring that caused the
#'                              \code{stopRule} to be \code{TRUE}, this will iteratively
#'                              random cells in the final ring
#'                              until the \code{stopRule} is \code{FALSE}. This will add back
#'                              the last removed cell and include it in the return result
#'                              for that event.\cr
#'   \code{"excludePixel"} \tab Like \code{"includePixel"}, but it will not add back the cell
#'                        that causes \code{stopRule} to be \code{TRUE}\cr
#'   \code{"excludeRing"} \tab Analogous to \code{"excludePixel"}, but for the entire final
#'                             ring of cells added. This will exclude the entire ring of cells
#'                             that caused the \code{stopRule} to be \code{TRUE}\cr
#' }
#'
#'
#' @param landscape     A \code{RasterLayer} object. This defines the possible
#'                      locations for spreading events to start and spread into.
#'                      This can also be used as part of \code{stopRule}.
#'
#' @param loci          A vector of locations in \code{landscape}.
#'                      These should be cell indices.
#'                      If user has x and y coordinates, these can be converted
#'                      with \code{\link[raster]{cellFromXY}}.
#'
#' @param spreadProb    Numeric, or \code{RasterLayer}.
#'                      If numeric of length 1, then this is the global probability
#'                      of spreading into each cell from a neighbour.
#'                      If a raster (or a vector of length \code{ncell(landscape)},
#'                      resolution and extent of \code{landscape}), then this will
#'                      be the cell-specific probability. Default is \code{0.23}.
#'                      If a \code{spreadProbLater} is provided, then this is
#'                      only used for the first iteration. Also called "escape
#'                      probability". See section on "Breaking out of spread events".
#'
#' @param persistence   A length 1 probability that an active cell will continue
#'                      to burn, per time step.
#'
#' @param mask          non-\code{NULL}, a \code{RasterLayer} object congruent with
#'                      \code{landscape} whose elements are \code{0,1}, where
#'                      \code{1} indicates "cannot spread to".
#'                      Currently not implemented, but identical behavior can be
#'                      achieved if \code{spreadProb} has zeros in all unspreadable
#'                      locations.
#'
#' @param maxSize       Numeric. Maximum number of cells for a single or
#'                      all events to be spread. Recycled to match \code{loci} length,
#'                      if it is not as long as \code{loci}.
#'                      See section on \code{Breaking out of spread events}.
#'
#' @param directions    The number of adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spread.
#'                      Leaving this \code{NULL} allows the spread to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param lowMemory     Logical. If true, then function uses package \code{ff}
#'                      internally. This is slower, but much lower memory footprint.
#'
#' @param returnIndices Logical. Should the function return a \code{data.table}
#'                      with indices and values of successful spread events, or
#'                      return a raster with values. See Details.
#'
#' @param returnDistances Logical. Should the function include a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is \code{FALSE}. See Details.
#'
#' @param spreadProbLater Numeric, or \code{RasterLayer}. If provided, then this
#'                      will become the spreadProb after the first iteration.
#'                      See Details.
#'
#' @param spreadState   \code{data.table}. This should be the output of a previous call
#'                      to \code{spread}, where \code{returnIndices} was \code{TRUE}.
#'                      Default \code{NA}, meaning the spread is starting from \code{loci}.
#'                      See Details.
#'
#' @param circle        Logical. If \code{TRUE}, then outward spread will be by
#'                      equidistant rings, rather than solely by adjacent cells
#'                      (via \code{directions} arg.). Default is \code{FALSE}.
#'                      Using \code{circle = TRUE} can be dramatically slower for
#'                      large problems.
#'                      Note, this should usually be used with \code{spreadProb = 1}.
#'
#' @param circleMaxRadius Numeric. A further way to stop the outward spread of events.
#'                      If \code{circle} is \code{TRUE}, then it will grow to this maximum radius.
#'                      See section on \code{Breaking out of spread events}.
#'                      Default is \code{NA}.
#'
#' @param stopRule      A function which will be used to assess whether each
#'                      individual cluster should stop growing.
#'                      This function can be an argument of \code{"landscape"},
#'                      \code{"id"}, \code{"cells"}, and any other named vectors,
#'                      a named list of named vectors, or a named \code{data.frame}
#'                      with column names passed to \code{spread} in the \code{...}.
#'                      Default \code{NA}, meaning that spreading will not stop
#'                      as a function of the landscape.
#'                      See section on "Breaking out of spread events" and examples.
#'
#' @param stopRuleBehavior Character. Can be one of \code{"includePixel"},
#'                      \code{"excludePixel"}, \code{"includeRing"}, or
#'                      \code{"excludeRing"}.
#'                      If \code{stopRule} contains a function, this argument is
#'                      used determine what to do with the cell(s) that caused
#'                      the rule to be \code{TRUE}. See details.
#'                      Default is \code{"includeRing"} which means to accept the
#'                      entire ring of cells that caused the rule to be \code{TRUE}.
#'
#' @param allowOverlap  Logical. If \code{TRUE}, then individual events can overlap
#'                      with one another, i.e., they do not interact (this is slower
#'                      than if \code{allowOverlap = FALSE}).
#'                      Default is \code{FALSE}.
#'
#' @param asymmetry     A numeric indicating the ratio of the asymmetry to be used.
#'                      Default is \code{NA}, indicating no asymmetry.
#'                      See details. This is still experimental.
#'                      \bold{Use with caution.}
#'
#' @param asymmetryAngle A numeric indicating the angle in degrees (0 is "up",
#'                      as in North on a map), that describes which way the
#'                      \code{asymmetry} is.
#'
#' @param quick  Logical. If \code{TRUE}, then several potentially time consuming
#'               checking (such as \code{inRange}) will be skipped.
#'               This should only be used if there is no concern about checking
#'               to ensure that inputs are legal.
#'
#' @param neighProbs A numeric vector, whose sum is 1.
#'                   It indicates the probabilities an individual spread iteration
#'                   spreading to \code{1:length(neighProbs)} neighbours.
#'
#' @param exactSizes Logical. If \code{TRUE}, then the \code{maxSize} will be
#'                   treated as exact sizes, i.e., the spread events will continue
#'                   until they are \code{floor(maxSize)}.
#'                   This is overridden by \code{iterations}, but if \code{iterations}
#'                   is run, and individual events haven't reached \code{maxSize},
#'                   then the returned \code{data.table} will still have at least
#'                   one active cell per event that did not achieve \code{maxSize},
#'                   so that the events can continue if passed into \code{spread}
#'                   with \code{spreadState}.
#'
#' @param relativeSpreadProb Logical. If \code{TRUE}, then \code{spreadProb} will
#'                      be rescaled *within* the \code{directions} neighbours, such that
#'                      the sum of the probabilities of all neighbours will be 1. Default
#'                      \code{FALSE}, unless \code{spreadProb} values are not contained
#'                      between 0 and 1, which will force \code{relativeSpreadProb}
#'                      to be \code{TRUE}.
#'
#' @param ...           Additional named vectors or named list of named vectors
#'                      required for \code{stopRule}. These
#'                      vectors should be as long as required e.g., length
#'                      \code{loci} if there is one value per event.
#'
#' @return Either a \code{RasterLayer} indicating the spread of the process in
#' the landscape or a \code{data.table} if \code{returnIndices} is \code{TRUE}.
#' If a \code{RasterLayer}, then it represents
#' every cell in which a successful spread event occurred. For the case of, say, a fire
#' this would represent every cell that burned. If \code{allowOverlap} is \code{TRUE},
#' This \code{RasterLayer} will represent the sum of the individual event ids
#' (which are numerics \code{seq_along(loci)}.
#' This will generally be of minimal use because it won't be possible to distinguish
#' if event 2 overlapped with event 5 or if it was just event 7.
#'
#' If \code{returnIndices} is \code{TRUE},
#' then this function returns a \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{id} \tab an arbitrary ID \code{1:length(loci)} identifying
#'                      unique clusters of spread events, i.e., all cells
#'                      that have been spread into that have a
#'                      common initial cell.\cr
#'   \code{initialLocus} \tab the initial cell number of that particular
#'                            spread event.\cr
#'   \code{indices} \tab The cell indices of cells that have
#'                        been touched by the spread algorithm.\cr
#'   \code{active} \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#' }
#'
#' This will generally be more useful when \code{allowOverlap} is \code{TRUE}.
#' @export
#' @importFrom raster extent maxValue minValue ncell ncol nrow raster res setValues
#' @importFrom ff ff as.ram
#' @importFrom ffbase ffwhich
#' @importFrom stats runif
#' @importFrom fpCompare %<=%
#' @docType methods
#'
#' @author Eliot McIntire
#' @author Steve Cumming
#' @seealso \code{\link{spread2}} for a different implementation of the same alogorithm.
#' It is more robust, meaning, there will be fewer unexplainable errors, and the behaviour
#' has been better tested, so it is more likely to be exactly as described under all
#' argument combinations.
#' Also, \code{\link{rings}} which uses \code{spread} but with specific argument
#' values selected for a specific purpose.
#' \code{\link[raster]{distanceFromPoints}}.
#' \code{cir} to create "circles"; it is fast for many small problems.
#'
#' @rdname spread
#'
setGeneric("spread", function(landscape, loci = NA_real_,
                              spreadProb = 0.23,
                              persistence = 0, mask = NA, maxSize = 1e8L,
                              directions = 8L, iterations = 1e6L,
                              lowMemory = getOption("spades.lowMemory"),
                              returnIndices = FALSE, returnDistances = FALSE,
                              mapID = NULL, id = FALSE, plot.it = FALSE,
                              spreadProbLater = NA_real_, spreadState = NA,
                              circle = FALSE, circleMaxRadius = NA_real_,
                              stopRule = NA, stopRuleBehavior = "includeRing",
                              allowOverlap = FALSE,
                              asymmetry = NA_real_, asymmetryAngle = NA_real_,
                              quick = FALSE, neighProbs = NULL, exactSizes = FALSE,
                              relativeSpreadProb = FALSE, ...) {
  standardGeneric("spread")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spread event grow.
#'
#' @param mapID    Deprecated use id
#'
#' @param id    Logical. If TRUE, returns a raster of events ids.
#'                 If FALSE, returns a raster of iteration numbers,
#'                 i.e., the spread history of one or more events. NOTE:
#'                 this is overridden if \code{returnIndices} is \code{TRUE}.
#'
#' @rdname spread
#'
#' @example inst/examples/example_spread.R
#'
setMethod(
  "spread",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, spreadProb, persistence,
                        mask, maxSize,
                        directions, iterations, lowMemory, returnIndices,
                        returnDistances, mapID, id, plot.it, spreadProbLater,
                        spreadState, circle, circleMaxRadius, stopRule,
                        stopRuleBehavior, allowOverlap, asymmetry, asymmetryAngle,
                        quick, neighProbs, exactSizes, relativeSpreadProb,
                        ...) {

    if (!is.null(neighProbs)) {
      if (isTRUE(allowOverlap)) stop("Can't use neighProbs and allowOverlap = TRUE together")
    }
    if (!is.null(mapID)) {
      warning("mapID is deprecated, use id")
      id <- mapID
    }
    if (!quick)
      if (!any(stopRuleBehavior %fin% c("includePixel", "excludePixel", "includeRing", "excludeRing")))
        stop("stopRuleBehaviour must be one of \"includePixel\", \"excludePixel\", \"includeRing\", or \"excludeRing\"")
    spreadStateExists <- is(spreadState, "data.table")
    spreadProbLaterExists <- TRUE

    if (!is(spreadProbLater, "Raster")) {
      if (anyNA(spreadProbLater)) {
        spreadProbLaterExists <- FALSE
        spreadProbLater <- spreadProb
      }
    }

    ### should sanity check map extents
    if (any(is.na(loci)))  {
      # start it in the centre cell, if there is no spreadState
      if (!spreadStateExists)
        loci <- (nrow(landscape) / 2L + 0.5) * ncol(landscape)
    }

    if (length(loci) == 0) stop("No loci. Nothing to do")

    if (any(!is.na(maxSize))) {
      msEqZero <- maxSize < 1
      if (any(msEqZero)) {
        loci <- loci[!msEqZero]
        maxSize <- maxSize[!msEqZero]
      }
    }

    if (spreadStateExists) {
      keepers <- spreadState$active==TRUE
      loci <- initialActiveCells <- spreadState[keepers, indices]
      #loci <- loci[!(loci %fin% spreadState[, indices])] # keep these for later
      initialLoci <- unique(spreadState$initialLocus)
    } else {
      initialLoci <- loci
    }

    # Check for probabilities
    if (!quick) {
      if (is(spreadProbLater, "RasterLayer") | is(spreadProb, "Rasterlayer")) {
        if ( (minValue(spreadProb) > 1L) || (maxValue(spreadProb) < 0L) ||
             (maxValue(spreadProb) > 1L) || (minValue(spreadProb) < 0L) ) {
          #stop("spreadProb is not a probability")
          relativeSpreadProb <- TRUE
        }
        if (spreadProbLaterExists)
          if ( ( (minValue(spreadProbLater) > 1L) || (maxValue(spreadProbLater) < 0L) ||
                 (maxValue(spreadProbLater) > 1L) || (minValue(spreadProbLater) < 0L) ) ) {
            # stop("spreadProbLater is not a probability")
            relativeSpreadProb <- TRUE
          }
      } else {
        if (!all(inRange(spreadProb))) {
          relativeSpreadProb <- TRUE
          stop("spreadProb is not a probability")
        }
        if (spreadProbLaterExists) {
          relativeSpreadProb <- TRUE
          if (!all(inRange(spreadProbLater))) stop("spreadProbLater is not a probability")
        }
      }
    }

    #integerProbs <- all(spreadProb[] == 1 | spreadProb[] == 0)

    ncells <- ncell(landscape)

    if (allowOverlap | returnDistances | spreadStateExists) {
      if (spreadStateExists) {
        spreads <- as.matrix(spreadState[,list(initialLocus, indices, id, active)])

      } else {
        spreads <- cbind(initialLocus = initialLoci, indices = initialLoci,
                         id = 1:length(loci), active = 1)
      }
    } else {
      if (lowMemory) {
        # create vector of 0s called spreads, which corresponds to the indices
        # of the landscape raster
        spreads <- ff(vmode = "short", 0, length = ncells)
      } else {
        spreads <- vector("integer", ncells)
      }
    }

    n <- 1L

    # circle needs directions to be 8
    if (circle | !is.na(asymmetry)) {
      if (circle) directions <- 8L # only required for circle
      initialLociXY <- cbind(id = seq_along(initialLoci), xyFromCell(landscape, initialLoci))
      id <- TRUE
      if (allowOverlap | returnDistances) {
        spreads <- cbind(spreads, dists = 0)
      }
    }

    # determine ... variables
    otherVars <- list(...)
    anyList <- unlist(lapply(otherVars, is.list) )

    if (any(anyList)) {
      otherVarsLists <- unlist(unname(otherVars), recursive = FALSE)
      otherVars[anyList] <- NULL
      otherVars <- append(otherVars, otherVarsLists)
    }

    # check validity of stopRule
    if (is.function(stopRule)) {
      id <- TRUE
      stopRuleObjs <- names(formals(stopRule))
      if (!quick) {
        if (any(is.na(match(stopRuleObjs,
                           c("id", "landscape", "cells", names(otherVars)))))) {
          stop("Arguments in stopRule not valid.\n",
               "The function definition must be a function of built-in options,",
               " (id, landscape, or cells) or user supplied variables.",
               " If user supplied, the variables",
               " must be passed as named vectors, or lists or data.frames.",
               " See examples.")
        }
      }
      LandRasNeeded <- any(stopRuleObjs == "landscape")
      colNamesPotentials <- c("id", "landscape"[LandRasNeeded], "cells", "prev")
      argNames <- c(colNamesPotentials, names(otherVars))
      whArgs <- match(names(formals(stopRule)), argNames)

      # Raster indexing is slow. If there is are Rasters submitted with the stopRule
      #  then this will convert them to vectors first. Clearly, this will have
      #  memory consequences if the Rasters are on disk, but spread is optimized for speed
      rasters <- unlist(lapply(otherVars[names(otherVars)], function(x) is(x, "Raster")))
      if (any(rasters)) {
        for (i in 1:which(rasters)) {
          otherVars[[names(rasters[i])]] <- otherVars[[names(rasters[i])]][]
        }
      }
      landRas <- landscape[] # For speed
    }

    if (!allowOverlap & !returnDistances) {#} & !spreadStateExists) {
      if (id | returnIndices | relativeSpreadProb) {
        if (!spreadStateExists) {
        #   #spreads[spreadState$indices] <- spreadState$id
        # } else {
          # give values to spreads vector at initialLoci
          spreads[loci] <- 1L:length(loci)
        }
      } else {
        spreads[loci] <- n
      }
      spreadsIndices <- unname(loci)
    }

    # Convert mask and NAs to 0 on the spreadProb Raster
    if (is(spreadProb, "Raster")) {
      # convert NA to 0s
      #isNASpreadProb <- is.na(spreadProb[])
      # if (anyNA(spreadProb[])) {
      #   isNASpreadProb <- is.na(spreadProb[])
      #   spreadProb[isNASpreadProb] <- 0L
      # }

    } else if (is.numeric(spreadProb)) {
      # Translate numeric spreadProb into a Raster, if there is a mask
      if (is(mask, "Raster")) {
        spreadProb <- raster(extent(landscape), res = res(landscape), vals = spreadProb)
      }
    }

    # Convert mask and NAs to 0 on the spreadProbLater Raster
    if (is(spreadProbLater, "Raster")) {
      # convert NA to 0s
      # if (!spreadProbLaterExists) {
      #   if (exists("isNASpreadProb", inherits = FALSE))
      #     isNASpreadProbLater <- isNASpreadProb
      # } else {
      #   if (anyNA(spreadProbLater[]))
      #     isNASpreadProbLater <- is.na(spreadProbLater[])
      # }
      # if (exists("isNASpreadProbLater", inherits = FALSE)) spreadProbLater[isNASpreadProbLater] <- 0L

    } else if (is.numeric(spreadProbLater)) {
       # Translate numeric spreadProbLater into a Raster, if there is a mask
       if (is(mask, "Raster")) {
         spreadProbLater <- raster(extent(landscape), res = res(landscape), vals = spreadProbLater)
       }
    }

    # Mask spreadProbLater and spreadProb
    if (is(mask, "Raster")) {
      spreadProbLater[mask[] == 1L] <- 0L
      spreadProb[mask[] == 1L] <- 0L
    }

    if (spreadStateExists) {
      if (allowOverlap | returnDistances) {
        stop("Using spreadState with either allowOverlap = TRUE or returnDistances = TRUE is not implemented")
      } else {
        if (sum(colnames(spreadState) %fin% c("indices", "id", "active", "initialLocus")) != 4) {
        #   spreads[loci] <- spreads[loci] + spreadState[, max(id)] # reassign old ones
        #   spreads[spreadState[, indices]] <- spreadState[, id]
        #   loci <- c(spreadState[active == TRUE, indices], loci) %>% na.omit()
        # } else {
          stop("spreadState must have at least columns: ",
               "indices, id, active, and initialLocus.")
        }
      }
    }


    if (!quick)
      if (any(loci > ncells)) stop("loci indices are not on landscape")

    ## Recycling maxSize as needed
    if (any(!is.na(maxSize))) {
      if (!is.integer(maxSize)) maxSize <- floor(maxSize)
      if (spreadStateExists) {
        sizeAll <- spreadState[, list(len = .N), by = id]
        #maxSize <- rep_len(maxSize, length(initialLoci) + NROW(sizeAll))
        size <- c(sizeAll[, len])#, rep_len(1L, length(initialLoci)))

      } else {
        maxSize <- rep_len(maxSize, length(loci))
        size <- rep_len(1L, length(loci))
      }
    } else {
      maxSize <- ncells
      size <- length(loci)
    }

    noMaxSize <- all(maxSize >= ncells) # will be used to omit testing for maxSize
    if (is.null(neighProbs)) {
      numNeighs <- NULL
    }

    if (!exists("numRetries", envir = .spadesEnv))
      assign("numRetries", rep(0, length(initialLoci)), envir = .spadesEnv)

    toColumn <- c("to", "indices")

    # while there are active cells
    while (length(loci) & (n <= iterations) ) {
      if (!is.null(neighProbs)) {
        numNeighs <- if (is.list(neighProbs)) {
          unlist(lapply(neighProbs, function(x) {
            sample.int(length(x), size = 1, replace = TRUE, prob = x)
          }))
        } else {
          sample.int(length(neighProbs), size = length(loci), replace = TRUE,
                     prob = neighProbs)
        }
      }

      # identify neighbours
      if (allowOverlap | returnDistances | spreadStateExists) {
        whActive <- spreads[,"active"] == 1 # spreads carries over
        potentials <- adj(landscape, loci, directions, pairs = TRUE,
                          id = spreads[whActive, "id"])#, numNeighs = numNeighs)
        spreads[whActive,"active"] <- 0
        potentials <- cbind(potentials, active = 1)
      } else {
        if (id | returnIndices | circle | relativeSpreadProb | !is.null(neighProbs)) {
          #potentials <- adj(landscape, loci, directions, pairs = TRUE)
          potentials <- adj(landscape, loci, directions, pairs = TRUE)#,
                                #numNeighs = numNeighs)
        } else {
          # must pad the first column of potentials
          potentials <- cbind(NA, adj(landscape, loci, directions, pairs = FALSE))
                                      #numNeighs=numNeighs))
        }
      }

      if (circle)
        potentials <- cbind(potentials, dists = 0)

      # keep only neighbours that have not been spread to yet
      if (allowOverlap | returnDistances | spreadStateExists) {
        #faster alternative to tapply, but cumbersome
        #potentialsOrig <- potentials

        if (TRUE) { # data.table version is faster for potentials > 500 or so
          #potentials <- potentialsOrig

          spreadsDT <- data.table(spreads)
          potentialsDT <- data.table(potentials)
          potentialsDT[, initialLocus := initialLoci[potentialsDT$id]]
          colnamesPDT <- colnames(potentialsDT)
          whIL <- which(colnamesPDT == "initialLocus")
          whFrom <- which(colnamesPDT == "from")
          setcolorder(potentialsDT, c(colnamesPDT[whIL], colnamesPDT[-c(whIL, whFrom)], colnamesPDT[whFrom]))
          setnames(potentialsDT, old = "to", new = "indices")
          d <- rbindlist(list(spreadsDT, potentialsDT), fill = TRUE)
          d <- data.table(d); setkey(d, "id");
          d[, duplicated := duplicated(indices), by = id]
          d <- d[duplicated == 0 & active == 1];
          set(d,, "duplicated", NULL)
          potentials <- as.matrix(d)
        } else {
          #potentials <- potentialsOrig
          potentialsFrom <- potentials[, "from"]
          colnames(potentials) <- colnames(spreads)
          # get rid of immediate "from" and replace with original "from"
          potentials[, "initialLocus"] <- initialLoci[potentials[, "id"]]
          d <- rbind(spreads, potentials)
          d <- cbind(d, "from" = c(rep(NA, NROW(spreads)), potentialsFrom))
          ids <- as.integer(unique(d[, "id"]))
          d <- do.call(rbind, lapply(ids, function(id) {
            cbind(d[d[, "id"] == id, , drop = FALSE],
                  duplicated = duplicated(d[d[, "id"] == id, "indices"]))
          }))

          lastCol <- ncol(d)
          potentials <- d[d[, "duplicated"] == 0 &
                            d[, "active"] == 1, , drop = FALSE][, -lastCol, drop = FALSE]
        }

      } else {
        keep <- spreads[potentials[, 2L]] == 0L
        potentials <- potentials[keep, , drop = FALSE]
      }

      if (n == 2) {
        spreadProb <- spreadProbLater
      }

      # extract spreadProb values from spreadProb argument
      if (is.numeric(spreadProb)) {
        if (n == 1 & spreadProbLaterExists) {
          # need cell specific values
          spreadProbs <- rep(spreadProb, NROW(potentials))
          #prevIndices <- potentials[, 1L] %fin% initialActiveCells#spreadState[active == TRUE, indices]
          spreadProb <- spreadProbLater
        } else {
          if (length(spreadProb) > 1) {
            spreadProbs <- spreadProb[potentials[, 2L]]
          } else {
            spreadProbs <- rep(spreadProb, NROW(potentials))
          }
        }
      } else {
        # here for raster spreadProb
        if (n == 1 & spreadProbLaterExists) {
          # need cell specific values
          spreadProbs <- spreadProb[][potentials[, 2L]]
          #prevIndices <- potentials[, 1L] %fin% initialActiveCells#spreadState[active == TRUE, indices]
          spreadProb <- spreadProbLater
        } else {
          spreadProbs <- spreadProb[][potentials[, 2L]]
        }
      }

      if (anyNA(spreadProbs)) spreadProbs[is.na(spreadProbs)] <- 0

      if (!is.na(asymmetry)) {
        if (allowOverlap | returnDistances) {
          a <- cbind(id = potentials[, 3L], to = potentials[, 2L],
                     xyFromCell(landscape, potentials[, 2L]))
        } else {
          a <- cbind(id = spreads[potentials[, 1L]], to = potentials[, 2L],
                     xyFromCell(landscape, potentials[, 2L]))
        }
        d <- directionFromEachPoint(from = initialLociXY, to = a)
        newSpreadProbExtremes <- (spreadProb[] * 2) / (asymmetry + 1) * c(1, asymmetry)
        angleQuality <- ((cos(d[, "angles"] - rad(asymmetryAngle)) + 1) / 2)
        spreadProbs <- newSpreadProbExtremes[1] + (angleQuality * diff(newSpreadProbExtremes))
        spreadProbs <- spreadProbs - diff(c(spreadProb[], mean(spreadProbs)))
      }

      if (!is.null(neighProbs) | relativeSpreadProb) {
        # This commented block is the data.table way of doing the neighProbs -- ]
        # seems slower under early tests, because of the on the fly creation of a data.table
        # bbb <- data.table(potentials)
        # numNeighsAvailable <- bbb[,.N,by="from"]$N
        # if (length(numNeighsAvailable) != length(numNeighs)) {
        #   activeCellContinue <- loci %in% unique(potentials[, "from"])
        #   numNeighs <- numNeighs[activeCellContinue]
        # }
        # anyTooSmall <- which(numNeighsAvailable<numNeighs)
        # if (length(anyTooSmall) > 0) {
        #   numNeighs[anyTooSmall] <- unname(numNeighsAvailable[anyTooSmall])
        # }
        # potentials <- as.matrix(bbb[, list(to = resample(to, numNeighs[.GRP])), by = "from"])

        aaa <- split(seq_along(potentials[, toColumn[spreadStateExists+1]]),
                     potentials[, "from"]);
        if (length(aaa) != length(numNeighs)) {
          activeCellContinue <- loci %in% unique(potentials[, "from"])
          numNeighs <- numNeighs[activeCellContinue]
        }

        tmpA <- unlist(lapply(aaa, length))
        tmpB <- which(tmpA < numNeighs)
        if (length(tmpB) > 0)
          numNeighs[tmpB] <- unname(tmpA[tmpB])

        if (relativeSpreadProb) {
          rescaledProbs <- tapply(spreadProbs, potentials[,"from"], function(x) {
            x / sum(x, na.rm = TRUE)
          }, simplify = FALSE)
          neighIndexToKeep <- unlist(lapply(seq_along(aaa), function(x)
            resample(aaa[[x]], size = numNeighs[x], prob = rescaledProbs[[x]])))
        } else {
          neighIndexToKeep <- unlist(lapply(seq_along(aaa), function(x)
            resample(aaa[[x]], size = numNeighs[x])))
        }
        potentials <- potentials[neighIndexToKeep,,drop=FALSE]
        spreadProbs <- spreadProbs[neighIndexToKeep]
        spreadProbs[spreadProbs>0] <- 1

      }

      #if (integerProbs) {
      #  potentials <- potentials[as.logical(spreadProbs), , drop = FALSE]
      #} else if (any(spreadProbs < 1)) {
      # if (relativeSpreadProb) {
      #   mb = microbenchmark(tapply = {
      #   spreadProbs <- unname(unlist(tapply(spreadProbs, potentials[,"from"], function(x) x/sum(x, na.rm=TRUE),
      #                 simplify = FALSE))[
      #                   order(unname(unlist(tapply(seq_along(spreadProbs),
      #                                              potentials[,"from"], function(x) x))))])
      #
      #   },dt={
      #   spreadProbs <- data.table(potentials, spreadProb = spreadProbs) %>%
      #     .[,spreadProb:=spreadProb/sum(spreadProb),by="from"] %>% .$spreadProb
      #   })
      #   print(paste("iteration", n, "; length =", NROW(potentials)))
      #   print(mb)
      # }
      potentials <- potentials[runif(NROW(potentials)) <= spreadProbs, , drop = FALSE]

      #}
      potentials <- potentials[sample.int(NROW(potentials)), , drop = FALSE] # random ordering so not always same
      if (!allowOverlap) { # here is where allowOverlap and returnDistances are different
        potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
      }

      # increment iteration
      n <- n + 1L

      if (length(potentials) > 0) {# potentials can become zero because all active cells are edge cells
        # implement circle
        if (!missing(circle)) {
          if (circle) {
            if (allowOverlap | returnDistances) {
              a <- cbind(potentials, xyFromCell(landscape, potentials[, 2L]))
            } else {
              a <- cbind(potentials, id = spreads[potentials[, "from"]],
                         xyFromCell(landscape, potentials[, "to"]))
            }
            # need to remove dists column because distanceFromEachPoint, adds one back
            a <- a[, !(colnames(a) %fin% c("dists")), drop = FALSE]
            # need 3 columns, id, x, y in both initialLociXY and a
            d <- distanceFromEachPoint(initialLociXY, a, angles = asymmetry) # d is sorted
            cMR <- (n-1) * res(landscape)[1]
            if (!any(is.na(circleMaxRadius))) {
              # don't bother proceeding if circleMaxRadius is larger than current iteration
              if (any(circleMaxRadius <= ( (n-1) * res(landscape)[1]))) {
                if (length(circleMaxRadius) > 1) { # if it is a vector of values
                  cMR <- circleMaxRadius[d[, "id"]]
                } else {
                  cMR <- circleMaxRadius
                }
              }
            }
            potentials <- d[, !(colnames(d) %fin% c("x", "y")), drop = FALSE]
            potentials <- potentials[(d[, "dists"] %<=% cMR), , drop = FALSE]
            # whKeep <- (d[, "dists"] %<=% cMR)
            # potentials <- potentials[whKeep, , drop = FALSE]
            # potentials[,"dists"] <- d[whKeep, !(colnames(d) %fin% c("x", "y")), drop = TRUE]
          }
        }

        events <- potentials[, 2L]

        if (!noMaxSize) {
          if (allowOverlap | returnDistances | spreadStateExists) {
            len <- tabulate(potentials[, 3L], length(maxSize))
          } else {
            len <- tabulate(spreads[potentials[, 1L]], length(maxSize)) # actually interested in potential[,2L], but they don't have values yet... can use their source
          }
          if ( any( (size + len) > maxSize & size <= maxSize) ) {
            whichID <- which(size + len > maxSize)
            toRm <- (size + len)[whichID] - maxSize[whichID] # remove some active cells, if more than maxSize
            for (i in 1:length(whichID)) {
              if (allowOverlap | returnDistances | spreadStateExists) {
                thisID <- which(potentials[, 3L] == whichID[i])
              } else {
                thisID <- which(spreads[potentials[, 1L]] == whichID[i])
              }

              if (length(thisID)) # some unusual cases where there are none on the spreads. Unsure how this occurs
                potentials <- potentials[-resample(thisID, toRm[i]), , drop = FALSE]
            }
            events <- potentials[, 2L]
          }

          #size <- size + len
          # which ones were removed
          #size[whichID] <- size[whichID] - toRm
          size <- pmin(size + len, maxSize) ## Quick? and dirty. fast but loose (too flexible)
        }

        # Implement stopRule section
        if (is.function(stopRule) & length(events) > 0) {
          if (allowOverlap | returnDistances) {
            prevCells <- cbind(id = spreads[, "id"],
                               landscape = if (LandRasNeeded) landRas[spreads[, "indices"]] else NULL,
                               cells = spreads[, "indices"], prev = 1)
            eventCells <- cbind(id = potentials[, "id"],
                                landscape = if (LandRasNeeded) landRas[events] else NULL,
                                cells = events, prev = 0)
          } else {
            #whgtZero <- which(spreads > 0)
            whgtZero <- spreadsIndices
            prevCells <- cbind(id = spreads[whgtZero],
                               landscape = if (LandRasNeeded) landRas[whgtZero] else NULL,
                               cells = whgtZero, prev = 1)
            eventCells <- cbind(id = spreads[potentials[, 1L]],
                                landscape = if (LandRasNeeded) landRas[potentials[, 2L]] else NULL,
                                cells = potentials[, 2L], prev = 0)
          }
          if (circle) {
            prevCells <- cbind(prevCells, dist = NA)
            eventCells <- cbind(eventCells, dist = potentials[, "dists"])
          }
          # don't need to continue doing ids that are not active
          tmp <- rbind(prevCells[prevCells[, "id"] %fin% unique(eventCells[, "id"]), ], eventCells)

          ids <- unique(tmp[, "id"])

          shouldStopList <- lapply(ids, function(id) {
            shortTmp <- tmp[tmp[, "id"] == id, ]
            args <- append(list(id = id), lapply(colNamesPotentials[-1], function(j) shortTmp[, j])) # instead of as.data.frame
            names(args) <- colNamesPotentials
            args <- append(args, otherVars)
            do.call(stopRule, args[whArgs])
          })
          if (any(lapply(shouldStopList, length) > 1))
            stop("stopRule does not return a length-one logical.",
                 " Perhaps stopRule need indexing by cells or id?")

          shouldStop <- unlist(shouldStopList)

          names(shouldStop) <- ids

          if (any(shouldStop)) {
            if (stopRuleBehavior != "includeRing") {
              if (stopRuleBehavior != "excludeRing") {
                whStop <- as.numeric(names(shouldStop)[shouldStop])
                whStopAll <- tmp[, "id"] %fin% whStop
                tmp2 <- tmp[whStopAll, ]

                whStopEvents <- eventCells[, "id"] %fin% whStop

                # If an event needs to stop, then must identify which cells are included
                out <- lapply(whStop, function(id) {
                  tmp3 <- tmp2[tmp2[, "id"] == id, ]
                  newOnes <- tmp3[, "prev"] == 0
                  ord <- seq_along(newOnes)
                  if (sum(newOnes) > 1) { # because of undesired behaviour of sample when length(x)==1
                    ord[newOnes] <- sample(ord[newOnes])
                    if (circle) ord[newOnes] <- ord[newOnes][order(tmp3[ord[newOnes], "dist"])]
                    tmp3 <- tmp3[ord, ]
                  }
                  startLen <- sum(!newOnes)
                  addIncr <- 1
                  done <- FALSE
                  args <- append(list(id = id),
                                 lapply(colNamesPotentials[-1], function(j) {
                                   tmp3[1:startLen, j]
                  })) # instead of as.data.frame
                  names(args) <- colNamesPotentials
                  args <- append(args, otherVars)
                  argsSeq <- seq_along(colNamesPotentials[-1]) + 1

                  while (!done) {
                    args[argsSeq] <- lapply(colNamesPotentials[-1], function(j) {
                      unname(c(args[[j]], tmp3[(startLen + addIncr), j]))
                    }) # instead of as.data.frame
                    done <- do.call(stopRule, args[whArgs])
                    addIncr <- addIncr + 1
                  }
                  if (stopRuleBehavior == "excludePixel") addIncr <- addIncr - 1
                  firstInd <- startLen + addIncr
                  lastInd <- NROW(tmp3)
                  sequ <- if (firstInd > lastInd) 0 else firstInd:lastInd
                  tmp3[sequ, , drop = FALSE]
                })
                eventRm <- do.call(rbind, out)[, "cells"]
                cellsKeep <- !(potentials[, 2L] %fin% eventRm)
              } else {
                cellsKeep <- rep(FALSE, NROW(potentials))
              }
              potentials <- potentials[cellsKeep, , drop = FALSE]
              events <- potentials[, 2L]
              eventCells <- eventCells[cellsKeep, , drop = FALSE]
            }
            toKeepSR <- !(eventCells[, "id"] %fin% as.numeric(names(which((shouldStop)))))
          }
        }

        if (length(events) > 0) {
          # place new value at new cells that became active
          if (allowOverlap | returnDistances | spreadStateExists) {
            fromCol <- colnames(potentials)=="from"

            spreads <- rbind(spreads, potentials[,!fromCol])
            if ((returnDistances | spreadStateExists) & !allowOverlap) {
              # 2nd place where allowOverlap and returnDistances differ
              notDups <- !duplicated(spreads[, "indices"])
              nrSpreads <- NROW(spreads)
              nrPotentials <- NROW(potentials)
              notDupsEvents <- notDups[-(1:(nrSpreads - nrPotentials))]
              spreads <- spreads[notDups, , drop = FALSE]
              events <- events[notDupsEvents]
            }
          } else {
            if (id | returnIndices | relativeSpreadProb) {
              # give new cells, the id of the source cell
              spreads[events] <- spreads[potentials[, 1L]]
            } else {
              spreads[events] <- n
            }
            spreadsIndices <- unname(c(spreadsIndices, events))
          }
          #spreadsIndices <- unname(c(spreadsIndices, events))
        }

        # remove the cells from "events" that push it over maxSize
        #  There are some edge cases that aren't captured above ... identify them...
        if (length(maxSize) > 1L) {
          if (exists("whichID", inherits = FALSE)) {
            # must update toKeepSR in case that is a second reason to stop event
            if (exists("toKeepSR", inherits = FALSE)) {
              if (allowOverlap | returnDistances) {
                maxSizeKeep <- !(spreads[spreads[, "active"] == 1, "id"] %fin% whichID)
                spreads <- spreads[c(rep(TRUE, sum(spreads[, "active"] == 0)), maxSizeKeep), ]
              } else {
                maxSizeKeep <- !spreads[events] %fin% whichID
              }
              events <- events[maxSizeKeep]
              toKeepSR <- toKeepSR[maxSizeKeep]
            }
            rm(whichID)
          }
        } else {
          if (all(size >= maxSize)) {
            events <- NULL
          }
        }

        # Remove cells that were stopped by stopRule
        if (is.function(stopRule)) {
          if (exists("toKeepSR", inherits = FALSE)) {
            events <- events[toKeepSR]
            if (allowOverlap | returnDistances) {
              spreads[c(rep(TRUE, sum(spreads[, "active"] == 0)), !toKeepSR), "active"] <- 0
            }
            rm(toKeepSR)
          }
        }
      } else { # there are no potentials -- possibly from failed runif, or spreadProbs all 0
        events <- NULL
      }

      if (exactSizes) {
        if (all(get("numRetries", inherits = FALSE, envir = .spadesEnv) < 10)) {
          if (spreadStateExists) {
            # tooSmall <- tabulate(spreads[c(spreadState[!keepers]$indices, spreadsIndices)],
            #                      length(maxSize)) < maxSize
            tooSmall <- tabulate(spreads[,"id"], length(maxSize)) < maxSize
            inactive <- tabulate(spreads[spreads[,"active"]==1,"id"], length(maxSize)) == 0

          } else {
            tooSmall <- tabulate(spreads, length(maxSize)) < maxSize
            inactive <- tabulate(spreads[events], length(maxSize)) == 0
          }

          needPersist <- tooSmall & inactive # these are ones that are stuck ... i.e., too small, and inactive
          needPersistJump <- TRUE
          if (any(needPersist)) {
            assign("numRetries", envir = .spadesEnv,
               get("numRetries", inherits = FALSE, envir = .spadesEnv) + needPersist)

            if (spreadStateExists) {
              whSmallInactive <- which(tooSmall & inactive)
              spreadsSmallInactive <- spreads[spreads[, "id"] %in% whSmallInactive, , drop = FALSE]
              if (needPersistJump) {
                message("Jumping to new active location, up to 1000 m away")
                mmm <- SpaDES::rings(landscape, loci = spreadsSmallInactive[, "indices"],
                                     maxRadius = 1000, minRadius = 1,
                                     returnIndices = TRUE)
                wh <- mmm[, list(whKeepLoci = resample(.I, 1)), by = id]$whKeepLoci
              } else {
                for (whSI in whSmallInactive) {
                  wh <- which(spreads[, "id"] == whSI)
                  wh <- tail(wh, 2) # pick last two ones from all inactive cells
                  keepLoci <- spreads[wh, "indices"]
                  events <- c(keepLoci, events)
                  spreads[wh, "active"] <- 1
                }
              }
            } else {
              keepLoci <- spreads[loci] %fin% which(tooSmall & inactive)
              events <- c(loci[keepLoci], events)
            }

          }
        }
      }

      # drop or keep loci
      if (is.na(persistence) | persistence == 0L) {
        loci <- NULL
      } else {
        if (inRange(persistence)) {
          loci <- loci[runif(length(loci)) <= persistence]
        } else {
          # here is were we would handle methods for raster* or functions
          stop("Unsupported type: persistence")
        }
      }

      if (plot.it) {
        if (n == 2 & !spreadStateExists) clearPlot()
        if (allowOverlap | returnDistances) {
          spreadsDT <- data.table(spreads);
          hab2 <- landscape;
          hab2[] <- 0;
          pixVal <- spreadsDT[, sum(id), by = indices]
          hab2[pixVal$indices] <- pixVal$V1;
          Plot(hab2, legendRange = c(0,sum(seq_along(initialLoci))))
        } else {
          plotCur <- raster(landscape)
          plotCur <- setValues(plotCur, spreads)
          Plot(plotCur)
        }
      }
      loci <- c(loci, events) # new loci list for next while loop, concat of persistent and new events
    } # end of while loop

    # Convert the data back to raster
    if (!allowOverlap & !returnDistances & !spreadStateExists) {
      if (lowMemory) {
        wh <- ffwhich(spreads, spreads > 0) %>% as.ram
        if (returnIndices) {
          completed <- data.table(indices = wh, id = spreads[wh], active = FALSE)
          if (NROW(potentials) > 0) {
            active <- data.table(indices = potentials[, 2L],
                                 id = spreads[potentials[, 1L]],
                                 active = TRUE)
          } else {
            active <- data.table(indices = numeric(0), id = numeric(0),
                                 active = logical(0))
          }
        }
      } else {
        #wh <- spreads > 0
        if (spreadStateExists) {
          wh <- c(spreadState[!keepers]$indices, spreadsIndices)
        } else {
          wh <- spreadsIndices
        }
        #if(!all.equal(sort(wh1), which(wh))) stop("WHoad")
        if (returnIndices) {
          #completed <- which(wh) %>%
          completed <- wh %>% #which(wh) %>%
            data.table(indices = ., id = spreads[.], active = FALSE)
          if (NROW(potentials) > 0) {
            active <- data.table(indices = potentials[, 2L],
                                 id = spreads[potentials[, 1L]],
                                 active = TRUE)
          } else {
            active <- data.table(indices = numeric(0), id = numeric(0),
                                 active = logical(0))
          }
        }
      }
    }

    if (returnIndices) {
      if (allowOverlap | returnDistances | spreadStateExists) {
        keepCols <- c(3, 1, 2, 4)
        if (circle) keepCols <- c(keepCols, 5)
        allCells <- data.table(spreads[, keepCols, drop=FALSE]) # change column order to match non allowOverlap
        set(allCells, , j = "active", as.logical(allCells$active))
        setkeyv(allCells, "id")

      } else {
        allCells <- rbindlist(list(active, completed)) # active first, so next line will keep active
        #allCells <- allCells[!duplicated(allCells$indices)] # some "completed" cells are still active. Keep the active ones.
        # setkeyv(completed, c("id","indices"))
        # setkeyv(active, c("id","indices"))
        # allCells <- completed[active, active := TRUE] #rbindlist(list(completed, active)) # not a copy
        if (spreadStateExists) {
          initEventID <- unique(spreadState$id)
        } else {
          initEventID <- allCells[indices %fin% initialLoci, id]
        }
        if (!all(is.na(initialLoci))) {
          dtToJoin <- data.table(id = sort(initEventID), initialLocus = initialLoci)
        } else {
          dtToJoin <- data.table(id = numeric(0), initialLocus = numeric(0))
        }
        #if (spreadStateExists) {
        #  spreadStateInitialLoci <- spreadState[, list(id = unique(id),
        #                                               initialLocus = unique(initialLocus))]
        #  dtToJoin <- rbindlist(list(spreadStateInitialLoci, dtToJoin))
        #}
        setkeyv(dtToJoin, "id")
        setkeyv(allCells, "id")

        # tack on initialLoci
        allCells <- dtToJoin[allCells]
      }
      allCells[]
      if (exists("numRetries", envir = .spadesEnv)) {
        if (sum(allCells$active) == 0) rm("numRetries", envir = .spadesEnv)
      }
      return(allCells)
    }

    #spre <- raster(landscape)
    landscape[] <- 0
    landscape@legend@colortable <- logical(0) # remove color table
    if (allowOverlap | returnDistances) {
      if (returnDistances & !allowOverlap) {
        landscape[spreads[, "indices"]] <- spreads[, "dists"]
      } else {
        spreadsDT <- data.table(spreads);
        if (returnDistances & allowOverlap) {
           pixVal <- spreadsDT[, min(dists), by = indices]
           message("returnDistances is TRUE, allowOverlap is TRUE, but returnIndices is FALSE; ",
                   "returning minimum distance raster.")
        } else {
           pixVal <- spreadsDT[, sum(id), by = indices]
        }
        landscape[pixVal$indices] <- pixVal$V1;
      }
    } else {
      landscape[wh] <- spreads[wh]
      if (exists("potentials"))
        if (NROW(potentials) > 0)
          landscape[potentials[, 1L]] <- spreads[potentials[, 2L]]
    }
    return(landscape)
  }
)

#' Identifies all cells within a ring around the focal cells
#'
#' Identifies the cell numbers of all cells within a ring defined by  minimum
#' and maximum distances from focal cells.
#' Uses \code{\link{spread}} under the hood, with specific values set.
#' Under many situations, this will be faster than using \code{rgeos::gBuffer}
#' twice (once for smaller ring and once for larger ring, then removing the
#' smaller ring cells).
#'
#' @export
#' @docType methods
#' @return This will return  a \code{data.table} with columns as described in
#'         \code{spread} when \code{returnIndices = TRUE}.
#'
#' @author Eliot McIntire
#' @inheritParams spread
#'
#' @param minRadius Numeric. Minimum radius to be included in the ring. Note:
#'             this is inclusive, i.e., >=
#' @param maxRadius Numeric. Maximum radius to be included in the ring. Note:
#'             this is inclusive, i.e., <=
#' @param ... Any other argument passed to \code{spread}
#'
#' @name rings
#' @aliases rings
#' @rdname rings
#' @seealso \code{\link{cir}} which uses a different algorithm.
#' \code{cir} tends to be faster when there are few starting points, \code{rings}
#' tends to be faster when there are many starting points. Another difference
#' between the two functions is that \code{rings} takes the centre of the pixel
#' as the centre of a circle, whereas \code{cir} takes the exact coordinates.
#' See example.
#'
#' @seealso \code{rgeos::gBuffer}
#'
#' @examples
#' library(raster)
#'
#' # Make random forest cover map
#' emptyRas <- raster(extent(0,1e2,0,1e2), res = 1)
#'
#' # start from two cells near middle
#' loci <- (ncell(emptyRas)/2 - ncol(emptyRas))/2 + c(-3, 3)
#'
#' # Allow overlap
#' emptyRas[] <- 0
#' Rings <- rings(emptyRas, loci = loci, allowOverlap = TRUE, returnIndices = TRUE)
#' # Make a raster that adds together all id in a cell
#' wOverlap <- Rings[,list(sumEventID=sum(id)),by="indices"]
#' emptyRas[wOverlap$indices] <- wOverlap$sumEventID
#' if (interactive()) {
#'   clearPlot()
#'   Plot(emptyRas)
#' }
#'
#' # No overlap is default, occurs randomly
#' emptyRas[] <- 0
#' Rings <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9, returnIndices = TRUE)
#' emptyRas[Rings$indices] <- Rings$id
#' if (interactive()) {
#'   clearPlot()
#'   Plot(emptyRas)
#' }
#'
#' # Variable ring widths, including centre cell for smaller one
#' emptyRas[] <- 0
#' Rings <- rings(emptyRas, loci = loci, minRadius = c(0,7), maxRadius = c(8, 18),
#'                returnIndices = TRUE)
#' emptyRas[Rings$indices] <- Rings$id
#' if (interactive()) {
#'   clearPlot()
#'   Plot(emptyRas)
#' }
setGeneric("rings", function(landscape, loci = NA_real_,
                             id = FALSE,
                             minRadius = 2, maxRadius = 5,
                             allowOverlap = FALSE, returnIndices = FALSE,
                             returnDistances = TRUE,
                              ...) {
  standardGeneric("rings")
})

#' @importFrom fpCompare %<=% %>=%
#' @rdname rings
setMethod(
     "rings",
     signature(landscape = "RasterLayer"),
     definition = function(landscape, loci,
                           id,
                           minRadius, maxRadius,
                           allowOverlap, returnIndices,
                           returnDistances,
                           ...) {
       spreadEvents <- spread(landscape, loci = loci, circle = TRUE,
                              circleMaxRadius = maxRadius, spreadProb = 1, id = TRUE,
                              returnDistances = TRUE, returnIndices = TRUE,
                              allowOverlap = allowOverlap, ...)
       if (length(minRadius) > 1 | length(maxRadius) > 1) {
         len <- length(loci)
         if (!(length(minRadius) == len | length(maxRadius) == len)) {
           warning("minRadius and maxRadius should be length 1 or same length as loci. ",
                   "Recycling values which may not produce desired effects.")
         }
         minRadius <- rep(minRadius, length.out = len)
         maxRadius <- rep(maxRadius, length.out = len)
         out <- rbindlist(lapply(seq_along(loci), function(j) {
           spreadEvents[id == j & (dists %>=% minRadius[j] & dists %<=% maxRadius[j])]
         }))

       } else {
         out <- spreadEvents[(dists %>=% minRadius)]
       }

       if (!returnIndices) {
         outRas <- numeric(ncell(landscape))
         if (returnDistances)
           outRas[] <- NA_real_
         else
           outRas[] <- 0

         if (allowOverlap) {
           if (returnDistances) {
             out2 <- out[, list(mDists = mean(dists)), by = indices]
             outRas[out2$indices] <- out2$mDists
           } else {
             out2 <- out[, list(sumID = sum(id)), by = indices]
             outRas[out2$indices] <- out2$sumID
           }
         } else {
           if (returnDistances)
             outRas[out$indices] <- out$dists
           else
             outRas[out$indices] <- out$dists
         }
         outRas <- raster(extent(landscape), res = res(landscape), vals = outRas)
         return(outRas)
       }
      #if(!allowOverlap) {
      #   setkey(out, indices)
      #   out[,dup:=duplicated(indices),by=indices]
      # }
       return(out)
})

#' Calculate distances and directions between many points and many grid cells
#'
#' This is a modification of \code{\link[raster]{distanceFromPoints}} for the case of many points.
#' This version can often be faster for a single point because it does not return a RasterLayer. This is
#' different than \code{\link[raster]{distanceFromPoints}} because it does not take the minimum
#' distance from the set of points to all cells. Rather this returns the every pair-wise point distance.
#' As a result, this can be used for doing inverse distance weightings, seed rain, cumulative effects
#' of distance-based processes etc. If memory limitation is an issue, maxDistance will keep memory
#' use down, but with the consequences that there will be a maximum distance returned. This function
#' has the potential to use a lot of memory if there are a lot of \code{from} and \code{to} points.
#'
#' This function is cluster aware. If there is a cluster running, it will use it. To start a cluster
#' use \code{\link[raster]{beginCluster}}, with N being the number of cores to use. See examples in
#' \code{\link{experiment}}.
#'
#' @param from Numeric matrix with 2 or 3 or more columns. They must include x and y,
#'             representing x and y coordinates of "from" cell. If there is a column
#'             named "id", it will be "id" from \code{to}, i.e,. specific pair distances.
#'             All other columns will be included in the return value of the function.
#' @param to Numeric matrix with 2  or 3 columns (or optionally more, all of which will be returned),
#'           x and y, representing x and y coordinates of "to" cells, and
#'           optional "id" which will be matched with "id" from \code{from}. Default is all cells.
#' @param landscape RasterLayer. optional. This is only used if \code{to} is NULL, in which case
#'                  all cells are considered \code{to}
#' @param angles Logical. If \code{TRUE}, then the function will return angles in radians,
#'                  as well as distances.
#' @param maxDistance Numeric in units of number of cells. The algorithm will build the whole surface
#'                    (from \code{from} to \code{to}), but will remove all distances that are above
#'                    this distance. Using this will keep memory use down.
#' @param cumulativeFn A function that can be used to incrementally accumulate values in each \code{to}
#'                     location, as the function iterates through each \code{from}. See Details.
#' @param distFn A function. This can be a function of landscape, fromCells, toCells, and
#'               dist. If cumulativeFn is supplied, then this will be used to convert
#'               the distances to some other set of units
#'               that will be accumulated by the \code{cumulativeFn}. See Details and examples.
#' @param ... Any additional objects needed for \code{distFn}.
#'
#' @inheritParams splitRaster
#'
#' @return A sorted matrix on \code{id} with same number of rows as \code{to},
#'         but with one extra column, \code{"dists"}
#'         indicating the distance between from and to.
#'
#' @seealso \code{\link{rings}}, \code{\link{cir}}, \code{\link[raster]{distanceFromPoints}},
#' which can all be made to do the same thing, under specific combinations of arguments.
#' But each has different primary use cases. Each is also faster under different conditions.
#' For instance, if \code{maxDistance} is relatively small compared to the number of cells
#' in the \code{landscape}, then \code{\link{cir}} will likely be faster. If a minimum
#' distance from all cells in the \code{landscape} to any cell in \code{from}, then
#' \code{distanceFromPoints} will be fastest. This function scales best when there are
#' many \code{to} points or all cells are used \code{to = NULL} (which is default).
#'
#' @details
#'
#' If the user requires an id (indicating the from cell for each to cell) to be returned with
#' the fuction, the user must add an identifier to the \code{from} matrix, such as "id".
#' Otherwise, the function will only return the coordinates and distances.
#'
#' \code{distanceFromEachPoint} calls \code{.pointDistance}, which is not intended to be called
#' directly by the user.
#'
#' This function has the potential to return a very large object, as it is doing pairwise
#' distances (and optionally directions) between from and to. If there are memory
#' limitations because there are many
#' \code{from} and many \code{to} points, then \code{cumulativeFn} and \code{distFn} can be used.
#' These two functions together will be used iteratively through the \code{from} points. The
#' \code{distFn} should be a transformation of distances to be used by the
#' \code{cumulativeFn} function. For example, if \code{distFn} is 1/(1+x), the default,
#' and \code{cumulativeFn} is \code{`+`}, then it will do a sum of inverse distance weights.
#' See examples.
#'
#' @name distanceFromEachPoint
#' @aliases distanceFromEachPoint
#' @export
#' @importFrom raster getCluster returnCluster
#' @importFrom parallel clusterApply
#' @rdname distances
#' @example inst/examples/example_distanceFromEachPoint.R
#'
distanceFromEachPoint <- function(from, to = NULL, landscape, angles = NA_real_,
                                  maxDistance = NA_real_, cumulativeFn = NULL,
                                  distFn = function(dist) 1 / (1 + dist), cl, ...) {
  matched <- FALSE
  fromColNames <- colnames(from)
  otherFromCols <- is.na(match(fromColNames, c("x", "y", "id")))

  if ("id" %in% fromColNames) {
    ids <- unique(from[, "id"])
  }
  if ("id" %in% colnames(to)) {
    matched <- TRUE
  }
  if (is.null(to)) {
    to <- xyFromCell(landscape, 1:ncell(landscape))
  }
  if (!is.null(cumulativeFn)) {
    forms <- names(formals(distFn))
    fromC <- "fromCell" %fin% forms
    if (fromC) fromCell <- cellFromXY(landscape, from[, c("x", "y")])
    toC <- "toCell" %fin% forms
    if (toC) toCell <- cellFromXY(landscape, to[, c("x", "y")])
    land <- "landscape" %fin% forms
    distFnArgs <- if (land) list(landscape = landscape[]) else NULL
    if (length(list(...)) > 0) distFnArgs <- append(distFnArgs, list(...))
    xDist <- "dist" %fin% forms
  }

  if (!matched) {
    nrowFrom <- NROW(from)
    if (nrowFrom > 1) {
      if (is.null(cumulativeFn)) {
        if ((any(otherFromCols) | isTRUE(angles))  ) {
          out <- lapply(seq_len(nrowFrom), function(k) {
             out <- .pointDistance(from = from[k, , drop = FALSE], to = to,
                                   angles = angles, maxDistance = maxDistance,
                                   otherFromCols = otherFromCols)
           })
          out <- do.call(rbind, out)
        } else {
          maxDistance2 <- if (is.na(maxDistance)) Inf else maxDistance
          out <- pointDistance3(fromX = from[, "x"], toX = to[, "x"],
                              fromY = from[, "y"], toY = to[, "y"],
                              maxDistance = maxDistance2)
        }
      } else {
        # if there is a cluster, then there are two levels of cumulative function,
        #  inside each cluster and outside, or "within and between clusters".
        #  This is the outer one.
        #  The inner one is the one defined by the user argument.
        outerCumFun <- function(x, from, fromCell, landscape, to, angles, maxDistance,
                                distFnArgs, fromC, toC, xDist, cumulativeFn, distFn) {

          cumVal <- rep_len(0, NROW(to))

          for (k in seq_len(nrowFrom)) {
            out <- .pointDistance(from = from[k, , drop = FALSE], to = to,
                                  angles = angles, maxDistance = maxDistance,
                                  otherFromCols = otherFromCols)
            indices <- cellFromXY(landscape, out[, c("x", "y")])
            if (k == 1) {
              if (fromC) distFnArgs <- append(distFnArgs, list(fromCell = fromCell[k]))
              if (toC) distFnArgs <- append(distFnArgs, list(toCell = toCell[indices]))
              if (xDist) distFnArgs <- append(distFnArgs, list(dist = out[, "dists", drop = FALSE]))
            } else {
              if (fromC) distFnArgs[["fromCell"]] <- fromCell[k]
              if (toC) distFnArgs[["toCell"]] <- toCell[indices]
              if (xDist) distFnArgs[["dist"]] <- out[, "dists"]
            }

            # call inner cumulative function
            if (length(indices) < ncell(landscape)) {
              cumVal <- do.call(
                cumulativeFn, args = list(cumVal, do.call(distFn, args = distFnArgs))
              )
            } else {
              cumVal[indices] <- do.call(
                cumulativeFn, args = list(
                  cumVal[indices], do.call(distFn, args = distFnArgs)
                )
              )
            }
          }
          return(cumVal)
        }

        if (missing(cl)) {
          cl <- tryCatch(getCluster(), error = function(x) NULL)
          on.exit(if (!is.null(cl)) returnCluster(), add = TRUE)
        }

        outerCumFunArgs <- list(landscape = landscape, to = to, angles = angles,
                          maxDistance = maxDistance, distFnArgs = distFnArgs,
                          fromC = fromC, toC = toC, xDist = xDist,
                          cumulativeFn = cumulativeFn, distFn = distFn)

        parFunFun <- function(x) {
          # this is a slightly tweaked version of outerCumFun, doing all calls
          do.call(outerCumFun, append(list(x = x, from = fromList[[x]],
                                           if (fromC) fromCell = fromCellList[[x]]), # nolint
                                      outerCumFunArgs))
        }

        if (!is.null(cl)) {
          parFun <- "clusterApply"
          seqLen <- seq_len(min(nrowFrom, length(cl)))
          inds <- rep(seq_along(cl), length.out = nrowFrom)
          fromList <- lapply(seqLen, function(ind) {
            from[inds == ind, , drop = FALSE]
          })

          if (fromC) fromCellList <- lapply(seqLen, function(ind) {
            fromCell[inds == ind]
          })
          parFunArgs <- list(cl = cl, x = seqLen , fun = parFunFun)
        } else {
          parFun <- "lapply"
          fromList <- list(from)
          if (fromC) fromCellList <- list(fromCell)
          parFunArgs <- list(X = 1, FUN = parFunFun)
        }

        # The actual call
        cumVal <- do.call(get(parFun), args = parFunArgs)

        # must cumulativeFn the separate cluster results
        while (length(cumVal) > 1) {
            cumVal[[2]] <- do.call(cumulativeFn, cumVal[1:2])
            cumVal[[1]] <- NULL
        }

        cumVal <- cumVal[[1]]

        if (is.null(to)) {
          out <- cbind(to, val = cumVal)
        } else {
          out <- cbind(to, val = cumVal[!is.na(cumVal)])
        }
      }
    } else {
      out <- .pointDistance(from = from, to = to, angles = angles,
                            maxDistance = maxDistance, otherFromCols = otherFromCols)
    }
  } else {
    out <- lapply(ids, function(k) {
      .pointDistance(from = from[from[, "id"] == k, , drop = FALSE],
                     to = to[to[, "id"] == k, , drop = FALSE],
                     angles = angles, maxDistance = maxDistance,
                     otherFromCols = otherFromCols)
    })
    out <- do.call(rbind, out)
  }
  return(out)
}

#' @aliases pointDistance
#' @keywords internal
#' @name .pointDistance
#' @rdname distances
.pointDistance <- function(from, to, angles = NA, maxDistance = NA_real_, otherFromCols = FALSE) {
  if (!is.na(maxDistance)) {
    to <- to[(abs(to[, "x"] - from[, "x"]) <= maxDistance)  &
             (abs(to[, "y"] - from[, "y"]) <= maxDistance), ]
  }

  # It is about 2x faster to use the compiled C routine from raster package
    #m1 <- to[, c("x", "y"), drop = FALSE]
    #m2 <- from[, c("x", "y"), drop = FALSE]
  #microbenchmark::microbenchmark(times = 1e2, {
   #dists <- sqrt((to[, "x"] - from[, "x"]) ^ 2 + (to[, "y"] - from[, "y"]) ^ 2)
  #  dists1 <- bSugar(to[,"x"], from[,"x"], to[,"y"], from[,"y"]),
  #  dists2 <- b8(to[,"x"], from[,"x"], to[,"y"], from[,"y"]),
  #  dists3 <- b(to[,"x"], from[,"x"], to[,"y"], from[,"y"]),
  #  cvers <- {
       #m1 <- to[, c("x", "y"), drop = FALSE]
       #m2 <- from[, c("x", "y"), drop = FALSE]

  #     dists <- .Call("distanceToNearestPoint",
  #                    to[, c("x", "y"), drop = FALSE], from[, c("x", "y"), drop = FALSE],
  #                    as.integer(0), PACKAGE = "raster")
  #  }

  #)
    # if (!is.na(angles)) {
    #   m1 <- to[, c("x", "y"), drop = FALSE]
    #   m2 <- from[, c("x", "y"), drop = FALSE]
    #   angls <- .pointDirection(m1, m2)
    #   dists <- cbind(dists = dists, angles = angls)
    # }

  # This is experimental C++ routine
  ##m1 <- to[, c("x", "y"), drop = FALSE]
  ##m2 <- from[, c("x", "y"), drop = FALSE]
  ##dists <- distC(m1[, "x"], m2[, "x"], m1[, "y"], m2[, "y"])
  # dists <- distC(to[, "x"], from[, "x"], to[, "y"], from[, "y"])
  # if (!is.na(angles)) {
  #   m1 <- to[, c("x", "y"), drop = FALSE]
  #   m2 <- from[, c("x", "y"), drop = FALSE]
  #   angls <- .pointDirection(m1, m2)
  #   dists <- cbind(dists = dists, angles = angls)
  # }

  # C call from raster
    # m1 <- to[, c("x", "y"), drop = FALSE]
    # m2 <- from[, c("x", "y"), drop = FALSE]
    #
    # dists <- .Call("distanceToNearestPoint",
    #       m1, m2, as.integer(0), PACKAGE = "raster")
    # if (!is.na(angles)) {
    #   angles <- .pointDirection(m1, m2)
    #   dists <- cbind(dists = dists, angles = angles)
    # }

    #dists <- cbind(to, dists = dists)
#  }, {
    #dists <- pointDistance2(to[,"x"], from[,"x"], to[,"y"], from[,"y"])
    dists <- pointDistance2(to, from)
    #  }
#  )

    if (!is.na(maxDistance)) {
      dists <- dists[dists[, "dists"] <= maxDistance, ]
    }
    if (any(otherFromCols)) {
      colNums <- seq_len(ncol(dists))
      dists <- cbind(dists = dists, from[, otherFromCols])
      colnames(dists)[-colNums] <- colnames(from)[otherFromCols]
    }
    return(dists)

}

#' Calculate matched point directions
#'
#' Internal function
#'
#' @rdname matchedPointDirection
#' @name .matchedPointDirection
#' @aliases matchedPointDirection
#' @keywords internal
.matchedPointDirection <- function(to, from) {
  ids <- unique(from[, "id"])
  orig <- order(to[, "id", drop = FALSE], to[, "to", drop = FALSE])
  to <- to[orig, , drop = FALSE]
  angls <- lapply(ids, function(i) {
    m1 <- to[to[, "id"] == i, c("x", "y"), drop = FALSE]
    m2 <- from[from[, "id"] == i, c("x", "y"), drop = FALSE]
    .pointDirection(m2, m1)
  })
  do.call(rbind, angls)
}

#' Calculate distances and directions between many points and many grid cells
#'
#' This is a modification of \code{\link[raster]{distanceFromPoints}} for the case of many points.
#' This version can often be faster for a single point because it does not return a RasterLayer. This is
#' different than \code{\link[raster]{distanceFromPoints}} because it does not take the minimum
#' distance from the set of points to all cells. Rather this returns the every pair-wise point distance.
#' As a result, this can be used for doing inverse distance weightings, seed rain, cumulative effects
#' of distance-based processes etc. If memory limitation is an issue, maxDistance will keep memory
#' use down, but with the consequences that there will be a maximum distance returned. This function
#' has the potential to use a lot of memory if there are a lot of \code{from} and \code{to} points.
#'
#' @param from matrix with 2 or 3 columns, x and y, representing x and y coordinates of "from" cell,
#'             and optional "id" which will be returned, and if "id" column is in \code{to},
#'             it will be matched with that.
#' @param to matrix with 2  or 3 columns (or optionally more, all of which will be returned),
#'           x and y, representing x and y coordinates of "to" cells, and
#'           optional "id" which will be matched with "id" from \code{from}. It makes no sense to
#'           have "id" column here with no "id" column in \code{from}
#' @param landscape RasterLayer. optional. This is only used if \code{to} is NULL, in which case
#'                  all cells are considered \code{to}
#' @rdname directions
#' @export
#' @seealso \code{\link{distanceFromEachPoint}}, which will also return directions if \code{angles}
#' is TRUE.
#'
#' @details \code{directionFromEachPoint} calls \code{.pointDirection}, which is
#' not intended to be called directly by the user.
#'
#' If knowing the which from cell matches with which to cell is important,
#' put a column "id" (e.g., starting cell) in the \code{from} matrix.
#'
#' @name directionFromEachPoint
#' @aliases directionFromEachPoint
#' @return A sorted matrix on \code{id} with same number of rows as \code{to},
#'         but with one extra column, \code{angles}
#'         indicating the angle in radians between from and to. For speed, this
#'         angle will be between -pi/2 and 3*pi/2. If the user wants this between
#'         say, 0 and 2*pi, then \code{angles \%\% (2*pi)} will do the trick. See example.
#'
#' @examples
#' library(raster)
#' N <- 2
#' dirRas <- raster(extent(0,40,0,40), res = 1)
#' coords <- cbind(x = round(runif(N, xmin(dirRas), xmax(dirRas)))+0.5,
#'                 y = round(runif(N, xmin(dirRas), xmax(dirRas)))+0.5,
#'                 id = 1:N)
#'
#' dirs1 <- directionFromEachPoint(from = coords, landscape = dirRas)
#' library(CircStats)
#' dirs1[, "angles"] <- deg(dirs1[,"angles"] %% (2*pi))
#' indices <- cellFromXY(dirRas,dirs1[, c("x", "y")])
#' minDir <- tapply(dirs1[, "angles"], indices, function(x) min(x)) # minimum angle
#' dirRas[] <- as.vector(minDir)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(dirRas)
#'   library(sp)
#'   start <- SpatialPoints(coords[, c("x", "y"), drop = FALSE])
#'   Plot(start, addTo = "dirRas")
#' }
directionFromEachPoint <- function(from, to = NULL, landscape) {
  matched <- FALSE
  nrowFrom <- NROW(from)
  if ("id" %in% colnames(from)) {
    ids <- unique(from[, "id"])
  } else if (nrowFrom > 1) {
    ids <- seq_len(nrowFrom)
  }

  if ("id" %in% colnames(to)) {
    matched <- TRUE
  }
  if (is.null(to))
    to <- xyFromCell(landscape, 1:ncell(landscape))
  if (!matched) {
    if (nrowFrom > 1) {
      out <- lapply(seq_len(nrowFrom), function(k) {
        out <- .pointDirection(from = from[k, , drop = FALSE], to = to)
        cbind(out, id = ids[k])
      })

      out <- do.call(rbind, out)
    } else {
      out <- .pointDirection(from = from, to = to)
    }
  } else {
    out <- lapply(ids, function(k) {
      .pointDirection(from = from[from[, "id"] == k, , drop = FALSE],
                      to = to[to[, "id"] == k, , drop = FALSE])
    })
    out <- do.call(rbind, out)
  }
}

#' Calculate the direction from a point to a set of points
#'
#' Internal function.
#'
#' @keywords internal
#' @rdname directions
.pointDirection <- function(from, to) {
  rise <- to[, "y"] - from[, "y"]
  run <- to[, "x"] - from[, "x"]
  angls <- pi / 2 - atan2(rise, run) # Convert to geographic 0 = North
  cbind(to, angles = angls)
}

#' A faster '\%in\%' based on fastmatch package
#'
#' A faster '\%in\%', directly pulled from \code{fastmatch::match}, based on
#' \url{http://stackoverflow.com/questions/32934933/faster-in-operator}.
#'
#' @param x      See \code{\link[fastmatch]{fmatch}}.
#' @param table  See \code{\link[fastmatch]{fmatch}}.
#'
#' @export
#' @importFrom fastmatch fmatch
#' @name %fin%
#' @aliases match
#' @rdname match
#'
`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}

