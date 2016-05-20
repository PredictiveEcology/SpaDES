if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("indices", "eventID", "initialLocus"))
}

###############################################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric landscape values (symmetric or asymmetric) and many other things.
#' Essentially, it starts from a collection of cells (\code{loci}) and spreads
#' to neighbours, according to the \code{directions} and \code{spreadProbLater} arguments.
#' This can become quite general, if \code{spreadProbLater} is 1 as it will expand
#' from every loci until all cells in the landscape have been covered.
#' With \code{mapID} set to \code{TRUE}, the resulting map will be classified
#' by the index of the pixel where that event propagated from.
#' This can be used to examine things like fire size distributions.
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
#' @section Breaking out of spread events:
#'
#' There are 4 ways for the spread to "stop" spreading. Here, each "event" is defined as
#' all cells that are spawned from a single starting loci. So, one spread call can have
#' multiple spreading "events". The ways outlines below are all acting at all times,
#' i.e., they are not mutually exclusive. Therefore, it is the user's
#' responsibility to make sure the different rules are interacting with
#' each other correctly:
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
#'   \code{stopRule} \tab This is a function that can use "landscape", "mapID", "cells", or any
#'                       named vector passed into \code{spread} in the \code{...}. This
#'                       can take on relatively complex functions. Passing in, say, a Raster
#'                       Layer to \code{spread} can access the individual values on that
#'                       arbitrary Raste Layer using "cells". See examples.
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
#' spreading occurs outwards in concentric circles or shapes, one pixel unit at a time, there
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
#'                              the last removed pixel and include it in the return result
#'                              for that event.\cr
#'   \code{"excludePixel"} \tab Like \code{"includePixel"}, but it will not add back the pixel
#'                        that causes \code{stopRule} to be \code{TRUE}\cr
#'   \code{"excludeRing"} \tab Analogous to \code{"excludePixel"}, but for the entire final
#'                             ring of cells added. This will exclude the entire ring of cells
#'                             that caused the \code{stopRule} to be \code{TRUE}\cr
#' }
#'
#'
#' @param landscape     A \code{RasterLayer} object. This defines the possible locations
#'                      for spreading events to start and spread into. This can also
#'                      be used as part of \code{stopRule}. Require input.
#'
#' @param loci          A vector of locations in \code{landscape}. These should be cell indexes.
#'                      If user has x and y coordinates, these can be converted with
#'                      \code{\link[raster]{cellFromXY}}.
#'
#' @param spreadProb    Numeric or rasterLayer.  The overall probability of
#'                      spreading, or probability raster driven. Default is \code{0.23}.
#'                      If a \code{spreadProbLater} is provided, then this is
#'                      only used for the first iteration. Also called Escape
#'                      probability. See section on \code{Breaking out of spread events}.
#'
#' @param persistence   A length 1 probability that an active cell will continue to burn,
#'                      per time step.
#'
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with
#'                      \code{landscape} whose elements are \code{0,1},
#'                      where 1 indicates "cannot spread to".
#'                      Currently not implemented, but identical behavior can be
#'                      achieved if \code{spreadProb} has zeros in all unspreadable
#'                      locations.
#'
#' @param maxSize       Numeric. Maximum number of cells for a single or
#'                      all events to be spread. Recycled to match \code{loci} length,
#'                      if it is not as long as \code{loci}.
#'                      See section on \code{Breaking out of spread events}.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spread.
#'                      Leaving this \code{NULL} allows the spread to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param lowMemory     Logical. If true, then function uses package \code{ff}
#'                      internally. This is slower, but much lower memory footprint.
#'
#' @param returnIndices Logical. Should the function return a data.table with
#'                      indices and values of successful spread events, or
#'                      return a raster with values. See Details.
#'
#' @param spreadProbLater    Numeric or rasterLayer. If provided, then this
#'                      will become the spreadProb after the first iteration. See details.
#'
#' @param spreadState   Data.table. This should be the output of a previous call to
#'                      \code{spread}, where \code{returnIndices} was \code{TRUE}. Default NA,
#'                      meaning the spread is starting from \code{loci}. See Details.
#'
#' @param circle        Logical. If TRUE, then outward spread will be by equidistant rings,
#'                      rather than solely by adjacent cells (via \code{direction} arg.). Default
#'                      is FALSE. Using \code{circle=TRUE} can be dramatically slower for large
#'                      problems. Note, this should usually be used with spreadProb = 1.
#'
#' @param circleMaxRadius Numeric. A further way to stop the outward spread of events. If
#'                      \code{circle} is \code{TRUE}, then it will grow to this maximum radius.
#'                      See section on
#'                      \code{Breaking out of spread events}. Default to NA.
#'
#' @param stopRule      A function which will be used to assess whether each individual cluster
#'                      should stop growing. This function can be an argument of "landscape",
#'                      "mapID", "cells", and
#'                      any other named vectors, a named list of named vectors,
#'                      or a named data.frame of with column names passed to spread in
#'                      the ... . Default NA meaning,
#'                      spreading will not stop as a function of the landscape. See section on
#'                      \code{Breaking out of spread events} and examples.
#'
#' @param stopRuleBehavior Character. Can be one of "includePixel", "excludePixel", "includeRing",
#'                      "excludeRing". If \code{stopRule} contains a function, this argument is
#'                      used determine what to do with the pixel(s) that caused the rule to be
#'                      \code{TRUE}. See details. Default is "includeRing" which means to
#'                      accept the entire ring of cells that caused the rule to be \code{TRUE}.
#'
#' @param allowOverlap  Logical. If \code{TRUE}, then individual events can overlap with one
#'                      another, i.e., they do not interact. Currently, this is slower than
#'                      if \code{allowOverlap} is \code{FALSE}. Default is \code{FALSE}.
#'
#' @param ...           Additional named vectors or named list of named vectors
#'                      required for \code{stopRule}. These
#'                      vectors should be as long as required e.g., length
#'                      \code{loci} if there is one value per event.
#'
#' @return Either a \code{RasterLayer} indicating the spread of the process in
#' the landscape or a \code{data.table} if \code{returnIndices} is \code{TRUE}.
#' If a \code{RasterLayer}, then it represents
#' every pixel in which a successful spread event occurred. For the case of, say, a fire
#' this would represent every pixel that burned. If \code{allowOverlap} is \code{TRUE},
#' This Raster layer will represent the sum of the individual event ids (which
#' are numerics \code{seq_along(loci)}. This will
#' generally be of minimal use because it won't be possible to distinguish if
#' event 2 overlapped with event 5 or if it was just event 7.
#'
#' If \code{returnIndices} is \code{TRUE},
#' then this function returns a \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{eventID} \tab an arbitrary ID \code{1:length(loci)} identifying
#'                      unique clusters of spread events, i.e., all cells
#'                      that have been spread into that have a
#'                      common initial pixel.\cr
#'   \code{initialLocus} \tab the initial pixel number of that particular
#'                            spread event.\cr
#'   \code{indices} \tab The pixel indices of cells that have
#'                        been touched by the spread algorithm.\cr
#'   \code{active} \tab a logical indicating whether the pixel is active (i.e.,
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
#'
#' @name spread
#' @aliases spread
#' @rdname spread
#'
setGeneric("spread", function(landscape, loci = NA_real_,
                              spreadProb = 0.23,
                              persistence = 0, mask = NA, maxSize = 1e8L,
                              directions = 8L, iterations = 1e6L,
                              lowMemory = getOption("spades.lowMemory"),
                              returnIndices = FALSE, mapID = FALSE, plot.it = FALSE,
                              spreadProbLater = NA_real_, spreadState = NA,
                              circle = FALSE, circleMaxRadius = NA_real_,
                              stopRule = NA, stopRuleBehavior = "includeRing",
                              allowOverlap = FALSE,
                              ...) {
  standardGeneric("spread")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spread event grow.
#'
#' @param mapID    Logical. If TRUE, returns a raster of events ids.
#'                 If FALSE, returns a raster of iteration numbers,
#'                 i.e., the spread history of one or more events. NOTE:
#'                 this is overridden if \code{returnIndices} is \code{TRUE}.
#'
#' @rdname spread
#'
#' @examples
#' library(raster)
#' library(RColorBrewer)
#'
#' # Make random forest cover map
#' emptyRas <- raster(extent(0,1e2,0,1e2), res = 1)
#' hab <- gaussMap(emptyRas,speedup = 1) # if raster is large (>1e6 cells), use speedup>1
#' names(hab) = "hab"
#' mask <- raster(emptyRas)
#' mask <- setValues(mask, 0)
#' mask[1:5000] <- 1
#' numCol <- ncol(emptyRas)
#' numCell <- ncell(emptyRas)
#' directions <- 8
#'
#' # Can use transparent as a color
#' setColors(hab) <- paste(c("transparent", brewer.pal(8, "Greys")))
#'
#' Plot(hab, new = TRUE, speedup = 3) # note speedup is equivalent to making pyramids,
#'                              # so, some details are lost
#'
#' # initiate 10 fires
#' startCells <- as.integer(sample(1:ncell(emptyRas),10))
#' fires <- spread(hab, loci = startCells,
#'                 0.235, 0, NULL, 1e8, 8, 1e6, mapID = TRUE)
#' #set colors of raster, including a transparent layer for zeros
#' setColors(fires, 10) <- c("transparent", brewer.pal(8,"Reds")[5:8])
#' Plot(fires, new = TRUE)
#' Plot(fires,addTo = "hab")
#'
#' #alternatively, set colors using cols= in the Plot function
#' Plot(hab, new = TRUE)
#' Plot(fires) # default color range makes zero transparent.
#' # Instead, to give a color to the zero values, use \code{zero.color=}
#' Plot(fires, addTo = "hab",
#'      cols = colorRampPalette(c("orange","darkred"))(10))
#' hab2 <- hab
#' Plot(hab2)
#' Plot(fires, addTo = "hab2", zero.color = "white",
#'      cols = colorRampPalette(c("orange","darkred"))(10))
#' # or overplot the original (NOTE: legend stays at original values)
#' Plot(fires,
#'      cols = topo.colors(10))
#'
#' ####################
#' ## Continue event by passing interrupted object into spreadState
#' ####################
#'
#' ## Interrupt a spread event using iterations - need returnIndices=TRUE to use outputs
#' ##   as new inputs in next iteration
#' fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), returnIndices=TRUE,
#'                 0.235, 0, NULL, 1e8, 8, iterations = 3, mapID = TRUE)
#' fires[,list(size=length(initialLocus)), by=eventID]  # See sizes of fires
#'
#' fires2 <- spread(hab, loci=NA_real_, returnIndices=TRUE, 0.235,
#'                  0, NULL, 1e8, 8, iterations = 2, mapID = TRUE,
#'                  spreadState=fires)
#' # NOTE events are assigned arbitrary IDs, starting at 1
#'
#' ## Add new fires to the already burning fires
#' fires3 <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), returnIndices=TRUE,
#'                      0.235, 0, NULL, 1e8, 8, iterations = 1, mapID = TRUE,
#'                                           spreadState=fires)
#' fires3[,list(size=length(initialLocus)), by=eventID]  # See sizes of fires
#' # NOTE old eventIDs are maintained, new events get ids begining above previous
#' # maximum (e.g., new fires 11 to 20 here)
#'
#' ## Use data.table and loci...
#' fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), returnIndices=TRUE,
#'                 0.235, 0, NULL, 1e8, 8, iterations = 2, mapID = TRUE)
#' fullRas <- raster(hab)
#' fullRas[] <- 1:ncell(hab)
#' burned <- fires[active == FALSE]
#' burnedMap <- rasterizeReduced(burned, fullRas, "eventID", "indices")
#' Plot(burnedMap, new=TRUE)
#'
#' ####################
#' ## stopRule examples
#' ####################
#'
#' # examples with stopRule, which means that the eventual size is driven by the values on the raster
#' #  passed in to the landscape argument
#' set.seed(1234)
#' stopRule1 <- function(landscape) sum(landscape)>50
#' stopRuleA <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0,
#'                 NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1)
#'
#' set.seed(1234)
#' stopRule2 <- function(landscape) sum(landscape)>100
#' # using stopRuleBehavior = "excludePixel"
#' stopRuleB <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0,
#'                 NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule2,
#'                 stopRuleBehavior = "excludePixel")
#'
#' # using stopRuleBehavior = "includeRing", means that end result is slightly larger patches, as a
#' #  complete "iteration" of the spread algorithm is used.
#' set.seed(1234)
#' stopRuleB_NotExact <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0,
#'                 NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule2)
#' Plot(stopRuleA, stopRuleB, stopRuleB_NotExact, new=TRUE)
#'
#' # Test that the stopRules work
#' # stopRuleA was not exact, so each value will "overshoot" the stopRule, here it was hab>50
#' foo <- cbind(vals=hab[stopRuleA], mapID = stopRuleA[stopRuleA>0]);
#' tapply(foo[,"vals"], foo[,"mapID"], sum) # Correct ... all are above 50
#'
#' # stopRuleB was exact, so each value will be as close as possible while rule still is TRUE
#' #  Because we have discrete cells, these numbers will always slightly under the rule
#' foo <- cbind(vals=hab[stopRuleB], mapID = stopRuleB[stopRuleB>0]);
#' tapply(foo[,"vals"], foo[,"mapID"], sum) # Correct ... all are above 50
#'
#' # stopRuleB_notExact will overshoot
#' foo <- cbind(vals=hab[stopRuleB_NotExact], mapID = stopRuleB_NotExact[stopRuleB_NotExact>0]);
#' tapply(foo[,"vals"], foo[,"mapID"], sum) # Correct ... all are above 50
#'
#'
#' # Cellular automata shapes
#' # Diamonds - can make them with: a boolean raster, directions = 4,
#' #    stopRule in place, spreadProb = 1
#' diamonds <- spread(hab>0, spreadProb = 1, directions = 4,
#'    mapID = TRUE, stopRule = stopRule2)
#' Plot(diamonds, new=TRUE)
#'
#' # Squares - can make them with: a boolean raster, directions = 8,
#' #    stopRule in place, spreadProb = 1
#' squares <- spread(hab>0, spreadProb = 1, directions = 8,
#'    mapID = TRUE, stopRule = stopRule2)
#' Plot(squares)
#'
#' # Interference shapes - can make them with: a boolean raster, directions = 8,
#' #    stopRule in place, spreadProb = 1
#' stopRule2 <- function(landscape) sum(landscape)>200
#' squashedDiamonds <- spread(hab>0, spreadProb = 1, loci = (ncell(hab)-ncol(hab))/2 + c(4, -4),
#'    directions = 4, mapID = TRUE, stopRule = stopRule2)
#' Plot(squashedDiamonds, new=TRUE)
#'
#' # Circles with spreadProb < 1 will give "more" circular shapes, but definitely not circles
#' stopRule2 <- function(landscape) sum(landscape)>200
#' seed <- sample(1e4,1)
#' set.seed(seed)
#' circlish <- spread(hab>0, spreadProb = 0.23, loci = (ncell(hab)-ncol(hab))/2 + c(4, -4),
#'    directions = 8, mapID = TRUE, circle = TRUE)#, stopRule = stopRule2)
#' set.seed(seed)
#' regularCA <- spread(hab>0, spreadProb = 0.23, loci = (ncell(hab)-ncol(hab))/2 + c(4, -4),
#'    directions = 8, mapID = TRUE)#, stopRule = stopRule2)
#'    print(seed)
#' Plot(circlish, regularCA, new=TRUE)
#'
#'
#' ####################
#' # complex stopRule
#' ####################
#'
#' initialLoci <- sample(seq_len(ncell(hab)), 2)#(ncell(hab)-ncol(hab))/2 + c(4, -4)
#' endSizes <- seq_along(initialLoci)*200
#'
#' # Can be a function of landscape, mapID, and/or any other named
#' #   variable passed into spread
#'
#' stopRule3 <- function(landscape, mapID, endSizes) sum(landscape)>endSizes[mapID]
#'
#' TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
#'    directions = 8, mapID = TRUE, stopRule = stopRule3, endSizes = endSizes,
#'    stopRuleBehavior = "excludePixel")
#' # or using named list of named elements:
#' #TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
#' #    directions = 8, mapID = TRUE, stopRule = stopRule3,
#' #    vars = list(endSizes = endSizes), stopRuleBehavior = "excludePixel")
#'
#' Plot(TwoCirclesDiffSize, new=TRUE)
#' cirs <- getValues(TwoCirclesDiffSize)
#' vals <- tapply(hab[TwoCirclesDiffSize], cirs[cirs>0], sum)
#'
#' # Stop if sum of landscape is big or mean of quality is too small
#' quality <- raster(hab)
#' quality[] <- runif(ncell(quality), 0, 1)
#' stopRule4 <- function(landscape, quality, cells) (sum(landscape)>20) | (mean(quality[cells])<0.3)
#'
#' TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
#'    directions = 8, mapID = TRUE, stopRule = stopRule4, quality = quality,
#'    stopRuleBehavior = "excludePixel")
#'
#' ##############
#' # allowOverlap
#' ##############
#'  set.seed(3113)
#'  initialLoci <- as.integer(sample(1:ncell(hab), 10))
#'  # using "landscape", "mapID", and a variable passed in
#'  maxVal <- rep(500,length(initialLoci))
#'  # define stopRule
#'  stopRule2 <- function(landscape,mapID,maxVal) sum(landscape)>maxVal[mapID]
#'  circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
#'                    mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior="includeRing",
#'                                      maxVal = maxVal, returnIndices = TRUE)
#'  (vals <- tapply(hab[circs$indices], circs$eventID, sum))
#'  vals<=maxVal # All TRUE
#'  overlapEvents <- raster(hab)
#'  overlapEvents[] <- 0
#'  toMap <- circs[,sum(eventID),by=indices]
#'  overlapEvents[toMap$indices] <- toMap$V1
#'  Plot(overlapEvents, new=TRUE)
#'
#'
setMethod(
  "spread",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, spreadProb,
                        persistence, mask,
                        maxSize, directions, iterations,
                        lowMemory, returnIndices, mapID,
                        plot.it, spreadProbLater, spreadState,
                        circle, circleMaxRadius, stopRule, stopRuleBehavior,
                        allowOverlap,
                        ...) {

    if(!any(stopRuleBehavior %in% c("includePixel","excludePixel","includeRing","excludeRing")))
      stop("stopRuleBehaviour must be one of \"includePixel\", \"excludePixel\", \"includeRing\", or \"excludeRing\"")
    spreadStateExists <- is(spreadState, "data.table")
    if (!is(spreadProbLater, "Raster")) {
      if (is.na(spreadProbLater)) {
        spreadProbLater <- spreadProb
      }
    }
    ### should sanity check map extents
    if (any(is.na(loci)))  {
      # start it in the centre cell, if there is no spreadState
      if (!spreadStateExists)
        loci <- (nrow(landscape)/2L + 0.5) * ncol(landscape)
    }

    if (spreadStateExists) {
      loci <- loci[!(loci %in% spreadState[,indices])] # keep these for later
      initialLoci <- loci
    } else {
      initialLoci <- loci
    }

    if (is(spreadProbLater,"RasterLayer") | is(spreadProb, "Rasterlayer")) {
      if ( (minValue(spreadProb)>1L) || (maxValue(spreadProb)<0L) ) {
        stop("spreadProb is not a probability")
      }
      if ( (minValue(spreadProbLater)>1L) || (maxValue(spreadProbLater)<0L) ) {
        stop("spreadProbLater is not a probability")
      }
    } else {
      if (!inRange(spreadProb)) stop("spreadProb is not a probability")
      if (!inRange(spreadProbLater)) stop("spreadProbLater is not a probability")
    }

    if(!allowOverlap) {
      if (lowMemory) { # create vector of 0s called spreads, which corresponds to the
                       #   indices of the landscape raster
        spreads <- ff(vmode = "short", 0, length = ncell(landscape))
      } else {
        spreads <- vector("integer", ncell(landscape))
      }
    } else {
      spreads <- cbind(initialLocus=initialLoci, indices=initialLoci, eventID=1:length(loci), active=1)
    }

    n <- 1L

    # circle needs directions to be 8
    if(!missing(circle)) {
      if(circle) {
        directions = 8L
        initialLociXY <- cbind(mapID = seq_along(initialLoci), xyFromCell(landscape, initialLoci))
        mapID <- TRUE
      }
    }

    # determine ... variables
    otherVars <- list(...)
    anyList <- unlist(lapply(otherVars,is.list) )

    if(any(anyList)) {
      otherVarsLists <- unlist(unname(otherVars), recursive = FALSE)
      otherVars[anyList] <- NULL
      otherVars <- append(otherVars, otherVarsLists)
    }

    # check validity of stopRule
    if(is.function(stopRule)){
      mapID <- TRUE
      if(any(is.na(match(names(formals(stopRule)),
                         c("mapID", "landscape", "cells", names(otherVars)))))){
        stop(paste("Arguments in stopRule not valid. The function definition",
             "must be a function of built-in options, ",
             "(mapID, landscape, or cells) or user supplied variables.",
             "If user supplied the variables",
             "must be passed as named vectors, or lists or data.frames.",
             " See examples."))
      }
    }

    if(!allowOverlap) {
      if (mapID | returnIndices) { # give values to spreads vector at initialLoci
        spreads[loci] <- 1L:length(loci)
      } else {
        spreads[loci] <- n
      }
    }

    # Convert mask and NAs to 0 on the spreadProbLater Raster
    if (is(spreadProbLater, "Raster")) {
      # convert NA to 0s
      spreadProbLater[is.na(spreadProbLater)] <- 0L
    } else if (is.numeric(spreadProbLater)) {
      # Translate numeric spreadProbLater into a Raster, if there is a mask
      if (is(mask, "Raster")) {
        spreadProbLater <- raster(extent(landscape), res = res(landscape), vals = spreadProbLater)
      }
    }

    # Convert mask and NAs to 0 on the spreadProb Raster
    if (is(spreadProb, "Raster")) {
      # convert NA to 0s
      spreadProb[is.na(spreadProb)] <- 0L
    } else if (is.numeric(spreadProb)) {
      # Translate numeric spreadProb into a Raster, if there is a mask
      if (is(mask, "Raster")) {
        spreadProb <- raster(extent(landscape), res = res(landscape), vals = spreadProb)
      }
    }

    # Mask spreadProbLater and spreadProb
    if (is(mask, "Raster")) {
      spreadProbLater[mask == 1L] <- 0L
    }
    if (is(mask, "Raster")) {
      spreadProb[mask == 1L] <- 0L
    }

    if (spreadStateExists) {
      if(!allowOverlap) {
        if (sum(colnames(spreadState) %in% c("indices", "eventID", "active", "initialLocus")) == 4) {
          spreads[loci] <- spreads[loci] + spreadState[, max(eventID)] # reassign old ones
          spreads[spreadState[,indices]] <- spreadState[, eventID]
          loci <- c(spreadState[active == TRUE, indices], loci) %>% na.omit()
        } else {
          stop("spreadState must have at least columns: ",
               "indices, eventID, active, and initialLocus.")
        }
      } else {
        stop("Using spreadState and allowOverlap = TRUE is not implemented")
      }
    }

    ## Recycling maxSize as needed
    if (any(!is.na(maxSize))) {
      if(!is.integer(maxSize)) maxSize <- floor(maxSize)
      if (spreadStateExists) {
        sizeAll <- spreadState[, list(len = length(initialLocus)), by = eventID]
        maxSize <- rep_len(maxSize, length(initialLoci) + NROW(sizeAll))
        size <- c(sizeAll[, len], rep_len(1L, length(initialLoci)))
      } else {
        maxSize <- rep_len(maxSize, length(loci))
        size <- rep_len(1L, length(loci))
      }
    } else {
      maxSize <- ncell(landscape)
      size <- length(loci)
    }

    # while there are active cells
    while (length(loci) & (n <= iterations) ) {

      # identify neighbours
      if(!allowOverlap) {
        if (mapID | returnIndices | circle) {
          potentials <- adj(landscape, loci, directions, pairs = TRUE)
        } else {
          # must pad the first column of potentials
          potentials <- cbind(NA, adj(landscape, loci, directions, pairs = FALSE))
        }
      } else {
        whActive <- spreads[,"active"]==1 # spreads carries over
        potentials <- adj(landscape, loci, directions, pairs = TRUE, id = spreads[whActive,"eventID"])
        spreads[whActive,"active"] <- 0
      }

      # keep only neighbours that have not been spread to yet
      if(!allowOverlap) {
        potentials <- potentials[spreads[potentials[, 2L]] == 0L, , drop = FALSE]
      } else {

        potentials <- cbind(potentials, active=1)
        colnames(potentials) <- colnames(spreads)
        potentials[,"initialLocus"] <- initialLoci[potentials[,"eventID"]]
        d <- rbind(spreads, potentials)
        d <- d[order(d[,"eventID"],d[,"indices"]),]
        keepsMat <- unlist(tapply(d[,"indices"], d[,"eventID"],function(x) !duplicated(x)))
        indices <- unlist(tapply(d[,"indices"], d[,"eventID"],function(x) x))
        potentials <- d[keepsMat & d[,"active"]==1,]

      }

      if (n == 2) {
        spreadProb <- spreadProbLater
      }

      if (is.numeric(spreadProb)) {
        if (n == 1 & spreadStateExists) { # need cell specific values
          spreadProbs <- rep(spreadProb, NROW(potentials))
          prevIndices <- potentials[, 1L] %in% spreadState[active == TRUE, indices]
          spreadProbs[prevIndices] <- spreadProbLater
        } else {
          spreadProbs <- spreadProb
        }
      } else {
        if (n == 1 & spreadStateExists) { # need cell specific values
          spreadProbs <- spreadProb[potentials[, 2L]]
          prevIndices <- potentials[, 1L] %in% spreadState[active == TRUE, indices]
          spreadProbs[prevIndices] <- spreadProbLater
        } else {
          spreadProbs <- spreadProb[potentials[, 2L]]
        }
      }
      if(any(spreadProbs<1)) {
        potentials <- potentials[runif(NROW(potentials)) <= spreadProbs, , drop = FALSE]
      }
      potentials <- potentials[sample.int(NROW(potentials)), , drop = FALSE] # random ordering so not always same
      if(!allowOverlap) {
        potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
      }

      if(length(potentials)>0) {# potentials can become zero because all active cells are edge cells
        # implement circle
        if(!missing(circle)) {
          if(circle) {
            if(!allowOverlap){
              a <- cbind(mapID=spreads[potentials[, 1L]], to=potentials[, 2L], xyFromCell(landscape, potentials[, 2L]))
            } else {
              a <- cbind(mapID=potentials[, 3L], to=potentials[, 2L], xyFromCell(landscape, potentials[, 2L]))
            }
            d <- .matchedPointDistance(a, initialLociXY)
            cMR <- n
            if(!any(is.na(circleMaxRadius))){
              if(any(circleMaxRadius<=n)){ # don't bother proceeding if circleMaxRadius is larger than current iteration
                if(length(circleMaxRadius)>1) { # if it is a vector of values
                  cMR <- circleMaxRadius[d[,"mapID"]]
                } else {
                  cMR <- circleMaxRadius
                }
              }
            }
            potentials <- potentials[(d[,"dists"] %<=% cMR),, drop = FALSE]
          }
        }


        events <- potentials[, 2L]

        # Implement maxSize
        if (length(maxSize) == 1L) {

          len <- length(events)
          if ((size + len) > maxSize) {
            keep <- len - ((size + len) - maxSize)
            samples <- sample(len, keep)
            events <- events[samples]
            potentials <- potentials[samples, , drop = FALSE]
          }
          size <- size + length(events)
        } else {
          if(!allowOverlap){
            len <- tabulate(spreads[potentials[, 1L]], length(maxSize))
          } else {
            len <- tabulate(potentials[, 3L], length(maxSize))
          }
          if ( any( (size + len) > maxSize & size <= maxSize) ) {
            whichID <- which(size + len > maxSize)
            toRm <- (size + len)[whichID] - maxSize[whichID]
            for (i in 1:length(whichID)) {
              if(!allowOverlap) {
                thisID <- which(spreads[potentials[, 1L]] == whichID[i])
              } else {
                thisID <- which(potentials[, 3L] == whichID[i])
              }

              potentials <- potentials[-sample(thisID, toRm[i]), , drop = FALSE]
            }
            events <- potentials[, 2L]
          }
          size <- pmin(size + len, maxSize) ## Quick? and dirty. fast but loose (too flexible)
        }

        # Implement stopRule section
        if(is.function(stopRule) & length(events)>0){

          if(!allowOverlap) {
            prevCells <- cbind(mapID=spreads[spreads>0],
                               landscape = landscape[spreads>0], cells = which(spreads>0), prev=1)
            eventCells <- cbind(mapID = spreads[potentials[, 1L]],
                                landscape = landscape[events], cells = events, prev=0)
          } else {
            prevCells <- cbind(mapID=spreads[,"eventID"],
                               landscape = landscape[spreads[,"indices"]],
                               cells = spreads[,"indices"], prev=1)
            eventCells <- cbind(mapID = potentials[, "eventID"],
                                landscape = landscape[events],
                                cells = events, prev=0)
          }
          tmp <- rbind(prevCells[prevCells[,"mapID"] %in%
                                   unique(eventCells[,"mapID"]),], eventCells) # don't need to continue doing ids that are not active

          ids <- unique(tmp[,"mapID"])

          shouldStop <- unlist(lapply(ids, function(mapID) {
            args <- append(as.data.frame(tmp[tmp[,"mapID"]==mapID,]), otherVars)
            args <- args[-(names(args)=="mapID")]
            args <- append(args, list(mapID=mapID))
            wh <- match(names(formals(stopRule)),names(args))
            do.call(stopRule, args[wh])
          }))

          names(shouldStop) <- ids

          if(any(shouldStop)) {
            if(stopRuleBehavior!="includeRing") {
              if(stopRuleBehavior!="excludeRing") {
                whStop <- as.numeric(names(shouldStop)[shouldStop])
                whStopAll <- tmp[,"mapID"] %in% whStop
                tmp2 <- tmp[whStopAll,]

                whStopEvents <- eventCells[,"mapID"] %in% whStop

                out <- lapply(whStop, function(mapID) {
                  tmp3 <- tmp2[tmp2[,"mapID"]==mapID,]
                  newOnes <- tmp3[,"prev"]==0
                  ord <- seq_along(newOnes)
                  ord[newOnes] <- sample(ord[newOnes])
                  tmp3 <- tmp3[ord,]
                  startLen <- sum(!newOnes)
                  addIncr <- 1
                  done <- FALSE
                  while(!done) {
                    args <- append(as.data.frame(tmp3[1:(startLen+addIncr),]), otherVars)
                    args <- args[-(names(args)=="mapID")]
                    args <- append(args, list(mapID=mapID))
                    wh <- match(names(formals(stopRule)),names(args))
                    done <- do.call(stopRule, args[wh])
                    addIncr <- addIncr+1
                  }
                  if(stopRuleBehavior=="excludePixel") addIncr <- addIncr - 1
                  firstInd <- startLen+addIncr
                  lastInd <- NROW(tmp3)
                  sequ <- if(firstInd>lastInd) 0 else firstInd:lastInd
                  tmp3[sequ,,drop=FALSE]#)
                })
                eventRm <- do.call(rbind, out)[,"cells"]
                cellsKeep <- !(potentials[,2L] %in% eventRm)
              } else {
                cellsKeep <- rep(FALSE, NROW(potentials))
              }
              potentials <- potentials[cellsKeep,,drop=FALSE]
              events <- potentials[,2L]
              eventCells <- eventCells[cellsKeep,,drop=FALSE]

            }
            toKeepSR <- !(eventCells[,"mapID"] %in% as.numeric(names(which((shouldStop)))))
          }

        }

        # increment iteration
        n <- n + 1L

        if (length(events) > 0){ # place new value at new cells that became active
          if(!allowOverlap) {
            if (mapID | returnIndices) {
              spreads[events] <- spreads[potentials[, 1L]]
            } else {
              spreads[events] <- n
            }
          } else {
            spreads <- rbind(spreads, potentials)
          }
        }

        # remove the cells from "events" that push it over maxSize
        if (length(maxSize) > 1L) {
          if (exists("whichID", inherits = FALSE)) {
            if(!allowOverlap) {
              maxSizeKeep <- !spreads[events] %in% whichID
            } else {
              maxSizeKeep <- !(spreads[spreads[,"active"]==1,"eventID"] %in% whichID)
              spreads <- spreads[c(rep(TRUE, sum(spreads[,"active"]==0)),maxSizeKeep),]
            }
            events <- events[maxSizeKeep]
            if(exists("toKeepSR",inherits = FALSE)) { # must update toKeepSR in case that is a second reason to stop event
              toKeepSR <- toKeepSR[maxSizeKeep]
            }
            rm(whichID)
          }

        } else {
          if (size >= maxSize) {
            events <- NULL
          }
        }

        # Remove cells that were stopped by stopRule
        if (is.function(stopRule)) {
          if (exists("toKeepSR", inherits = FALSE)) {
            events <- events[toKeepSR]
            if(allowOverlap) {
              spreads[c(rep(TRUE, sum(spreads[,"active"]==0)),!toKeepSR),"active"] <- 0
            }
            rm(toKeepSR)
          }
        }

      } else {
        events <- NULL
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
        if(n==2) clearPlot()
        if(!allowOverlap) {
          plotCur <- raster(landscape)
          plotCur <- setValues(plotCur, spreads)
          Plot(plotCur)
        } else {
          spreadsDT <- data.table(spreads);
          hab2 <- landscape;
          hab2[] <- 0;
          pixVal <- spreadsDT[,sum(eventID),by=indices]
          hab2[pixVal$indices] <- pixVal$V1;
          Plot(hab2, legendRange = c(0,sum(seq_along(initialLoci))))
        }

      }
      loci <- c(loci, events)
    }

    # Convert the data back to raster
    if(!allowOverlap) {
      if (lowMemory){
        wh <- ffwhich(spreads, spreads > 0) %>% as.ram
        if (returnIndices) {
          completed <- data.table(indices = wh, eventID = spreads[wh], active = FALSE)
          if (NROW(potentials) > 0) {
            active <- data.table(indices = potentials[, 2L],
                                 eventID = spreads[potentials[, 1L]],
                                 active = TRUE)
          } else {
            active <- data.table(indices = numeric(0), eventID = numeric(0),
                                 active = logical(0))
          }
        }
      } else {

        wh <- spreads > 0
        if (returnIndices) {
          completed <- which(wh) %>%
            data.table(indices = ., eventID = spreads[.], active = FALSE)
          if (NROW(potentials) > 0) {
            active <- data.table(indices = potentials[, 2L],
                                 eventID = spreads[potentials[, 1L]],
                                 active = TRUE)
          } else {
            active <- data.table(indices = numeric(0), eventID = numeric(0),
                                 active = logical(0))
          }
        }
      }
    }

    if (returnIndices) {
      if(!allowOverlap) {
        allCells <- rbindlist(list(completed, active))
        initEventID <- allCells[indices %in% initialLoci, eventID]
        if (!all(is.na(initialLoci))) {
          dtToJoin <- data.table(eventID = sort(initEventID), initialLocus = initialLoci)
        } else {
          dtToJoin <- data.table(eventID = numeric(0), initialLocus = numeric(0))
        }
        if (spreadStateExists) {
          spreadStateInitialLoci <- spreadState[, list(eventID = unique(eventID),
                                                       initialLocus = unique(initialLocus))]
          dtToJoin <- rbindlist(list(spreadStateInitialLoci, dtToJoin))
        }
        setkey(dtToJoin, eventID)
        setkey(allCells, eventID)

        allCells <- dtToJoin[allCells]
      } else {
        allCells <- data.table(spreads[,c(3,1,2,4)]) # change column order to match non allowOverlap
        allCells[,active:=as.logical(active)]
      }
      allCells[]
      return(allCells)
    }

    spre <- raster(landscape)
    spre[] <- 0
    if(!allowOverlap) {
      spre[wh] <- spreads[wh]
      if (exists("potentials"))
        if (NROW(potentials) > 0)
          spre[potentials[, 1L]] <- spreads[potentials[, 2L]]
    } else {
      spreadsDT <- data.table(spreads);
      pixVal <- spreadsDT[,sum(eventID),by=indices]
      spre[pixVal$indices] <- pixVal$V1;
    }
    return(spre)
  }
)


.matchedPointDistance <- function(a, b) {
    ids <- unique(b[,"mapID"])
    orig <- order(a[,"mapID",drop=FALSE],a[,"to",drop=FALSE])
    a <- a[orig,,drop=FALSE]
    dists <- cbind(dists=lapply(ids, function(i){
      m1 <- a[a[,"mapID"]==i,c("x","y"), drop = FALSE]
      m2 <- b[b[,"mapID"]==i,c("x","y"), drop = FALSE]
      sqrt((m1[,"x"] - m2[,"x"])^2 + (m1[,"y"] - m2[,"y"])^2)
    }) %>% unlist(), a)
    return(dists[order(orig),,drop=FALSE])


    }
