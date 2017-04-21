if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".GRP", "N", "distance", "initialPixels", "pixels", "state", "tooBig",
                           "size", "actualSpreadProbAdj", "actualSpreadProbAdj2",
                           "newQuantity", "quantityAdj", "quantityAdj2"))
}

###############################################################################
#' Simulate a contagious spread process on a landscape, with data.table internals
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric, symmetric (currently) landscape values and many other things.
#' Essentially, it starts from a collection of cells (\code{start}, called "events")
#' and spreads to neighbours, according to the \code{directions}
#' and \code{spreadProb} with modifications due to other arguments. \bold{NOTE:}
#' the \code{spread} function is similar, but sometimes slightly faster, but less
#' robust, and more difficult to use iteratively.
#'
#' There are 2 main underlying algorithms for active cells to "spread" to
#' nearby cells (adjacent cells): \code{spreadProb} and \code{neighProb}.
#' Using \code{spreadProb}, every "active" pixel will assess all
#' neighbours (either 4 or 8, depending on  \code{directions}), and will "activate"
#' whichever neighbours successfully pass independent calls to
#' \code{runif(1,0,1)<spreadProb}.
#' The algorithm will iterate again and again, each time starting from the newly
#' "activated" cells. Several built-in decisions are as follows.
#' 1. no active cell can active a cell that was already activated by
#' the same event (i.e., "it won't go backwards"). 2. If \code{allowOverlap} is
#' \code{FALSE}, then the previous rule will also apply, regardless of which
#' "event" caused the pixels to be previously active.
#'
#' This function can be interrupted before all active cells are exhausted if
#' the \code{iterations} value is reached before there are no more active
#' cells to spread2 into. The interrupted output (a data.table) can be passed
#' subsequently as an input to this same function (as \code{start}).
#' This is intended to be used for situations where external events happen during
#' a spread2 event, or where one or more arguments to the spread2 function
#' change before a spread2 event is completed.
#' For example, if it is desired that the \code{spreadProb} change before a
#' spread2 event is completed because, for example, a fire is spreading, and a
#' new set of conditions arise due to a change in weather.
#'
#' \code{asymmetry} here is slightly different than in the \code{spread} function,
#' so that it can deal with a RasterLayer of \code{asymmetryAngle}.
#' Here, the \code{spreadProb} values of a given set of neighbours around each active pixel
#' are adjusted to create \code{adjustedSpreadProb} which is calculated maintain the
#' following
#' two qualities: \deqn{mean(spreadProb) = mean(ajustedSpreadProb)} and
#' \deqn{max(spreadProb)/min(spreadProb) = asymmetry} along the axis of
#' \code{asymmetryAngle}. NOTE: this means that the 8 neighbours around an active
#' cell may not fulfill the preceeding equality if \code{asymmetryAngle} is not
#' exactly one of the 8 angles of the 8 neighbours. This means that
#' \deqn{max(spreadProb)/min(spreadProb)} will generally be less than
#' \code{asymmetry}, for the 8 neighbours. The exact adjustment to the spreadProb
#' is calculated with:
#' \deqn{angleQuality <- (cos(angles - rad(asymmetryAngle))+1)/2}
#' which is multiplied to get an angle-adjusted spreadProb:
#' \deqn{spreadProbAdj <- actualSpreadProb * angleQuality}
#' which is then rescaled:
#' \deqn{adjustedSpreadProb = (spreadProbAdj - min(spreadProbAdj)) * par2 + par1},
#' where par1 and par2 are parameters calculated internally to make the 2 conditions above true.
#'
#' @section Breaking out of spread2 events:
#'
#' There are 3 ways for the spread2 to "stop" spreading.
#' Here, each "event" is defined as all cells that are spawned from each unique
#' \code{start} location.
#' The ways outlined below are all acting at all times, i.e., they are not
#' mutually exclusive.
#' Therefore, it is the user's responsibility to make sure the different rules
#' are interacting with each other correctly.
#'
#' \tabular{ll}{
#'   \code{spreadProb} \tab Probabilistically, if spreadProb is low enough,
#'                          active spreading events will stop. In practice,
#'                          this number generally should be below 0.3 to actually
#'                          see an event stop\cr
#'   \code{maxSize} \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. \code{spreadProb} will still
#'                       be active, so, it is possible that the end size of each event
#'                       is smaller than \code{maxSize}, but they will not be greater
#'                       than \code{maxSize}\cr
#'   \code{exactSize} \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. This will override an event that stops probabilistically
#'                       via \code{spreadProb}, but forcing its last set of active cells to
#'                       try again to find neighbours. It will try 5 times per event, before giving up.
#'                       During those 5 times, it will try twice to "jump" up to 4 cells outwards
#'                       from each of the active cells\cr
#'   \code{iterations} \tab This is a hard cap on the number of internal iterations to
#'                          complete before returning the current state of the system
#'                          as a data.table \cr
#' }
#'
#' @param landscape     A \code{RasterLayer} object. This defines the possible locations
#'                      for spreading events to start and spread2 into. Required.
#'
#' @param start     Either a vector of pixel numbers to initiate spreading, or a
#'                  data.table that is the output of a previous \code{spread2}.
#'                  If a vector, they should be cell indices (pixels) on the \code{landscape}.
#'                  If user has x and y coordinates, these can be converted with
#'                  \code{\link[raster]{cellFromXY}}.
#'
#' @param spreadProb    Numeric or RasterLayer. If numeric of length 1, then this is
#'                      the global probability of
#'                      spreading into each cell from a neighbor. If a raster then this must
#'                      be the cell-specific probability of a "receiving" potential cell.
#'                      Default is \code{0.23}.
#'
#' @param asRaster Logical, length 1. If \code{TRUE}, the function will return a \code{Raster}
#'                 where raster non NA values indicate the cells that were "active", and the
#'                 value is the initial starting pixel.
#'
#' @param maxSize       Numeric. Maximum number of cells for a single or
#'                      all events to be spread2. Recycled to match \code{start} length,
#'                      if it is not as long as \code{start}. This will be overridden if
#'                      \code{exactSize} also provided.
#'                      See section on \code{Breaking out of spread2 events}.
#'
#' @param exactSize Numeric vector, length 1 or \code{length(start)}.
#'                  Similar to \code{maxSize}, but these will be the exact
#'                  final sizes of the events.  i.e., the spread2 events
#'                  will continue until they are \code{floor(exactSize)}.
#'                  This will override \code{maxSize} if both provided.
#'                  See Details.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spread2.
#'                      Leaving this \code{NULL} allows the spread2 to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param returnDistances Logical. Should the function include a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is FALSE. See Details.
#'
#' @param returnFrom Logical. Should the function return a column with the
#'                      source, i.e, "from" pixel, for each iteration.
#'
#' @param circle        Logical. If TRUE, then outward spread2 will be by equidistant rings,
#'                      rather than solely by adjacent cells (via \code{directions} arg.). Default
#'                      is FALSE. Using \code{circle = TRUE} can be dramatically slower for large
#'                      problems. Note, this will likely create unexpected results if \code{spreadProb} < 1.
#'
#' @param skipChecks Logical. If TRUE, the argument checking (i.e., assertions) will be
#'              skipped. This should likely only be used once it is clear that the function
#'              arguments are well understood and function speed is of the primary improtance.
#'              This is likely most useful in repeated iteration cases i.e., if this call
#'              is using the previous output from this same function.
#'
#' @param neighProbs An optional numeric vector, whose sum is 1. It indicates the
#'                   probabilities that an individual
#'                   spread iteration will spread to \code{1, 2, ..., length(neighProbs)}
#'                   neighbours, respectively. If this is used (i.e., something other than
#'                   NA), \code{circle} and \code{returnDistances} will not work currently.
#'
#' @param asymmetry     A numeric or \code{RasterLayer} indicating the ratio of the
#'                      asymmetry to be used. i.e., 1 is no asymmetry; 2 means that the
#'                      angles in the direction of the \code{asymmetryAngle} are 2x the
#'                      \code{spreadProb}
#'                      of the angles opposite tot he \code{asymmetryAngle}  Default is
#'                      NA, indicating no asymmetry. See details. This is still experimental.
#'                      Use with caution.
#'
#' @param asymmetryAngle A numeric or \code{RasterLayer} indicating the angle in degrees
#'                      (0 is "up", as in North on a map),
#'                      that describes which way the \code{asymmetry} is.
#'
#' @inheritParams spread
#'
#' @details
#'
#' If \code{exactSize} or \code{maxSize} are used, then spreading will continue and stop
#' before or at \code{maxSize} or at \code{exactSize}. If \code{iterations} is specified,
#' then the function will end, and the returned \code{data.table} will still
#' may (if \code{maxSize}) or will (if \code{exactSize}) have at least one active
#' cell per event that did not already achieve \code{maxSize} or \code{exactSize}. This
#' will be very useful to build new, customized higher-level wrapper functions that iteratively
#' call \code{spread2}.
#'
#' @note
#' \code{exactSize} may not be achieved if there aren't enough cells in the map.
#' Also, \code{exactSize} may not be achieved because the active cells are "stuck",
#' i.e., they have no unactivated cells to move to; or the \code{spreadProb} is low.
#' In the latter two cases, the algorithm will retry again, but it will only
#' re-try from the last iterations active cells.
#' The algorithm will only retry 5 times before quitting.
#' Currently, there will also be an attempt to "jump" up to four cells away from
#' the active cells to try to continue spreading.
#'
#' A common way to use this function is to build wrappers around this, followed
#' by iterative calls in a \code{while} loop. See example.
#'
#' @section Building custom spreading events:
#'
#' This function can be used iteratively, with relatively little overhead compared to using
#' it non-iteratively. In general, this function can be called with arguments set as user
#' needs, and with specifying iterations = 1 (say). This means that the function will spread
#' outwards 1 iteration, then stop. The returned object will be a data.table or RasterLayer
#' that can be passed immediately back as the start argument into a subsequent
#' call to \code{spread2}. This means that every argument can be updated at each iteration.
#'
#' When using this function iteratively, there are several things to keep in mind.
#' The output will likely be sorted differently than the input (i.e., the
#' order of start, if a vector, may not be the same order as that returned).
#' This means that when passing the same object back into the next iteration of the
#' function call, \code{maxSize} or \code{exactSize} may not be in the same order.
#' To get the same order, the easiest thing to do is sort the initial \code{start}
#' objects by their pixel location, increasing.
#' Then, of course, sorting any vectorized arguments (e.g., \code{maxSize}) accordingly.
#'
#' \bold{NOTE}: the \code{data.table} or \code{RasterLayer} should not use be altered
#' when passed back into \code{spread2}.
#'
#' @return
#' Either a \code{data.table} (\code{asRaster=FALSE}) or a \code{RasterLayer}
#' (\code{asRaster=TRUE}, the default).
#' The \code{data.table} will have one attribute named \code{spreadState}, which
#' is a list containing a \code{data.table} of current cluster-level information
#' about the spread events.
#' If \code{asRaster=TRUE}, then the \code{data.table} that would have been
#' returned is attached to the Raster as an attribute named \code{pixel} as it
#' provides pixel-level information about the spread events.
#'
#' The \code{RasterLayer} represents every cell in which a successful spread2 event occurred.
#' For the case of, say, a fire this would represent every cell that burned.
#' If \code{allowOverlap} is \code{TRUE}, the return will always be a \code{data.table}.
#'
#' If \code{asRaster} is \code{FALSE}, then this function returns a
#' \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{initialPixels} \tab the initial cell number of that particular
#'                            spread2 event.\cr
#'   \code{pixels} \tab The cell indices of cells that have
#'                        been touched by the spread2 algorithm.\cr
#'   \code{state} \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#' }
#'
#' The attribute saved with the name "spreadState" (e.g., \code{attr(output, "spreadState")})
#' includes a \code{data.table} with columns:
#' \tabular{ll}{
#'   \code{id} \tab An arbitrary code, from 1 to \code{length(start)} for each "event".\cr
#'   \code{initialPixels} \tab the initial cell number of that particular
#'                            spread2 event.\cr
#'   \code{numRetries} \tab The number of re-starts the event did because it got
#'                          stuck (normally only because \code{exactSize} was used
#'                          and was not achieved.\cr
#'   \code{maxSize} \tab The number of pixels that were provided as inputs via
#'                      \code{maxSize} or \code{exactSize}.\cr
#'   \code{size} \tab The current size, in pixels, of each event.\cr
#' }
#' and several other objects that provide significant speed ups in iterative calls to
#' spread2. If the user runs \code{spread2} iteratively, there will likely be significant
#' speed gains if the \code{data.table} passed in to \code{start} should have the attribute
#' attached, or re-attached if it was lost, e.g., via
#' \code{setattr(outInput, "spreadState", attr(out, "spreadState"))}, where \code{out} is the
#' returned \code{data.table} from the previous call to \code{spread2}, and \code{outInput} is
#' the modified \code{data.table}. Currently, the modified \code{data.table} \bold{must} have the
#' same order as \code{out}.
#'
#' @export
#' @importFrom raster ncell raster res ncol
#' @importFrom bit bit
#' @importFrom data.table uniqueN as.data.table data.table set setkeyv setnames
#' @importFrom data.table ':=' rbindlist setcolorder setattr alloc.col
#' @importFrom checkmate assertClass assert checkNumeric checkDataTable qassert assertNumeric
#' @importFrom checkmate checkLogical checkClass
#' @importFrom stats runif
#' @importFrom fpCompare %<=% %>>%
#' @docType methods
#'
#' @author Eliot McIntire
#' @author Steve Cumming
#' @seealso \code{\link{spread}} for a different implementation of the same alogorithm.
#' \code{spread} is less robust but it is often slightly faster.
#'
#' @name spread2
#' @aliases spread2
#' @rdname spread2
#'
setGeneric("spread2", function(landscape, start = ncell(landscape)/2 - ncol(landscape)/2,
                               spreadProb = 0.23, asRaster = TRUE,
                               maxSize, exactSize,
                               directions = 8L, iterations = 1e6L,
                               returnDistances = FALSE, returnFrom = FALSE,
                               plot.it = FALSE,
                               circle = FALSE,
                               asymmetry = NA_real_, asymmetryAngle = NA_real_,
                               allowOverlap = FALSE,
                               neighProbs = NA_real_, skipChecks = FALSE) {
  standardGeneric("spread2")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spread2 event grow.
#'
#' @rdname spread2
#'
#' @example inst/examples/example_spread2.R
#'
setMethod(
  "spread2",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, start, spreadProb, asRaster,
                        maxSize, exactSize,
                        directions, iterations,
                        returnDistances, returnFrom,
                        plot.it,
                        circle,
                        asymmetry, asymmetryAngle,
                        allowOverlap,
                        neighProbs, skipChecks) {

    #### assertions ###############
    if (!skipChecks) {
      assertClass(landscape, "Raster")
      ncells <- ncell(landscape)
      numCols <- ncol(landscape)

      assert(
        checkNumeric(start, min.len = 0, max.len = ncells, lower = 1, upper = ncells),
        checkClass(start, "Raster"),
        checkDataTable(start))

      qassert(neighProbs, "n[0,1]")
      assertNumeric(sum(neighProbs), lower = 1, upper = 1)

      assert(
        checkNumeric(spreadProb, 0, 1, min.len = 1, max.len = ncells),
        checkClass(spreadProb, "RasterLayer")
      )
      assert(
        checkNumeric(asymmetry, 0, Inf, min.len = 1, max.len = 1),
        checkClass(asymmetry, "RasterLayer")
      )
      assert(
        checkNumeric(asymmetryAngle, 0, 360, min.len = 1, max.len = 1),
        checkClass(asymmetryAngle, "RasterLayer")
      )
      qassert(directions, "N1[4,8]")
      qassert(iterations, "N1[0,Inf]")
      qassert(circle, "B")
      # if (circle)
      #   qassert(spreadProb, "N1[1,1]")

      if(!missing(maxSize)) {
        if(is.data.table(start)) {
          N <- uniqueN(start,by = "initialPixels")
          assert(
            checkNumeric(maxSize, min.len = 1, max.len=1),
            checkNumeric(maxSize, min.len = N,
                         max.len=N)
          )
        } else {
          assert(
            checkNumeric(maxSize, min.len = 1, max.len=1),
            checkNumeric(maxSize, min.len = NROW(start), max.len=NROW(start))
          )
        }
      }
      if(!missing(exactSize)) {
        if(is.data.table(start)) {
          N <- uniqueN(start,by = "initialPixels")
          assert(
            checkNumeric(exactSize, min.len = 1, max.len=1),
            checkNumeric(exactSize, min.len = N,
                         max.len=N)
          )
        } else {
          assert(
            checkNumeric(exactSize, min.len = 1, max.len=1),
            checkNumeric(exactSize, min.len = NROW(start), max.len=NROW(start))
          )
        }

      }
    } else {
      ncells <- ncell(landscape)
      numCols <- ncol(landscape)
    }
    ##### End assertions

    smallRaster <- ncells < 4e7
    canUseAvailable <- !allowOverlap
    # required function
    spreadProbHas0 <- if(!is.numeric(spreadProb)) {
      if(is(spreadProb, "Raster")) minValue(spreadProb)==0 else stop("expecting a Raster, data.table or numeric for start")
    } else {
      any(spreadProb==0)
    }

    ##### Set up dt and clusterDT objects
    if(missing(maxSize)) {
      maxSize <- NA
    }

    if(missing(exactSize)) {
      exactSize <- NA
    } else {
      maxSize <- exactSize
    }
    sizeType <- if(!anyNA(exactSize)) "exactSize" else "maxSize"

    needDistance <- returnDistances | circle # returnDistances = TRUE and circle = TRUE both require distance calculations
    maxRetriesPerID <- 5 # This means that if an event can not spread any more, it will try 5 times, including 2 jumps

    if(!is.numeric(start) & !is.data.table(start)) {
      if(is(start, "Raster")) {
        start <- attr(start, "pixel")
      } else {
        stop("Start must be either a vector of pixels, a data.table from",
             "previous spread2 or a Raster from a previous spread2")
      }
    }

    if (!is.data.table(start)) { # A "new" entry into spread2 -- need to set up stuff
      if(canUseAvailable) {
        if(smallRaster) {
          notAvailable <- bit(ncells)
        } else {
          notAvailable <- ff(vmode = "boolean", FALSE, length = ncells)
        }
        notAvailable[start] <- TRUE
      }

      start <- as.integer(start)

      whActive <- seq_along(start)
      whInactive <- integer()
      #dt <- as.data.table(cbind(initialPixels=start, pixels=start))
      dt <- data.table(initialPixels=start)
      if(returnFrom) {
        set(dt, , "from", NA_integer_)
      }
      set(dt, , "pixels", start)
      set(dt, , "state", "activeSource")

      #clusterDT=as.data.table(cbind(id=whActive, initialPixels=start, numRetries=0L));
      clusterDT=data.table(id=whActive, initialPixels=start, numRetries=0L);

      if(!anyNA(maxSize)) {
        set(clusterDT, , "maxSize", maxSize)
        set(dt, which(clusterDT$maxSize==1), "state", "inactive") # de-activate ones that are 1 cell
      }
      if(!anyNA(exactSize)) {
        set(clusterDT, , "exactSize", TRUE)
      }

      setkeyv(clusterDT, "initialPixels")
      if (needDistance) set(dt, , "distance", 0) # it is zero distance to self
      totalIterations <- 0

    } else { # a "return" entry into spread2
      dt <- start
      if(!is.null(attr(start, "spreadState"))) {
        clusterDT <- attr(start, "spreadState")$clusterDT #data.table(id=unique(start$id), initialPixels=unique(start$initialPixels), key = "initialPixels")
        if (!key(clusterDT) == "initialPixels") # should have key if it came directly from output of spread2
          setkeyv(clusterDT, "initialPixels")
        if(!anyNA(maxSize)) {
          if(any(maxSize != clusterDT$maxSize)) {
            message(sizeType, " provided. It does not match with size attr(start, 'cluster')$maxSize. ",
                    "Using the new ",sizeType," provided. Perhaps sorted differently? Try sorting initial ",
                    "call to spread2 so that pixel number of start cells is strictly increasing")
            clusterDT$maxSize <- maxSize
          }
        }
        if(any(colnames(clusterDT)=="maxSize")) maxSize <- clusterDT$maxSize
        whActive <- attr(start, "spreadState")$whActive
        whInactive <- attr(start, "spreadState")$whInactive
        totalIterations <- attr(start, "spreadState")$totalIterations
        if(canUseAvailable)
          notAvailable <- attr(start, "spreadState")$notAvailable

      } else { # case where user has deleted the attributes
        whActive <- which(start$state=="activeSource")
        whInactive <- which(start$state=="inactive")
        canUseAvailable <- FALSE # not worth it if it has to be remade each time
        totalIterations <- if(needDistance) max(start$distance) else 0
        unIP <- unique(dt$initialPixels)
        clusterDT=data.table(id=seq_along(unIP),
                             initialPixels=unIP, numRetries=0L)
        if(!anyNA(maxSize)) {
          set(clusterDT, , "maxSize", maxSize)
          if(!anyNA(exactSize)) {
            set(clusterDT, , "exactSize", TRUE)
          }
          set(clusterDT, , "size", dt[,.N,by="initialPixels"]$N)
          setkeyv(clusterDT, "initialPixels")
        }
      }

    }
    needRetryID <- integer()
    whNeedRetry <- integer()
    dtPotentialColNames <- c("id", "from", "to", "state", "distance"[needDistance]) # keep for use later

    its <- 0 # start at iteration 0, note: totalIterations is also maintained, which persists during iterative calls to spread2

    while (length(needRetryID) | (length(whActive) &  its < iterations)) {

      # Step 1
      # Get neighbours, either via adj (default) or cir (jumping if stuck)
      if (length(needRetryID) > 0) {
        ## get slightly further neighbours
        dtRetry <- dt[whNeedRetry]
        browser()
        if (any(((clusterDT$numRetries + 1) %% 5) == 0)) { # jump every 5, starting at 4
          resCur <- res(landscape)[1]
          fromPixels <- dtRetry$pixels
          dtPotential <- cir(landscape, loci = fromPixels, includeBehavior = "excludePixels",
                             minRadius = resCur, maxRadius = 4*resCur, allowOverlap = TRUE)[,c("id","indices")]
          dtPotential <- matrix(as.integer(dtPotential), ncol = 2)
          colnames(dtPotential) <- c("id", "to")
          dtPotential <- cbind(id = dtRetry$initialPixels[dtPotential[,"id"]],
                               from = fromPixels[dtPotential[,"id"]],
                               to = dtPotential[, "to"])
        } else {
          ## get adjacent neighbours
          dtPotential <- adj(#landscape,
            directions = directions,
            numCell = ncells,
            numCol = numCols,
            id = dtRetry$initialPixels,
            cells = dtRetry$pixels)
        }

        set(dt, whActive, "state", "holding") # take them out of commission for this iteration
        set(dt, whNeedRetry, "state", "activeSource")

      } else {
        browser()
        ## Spread to immediate neighbours
        dtPotential <- adj(#landscape,
          numCell = ncells,
          numCol = numCols,
          directions = directions,
          id = dt$initialPixels[whActive],
          cells = dt$pixels[whActive], cutoff.for.data.table = 5e2,
          returnDT = TRUE)

        # only iterate if it is not a Retry situation
        its <- its + 1
        totalIterations <- totalIterations + 1
      }

      if (length(needRetryID) > 0) {
        # of all possible cells in the jumping range, take just 2
        if(!is.data.table(dtPotential)) {
          dtPotential <- as.data.table(dtPotential)
        }

        dtPotential <- dtPotential[,list(to = resample(to, 2)),by = c("id", "from")]
        needRetryID <- integer()
      }

      # randomize row order so duplicates are not always in same place
      i <- sample.int(NROW(dtPotential))
      if(!is.data.table(dtPotential)) {
        dtPotential <- as.data.table(dtPotential)
      }
      for (x in colnames(dtPotential)) set(dtPotential, , x, dtPotential[[x]][i])

      # calculate distances, if required ... attach to dt
      if (needDistance) {
        fromPts <- xyFromCell(landscape, dtPotential$id)
        toPts <- xyFromCell(landscape, dtPotential$to)
        dists <- pointDistance(p1 = fromPts, p2 = toPts, lonlat = FALSE)
        if (!is.na(asymmetry)) {
          actualAsymmetry <- if (length(asymmetry) == 1) {
            asymmetry
          } else {
            asymmetry[dtPotential$to]
          }
          actualAsymmetryAngle <- if (length(asymmetryAngle) == 1) {
            asymmetryAngle
          } else {
            asymmetryAngle[dtPotential$to]
          }

          angleQualities <- angleQuality(dtPotential, landscape, actualAsymmetryAngle)
          naAQ <- is.na(angleQualities[,"angleQuality"])
          angleQualities[naAQ,"angleQuality"] <- 1
          # convert dists to effective distance
          effDists <- dists * ( (2 - angleQualities[,"angleQuality"])/2 * (actualAsymmetry - 1) + 1)
        }

        if (circle) {
          #distKeepers <- dists %<=% totalIterations #& dists %<=% totalIterations  #& dists %>>% (totalIterations - 1)
          if(!is.na(asymmetry)) {
            distKeepers <- effDists %<=% totalIterations & effDists %>>% (totalIterations - 1)
            dtPotentialAsymmetry <- dtPotential[!distKeepers]
            if(sum(distKeepers)==0) { # all failed
              set(dt, ,"state","successful")
            } else {
              unDTPotAssym <- unique(dtPotentialAsymmetry$from)
              if(length(unDTPotAssym) == length(unique(dt$pixel))) {
                set(dt, ,"state","successful")
              } else {
                dt[pixels %in% unDTPotAssym,state:="successful"]
              }

            }

          } else {
            distKeepers <- dists %<=% totalIterations & dists %>>% (totalIterations - 1)
          }
          dtPotential <- dtPotential[distKeepers]
          dists <- dists[distKeepers]
        }
        set(dtPotential, , "distance", dists)
        if(!is.na(asymmetry)) {
          set(dtPotential, , "effectiveDistance", effDists[distKeepers])
        }
      }

      set(dtPotential, , "state", "successful")
      ## Alternative algorithm for finding potential neighbours -- uses a specific number of neighbours
      if (!anyNA(neighProbs)) {
        numNeighsByPixel <- unique(dtPotential, by = c("id", "from"))
        if(is.list(neighProbs)) {
          if(NROW(numNeighsByPixel)!=length(neighProbs)) {
            neighProbs1 <- neighProbs[match(numNeighsByPixel$from, start[state=="activeSource"]$pixels)]

          } else {
            neighProbs1 <- neighProbs
          }
          set(numNeighsByPixel, , "numNeighs", unlist(lapply(
            neighProbs1, function(np) sample.int(size = 1, n = length(np),
                                                replace = TRUE, prob = np))))

        } else {
          set(numNeighsByPixel, , "numNeighs",
              sample.int(size = NROW(numNeighsByPixel), n = length(neighProbs),
                         replace = TRUE, prob = neighProbs))
        }
        setkeyv(numNeighsByPixel, c("id", "from"))

        # remove duplicates within dtPotential
        dupsWithinDtPotential <- duplicatedInt(dtPotential$to)
        successCells <- dtPotential$to[!dupsWithinDtPotential] # remove the dupsWithinDtPotential
        potentialNotAvailable <- notAvailable[successCells]
        whNoDupsCurItAndAll <- seq_along(dtPotential$to)[!dupsWithinDtPotential][!potentialNotAvailable]
        notAvailable[successCells[!potentialNotAvailable]] <- TRUE
        dtPotential <- dtPotential[whNoDupsCurItAndAll]

        # dups <- duplicatedInt(c(dt$pixels, dtPotential$to))
        # if(any(dups)) {
        #   dups <- dups[-seq_along(dt$pixels)]
        #   dtPotential <- dtPotential[!dups]
        # }
        setkeyv(dtPotential, c("id", "from")) # sort so it is the same as numNeighsByPixel
        if (NROW(dtPotential)) {
          if(length(spreadProb)>1) {
            browser()
            set(dtPotential, , "spreadProb", spreadProb[][dtPotential$to])
          } else {
            set(dtPotential, , "spreadProb", spreadProb)
          }
          spreadProbNA <- is.na(dtPotential$spreadProb) # This is where a mask enters
          if(any(spreadProbNA)) {
            dtPotential <- dtPotential[!spreadProbNA]
            # code below is a possible replacement for previous line -- faster for small problems
            # colnamesDtPot <- colnames(dtPotential)
            # ll <-  lapply(colnamesDtPot, function(x) dtPotential[[x]][!spreadProbNA])
            # names(ll) <- colnamesDtPot
            # dtPotential <- as.data.table(ll)
          }
          # might be zero length because of spreadProb NAs
          if(NROW(dtPotential)) {
            # If it is a corner or has had pixels removed bc of duplicates, it may not have enough neighbours
            numNeighsByPixel <- numNeighsByPixel[dtPotential[, .N, by = c("id", "from")]]
            set(numNeighsByPixel, , "numNeighs", pmin(numNeighsByPixel$N, numNeighsByPixel$numNeighs, na.rm=TRUE))
            dtPotential <- dtPotential[numNeighsByPixel[dtPotential][,
                                  resampleZeroProof(spreadProbHas0, .I, n = numNeighs, prob=spreadProb), by = "from"]$V1]
          }

          set(dtPotential, , "spreadProb", NULL)
        }

        setcolorder(dtPotential, dtPotentialColNames)
        dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom)

      } else { ## standard algorithm ... runif against spreadProb

        # Extract spreadProb for the current set of potentials
        actualSpreadProb <- if (length(spreadProb) == 1) {
          rep(spreadProb, NROW(dtPotential))
        } else {
          spreadProb[dtPotential$to]
        }

        # modify actualSpreadProb if there is asymmetry
        if (!is.na(asymmetry) & !circle) {
          actualAsymmetry <- if (length(asymmetry) == 1) {
            asymmetry
          } else {
            asymmetry[dtPotential$to]
          }
          actualAsymmetryAngle <- if (length(asymmetryAngle) == 1) {
            asymmetryAngle
          } else {
            asymmetryAngle[dtPotential$to]
          }

          angleQualities <- angleQuality(dtPotential, landscape, actualAsymmetryAngle)

          naAQ <- is.na(angleQualities[,"angleQuality"])
          angleQualities[naAQ,"angleQuality"] <- actualSpreadProb[naAQ]

          actualSpreadProb <- asymmetryAdjust(angleQualities, actualSpreadProb, actualAsymmetry)

        }

        # Evaluate against spreadProb -- next lines are faster than: dtPotential <- dtPotential[spreadProbSuccess]
        spreadProbSuccess <- runifC(NROW(dtPotential)) <= actualSpreadProb


        # Remove duplicates, which was already done for neighProbs situation
        if (allowOverlap | !canUseAvailable) {
          if (needDistance)
            setcolorder(dtPotential, neworder = c(dtPotentialColNames[(dtPotentialColNames %in% colnames(dtPotential))],
                                                  colnames(dtPotential)[!(colnames(dtPotential) %in% dtPotentialColNames)]))


          dtPotential <- dtPotential[spreadProbSuccess]

          dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom)

          dt[, `:=`(dups = duplicatedInt(pixels)), by = initialPixels]
          dupes <- dt$dups
          set(dt, , "dups", NULL)
          dt <- dt[!dupes]

        } else {
          successCells <- dtPotential$to[spreadProbSuccess]
          dupsWithinDtPotential <- duplicatedInt(successCells)

          successCells <- successCells[!dupsWithinDtPotential] # remove the dupsWithinDtPotential
          potentialNotAvailable <- notAvailable[successCells]
          # remove duplicatedInt which was slow

          # 3 reasons why potentials are not selected
          whSuccNoDupsCurItAndAll <- seq_along(spreadProbSuccess)[spreadProbSuccess][!dupsWithinDtPotential][!potentialNotAvailable]
          notAvailable[successCells[!potentialNotAvailable]] <- TRUE
          dtPotential <- dtPotential[whSuccNoDupsCurItAndAll]

          if (needDistance) # distance column is second last, but needs to be last: to merge with dt, need: from, to, state in that order
            setcolorder(dtPotential, neworder = c(dtPotentialColNames[(dtPotentialColNames %in% colnames(dtPotential))],
                                                  colnames(dtPotential)[!(colnames(dtPotential) %in% dtPotentialColNames)]))

          dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom)
        }

      }

      # Remove any pixels that push each cluster over their maxSize limit
      if(!anyNA(maxSize) | !(anyNA(exactSize))) {
        browser()
        setkeyv(dt,"initialPixels") # must sort because maxSize is sorted
        #currentSize <- dt[,.N,by=initialPixels][,`:=`(maxSize=clusterDT$maxSize,
        #                                              tooBigByNCells=N-clusterDT$maxSize)]
        set(clusterDT, , "size", dt[,list(size=as.integer(.N)),by="initialPixels"]$size)
        # THis next line is a work around for a problem that doesn't make sense -- See: https://stackoverflow.com/questions/29615181/r-warning-when-creating-a-long-list-of-dummies
        alloc.col(clusterDT, 7)
        set(clusterDT, , "tooBigByNCells", clusterDT$size-as.integer(clusterDT$maxSize))

        currentSizetooBigByNCells <- clusterDT[tooBigByNCells > 0]
        if (NROW(currentSizetooBigByNCells) > 0) {
          # sort them so .GRP works on 3rd line
          setkeyv(currentSizetooBigByNCells, "initialPixels")
          #dt <- data.table::copy(dt2)
          set(dt, ,"origIndex", seq_len(NROW(dt)))
          dt1 <- dt[state=="successful"]
          dt1b <- dt1[currentSizetooBigByNCells] # attach tooBigByNCells
          dt1a <- dt1b[,list(omit=resample(origIndex,tooBigByNCells)),by="initialPixels"]
          dt <- dt[-dt1a$omit][,list(initialPixels, pixels, state)]
          dt[dt1a, state:="inactive"]
          #set(dt, , "origIndex", NULL)

          # dt <- dt[-dt[state == "successful" & (initialPixels %in% currentSizetooBigByNCells$initialPixels),
          #              resample(.I, currentSizetooBigByNCells[.GRP]$tooBigByNCells), by = initialPixels]$V1][
          #                initialPixels %in% currentSizetooBigByNCells$initialPixels, state := "inactive"]
          clusterDT[currentSizetooBigByNCells[,list(initialPixels)],size:=size-tooBigByNCells]

        }

        if (!(anyNA(exactSize))) {
          currentSizeTooSmall <- clusterDT[tooBigByNCells < 0]
          if (NROW(currentSizeTooSmall) > 0) {
            # successful means will become activeSource next iteration
            dt2 <- dt[initialPixels %in% currentSizeTooSmall$initialPixels & (state == "successful" | state == "holding")]
            setkeyv(dt2, "initialPixels")
            #setkeyv(currentSizeTooSmall, "initialPixels")
            currentSizeTooSmall <- currentSizeTooSmall[!dt2]
          }
          # if the ones that are too small are unsuccessful, make them "needRetry"
          whNeedRetry <- which(dt$initialPixels %in% currentSizeTooSmall$initialPixels &
                                 (dt$state != "successful" & dt$state != "inactive"))
          if (length(whNeedRetry)) {
            needRetryID <- clusterDT$initialPixels %in% unique(dt$initialPixels[whNeedRetry])
            tooManyRetries <- clusterDT$numRetries > maxRetriesPerID
            if (sum(tooManyRetries * needRetryID) > 0) {
              needRetryID <- needRetryID & !(needRetryID * tooManyRetries)
              whNeedRetry <- whNeedRetry[dt$initialPixels[whNeedRetry] %in%
                                           clusterDT$initialPixels[needRetryID]]
            }
            needRetryID <- which(needRetryID)

            set(dt, whNeedRetry, "state", "needRetry")
            set(clusterDT, needRetryID, "numRetries", clusterDT$numRetries[needRetryID] + 1L)
          }
        }
      } # end maxSize based removals

      # Change states of cells
      #if(!anyNA(maxSize) | !(anyNA(exactSize)) | allowOverlap) { # these do resorting via setkeyv
      notInactive <- dt$state!="inactive" # currently activeSource, successful, or holding
      whNotInactive <- which(notInactive)
      # } else {
      #   whNotInactive <- seq_len(length(whActive) + NROW(dtPotential))
      #   if(length(whInactive))
      #     whNotInactive <- max(whInactive) + whNotInactive
      # }

      activeStates <- dt$state[whNotInactive]
      whActive <- whNotInactive[activeStates == "successful" | activeStates == "holding" ]
      whInactive <- whNotInactive[activeStates == "activeSource"]
      set(dt, whNotInactive, "state",
          c("inactive", "activeSource", "activeSource", "needRetry")[
            fmatch(activeStates, c("activeSource", "holding", "successful", "needRetry"))])

      if (plot.it) {
        newPlot <- FALSE
        if(totalIterations==1) {
          newPlot <- TRUE
        }
        if (newPlot | !(exists("spread2Ras", inherits = FALSE)))
          spread2Ras <- raster(landscape)
        if (returnDistances) {
          spread2Ras[dt$pixels] <- dt$distance
          newPlot <- TRUE # need to rescale legend each time
        } else {
          set(dt, , "order", seq_along(dt$initialPixels))
          setkeyv(dt, "initialPixels")
          spread2Ras[dt$pixels] <- dt[clusterDT]$id # get id column from clusterDT
          setkeyv(dt, "order")
          set(dt, , "order", NULL)
        }
        Plot(spread2Ras, new = newPlot)
      }
    } # end of main loop

    if(!is.null(clusterDT$tooBigByNCells)) set(clusterDT, , "tooBigByNCells", NULL)
    attrList <- list(clusterDT=clusterDT, whActive=whActive,
                     whInactive=whInactive, whNeedRetry=whNeedRetry,
                     needRetryID=needRetryID, totalIterations=totalIterations)
    if(canUseAvailable)
      attrList <- append(attrList, list(notAvailable=notAvailable))

    setattr(dt, "spreadState", attrList)
    # setattr(dt, "clusterDT", clusterDT)
    # setattr(dt, "whActive", whActive)
    # setattr(dt, "whInactive", whInactive)
    # setattr(dt, "whNeedRetry", whNeedRetry)
    # setattr(dt, "needRetryID", needRetryID)
    # setattr(dt, "totalIterations", totalIterations)
    # if(canUseAvailable)
    #   setattr(dt, "notAvailable", notAvailable)

    if (asRaster) {
      ras <- raster(landscape)
      # inside unit tests, this raster gives warnings if it is only NAs
      suppressWarnings(ras[dt$pixels] <- clusterDT[dt]$id)
      setattr(ras, "pixel", dt)
      return(ras)
    }

    return(dt)
  }
)


