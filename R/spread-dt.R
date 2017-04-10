if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".GRP", "N", "distance", "initialPixels", "pixels", "state", "tooBig"))
}

###############################################################################
#' Simulate a contagious spread process on a landscape, with data.table internals
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric, symmetric (currently) landscape values and many other things.
#' Essentially, it starts from a collection of cells (\code{start}, called "events")
#' and spreads to neighbours, according to the \code{directions}
#' and \code{spreadProb} with modifications due to other arguments.
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
#' cells to spreadDT into. The interrupted output (a data.table) can be passed
#' subsequently as an input to this same function (as \code{start}).
#' This is intended to be used for situations where external events happen during
#' a spreadDT event, or where one or more arguments to the spreadDT function
#' change before a spreadDT event is completed.
#' For example, if it is desired that the \code{spreadProb} change before a
#' spreadDT event is completed because, for example, a fire is spreading, and a
#' new set of conditions arise due to a change in weather.
#'
#' @section Breaking out of spreadDT events:
#'
#' There are 3 ways for the spreadDT to "stop" spreading.
#' Here, each "event" is defined as all cells that are spawned from each unique
#' \code{start} location.
#' So, one spreadDT call can have multiple spreading "events".
#' The ways outlined below are all acting at all times, i.e., they are not
#' mutually exclusive.
#' Therefore, it is the user's responsibility to make sure the different rules
#' are interacting with each other correctly.
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
#'   \code{iterations} \tab This is a hard cap on the number of internal iterations to
#'                          complete before returning the current state of the system
#'                          as a data.table \cr
#' }
#'
#' @param landscape     A \code{RasterLayer} object. This defines the possible locations
#'                      for spreading events to start and spreadDT into. Required.
#'
#' @param start     Either a vector of pixel numbers to initiate spreading, or a
#'                  data.table that is the output of a previous \code{spreadDT}.
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
#'                      all events to be spreadDT. Recycled to match \code{start} length,
#'                      if it is not as long as \code{start}. This will be overridden if
#'                      \code{exactSize} also provided.
#'                      See section on \code{Breaking out of spreadDT events}.
#'
#' @param exactSize Numeric vector, length 1 or \code{length(start)}.
#'                  Similar to \code{maxSize}, but these will be the exact
#'                  final sizes of the events.  i.e., the spreadDT events
#'                  will continue until they are \code{floor(exactSize)}.
#'                  This will override \code{maxSize} if both provided.
#'                  See Details.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spreadDT.
#'                      Leaving this \code{NULL} allows the spreadDT to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param returnDistances Logical. Should the function include a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is FALSE. See Details.
#'
#' @param circle        Logical. If TRUE, then outward spreadDT will be by equidistant rings,
#'                      rather than solely by adjacent cells (via \code{directions} arg.). Default
#'                      is FALSE. Using \code{circle = TRUE} can be dramatically slower for large
#'                      problems. Note, this will likely create unexpected results if \code{spreadProb} < 1.
#'
#' @param allowOverlap  Logical. If \code{TRUE}, then individual events can overlap with one
#'                      another, i.e., they do not interact. Currently, this is slower than
#'                      if \code{allowOverlap} is \code{FALSE}. Default is \code{FALSE}.
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
#' @details
#'
#' If \code{exactSize} or \code{maxSize} are used, then spreading will continue and stop
#' before or at \code{maxSize} or at \code{exactSize}. If \code{iterations} is specified,
#' then the function will end, and the returned \code{data.table} will still
#' may (if \code{maxSize}) or will (if \code{exactSize}) have at least one active
#' cell per event that did not already achieve \code{maxSize} or \code{exactSize}. This
#' will be very useful to build new, customized higher-level wrapper functions that iteratively
#' call \code{spreadDT}.
#'
#' @note
#' \code{exactSize} may not be achieved if there aren't enough cells in the map. Also,
#' \code{exactSize} may not be achieved because 2) the active cells are "stuck", i.e.,
#' they have no unactivated cells to move to or 3) the \code{spreadProb} is low. In the
#' latter 2 cases, the algorithm will retry again, but it will only re-try from the last
#' iterations active cells. The algorithm will only retry 10 times before quitting. Currently,
#' there will also be an attempt to "jump" up to 4 cells away from the active cells to
#' try to continue spreading.
#'
#' A common way to use this function is to build wrappers around this, followed by iterative
#' calls in a \code{while} loop. See example.
#'
#' When using this function iteratively, there are several things to be wary about. 1) The output
#' will likely be sorted differently than the input (i.e., the order of start, if a vector,
#' may not be the same order as that returned). This means that when passing the same object
#' back into the next iteration of the function call, \code{maxSize} or \code{exactSize} may
#' not be in the same order. To get the same order, use e.g.,
#' \code{maxSize=attr(out, "cluster")$maxSize}.
#'
#' @return Either a \code{data.table} (\code{asRaster=FALSE}) or a \code{RasterLayer}
#' (\code{asRaster=TRUE}, the default). The \code{data.table} will have one attribute named
#' "cluster" as it provides cluster-level or event-level information about the
#' spread events. If \code{asRaster} is TRUE, then the \code{data.table} will be attached
#' to the Raster as an attribute named "pixel" as it provides pixel-level information about
#' the spread events.
#'
#' The \code{RasterLayer} represents every cell in which a successful spreadDT event occurred.
#' For the case of, say, a fire this would represent every cell that burned.
#' If \code{allowOverlap} is \code{TRUE}, the return will always be a \code{data.table}.
#'
#' If \code{asRaster} is \code{FALSE}, then this function returns a
#' \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{initialPixels} \tab the initial cell number of that particular
#'                            spreadDT event.\cr
#'   \code{pixels} \tab The cell indices of cells that have
#'                        been touched by the spreadDT algorithm.\cr
#'   \code{state} \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#' }
#'
#' The attribute saved with the name "cluster" (e.g., \code{attr(output, "cluster")}) is
#' a \code{data.table} with columns:
#' \tabular{ll}{
#'   \code{id} \tab An arbitrary code, from 1 to \code{length(start)} for each "event".\cr
#'   \code{initialPixels} \tab the initial cell number of that particular
#'                            spreadDT event.\cr
#'   \code{numRetries} \tab The number of re-starts the event did because it got
#'                          stuck (normally only because \code{exactSize} was used
#'                          and was not achieved.\cr
#'   \code{maxSize} \tab The number of pixels that were provided as inputs via
#'                      \code{maxSize} or \code{exactSize}.\cr
#'   \code{size} \tab The current size, in pixels, of each event.\cr
#' }
#'
#'
#' @export
#' @importFrom raster ncell raster res ncol
#' @importFrom bit bit
#' @importFrom data.table uniqueN as.data.table data.table set setkeyv setnames
#' @importFrom data.table ':=' rbindlist setcolorder
#' @importFrom checkmate assertClass assert checkNumeric checkDataTable qassert assertNumeric
#' @importFrom checkmate checkLogical checkClass
#' @importFrom stats runif
#' @importFrom fpCompare %<=% %>>%
#' @docType methods
#'
#' @author Eliot McIntire
#' @author Steve Cumming
#' @seealso \code{\link{rings}} which uses \code{spreadDT} but with specific argument
#' values selected for a specific purpose. \code{\link[raster]{distanceFromPoints}}
#'
#' @name spreadDT
#' @aliases spreadDT
#' @rdname spreadDT
#'
setGeneric("spreadDT", function(landscape, start = ncell(landscape)/2 - ncol(landscape)/2,
                                spreadProb = 0.23, asRaster = TRUE,
                                maxSize, exactSize,
                                directions = 8L, iterations = 1e6L,
                                returnDistances = FALSE,
                                plot.it = FALSE,
                                circle = FALSE,
                                allowOverlap = FALSE,
                                neighProbs = NA_real_, skipChecks = FALSE) {
  standardGeneric("spreadDT")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spreadDT event grow.
#'
#' @rdname spreadDT
#'
#' @example inst/examples/example_spreadDT.R
#'
setMethod(
  "spreadDT",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, start, spreadProb, asRaster,
                        maxSize, exactSize,
                        directions, iterations,
                        returnDistances, plot.it,
                        circle, allowOverlap,
                        neighProbs, skipChecks) {

    #### assertions ###############
    if (!skipChecks) {
      assertClass(landscape, "Raster")
      ncells <- ncell(landscape)
      numCols <- ncol(landscape)

      assert(
        checkNumeric(start, min.len = 0, max.len = ncells, lower = 1, upper = ncells),
        checkClass(start, "Raster"),
        checkDataTable(start, ncols = 3, types = c(rep("numeric", 2), "character")))

      qassert(neighProbs, "n[0,1]")
      assertNumeric(sum(neighProbs), lower = 1, upper = 1)

      assert(
        checkNumeric(spreadProb, 0, 1, min.len = 1, max.len = ncells),
        checkClass(spreadProb, "RasterLayer")
      )
      qassert(directions, "N1[4,8]")
      qassert(iterations, "N1[1,Inf]")
      qassert(circle, "B")
      if (circle)
        qassert(spreadProb, "N1[1,1]")

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
    maxRetriesPerID <- 10 # This means that if an event can not spread any more, it will try 10 times, including 2 jumps

    if(!is.numeric(start) & !is.data.table(start)) {
      if(is(start, "Raster")) {
        start <- attr(start, "pixel")
      } else {
        stop("Start must be either a vector of pixels, a data.table from",
             "previous spreadDT or a Raster from a previous spreadDT")
      }
    }
    if (!is.data.table(start)) { # A "new" entry into spreadDT -- need to set up stuff
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
      dt <- data.table(initialPixels=start, pixels=start)
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

    } else { # a "return" entry into spreadDT
      clusterDT <- attr(start, "cluster")#data.table(id=unique(start$id), initialPixels=unique(start$initialPixels), key = "initialPixels")
      if (!key(clusterDT) == "initialPixels") # should have key if it came directly from output of spreadDT
        setkeyv(clusterDT, "initialPixels")
      if(!anyNA(maxSize)) {
        if(any(maxSize != clusterDT$maxSize)) {
          message(sizeType, " provided. It does not match with size attr(start, 'cluster')$maxSize. ",
                  "Using the new ",sizeType," provided. Perhaps sorted differently?")
          clusterDT$maxSize <- maxSize
        }
      }
      if(any(colnames(clusterDT)=="maxSize")) maxSize <- clusterDT$maxSize
      set(clusterDT, ,"numRetries", 0)
      dt <- start
      whActive <- attr(start, "whActive")
      whInactive <- attr(start, "whInactive")
      if(canUseAvailable)
        notAvailable <- attr(dt, "notAvailable")

    }
    dtPotentialColNames <- c("id", "from", "to", "state", "distance"[needDistance])

    if (needDistance) set(dt, , "distance", 0) # it is zero distance to self

    its <- 0

    needRetryID <- integer()
    whNeedRetry <- integer()
    while ((length(needRetryID) | length(whActive)) &  its < iterations) {

      # Step 1
      # Get neighbours, either via adj (default) or cir (jumping if stuck)
      if (length(needRetryID) > 0) {
        ## get slightly further neighbours
        dtRetry <- dt[whNeedRetry]
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
      }

      if (length(needRetryID) > 0) {
        # of all possible cells in the jumping range, take just 2
        if(!is.data.table(dtPotential)) {
          dtPotential <- as.data.table(dtPotential)
        }

        dtPotential <- dtPotential[,list(to = resample(to, 2)),by = c("id", "from")]
        needRetryID <- integer()
      } #else {

      # randomize row order so duplicates are not always in same place
      i <- sample.int(NROW(dtPotential))
      if(!is.data.table(dtPotential)) {
        dtPotential <- as.data.table(dtPotential)
      } #else {
      for (x in colnames(dtPotential)) set(dtPotential, , x, dtPotential[[x]][i])
      #}
      #}


      # calculate distances, if required ... attach to dt
      if (needDistance) {
        fromPts <- xyFromCell(landscape, dtPotential$id)
        toPts <- xyFromCell(landscape, dtPotential$to)
        #set(dtPotential, , "distance", pointDistance(p1 = fromPts, p2 = toPts, lonlat=FALSE))
        dists <- pointDistance(p1 = fromPts, p2 = toPts, lonlat = FALSE)
        if (circle) {
          distKeepers <- dists %<=% its & dists %>>% (its - 1)
          dtPotential <- data.table(dtPotential[distKeepers])
          # mat <- cbind(from = dtPotential$from[distKeepers],
          #              to = dtPotential$to[distKeepers],
          #              id = dtPotential$id[distKeepers])
          if (needDistance)
            set(dtPotential, , "distance", dists[distKeepers])
           # mat <- cbind(mat, distance = dists[distKeepers])
          #dtPotential <- as.data.table(mat)
        }
      }

      set(dtPotential, , "state", "successful")

      ## Alternative algorithm for finding potential neighbours -- uses a specific number of neighbours
      if (!anyNA(neighProbs)) {
        numNeighsByPixel <- unique(dtPotential, by = c("id", "from"))
        if(is.list(neighProbs)) {
          set(numNeighsByPixel, , "numNeighs", unlist(lapply(
            neighProbs, function(np) sample.int(size = 1, n = length(np),
                                                replace = TRUE, prob = np))))
        } else {
          set(numNeighsByPixel, , "numNeighs",
              sample.int(size = NROW(numNeighsByPixel), n = length(neighProbs),
                         replace = TRUE, prob = neighProbs))
        }
        setkeyv(numNeighsByPixel, c("id", "from"))

        # remove duplicates from the existing "pixels" and new "potential pixels", since it must select exactly numNeighs
        dups <- duplicatedInt(c(dt$pixels, dtPotential$to))
        if(any(dups)) {
          dups <- dups[-seq_along(dt$pixels)]
          dtPotential <- dtPotential[!dups]
        }
        setkeyv(dtPotential, c("id", "from")) # sort so it is the same as numNeighsByPixel
        if (NROW(dtPotential)) {
          if(length(spreadProb)>1)
            set(dtPotential, , "spreadProb", spreadProb[dtPotential$to])
          else
            set(dtPotential, , "spreadProb", spreadProb)
          spreadProbNA <- is.na(dtPotential$spreadProb) # This is where a mask enters
          if(any(spreadProbNA)) {
            dtPotential <- dtPotential[!spreadProbNA]
            # code below is a possible replacement for previous line -- faster for small problems
            # colnamesDtPot <- colnames(dtPotential)
            # ll <-  lapply(colnamesDtPot, function(x) dtPotential[[x]][!spreadProbNA])
            # names(ll) <- colnamesDtPot
            # dtPotential <- as.data.table(ll)
          }
          # If it is a corner or has had pixels removed bc of duplicates, it may not have enough neighbours
          numNeighsByPixel <- numNeighsByPixel[dtPotential[, .N, by = c("id", "from")]]
          set(numNeighsByPixel, , "numNeighs", pmin(numNeighsByPixel$N, numNeighsByPixel$numNeighs, na.rm=TRUE))

          dtPotential <- dtPotential[
            dtPotential[,list(keepIndex=
                                resampleZeroProof(spreadProbHas0, .I,numNeighsByPixel$numNeighs[.GRP], spreadProb)),
                        by="from"]$keepIndex]

          set(dtPotential, , "spreadProb", NULL)
        }

        setcolorder(dtPotential, dtPotentialColNames)
        set(dtPotential, , "from", dtPotential$id)
        set(dtPotential, , "id", NULL)

        dt <- rbindlist(list(dt, dtPotential))
      } else { ## standard algorithm ... runif against spreadProb

        # Extract spreadProb for the current set of potentials
        actualSpreadProb <- if (length(spreadProb) == 1) {
          spreadProb
        } else {
          spreadProb[dtPotential$to]
        }

        # Evaluate against spreadProb -- next lines are faster than: dtPotential <- dtPotential[spreadProbSuccess]
        spreadProbSuccess <- runifC(NROW(dtPotential)) <= actualSpreadProb


      # Remove duplicates, which was already done for neighProbs situation
       if (allowOverlap) {
          if (needDistance)
            setcolorder(dtPotential, neworder = dtPotentialColNames)

          dtPotential <- dtPotential[spreadProbSuccess]
          set(dtPotential, , "from", dtPotential$id)
          set(dtPotential, , "id", NULL)

          dt <- rbindlist(list(dt, dtPotential))

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
          whKeep <- seq_along(spreadProbSuccess)[spreadProbSuccess][!dupsWithinDtPotential][!potentialNotAvailable]
          notAvailable[successCells[!potentialNotAvailable]] <- TRUE
          dtPotential <- dtPotential[whKeep]

          if (needDistance)
            setcolorder(dtPotential, neworder = dtPotentialColNames)

          set(dtPotential, , "from", dtPotential$id)
          set(dtPotential, , "id", NULL)

          #setcolorder(dtPotential, neworder = dtPotentialColNames)
          # convert state of all those still left, move potentialPixels into pixels column
          dt <- rbindlist(list(dt, dtPotential))
        }
#      } else {

      }

      # Remove any pixels that push each cluster over their maxSize limit
      if(!anyNA(maxSize) | !(anyNA(exactSize))) {
        setkeyv(dt,"initialPixels") # must sort because maxSize is sorted
        #currentSize <- dt[,.N,by=initialPixels][,`:=`(maxSize=clusterDT$maxSize,
        #                                              tooBig=N-clusterDT$maxSize)]
        set(clusterDT, , "size", dt[,list(size=.N),by="initialPixels"]$size)
        set(clusterDT, , "tooBig", clusterDT$size-clusterDT$maxSize)

        currentSizeTooBig <- clusterDT[tooBig > 0]
        if (NROW(currentSizeTooBig) > 0) {
          # sort them so .GRP works on 3rd line
          setkeyv(currentSizeTooBig, "initialPixels")
          dt <- dt[-dt[state == "successful" & (initialPixels %in% currentSizeTooBig$initialPixels),
                       resample(.I, currentSizeTooBig[.GRP]$tooBig), by = initialPixels]$V1][
                         initialPixels %in% currentSizeTooBig$initialPixels, state := "inactive"]
        }

        if (!(anyNA(exactSize))) {
          currentSizeTooSmall <- clusterDT[tooBig < 0]
          if (NROW(currentSizeTooSmall) > 0) {
            # successful means will become activeSource next iteration
            dt2 <- dt[initialPixels %in% currentSizeTooSmall$initialPixels & (state == "successful" | state == "holding")]
            setkeyv(dt2, "initialPixels")
            setkeyv(currentSizeTooSmall, "initialPixels")
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
      if(!anyNA(maxSize) | !(anyNA(exactSize)) | allowOverlap) { # these do resorting via setkeyv
        notInactive <- dt$state!="inactive" # currently activeSource, successful, or holding
        whNotInactive <- which(notInactive)
      } else {
        whNotInactive <- seq_len(length(whActive) + NROW(dtPotential))
        if(length(whInactive))
          whNotInactive <- max(whInactive) + whNotInactive
      }

      #if(length(whInactive))
      activeStates <- dt$state[whNotInactive]
      whActive <- whNotInactive[activeStates == "successful"]
      whInactive <- whNotInactive[activeStates == "activeSource"]
      set(dt, whNotInactive, "state",
          c("inactive", "activeSource", "activeSource")[
            fmatch(activeStates, c("activeSource", "holding", "successful"))])

      if (plot.it) {
        newPlot <- FALSE
        if (!exists("ras", inherits = FALSE)) {
          newPlot <- TRUE
        }
        if (newPlot)
          ras <- raster(landscape)
        if (returnDistances) {
          ras[dt$pixels] <- dt$distance
          newPlot <- TRUE
        } else {
          setkeyv(dt, "initialPixels")
          ras[dt$pixels] <- dt[clusterDT]$id
        }
        Plot(ras, new = newPlot)
      }
    } # end of main loop

    if(!is.null(clusterDT$tooBig)) set(clusterDT, , "tooBig", NULL)
    attr(dt, "cluster") <- clusterDT
    attr(dt, "whActive") <- whActive
    attr(dt, "whInactive") <- whInactive
    if(canUseAvailable)
      attr(dt, "notAvailable") <- notAvailable

    if (asRaster) {
      ras <- raster(landscape)
      # inside unit tests, this raster gives warnings if it is only NAs
      suppressWarnings(ras[dt$pixels] <- clusterDT[dt]$id)
      attr(ras, "pixel") <- dt
      return(ras)
    }

    return(dt)
  }
)


