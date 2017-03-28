###############################################################################
#' Simulate a spreadDT process on a landscape.
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric landscape values (symmetric or asymmetric) and many other things.
#' Essentially, it starts from a collection of cells (\code{start}) and spreads
#' to neighbours, according to the \code{directions} and \code{spreadProbLater} arguments.
#' This can become quite general, if \code{spreadProbLater} is 1 as it will expand
#' from every start until all cells in the landscape have been covered.
#' With \code{id} set to \code{TRUE}, the resulting map will be classified
#' by the index of the cell where that event propagated from.
#' This can be used to examine things like fire size distributions.
#'
#' This function can be interrupted before all active cells are exhausted if
#' the \code{iterations} value is reached before there are no more active
#' cells to spreadDT into. The interrupted output (a data.table) can be passed subsequently
#' as an input to this same function (as \code{start}). This is intended
#' to be used for situations where external
#' events happen during a spreadDT event, or where one or more arguments to the spreadDT
#' function change before a spreadDT event is completed. For example, if it is
#' desired that the \code{spreadProb} change before a spreadDT event is completed because,
#' for example, a fire is spreading, and a new set of conditions arise due to
#' a change in weather.
#'
#' \code{asymmetry} is currently used to modify the \code{spreadProb} in the following way.
#' First for each active cell, spreadProb is converted into a length 2 numeric of Low and High
#' spreadDT probabilities for that
#' cell: \code{spreadProbsLH <- (spreadProb*2) // (asymmetry+1)*c(1,asymmetry)},
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
#' @section Breaking out of spreadDT events:
#'
#' There are 4 ways for the spreadDT to "stop" spreading. Here, each "event" is defined as
#' all cells that are spawned from a single starting start. So, one spreadDT call can have
#' multiple spreading "events". The ways outlines below are all acting at all times,
#' i.e., they are not mutually exclusive. Therefore, it is the user's
#' responsibility to make sure the different rules are interacting with
#' each other correctly. Using \code{spreadProb} or \code{size} are computationally
#' fastest, sometimes dramatically so.
#'
#' \tabular{ll}{
#'   \code{spreadProb} \tab Probabilistically, if spreadProb is low enough,
#'                          active spreading events will stop. In practice,
#'                          active spreading events will stop. In practice,
#'                          this number generally should be below 0.3 to actually
#'                          see an event stop\cr
#'   \code{size} \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. This can be vectorized, one value
#'                       for each event   \cr
#'   \code{circleMaxRadius} \tab If \code{circle} is TRUE, then this will be the maximum
#'                       radius reached, and then the event will stop. This is
#'                       vectorized, and if length is >1, it will be matched
#'                       in the order of \code{start}\cr
#'   \code{stopRule} \tab This is a func
#'   tion that can use "landscape", "id", "cells", or any
#'                       named vector passed into \code{spreadDT} in the \code{...}. This
#'                       can take on relatively complex functions. Passing in, say, a Raster
#'                       Layer to \code{spreadDT} can access the individual values on that
#'                       arbitrary Raster Layer using "cells". These will be calculated
#'                       within all the cells of the individual event (equivalent to a
#'                       "group_by(event)" in dplyr. So, \code{sum(arbitraryRaster[cells])}
#'                       would sum up all the raster values on the arbitraryRaster Raster
#'                       that are overlaid by the individual event. This can then be used in
#'                       a logical statement.  See examples.
#'                       To confirm the cause of stopping, the user can assess the values
#'                       after the function has finished.\cr
#' }
#'
#' The spreadDT function does not return the result of this stopRule. If,
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
#' @param landscape     A \code{RasterLayer} object. This defines the possible locations
#'                      for spreading events to start and spreadDT into. This can also
#'                      be used as part of \code{stopRule}. Require input.
#'
#' @param start     Either a vector of pixel numbers to initiate spreading, or a
#'                  data.table that is the output of a previous \code{spreadDT}.
#'                  If a vector, they should be cell indices (pixels) on the \code{landscape}.
#'                  If user has x and y coordinates, these can be converted with
#'                  \code{\link[raster]{cellFromXY}}.
#'
#' @param spreadProb    Numeric or rasterLayer. If numeric of length 1, then this is
#'                      the global probability of
#'                      spreading into each cell from a neighbor. If a raster then this must
#'                      be the cell-specific probability of a "receiving" potential cell.
#'                      Default is \code{0.23}.
#'
#' @param escapeProb    Numeric or RasterLayer, optional. If provided, then this overrides
#'                      the \code{spreadProb} for the first iteration, the "Escape
#'                      Probability". See section on "Breaking out of spreadDT events".
#'
#' @param persistence   A length 1 probability that an active cell will continue to burn,
#'                      per time step.
#'
#' @param size       Numeric. Maximum number of cells for a single or
#'                      all events to be spreadDT. Recycled to match \code{start} length,
#'                      if it is not as long as \code{start}.
#'                      See section on \code{Breaking out of spreadDT events}.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spreadDT.
#'                      Leaving this \code{NULL} allows the spreadDT to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param returnDistances Logical. Should the function inclue a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is FALSE. See Details.
#'
#' @param circle        Logical. If TRUE, then outward spreadDT will be by equidistant rings,
#'                      rather than solely by adjacent cells (via \code{directions} arg.). Default
#'                      is FALSE. Using \code{circle = TRUE} can be dramatically slower for large
#'                      problems. Note, this will likely create unexpected results if \code{spreadProb} < 1.
#'
#' @param circleMaxRadius Numeric. A further way to stop the outward spreadDT of events. If
#'                      \code{circle} is \code{TRUE}, then it will grow to this maximum radius.
#'                      See section on
#'                      \code{Breaking out of spreadDT events}. Default to NA.
#'
#' @param stopRule      A function which will be used to assess whether each individual cluster
#'                      should stop growing. This function can be an argument of "landscape",
#'                      "id", "cells", and
#'                      any other named vectors, a named list of named vectors,
#'                      or a named data.frame of with column names passed to spreadDT in
#'                      the ... . Default NA meaning that
#'                      spreading will not stop as a function of the landscape. See section on
#'                      \code{Breaking out of spreadDT events} and examples.
#'
#' @param stopRuleBehavior Character. Can be one of "includePixel", "excludePixel", "includeRing",
#'                      "excludeRing". If \code{stopRule} contains a function, this argument is
#'                      used determine what to do with the cell(s) that caused the rule to be
#'                      \code{TRUE}. See details. Default is "includeRing" which means to
#'                      accept the entire ring of cells that caused the rule to be \code{TRUE}.
#'
#' @param allowOverlap  Logical. If \code{TRUE}, then individual events can overlap with one
#'                      another, i.e., they do not interact. Currently, this is slower than
#'                      if \code{allowOverlap} is \code{FALSE}. Default is \code{FALSE}.
#'
#' @param asymmetry     A numeric indicating the ratio of the asymmetry to be used. Default is
#'                      NA, indicating no asymmetry. See details. This is still experimental.
#'                      Use with caution.
#'
#' @param asymmetryAngle A numeric indicating the angle in degrees (0 is "up", as in North on a map),
#'                      that describes which way the \code{asymmetry} is.
#'
#' @param quick Logical. If TRUE, then several potentially time consuming checking (such as
#'              \code{inRange}) will be skipped. This should only be used if there is no
#'              concern about checking to ensure that inputs are legal.
#'
#' @param neighProbs An optional numeric vector, whose sum is 1. It indicates the
#'                   probabilities that an individual
#'                   spread iteration will spread to \code{1, 2, ..., length(neighProbs)}
#'                   neighbours, respectively.
#'
#' @param exactSize Logical. If TRUE, then the \code{size} will be treated as exact sizes,
#'                   i.e., the spreadDT events will continue until they are
#'                   \code{floor(size)}. This is overridden by \code{iterations}, but
#'                   if \code{iterations} is run, and individual events haven't reached
#'                   \code{size}, then the returned \code{data.table} will still have
#'                   at least one active cell per event that did not achieve \code{size},
#'                   so that the events can continue if passed into \code{spreadDT} with
#'                   \code{spreadState}.
#'
#' @param ...           Additional named vectors or named list of named vectors
#'                      required for \code{stopRule}. These
#'                      vectors should be as long as required e.g., length
#'                      \code{start} if there is one value per event.
#'
#' @return Either a \code{RasterLayer} (if \code{asRaster} is \code{TRUE}, the default).
#' If a \code{RasterLayer}, then it represents
#' every cell in which a successful spreadDT event occurred. For the case of, say, a fire
#' this would represent every cell that burned. If \code{allowOverlap} is \code{TRUE},
#' the return will always be a \code{data.table}.
#'
#' If \code{asRaster} is false, then this function returns a \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{id} \tab an arbitrary ID \code{1:length(start)} identifying
#'                      unique clusters of spreadDT events, i.e., all cells
#'                      that have been spreadDT into that have a
#'                      common initial cell.\cr
#'   \code{initialLocus} \tab the initial cell number of that particular
#'                            spreadDT event.\cr
#'   \code{indices} \tab The cell indices of cells that have
#'                        been touched by the spreadDT algorithm.\cr
#'   \code{active} \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#' }
#'
#' This will generally be more useful when \code{allowOverlap} is \code{TRUE}.
#' @export
#' @importFrom raster ncell raster res
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
                                persistence = 0, size,
                                directions = 8L, iterations = 1e6L,
                                returnDistances = FALSE,
                                plot.it = FALSE,
                                spreadProbLater = NA_real_, spreadState = NA,
                                circle = FALSE, circleMaxRadius = NA_real_,
                                stopRule = NA, stopRuleBehavior = "includeRing",
                                allowOverlap = FALSE,
                                asymmetry = NA_real_, asymmetryAngle = NA_real_,
                                quick = FALSE, neighProbs = NA_real_, exactSize = FALSE,
                                relativeSpreadProb = FALSE, ...) {
  standardGeneric("spreadDT")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spreadDT event grow.
#'
#' @rdname spreadDT
#'
#' @example inst/examples/example_spread.R
#'
setMethod(
  "spreadDT",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, start, spreadProb, asRaster,
                        persistence,
                        size,
                        directions, iterations,
                        returnDistances, plot.it, spreadProbLater,
                        circle, circleMaxRadius, stopRule,
                        stopRuleBehavior, allowOverlap, asymmetry, asymmetryAngle,
                        quick, neighProbs, exactSize, relativeSpreadProb,
                        ...) {


    #assertions
    assertClass(landscape, "Raster")
    ncells <- ncell(landscape)
    assert(
      checkNumeric(start, min.len=0, max.len=ncells, lower = 1, upper=ncells),
      checkDataTable(start, ncols=5, types=rep("numeric", 5)))

    qassert(neighProbs, "n[0,1]")
    assertNumeric(sum(neighProbs), lower = 1, upper = 1)

    assert(
      checkNumeric(spreadProb, 0, 1, min.len=1, max.len=ncells),
      checkClass(spreadProb, "RasterLayer")
    )
    qassert(directions, "N1[4,8]")
    qassert(iterations, "N1[1,Inf]")
    qassert(circle, "B")
    if(circle)
      qassert(spreadProb, "N1[1,1]")

    if(!missing(size)) {
      assert(
        checkNumeric(size, min.len = 1, max.len=1),
        checkNumeric(size, min.len = NROW(start), max.len=NROW(start))
      )
    } else {
      size <- NA
    }
    ##### End assertions

    needDistance <- returnDistances | circle
    maxRetriesPerID <- 10 # This means that if an event can not spread any more, it will try 10 times, including 2 jumps


    if(!is.data.table(start)) {
      start <- as.integer(start)
      dtInitial <- data.table(initialPixels=start, size = size)
      #if(!anyNA(neighProbs)) set(dtInitial, , "neighProbs", neighProbs)
      setkey(dtInitial, "initialPixels")
      ids <- data.table(id=1:NROW(start), initialPixels=start, numRetries=0, key="initialPixels")
      dt <- data.table(initialPixels=start, pixels=start,
                       potentialPixels=start, state="activeSource")#, distance=NA_real_)
    } else {
      dtInitial <- data.table(initialPixels=unique(start$initialPixels), size = size)
      #if(!anyNA(neighProbs)) set(dtInitial, , "neighProbs", neighProbs)
      setkey(dtInitial, "initialPixels")
      ids <- start[,list(id, initialPixels)]
      dt <- start
      set(dt, , "id", NULL)
    }
    if(needDistance)
      set(dt, , "distance", 0) # it is zero distance to self

    its <- 0

    setkeyv(dt, "state")

    needRetryID <- numeric()
    whSucc <- which(dt$state=="activeSource")

    while((length(needRetryID) | length(whSucc)) &  its < iterations) {

      #print(dtInitial)

      if(length(needRetryID)>0){
        dtState <- dt$state=="needRetry"
        dtRetry <- dt[dtState];
        if(any(((ids$numRetries+1) %% 5) == 0)) { # jump every 5, starting at 4
          resCur <- res(landscape)[1]
          fromPixels <- dt[dtState]$pixels
          potentialPixels <- cir(landscape, loci = fromPixels, includeBehavior = "excludePixels",
                                 minRadius = resCur, maxRadius=4*resCur, allowOverlap = TRUE)
          potentialPixels <- data.table(id=as.integer(potentialPixels[,"id"]),
                                        indices = as.integer(potentialPixels[,"indices"]))[
                                          ,`:=`(from=as.integer(fromPixels[id]), id=as.integer(dtRetry$initialPixels[id]))]

          dtPotential <- potentialPixels[,list(to=resample(indices, 2)),by=c("id","from")]

        } else {
          adjMat <- adj(landscape, directions=directions, id=dtRetry$initialPixels,
                        cells = dtRetry$pixels)
          dtPotential <- data.table(from=as.integer(adjMat[,"from"]),
                                    to=as.integer(adjMat[,"to"]),
                                    id=as.integer(adjMat[,"id"]))
        }
        set(dtPotential, ,"state", "potential" )

        whActiveSrc <- which(dt$state == "activeSource")
        set(dt, whActiveSrc, "state", "holding")
        whNeedRetry <- which(dt$state == "needRetry")
        set(dt, whNeedRetry, "state", "activeSource")
        needRetryID <- numeric()

      } else { # Spread to immediate neighbours
        dtActiveSrc <- dt$state == "activeSource"
        adjMat <- adj(landscape, directions=directions, id=dt$initialPixels[dtActiveSrc],
                      cells = dt$pixels[dtActiveSrc])
        dtPotential <- data.table(from=as.integer(adjMat[,"from"]),
                                  to=as.integer(adjMat[,"to"]),
                                  id=as.integer(adjMat[,"id"]),
                                  state="potential")#, distance=NA_real_)

        # Only increment iteration if it is NOT a retry situation
        its <- its + 1
      }
      if(needDistance)
        set(dtPotential, , "distance", NA_real_)

      setnames(dtPotential, c("from", "to", "id"), c("pixels", "potentialPixels", "initialPixels"))
      setcolorder(dtPotential, names(dt))

      # randomize row order so duplicates are not always in same place
      i <- sample.int(NROW(dtPotential))
      for(x in colnames(dtPotential)) set(dtPotential, , x, dtPotential[[x]][i])

      # combine potentials to previous
      #dt <- rbindlist(list(dt, dtPotential))
      # Remove duplicates within id
      if(needDistance) {
        fromPts <- xyFromCell(landscape,dtPotential$initialPixels)
        toPts <- xyFromCell(landscape,dtPotential$potentialPixels)
        set(dtPotential, , "distance", pointDistance(p1 = fromPts, p2 = toPts, lonlat=FALSE))
        if(circle) {
          dtPotential <- dtPotential[(distance %<=% its & distance %>>% (its-1))]
        }
      }

      if(!anyNA(neighProbs)) {

        # Get neighbours, either via adj (default), cir (jumping if stuck), or numNeighs
        numNeighsByPixel <- unique(dtPotential, by = c("initialPixels", "pixels"))
        set(numNeighsByPixel, , "numNeighs",
            sample.int(size = NROW(numNeighsByPixel), n = length(neighProbs),
                       replace = TRUE, prob = neighProbs))
        setkeyv(numNeighsByPixel, c("initialPixels", "pixels"))

        # remove duplicates from the already selected "pixels" and new "potentialPixels", since it must select exactly numNeighs
        dups <- duplicated(c(dt$pixels, dtPotential$potentialPixels))
        dups <- dups[-seq_along(dt$pixels)]
        dtPotential <- dtPotential[!dups]
        setkeyv(dtPotential, c("initialPixels", "pixels")) # sort so it is the same as numNeighsByPixel
        set(dtPotential, , "spreadProb", spreadProb[dtPotential$potentialPixels])
        # If it is a corner or has had pixels removed bc of duplicates, it may not have enough neighbours
        numNeighsByPixel <- numNeighsByPixel[dtPotential[,.N,by=c("initialPixels", "pixels")]]
        set(numNeighsByPixel, , "numNeighs", pmin(numNeighsByPixel$N, numNeighsByPixel$numNeighs, na.rm=TRUE))

        dtPotential <- dtPotential[dtPotential[,list(keepIndex=
                                                       resample(.I, numNeighsByPixel$numNeighs[.GRP],
                                                                prob=spreadProb/sum(spreadProb,na.rm=TRUE))),
                                               by="pixels"]$keepIndex]
        set(dtPotential, , "spreadProb", NULL)
      } else {

        # Extract spreadProb for the current set of potentials
        actualSpreadProb <- if(length(spreadProb)==1) {
          spreadProb
        } else {
          spreadProb[dtPotential$potentialPixels]
        }

        # Evaluate against spreadProb --> convert "potential" to "successful"
        dtPotential <- dtPotential[runif(NROW(dtPotential))<actualSpreadProb]
      }
      set(dtPotential, , "state", "successful")

      dt <- rbindlist(list(dt, dtPotential))

      # Remove duplicates
      if(anyNA(neighProbs)) {
          if(allowOverlap) {
          dt[,`:=`(dups=duplicated(potentialPixels)),by=initialPixels]
          dupes <- dt$dups
          set(dt, , "dups", NULL)
        } else {
          dupes <- duplicated(dt$potentialPixels)
        }
        # remove any duplicates
        dt <- dt[!dupes]
      }


      if(!anyNA(size)) {
        setkeyv(dt,"initialPixels") # must sort because size is sorted
        currentSize <- dt[,.N,by=initialPixels][,`:=`(size=dtInitial$size,
                                                      tooBig=N-dtInitial$size)]

        currentSizeTooBig <- currentSize[tooBig>0]
        if(NROW(currentSizeTooBig)>0) {
          # sort them so .GRP works on 3rd line
          setkeyv(currentSizeTooBig, "initialPixels")
          dt <- dt[-dt[state=="successful" & (initialPixels %in% currentSizeTooBig$initialPixels),
                       resample(.I, currentSizeTooBig[.GRP]$tooBig),by=initialPixels]$V1][
                         initialPixels %in% currentSizeTooBig$initialPixels,state:="inactive"]
        }

        if(exactSize) {
          currentSizeTooSmall <- currentSize[tooBig<0]
          if(NROW(currentSizeTooSmall)>0) {
            dt2 <- dt[initialPixels %in% currentSizeTooSmall$initialPixels & (state=="successful" | state=="holding")] # successful means will become activeSource next iteration
            setkeyv(dt2, "initialPixels")
            setkeyv(currentSizeTooSmall, "initialPixels")
            currentSizeTooSmall <- currentSizeTooSmall[!dt2]
          }
          # if the ones that are too small are unsuccessful, make them "needRetry"
          keep <- which(dt$initialPixels %in% currentSizeTooSmall$initialPixels &
                          (dt$state!="successful" & dt$state!="inactive"))
          if(length(keep)) {

            needRetryID <- ids$initialPixels %in% unique(dt$initialPixels[keep])
            tooManyRetries <- ids$numRetries > maxRetriesPerID
            if(sum(tooManyRetries * needRetryID)>0) {
              needRetryID <- needRetryID & !(needRetryID * tooManyRetries)
              keep <- keep[dt$initialPixels[keep] %in%
                             ids$initialPixels[needRetryID]]
            }
            needRetryID <- which(needRetryID)

            set(dt,keep,"state","needRetry")
            set(ids,needRetryID,"numRetries",ids$numRetries[needRetryID]+1)
          }
        }
      }

      # Change states of cells
      set(dt, which(dt$state=="activeSource"), "state", "inactive")
      set(dt, which(dt$state=="holding"), "state", "successful")# return holding cells to successful
      whSucc <- which(dt$state=="successful")
      set(dt, whSucc, "state", "activeSource")# return holding cells to successful
      set(dt, whSucc, "pixels", dt$potentialPixels[whSucc])# return holding cells to successful

      if(plot.it) {
        newPlot <- FALSE
        if(!exists("ras", inherits = FALSE)) {
          newPlot <- TRUE
        }
        if(newPlot)
          ras <- raster(landscape)
        if(returnDistances) {
          ras[dt$potentialPixels] <- dt$distance
          newPlot <- TRUE
        } else {
          setkeyv(dt, "initialPixels")
          ras[dt$potentialPixels] <- dt[ids]$id
        }
        Plot(ras, new=newPlot)
      }

    } # end of main loop

    ## clean up ##
    set(dt, , "pixels", dt$potentialPixels)
    set(dt, , "potentialPixels", NULL)

    # join ids to main table
    dt <- dt[ids,on="initialPixels"]
    setkeyv(dt, "id") # so order is same as start

    if(asRaster) {
      ras <- raster(landscape)
      # inside unit tests, this raster gives warnings if it is only NAs
      suppressWarnings(ras[dt$pixels] <- dt$id)
      return(ras)
    }
    return(dt)
  }
)

