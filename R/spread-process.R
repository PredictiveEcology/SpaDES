if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("indices", "eventID", "initialLocus"))
}

###############################################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulated fires or other things.
#' Essentially, it starts from a collection of cells (\code{loci}) and spreads
#' to neighbours, according to the \code{directions} and \code{spreadProbLater} arguments.
#' This can become quite general, if \code{spreadProbLater} is 1 as it will expand
#' from every loci until all pixels in the landscape have been covered.
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
#' @param landscape     A \code{RasterLayer} object.
#'
#' @param loci          A vector of locations in \code{landscape}
#'
#' @param spreadProb    Numeric or rasterLayer.  The overall probability of
#'                      spreading, or probability raster driven. Default is 0.23.
#'                      If a \code{spreadProbLater} is provided, then this is
#'                      only used for the first iteration. Also called Escape
#'                      probability.
#'
#' @param persistence   A probability that an active cell will continue to burn,
#'                      per time step.
#'
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with
#'                      \code{landscape} whose elements are \code{0,1},
#'                      where 1 indicates "cannot spread to".
#'                      Currently not implemented.
#'
#' @param maxSize       Vector of the maximum number of pixels for a single or
#'                      all events to be spread.
#'                      Recycled to match \code{loci} length.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case).
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
#'                      \code{spread}. See Details.
#'
#' @param ...           Additional parameters.
#'
#' @return Either a \code{RasterLayer} indicating the spread of the process in
#' the landscape or a \code{data.table}. If a \code{RasterLayer}, then it represents
#' every pixel in which a successful spread event occurred. For the case of, say, a fire
#' this would represent every pixel that burned. If \code{returnIndices} is \code{TRUE},
#' then this function returns a \code{data.table} with columns:
#'
#' \code{indices} is the pixel indices of pixels that have been touched by the spread
#' algorithm.
#'
#' \code{eventID} is an arbitrary ID \code{1:length(loci)} identifying unique clusters
#' of spread events, i.e., all pixels that have been spread into that have a common
#' initial pixel.
#'
#' \code{active} is a logical indicating whether the pixel is active (i.e., could still
#' be a source for spreading) or not (no spreading will occur from these pixels).
#'
#' \code{initialIndex}, the initial pixel number of that particular spread event.
#'
#'
#'
#' @export
#' @importFrom raster extent maxValue minValue ncell ncol nrow raster res setValues
#' @importFrom ff ff as.ram
#' @importFrom ffbase ffwhich
#' @importFrom stats runif
#' @docType methods
#'
#' @author Steve Cumming \email{Steve.Cumming@@sbf.ulaval.ca}
#' @author Eliot McIntire
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
#' a <- raster(extent(0,1e2,0,1e2), res = 1)
#' hab <- gaussMap(a,speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab) = "hab"
#' cells <- loci <- b <- as.integer(sample(1:ncell(a),1e1))
#' mask <- raster(a)
#' mask <- setValues(mask, 0)
#' mask[1:5000] <- 1
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' directions <- 8
#'
#' # Transparency involves putting two more hex digits on the color code: 00 is fully transparent.
#' setColors(hab) <- paste(c("#FFFFFF", brewer.pal(8, "Greys")), c("00", rep("FF", 8)), sep = "")
#'
#' #dev(4)
#' Plot(hab, new = TRUE, speedup = 3) # note speedup is equivalent to making pyramids,
#'                              # so, some details are lost
#'
#' # initiate 10 fires at to loci
#' fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
#'                 0.235, 0, NULL, 1e8, 8, 1e6, mapID = TRUE)
#' #set colors of raster, including a transparent layer for zeros
#' setColors(fires, 10) <- c("#00000000", brewer.pal(8,"Reds")[5:8])
#' Plot(fires)
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
#' ## Use interrupt a spread event using iterations - need returnIndices=TRUE to use outputs
#' ##   as new inputs in next iteration
#' fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), returnIndices=TRUE,
#'                 0.235, 0, NULL, 1e8, 8, iterations = 3, mapID = TRUE)
#' fires[,list(size=length(initialLocus)), by=eventID]  # See sizes of fires
#'
#' ## Continue event by passing interrupted object into spreadState
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
setMethod(
  "spread",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, spreadProb,
                        persistence, mask,
                        maxSize, directions, iterations,
                        lowMemory, returnIndices, mapID,
                        plot.it, spreadProbLater, spreadState,
                        ...) {

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

    if (lowMemory) {
      spreads <- ff(vmode = "short", 0, length = ncell(landscape))
    } else {
      spreads <- vector("integer", ncell(landscape))
    }

    n <- 1L
    if (mapID | returnIndices) {
      spreads[loci] <- 1L:length(loci)
    } else {
      spreads[loci] <- n
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
      if (sum(colnames(spreadState) %in% c("indices", "eventID", "active", "initialLocus")) == 4) {
        spreads[loci] <- spreads[loci] + spreadState[, max(eventID)] # reassign old ones
        spreads[spreadState[,indices]] <- spreadState[, eventID]
        loci <- c(spreadState[active == TRUE, indices], loci) %>% na.omit()
      } else {
        stop("spreadState must have at least columns: ",
             "indices, eventID, active, and initialLocus.")
      }
    }

    ## Recycling maxSize as needed
    if (any(!is.na(maxSize))) {
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
      if (mapID | returnIndices) {
        potentials <- adj(landscape, loci, directions, pairs = TRUE)
      } else {
        # must pad the first column of potentials
        potentials <- cbind(NA, adj(landscape, loci, directions, pairs = FALSE))
      }

      # keep only neighbours that have not been spread to yet
      potentials <- potentials[spreads[potentials[, 2L]] == 0L, , drop = FALSE]

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

      potentials <- potentials[runif(NROW(potentials)) <= spreadProbs, , drop = FALSE]
      potentials <- potentials[sample.int(NROW(potentials)), , drop = FALSE]
      potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
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
        len <- tabulate(spreads[potentials[, 1L]], length(maxSize))
        if ( any( (size + len) > maxSize & size < maxSize) ) {
          whichID <- which(size + len > maxSize)
          toRm <- (size + len)[whichID] - maxSize[whichID]

          for (i in 1:length(whichID)) {
            thisID <- which(spreads[potentials[, 1L]] == whichID[i])
            potentials <- potentials[-sample(thisID, toRm[i]), , drop = FALSE]
          }
          events <- potentials[, 2L]
        }
        size <- pmin(size + len, maxSize) ## Quick? and dirty. fast but loose (too flexible)
      }

      # increment iteration
      n <- n + 1L

      if (length(events) > 0){
        if (mapID | returnIndices) {
          spreads[events] <- spreads[potentials[, 1L]]
        } else {
          spreads[events] <- n
        }
      }

      if (length(maxSize) > 1L) {
        if (exists("whichID")) {
          events <- events[!spreads[events] %in% whichID]
          rm(whichID)
        }

      } else {
        if (size >= maxSize) {
          events <- NULL
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

      loci <- c(loci, events)

      if (plot.it) {
        plotCur <- raster(landscape)
        plotCur <- setValues(plotCur, spreads)
        Plot(plotCur, ...)
      }
    }

    # Convert the data back to raster
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

    if (returnIndices) {
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
      return(allCells)
    }

    spre <- raster(landscape)
    spre[] <- 0
    spre[wh] <- spreads[wh]
    if (exists("potentials"))
      if (NROW(potentials) > 0)
        spre[potentials[, 1L]] <- spreads[potentials[, 2L]]
    return(spre)
  }
)

