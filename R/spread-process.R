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
#' @param persistence   A probability that a burning cell will continue to burn,
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
#' @param ...           Additional parameters.
#'
#' @return A \code{RasterLayer} indicating the spread of the process in the landscape.
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
                              spreadProbLater = NA_real_, ...) {
  standardGeneric("spread")
})

#' @param plot.it  If TRUE, then plot the raster at every iteraction,
#'                   so one can watch the spread event grow.
#'
#' @param mapID    Logical. If TRUE, returns a raster of events ids.
#'                 If FALSE, returns a raster of iteration numbers,
#'                 i.e., the spread history of one or more events.
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
#' Plot(fires, addTo = "hab2$hab", zero.color = "white",
#'      cols = colorRampPalette(c("orange","darkred"))(10))
#' # or overplot the original (NOTE: legend stays at original values)
#' Plot(fires,
#'      cols = topo.colors(10))
#'
setMethod(
  "spread",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, spreadProb,
                        persistence, mask,
                        maxSize, directions, iterations,
                        lowMemory, returnIndices, mapID,
                        plot.it, spreadProbLater, ...) {

    if(!is(spreadProbLater, "Raster")) {
      if(is.na(spreadProbLater)) {
        spreadProbLater <- spreadProb
      }
    }
    ### should sanity check map extents
    if (any(is.na(loci)))  {
      # start it in the centre cell
      loci <- (nrow(landscape)/2L + 0.5) * ncol(landscape)
    }

    if(is(spreadProbLater,"RasterLayer") | is(spreadProb, "Rasterlayer")) {
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

    ## Recycling maxSize as needed
    maxSize <- if(any(!is.na(maxSize))) {
      rep_len(maxSize, length(loci))
    } else {
      ncell(landscape)
    }

    if(lowMemory) {
      spreads <- ff(vmode = "short", 0, length = ncell(landscape))
    } else {
      spreads <- vector("integer", ncell(landscape))
    }

    n <- 1L
    if (mapID) {
      spreads[loci] <- 1L:length(loci)
      if(length(maxSize) > 1L){
        size <- rep_len(1L, length(loci))
      } else {
        size <- length(loci)
      }
    } else {
      spreads[loci] <- n
      size <- length(loci)
    }

    # Convert mask and NAs to 0 on the spreadProbLater Raster
    if (is(spreadProbLater, "Raster")) {
      # convert NA to 0s
      spreadProbLater[is.na(spreadProbLater)] <- 0L
    } else if (is.numeric(spreadProbLater)) {
      # Translate numeric spreadProbLater into a Raster, if there is a mask
      if(is(mask, "Raster")) {
        spreadProbLater <- raster(extent(landscape), res = res(landscape), vals = spreadProbLater)
      }
    }


    # Convert mask and NAs to 0 on the spreadProb Raster
    if (is(spreadProb, "Raster")) {
      # convert NA to 0s
      spreadProb[is.na(spreadProb)] <- 0L
    } else if (is.numeric(spreadProb)) {
      # Translate numeric spreadProb into a Raster, if there is a mask
      if(is(mask, "Raster")) {
        spreadProb <- raster(extent(landscape), res = res(landscape), vals = spreadProb)
      }
    }

    # Mask spreadProbLater and spreadProb
    if(is(mask, "Raster")) {
      spreadProbLater[mask == 1L] <- 0L
    }
    if(is(mask, "Raster")) {
      spreadProb[mask == 1L] <- 0L
    }

    # while there are active cells
    while (length(loci)) {

      # identify neighbours
      if (mapID) {
        potentials <- adj(landscape, loci, directions, pairs = TRUE)
      } else {
        # must pad the first column of potentials
        potentials <- cbind(NA, adj(landscape, loci, directions, pairs = FALSE))
      }

      # keep only neighbours that have not been spread to yet
      potentials <- potentials[spreads[potentials[, 2L]] == 0L, , drop = FALSE]

      if (n==2) {
        spreadProb <- spreadProbLater
      }

      if (is.numeric(spreadProb)) {
        spreadProbs <- spreadProb
      } else {
        spreadProbs <- spreadProb[potentials[, 2L]]
      }

      potentials <- potentials[runif(NROW(potentials)) <= spreadProbs,, drop = FALSE]
      potentials <- potentials[sample.int(NROW(potentials)),, drop = FALSE]
      potentials <- potentials[!duplicated(potentials[, 2L]),, drop = FALSE]
      events <- potentials[, 2L]

      # Implement maxSize
      if(length(maxSize) == 1L) {
        len <- length(events)
        if((size+len) > maxSize) {
          keep <- len - ((size+len) - maxSize)
          samples <- sample(len,keep)
          events <- events[samples]
          potentials <- potentials[samples, , drop = FALSE]
        }
        size <- size + length(events)
      } else {
        len <- tabulate(spreads[potentials[, 1L]], length(maxSize))
        if ( any( (size + len) > maxSize & size < maxSize) ) {
          whichID <- which(size + len > maxSize)
          toRm <- (size + len)[whichID] - maxSize[whichID]

          for(i in 1:length(whichID)){
            thisID <- which(spreads[potentials[, 1L]] == whichID[i])
            potentials <- potentials[-sample(thisID, toRm[i]), , drop = FALSE]
          }
          events <- potentials[, 2L]
        }
        size <- pmin(size + len, maxSize) ## Quick? and dirty,
                                          ## fast but loose (too flexible)
      }

      # update eligibility map
      n <- n + 1L

      if (length(events) > 0){
        if (mapID) {
          spreads[events] <- spreads[potentials[, 1L]]
        } else {
          spreads[events] <- n
        }
      }

      if(length(maxSize) > 1L){
        if(exists("whichID")){
          events <- events[!spreads[events] %in% whichID]
          rm(whichID)
        }

      } else {
        if(size >= maxSize) {
          events <- NULL
        }
      }

      # drop or keep loci
      if (is.na(persistence) | is.na(persistence) | persistence == 0L) {
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

      if (plot.it){
        plotCur <- raster(landscape)
        plotCur <- setValues(plotCur,spreads)
        Plot(plotCur, ...)
      }
    }

    # Convert the data back to raster
    if(lowMemory){
      wh <- ffwhich(spreads, spreads>0) %>% as.ram
      if(returnIndices) {
        return(data.table(indices = wh, value = spreads[wh]))
      }
#      spre[wh] <- spreads[wh]
    } else {
      wh <- spreads>0
      if(returnIndices) {
        return((wh) %>%
          which %>%
          data.table(indices = ., value = spreads[.]))
      }
#      spre[wh] <- spreads[wh]
    }
    spre <- raster(landscape)
    spre[] <- 0
    spre[wh] <- spreads[wh]
    return(spre)
  }
)
