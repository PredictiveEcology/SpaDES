################################################################################
#' \code{SELES} - Transitioning to next time step
#'
#' @description
#' Describes the probability of an agent successfully persisting until next
#' time step. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A \code{SELES}-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' \href{http://www.gowlland.ca/}{SELES}.
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param p realized probability of persisting (i.e., either 0 or 1).
#'
#' @param agent \code{SpatialPoints*} object.
#'
#' @return Returns new \code{SpatialPoints*} object with potentially fewer agents.
#'
#' @importFrom sp 'coordinates<-'
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELEStransitions
#'
#' @author Eliot McIntire
transitions <- function(p, agent) {
    coordinates(agent)[which(p == 0), ] <- NA
    return(agent)
}

##############################################################
#' SELES - Number of Agents to initiate
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A \code{SELES}-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' \href{http://www.gowlland.ca/}{SELES}.
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param N         Number of agents to intitate (integer scalar).
#' @param probInit  Probability of initalizing an agent at the location.
#'
#' @return A numeric, indicating number of agents to start
#'
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESnumAgents
#'
#' @author Eliot McIntire
numAgents <- function(N, probInit) {
  stopifnot((length(N) == 1), (is.numeric(N)))
  return(N)
}

##############################################################
#' \code{SELES} - Initiate agents
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT FULLY IMPLEMENTED.
#'
#' A \code{SELES}-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' \href{http://www.gowlland.ca/}{SELES}.
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map RasterLayer with extent and resolution of desired return object
#'
#' @param numAgents numeric resulting from a call to \code{\link{numAgents}}
#'
#' @param probInit a Raster resulting from a \code{\link{probInit}} call
#'
#' @param asSpatialPoints logical. Should returned object be \code{RasterLayer}
#'                        or \code{SpatialPointsDataFrame} (default)
#'
#' @param indices numeric. Indices of where agents should start
#'
#' @return A SpatialPointsDataFrame, with each row representing an individual agent
#'
#' @include agent.R
#' @importFrom raster getValues ncell raster xyFromCell
#' @importFrom stats runif
#' @export
#' @docType methods
#' @rdname initiateAgents
#'
#' @author Eliot McIntire
#'
#' @examples
#' library(dplyr)
#' library(raster)
#' map <- raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10, val = 0, res = 1)
#' map <- gaussMap(map, scale = 1, var = 4, speedup = 1)
#' pr <- probInit(map, p = (map/maxValue(map))^2)
#' agents <- initiateAgents(map, 100, pr)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(map)
#'   Plot(agents, addTo = "map")
#' }
#'
#' # Note, can also produce a Raster representing agents,
#' # then the number of points produced can't be more than
#' # the number of pixels:
#' agentsRas <- initiateAgents(map, 30, pr, asSpatialPoints = FALSE)
#' if (interactive()) Plot(agentsRas)
#'
#' # Check that the agents are more often at the higher probability areas based on pr
#' out <- data.frame(stats::na.omit(crosstab(agentsRas, map)), table(round(map[]))) %>%
#'    dplyr::mutate(selectionRatio = Freq/Freq.1) %>%
#'    dplyr::select(-Var1, -Var1.1) %>%
#'    dplyr::rename(Present = Freq, Avail = Freq.1, Type = Var2)
#' out
#'
setGeneric("initiateAgents",
          function(map, numAgents, probInit, asSpatialPoints = TRUE, indices) {
            standardGeneric("initiateAgents")
})

#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "missing", "missing", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints) {
    initiateAgents(map, indices = 1:ncell(map), asSpatialPoints = asSpatialPoints)
})

#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "missing", "Raster", "ANY", "missing"),
  function(map, probInit, asSpatialPoints) {
    wh <- which(runif(ncell(probInit)) < getValues(probInit))
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "numeric", "missing", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints, indices) {
    wh <- sample(1:ncell(map), size = numAgents, replace = asSpatialPoints)
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "numeric", "Raster", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints) {
    vals <- getValues(probInit)
    wh <- sample(1:ncell(probInit), numAgents, replace = asSpatialPoints,
                 prob = vals / sum(vals))
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @rdname initiateAgents
setMethod("initiateAgents",
          signature = c("Raster", "missing", "missing", "ANY", "numeric"),
          function(map, numAgents, probInit, asSpatialPoints, indices) {
            if (asSpatialPoints) {
              if (length(indices > 0)) {
                xys <- xyFromCell(map, indices, spatial = asSpatialPoints)
                xys@coords <- xys@coords + cbind(runif(length(indices), -res(map)[1] / 2, res(map)[1] / 2),
                                                 runif(length(indices), -res(map)[2] / 2, res(map)[2] / 2))
                xys@bbox <- cbind(apply(coordinates(xys), 2, min), apply(coordinates(xys), 2, max))
                return(xys)
              }
            } else {
              tmp <- raster(extent(map), res = res(map), vals = 0)
              tmp[indices] <- 1
              return(tmp)
            }
})

################################################################################
#' \code{SELES} - Agent Location at initiation
#'
#' @description
#' Sets the the location of the intiating agents. NOT YET FULLY IMPLEMENTED.
#'
#' A \code{SELES}-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' \href{http://www.gowlland.ca/}{SELES}.
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A \code{SpatialPoints*}, \code{SpatialPolygons*}, or \code{Raster*} object.
#'
#' @return Object of same class as provided as input.
#'          If a \code{Raster*}, then zeros are converted to \code{NA}.
#'
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESagentLocation
#' @author Eliot McIntire
agentLocation <- function(map) {
  if (length(grep(pattern = "Raster", class(map))) == 1) {
    map[map == 0] <- NA
  } else if (length(grep(pattern = "SpatialPoints", class(map))) == 1) {
    map
  } else if (!is.na(pmatch("SpatialPolygons", class(map)))) {
    map
  } else {
    stop("only raster, Spatialpoints or SpatialPolygons implemented")
  }
  return(map)
}

##############################################################
#' \code{SELES} - Probability of Initiation
#'
#' @description
#' Describes the probability of initiation of agents or events.  THIS IS NOT FULLY IMPLEMENTED.
#'
#' A \code{SELES}-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' \href{http://www.gowlland.ca/}{SELES}.
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A \code{spatialObjects} object. Currently, only provides CRS and, if p is not
#' a raster, then all the raster dimensions.
#'
#' @param p probability, provided as a numeric or raster
#'
#' @param absolute logical. Is \code{p} absolute probabilities or relative?
#'
#' @return An RasterLayer with probabilities of initialization. There are several combinations
#' of inputs possible and they each result in different behaviors.
#'
#' If \code{p} is numeric or Raster and between 0 and 1, it is treated as an absolute probability, and a map
#' will be produced with the p value(s) everywhere.
#'
#' If \code{p} is numeric or Raster and not between 0 and 1, it is treated as a relative probability, and a map
#' will be produced with p/max(p) value(s) everywhere
#'
#' If \code{absolute} is provided, it will override the previous statements, unless \code{absolute}
#' is TRUE and p is not between 0 and 1 (i.e., is not a probability)
#'
#' @importFrom raster cellStats crs extent setValues raster
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESprobInit
#' @author Eliot McIntire
probInit <- function(map, p = NULL, absolute = NULL) {
  if (all(inRange(p, 0, 1))) {
    if (is.null(absolute)) {
      absolute <- TRUE
    }
  } else {
    absolute <- FALSE
  }
  if (is.numeric(p)) {
    probInit <- raster(extent(map), nrows = nrow(map), ncols = ncol(map),
                       crs = crs(map))
    p <- rep(p, length.out = ncell(map))
    probInit <- setValues(probInit, p / (sum(p) * (1 - absolute) + 1 * (absolute)))

  } else if (is(p, "RasterLayer")) {
    probInit <- p / (cellStats(p, sum) * (1 - absolute) + 1 * (absolute))
  } else if (is(map, "SpatialPolygonsDataFrame")) {
    probInit <- p / sum(p)
  } else {
    stop("Error initializing probability map: bad inputs")
  }
  return(probInit)
}

#' Patch size
#'
#' @param patches Number of patches.
#'
#' @importFrom raster freq
patchSize <- function(patches) {
  return(freq(patches))
}
