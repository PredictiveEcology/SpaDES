##############################################################
#' SELES - Transitioning to next time step
#'
#' A SELES-like function to maintain conceptual backwards compatability with that simulation
#' tool. Describes the probability of an agent successfully persisting until next
#' time step. THIS IS NOT FULLY IMPLEMENTED.
#' This is intended to ease transitinos from
#' \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful
#'
#' @param p realized probability of persisting (i.e., either 0 or 1)
#'
#' @param agent SpatialPoints* object
#'
#' @return Returns new SpatialPoints* object with potentially fewer agents
#'
#' @import sp
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELEStransitions
#'
#' @author Eliot McIntire
transitions <- function(p, agent) {
    coordinates(agent)[which(p==0),] = NA
    return(agent)
}

##############################################################
#' SELES - Number of Agents to initiate
#'
#' A SELES-like function to maintain conceptual backwards compatability with that simulation
#' tool. Sets the the number of agents to initiate. THIS IS NOT FULLY IMPLEMENTED.
#' This is intended to ease transitinos from
#' \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful
#'
#' @param N Number of agents to intitate
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
    if ((length(N) == 1) && (is.numeric(N))) numAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(numAgents)
}

##############################################################
#' SELES - Initiate agents
#'
#' A SELES-like function to maintain conceptual backwards compatability with that simulation
#' tool. Sets the the number of agents to initiate. THIS IS NOT FULLY IMPLEMENTED.
#' This is intended to ease transitinos from
#' \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful
#'
#' @param map RasterLayer with extent and resolution of desired return object
#'
#' @param numAgents numeric resulting from a call to \code{\link{numAgents}}
#'
#' @param probInit RasterLayer resulting from a call to \code{\link{probInit}}
#'
#' @param asRaster logical. Should returned object be raster or SpatialPointsDataFrame (default)
#'
#' @param indices numeric. Indices of where agents should start
#'
#' @return A SpatialPointsDataFrame, with each row representing an individual agent
#'
#' @include agent.R
#' @import raster
#' @export
#' @docType methods
#' @rdname initiateAgents
#' @name initiateAgents
#'
#' @author Eliot McIntire
#' @param probInit a Raster resulting from a \code{\link{probInit}} call
setGeneric("initiateAgents",
          function(map, numAgents, probInit, asSpatialPoints=TRUE, indices) {
            standardGeneric("initiateAgents")
          })

#' @rdname initiateAgents
#' @name initiateAgents
setMethod("initiateAgents",
          signature=c("Raster", "missing", "missing", "ANY", "missing"),
          function(map, numAgents, probInit, asSpatialPoints) {
            initiateAgents(map, indices=1:ncell(map), asSpatialPoints=asSpatialPoints)
})

#' @rdname initiateAgents
#' @name initiateAgents
setMethod("initiateAgents",
          signature=c("Raster", "missing", "Raster", "ANY", "missing"),
          function(map, probInit, asSpatialPoints) {
            wh <- which(runif(ncell(probInit)) < getValues(probInit))
            initiateAgents(map, indices=wh, asSpatialPoints=asSpatialPoints)
          })

#' @rdname initiateAgents
#' @name initiateAgents
setMethod("initiateAgents",
          signature=c("Raster", "numeric", "missing", "ANY", "missing"),
          function(map, numAgents, probInit, asSpatialPoints, indices) {
            wh <- sample(1:ncell(map), size = numAgents, replace = asSpatialPoints)
            initiateAgents(map, indices=wh, asSpatialPoints=asSpatialPoints)
          })

#' @rdname initiateAgents
#' @name initiateAgents
setMethod("initiateAgents",
          signature=c("Raster", "numeric", "Raster", "ANY", "missing"),
          function(map, numAgents, probInit, asSpatialPoints) {
            vals <- getValues(probInit)
            wh <- sample(1:ncell(probInit), numAgents, replace = asSpatialPoints,
                   prob=vals/sum(vals))
            initiateAgents(map, indices=wh, asSpatialPoints=asSpatialPoints)
          })

#' @rdname initiateAgents
#' @name initiateAgents
setMethod("initiateAgents",
          signature=c("Raster", "missing", "missing", "ANY", "numeric"),
          function(map, numAgents, probInit, asSpatialPoints, indices) {
            if(asSpatialPoints) {
              if(length(indices>0)) {
                return(xyFromCell(map, indices, spatial=asSpatialPoints))
              }
            } else {
              tmp <- raster(map)
              tmp[indices] <- 1
              return(tmp)
            }
          })


##############################################################
#' SELES - Agent Location at initiation
#'
#' A SELES-like function to maintain conceptual backwards compatability with that simulation
#' tool. Sets the the location of the intiating agents.  THIS IS NOT FULLY IMPLEMENTED.
#' This is intended to ease transitinos from
#' \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful
#'
#' @param map SpatialPoints*, SpatialPolygons* or Raster*
#'
#' @return Object of same class as provided as input. If a Raster*, then zeros are converted
#' to NAs
#'
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESagentLocation
#' @author Eliot McIntire
agentLocation <- function(map) {
    if (length(grep(pattern = "Raster", class(map)))==1) {
        map[map==0] <- NA
    } else if (length(grep(pattern = "SpatialPoints", class(map)))==1) {
        map
    } else if (!is.na(pmatch("SpatialPolygons", class(map)))) {
        map
    } else {
        stop("only raster, Spatialpoints or SpatialPolygons implemented")
    }
    return(map)
}

##############################################################
#' SELES - Probability of Initiation
#'
#' A SELES-like function to maintain conceptual backwards compatability with that simulation
#' tool. Describes the probability of initiation of agents or events.  THIS IS NOT FULLY IMPLEMENTED.
#' This is intended to ease transitinos from
#' \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful
#'
#' @param map A \code{.spatialObjects} object. Currently, only provides CRS and, if p is not
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
#' @import raster
#' @import sp
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESprobInit
#' @author Eliot McIntire
probInit = function(map, p=NULL, absolute=NULL) {
  if(all(inRange(p, 0, 1))) {
    if(is.null(absolute)) {
      absolute <- TRUE
    }
  } else {
    absolute <- FALSE
  }
  if (is.numeric(p)) {
    probInit <- raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
    p <- rep(p, length.out=ncell(map))
    probInit <- setValues(probInit, p/(sum(p)*(1-absolute)+1*(absolute)))

  } else if (is(p,"RasterLayer")) {
    probInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  } else if (is(map,"SpatialPolygonsDataFrame")) {
    probInit = p/sum(p)
  } else {
    stop("Error initializing probability map: bad inputs")
  }
  return(probInit)
}

###
patchSize = function(patches) {
  return(freq(patches))
}
