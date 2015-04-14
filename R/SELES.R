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
numAgents <- function(N) {
    if ((length(N) == 1) && (is.numeric(N))) numAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(numAgents)
}

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
#' @param map A \code{spatialObjects} object. Provides CRS and is related to probabilities.
#'
#' @param p probability, provided as a numeric or raster
#'
#' @param absolute logical. Is \code{p} absolute probabilities or relative?
#'
#' @return An RasterLayer with probabilities of initialization
#'
#' @seealso \code{\link{print}} and \code{\link{cat}}
#'
#' @import raster
#' @import sp
#' @include agent.R
#' @export
#' @docType methods
#' @rdname SELESprobInit
#' @author Eliot McIntire
probInit = function(map, p=NULL, absolute=FALSE) {
  if (length(p) == 1) {
    probInit = raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
    probInit = setValues(probInit, rep(p,length(probInit)))
  } else if (is(p,"RasterLayer")) {
    probInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  } else if (is(map,"SpatialPolygonsDataFrame")) {
    probInit = p/sum(p)
  } else if (is(p,"NULL"))  {
    probInit = map/(cellStats(p,sum)*(1-absolute)+1*(absolute))
  } else {
    stop("Error initializing probability map: bad inputs")
  }
  return(probInit)
}

###
patchSize = function(patches) {
  return(freq(patches))
}
