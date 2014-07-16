# Not implemented yet
# move = function(hypothesis = NULL) {
#     if (hypothesis == "TwoDT") move = "TwoDT"
#     if (hypothesis == "crw") move = "crw"
# }


##############################################################
#' Simple Correlated Random Walk
#'
#' This version uses just turn angles and step lengths to define the correlated random walk.
#' 
#' This simple version of a correlated random walk is largely the version that 
#' was presented in Turchin 1998, but it was also used with bias modifications 
#' in McIntire, Schultz, Crone 2007.
#'
#' @param agent An object of class mobileAgent.
#'
#' @param step.len Numeric vector of length 1 or number of agents describing step length.
#' 
#' @param dir.sd Numeric vector of length 1 or number of agents describing sd of wrapped normal turn angles.
#' 
#' @param lonlat logical. If TRUE, coordinates should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters)
#' 
#' @return An agent object with updated spatial position defined by a single occurrence
#' of step length(s) and turn angle(s).
#' 
#' @seealso \code{\link{mobileAgent}}, \code{\link{pointDistance}}
#' 
#' @references Turchin, P. 1998. Quantitative analysis of movement: measuring and modeling population redistribution in animals and plants. Sinauer Associates, Sunderland, MA.
#' @references McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007. Designing a network for butterfly habitat restoration: where individuals, populations and landscapes interact. Journal of Applied Ecology 44:725-736.
#' 
#' @export
#' @docType methods
#' @rdname crw-method
#'
#@examples
#NEED EXAMPLES
crw = function(agent, step.len, dir.sd, lonlat) {
    if (missing(lonlat)) {
        stop("you must provide a \"lonlat\" argument (TRUE/FALSE)")
    }
    stopifnot(is.logical(lonlat))
    
    ### should convert to S4 for a mobileAgent
    n = length(agent)
    rand.dir = rnorm(n, agent@heading, dir.sd)
    rand.dir = ifelse(rand.dir>180, rand.dir-360, ifelse(rand.dir<(-180), 360+rand.dir, rand.dir))
    
    last.position = agentPosition(agent)
    
    # these should use `coordinates(agent) <-` or similar set methods
    agent@spatial@coords[,"y"] = last.position@coords[,"y"] + cos(rad(rand.dir)) * step.len
    agent@spatial@coords[,"x"] = last.position@coords[,"x"] + sin(rad(rand.dir)) * step.len
    
    agent@heading = heading(last.position, agentPosition(agent))
    agent@distance = pointDistance(last.position, agentPosition(agent), lonlat=lonlat)
    
    return(agent)
}

