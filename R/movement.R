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
#' @param agent       A SpatialPointsDataFrame.
#'
#' @param stepLength  Numeric vector of length 1 or number of agents describing
#'                    step length.
#' 
#' @param sd          Numeric vector of length 1 or number of agents describing
#'                    standard deviation of wrapped normal turn angles.
#' 
#' @param lonlat      Logical. If \code{TRUE}, coordinates should be in degrees.
#'                    If \code{FALSE} coordinates represent planar ('Euclidean')
#'                    space (e.g. units of meters)
#' 
#' @return An agent object with updated spatial position defined by a single
#'          occurence of step length(s) and turn angle(s).
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
#' @author Eliot McIntire
#'
#@examples
#NEED EXAMPLES
crw = function(agent, stepLength, sd, lonlat) {
    if (missing(lonlat)) {
        stop("you must provide a \"lonlat\" argument (TRUE/FALSE)")
    }
    stopifnot(is.logical(lonlat))
    
    ### should convert to S4 for a mobileAgent
    n = length(agent)
    rand.dir = rnorm(n, agent@heading, sd)
    rand.dir = ifelse(rand.dir>180, rand.dir-360, ifelse(rand.dir<(-180), 360+rand.dir, rand.dir))
    
    last.position = agentPosition(agent)
    
    # these should use `coordinates(agent) <-` or similar set methods
    agent@spatial@coords[,"y"] = last.position@coords[,"y"] + cos(rad(rand.dir)) * stepLength
    agent@spatial@coords[,"x"] = last.position@coords[,"x"] + sin(rad(rand.dir)) * stepLength
    
    agent@heading = heading(last.position, agentPosition(agent))
    agent@distance = pointDistance(last.position, agentPosition(agent), lonlat=lonlat)
    
    return(agent)
}

