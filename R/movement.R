### MOVEMENT LIBRARY

Transitions = function(p, agent) {
    agent@spatial@coords[which(p==0),] = NA
    return(agent)
}

NumAgents = function(N) {
    if ((length(N) == 1) && (is.numeric(N))) NumAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(NumAgents)
}

move = function(hypothesis = NULL) {
    if (hypothesis == "TwoDT") move = "TwoDT"
    if (hypothesis == "crw") move = "crw"
}

AgentLocation = function(map) {
    if (length(grep(pattern = "Raster", class(map)))==1) {
        map[map==0] = NA
    } else if (length(grep(pattern = "SpatialPoints", class(map)))==1) {
        map
    } else if (!is.na(pmatch("SpatialPolygons",class(map)))) {
        map
    } else {
        stop("only raster, Spatialpoints or SpatialPolygons implemented")
    }
    return(map)
}

##############################################################
#' GaussMap
#'
#' Produces a raster of a random gaussian process. 
#' 
#' This is a wrapper for the \code{RFsimulate} function in the RandomFields 
#' package. The main addition is the \code{speedup} argument which allows
#' for faster map generation. A \code{speedup} of 1 is normal and will get
#' progressively faster as the number increases, at the expense of coarser pixel
#' resolution of the pattern generated
#'
#' @param ext An object of class \code{extent} giving the dimensions of output map.
#'
#' @param scale The spatial scale in map units of the Gaussian pattern.
#'
#' @param var Spatial variance.
#'
#' @param speedup An index of how much faster than normal to generate maps.
#'
#' @return A map of extent \code{ext} with a Gaussian random pattern.
#' 
#' @seealso \code{\link{RFsimulate}} and \code{\link{extent}}
#' 
#' @import RandomFields
#' @import raster
#' @export
#' @docType methods
#' @rdname gaussmap-method
#'
#@examples
#EXAMPLES NEEDED
GaussMap = function(ext, scale = 10, var = 1, speedup = 10) {#, fast = T, n.unique.pixels = 100) {
    xmn = ext@xmin
    xmx = ext@xmax
    ymn = ext@ymin
    ymx = ext@ymax
    nc = (xmx-xmn)/speedup # ifelse(fast, min(n.unique.pixels,xmx-xmn),xmx-xmn)
    nr = (ymx-ymn)/speedup # ifelse(fast, min(ymx-ymn,n.unique.pixels),ymx-ymn)
    xfact = (xmx-xmn)/nc
    yfact = (ymx-ymn)/nr
    
    model <- RMexp(scale=scale, var = var)
    x.seq = 1:nc
    y.seq = 1:nr
    sim <- raster(RFsimulate(model, x = x.seq, y = y.seq, grid = T))
    sim <- sim - cellStats(sim, "min")
    
    if(speedup>1) GaussMap <- disaggregate(sim, c(xfact, yfact))
    extent(GaussMap) <- ext
    return(GaussMap)
}

# This is a modified version found in CircStats to allow for multiple angles at once
dwrpnorm = function (theta, mu, rho, sd = 1, acc = 1e-05, tol = acc) {
    if (missing(rho)) {
        rho <- exp(-sd^2/2)
    }
    if (rho < 0 | rho > 1)
        stop("rho must be between 0 and 1")
    var <- -2 * log(rho)
    term <- function(theta, mu, var, k) {
        1/sqrt(var * 2 * pi) * exp(-((theta - mu + 2 * pi * k)^2)/(2 *
                                                                       var))
    }
    k <- 0
    Next <- term(theta, mu, var, k)
    Last <- Next
    delta <- rep(1, length(Last))
    while (any(delta > tol)) {
        keep = delta>tol
        k <- k + 1
        Last[keep] <- Next[keep]
        Next[keep] <- Last[keep] + term(theta[keep], mu[keep], var, k) + term(theta[keep],
                                                                              mu[keep], var, -k)
        delta[keep] <- abs(Next[keep] - Last[keep])
    }
    Next
}

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

##############################################################
#' Ring Probabilities
#'
#' Details here.
#'
#' @param agent Description of this.
#'
#' @param rings Description of this.
#'
#' @param step.len Description of this.
#'
#' @param dir.sd Description of this.
#'
#' @return Decribe what it returns: fromto.
#' 
##' @import CircStats
#' @import data.table
#' @export
#' @docType methods
#' @rdname ringprobs
#'
#' #@examples
#' # NEED EXAMPLES
ring.probs = function(agent, rings, step.len, dir.sd) {
    if (!is(agent, "agent")) {
        stop("must be an agent class") # checking should be done using S4 signatures
    }
    if (!is(rings, "NextPossiblePosition")) {
        stop("rings must be an NextPossiblePosition class") # checking should be done using S4 signatures
    }
    n = length(agent)
    
    DT = data.table(data.frame(agentPosition(agent), ids=agentID(agent), heading.rad=rad(agent@heading)))
    setkey(DT, ids)
    setkey(rings, ids)
    fromto = rings[DT]
    
    # the next lines aren't working:
    #   possibly because R thinks `headi`, and `ProbTurn` are globals.
    fromto[, headi:=heading(from=SpatialPoints(cbind(x=fromto$x.1, y=fromto$y.1)),
                            to=SpatialPoints(cbind(x=fromto$x,y=fromto$y)))]
    fromto[, ProbTurn:=dwrpnorm(theta=rad(headi), mu=heading.rad, sd=dir.sd/50)] # why 50?
    
    return(fromto)
}

##############################################################
#' Circle around an agent.
#'
#' Identifies the xy coordinates of a circle around all live agents.
#'
#' @param agent Description of this.
#'
#' @param radiuses Description of this, including why it isn't called
#' radii ;p
#'
#' @param raster_world Description of this.
#'
#' @param scale_raster Description of this.
#'
#' @return A list of data.frames with x and y coordinates of each 
#' unique pixel of the circle around each individual.
#' 
#' #@seealso \code{\link{print}} and \code{\link{cat}}
#' 
#' @import data.table sp raster
#' @export
#' @docType methods
#' @rdname cir-method
#'
# @examples
#  NEED EXAMPLES
cir = function(agent, radiuses, raster_world, scale_raster) {
    ### identify the pixels ("patches" in NetLogo) that are at
    ###  a buffer distance of the individual location.
    
    # create an index sequence for the number of individuals
    seq_num_ind<-seq_len(length(agent)) 
    
    # n = optimum number of points to create the circle for a given individual;
    #       gross estimation (checked that it seems to be enough so that pixels
    #       extracted are almost always duplicated, which means there is small
    #       chance that we missed some on the circle).
    n.angles <- ( ceiling((radiuses/scale_raster)*2*pi) + 1 )
    
    ### Eliot's code to replace the createCircle of the package PlotRegionHighlighter
    positions = coordinates(agent)
    
    # create individual IDs for the number of points that will be done for their circle
    ids <- rep.int(seq_num_ind, times=n.angles)
    
    # create vector of radius for the number of points that will be done for each individual circle
    rads <- rep.int(radiuses, times=n.angles)
    
    # extract the individuals' current positions
    xs <- rep.int(positions[,1], times=n.angles)
    ys <- rep.int(positions[,2], times=n.angles)
        
    # calculate the angle increment that each individual needs to do to complete a circle (2 pi)
    angle.inc <- rep.int(2*pi, length(n.angles)) / n.angles
    
    # repeat this angle increment the number of times it needs to be done to complete the circles
    angs <- rep.int(angle.inc, times=n.angles)
    
    ### Eliot' added's code:
    DT = data.table(ids, angs, xs, ys, rads)
    DT[, angles:=cumsum(angs), by=ids] # adds new column `angles` to DT that is the cumsum of angs for each id
    DT[, x:=cos(angles)*rads+xs] # adds new column `x` to DT that is the cos(angles)*rads+xs
    DT[, y:=sin(angles)*rads+ys] # adds new column `y` to DT that is the cos(angles)*rads+ys
    
    # put the coordinates of the points on the circles from all individuals in the same matrix
    coords.all.ind <- DT[, list(x,y,ids)]
    
    # extract the pixel IDs under the points
    coords.all.ind[, pixIDs:=cellFromXY(raster_world,coords.all.ind)]

    # use only the unique pixels
    coords.all.ind.unq = coords.all.ind[, list(pixIDs=unique(pixIDs)), by=ids]
    coords.all.ind.unq = na.omit(coords.all.ind.unq)
    coords.all.ind.unq[, pixIDs.unq:=extract(raster_world,pixIDs)] # where is `pixIDs.unq` used???
    
    # extract the coordinates for the pixel IDs
    pixels = xyFromCell(raster_world, coords.all.ind.unq$pixIDs)
    pixels_ind_ids_merged = cbind(coords.all.ind.unq, pixels)
    
    # list of df with x and y coordinates of each unique pixel of the circle of each individual
    return(pixels_ind_ids_merged)
}



#cir = function (agent, radiuses, n.angles = 36)
# {
#    n = length(agent)
#    if (length(radiuses)==1) { #if radiuses is a single number
#      radiuses = rep(radiuses,n)
#    }
#
#    # Begin creating vectors of each item, which will have a length of sum(n.angles), i.e.,
#    #    n.angles values for each of the agents
#    ids = rep(1:n,times=n.angles)
#    rads = rep(radiuses,times = n.angles)
#    xs = rep(coordinates(agent)$x, times = n.angles)
#    ys = rep(coordinates(agent)$y, times = n.angles)
#    nvs = rep(c(0,n.angles[-length(n.angles)]), times = n.angles) # To be used below to do calculation for angle increments
#    if (length(n.angles)==1) { # if n.angles is given as a single number of one number for each agent
#      angle.inc <- 2 * pi/n.angles
#    } else {
#      angle.inc <- rep(2 * pi, length(n.angles))/n.angles
#    }
#    angs = rep(angle.inc, times = n.angles)
#
#    #find the angles for each of the n.angles line segments around each agent   
#    dnvs = c(0,diff(ids)) # determine the index that separates two caribous
#    nvs[dnvs==0] = 0 # make all values of the nvs = 0
#    nvs2 = cumsum(nvs)
#    cum = 1:length(ids)
#    index = cum - nvs2 - 1 # This is the series of indices for each angle increment
#    angles = angs * index
#
#    # Calculate the x and y coordinates of the points in the rings
#    x = cos(angles)*rads+xs
#    y = sin(angles)*rads+ys
#
#    coords = data.table(cbind(x,y))
#    est.circles = SpatialPointsDataFrame(coords,data.table(ids,rads))
#    return(est.circles)
#}
