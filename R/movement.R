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
    if (hypotehsis == "crw") move = "crw"
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

crw = function(agent, step.len, dir.sd, hab = NULL) {
    n = length(agent)
    rand.dir = rnorm(n, agent@heading, dir.sd)
    rand.dir = ifelse(rand.dir>180, rand.dir-360, ifelse(rand.dir<(-180), 360+rand.dir, rand.dir))
    
    last.position = position(agent)
    
    # these should use `coordinates(agent) <-` or similar set methods
    agent@spatial@coords[,"y"] = last.position@coords[,"y"] + cos(rad(rand.dir)) * step.len
    agent@spatial@coords[,"x"] = last.position@coords[,"x"] + sin(rad(rand.dir)) * step.len
    
    agent@heading = heading(last.position, position(agent))
    agent@distance = distance(last.position, position(agent))
    
    return(agent)
}

ring.probs = function(agent, rings, step.len, dir.sd, hab = NULL) {
    if (!is(agent, "agent")) {
        stop("must be an agent class") # checking should be done using S4 signatures
    }
    if (!is(rings, "NextPossiblePosition")) {
        stop("rings must be an NextPossiblePosition class") # checking should be done using S4 signatures
    }
    n = length(agent)
    
    dt1 = data.table(data.frame(position(agent), ids=agent@ID, heading.rad=rad(agent@heading)))
    setkey(dt1, ids)
    setkey(rings, ids)
    fromto = rings[dt1]
    
    fromto[, headi:=heading(from=SpatialPoints(cbind(x=fromto$x.1, y=fromto$y.1)),
                            to=SpatialPoints(cbind(x=fromto$x,y=fromto$y)))]
    fromto[, ProbTurn:=dwrpnorm(theta=rad(headi), mu=heading.rad, sd=dir.sd/50)] # why 50?
    
    return(fromto)
}
# identifies the xy coordinates of a circle around all live agents
#  key function is draw.circles in the plotrix package

## Results double checked
cir = function(agent, radiuses, raster_world, scale_raster){
    ### identify the pixels ("patches" in NetLogo) that are at
    ###  a buffer distance of the individual location.
    
    # create an index sequence for the number of individuals
    seq_num_ind<-seq_len(length(agent)) 
    
    # n = optimum number of points to create the circle for a given individual;
    #       gross estimation (checked that it seems to be enough so that pixels
    #       extracted are almost always duplicated, which means there is small
    #       chance that we missed some on the circle).
    n.angles<-(ceiling((radiuses/scale_raster)*2*pi)+1)
    
    ### Eliot's code to replace the createCircle of the package PlotRegionHighlighter
    positions = coordinates(agent)
    
    # create individual IDs for the number of points that will be done for their circle
    ids <- rep.int(seq_num_ind, times=n.angles)
    
    # create vector of radius for the number of points that will be done for each individual circle
    rads <- rep.int(radiuses, times=n.angles)
    
    # extract the individuals' current positions
    xs <- rep.int(positions[,1], times=n.angles)
    ys <- rep.int(positions[,2], times=n.angles)
    
    # to be used below to do calculation for angle increments
    nvs <- rep.int(c(0,n.angles[-length(n.angles)]), times=n.angles)
    
    # calculate the angle increment that each individual needs to do to complete a circle (2 pi)
    angle.inc <- rep.int(2*pi, length(n.angles)) / n.angles
    
    # repeat this angle increment the number of times it needs to be done to complete the circles
    angs<-rep.int(angle.inc,times=n.angles)
    
    ### Eliot' added's code:
    a1 = Sys.time()
    dt1 = data.table(ids, angs, xs, ys, rads)
    dt1[,angles:=cumsum(angs),by=ids]
    dt1[,x:=cos(angles)*rads+xs]
    dt1[,y:=sin(angles)*rads+ys]
    
    # put the coordinates of the points on the circles from all individuals in the same matrix
    coordinates_all_ind <- dt1[,list(x,y,ids)]    #cbind(x,y)
    
    # extract the pixel IDs under the points
    coordinates_all_ind[,pixels_under_coordinates := cellFromXY(raster_world,coordinates_all_ind)]
    coordinates_all_ind_unique =
        coordinates_all_ind[,list(pixels_under_coordinates = unique(pixels_under_coordinates)), by=ids]
    coordinates_all_ind_unique = na.omit(coordinates_all_ind_unique)
    coordinates_all_ind_unique[,unique_pixels_values := extract(raster_world,pixels_under_coordinates)]
    
    # extract the coordinates for the pixel IDs
    pixels = xyFromCell(raster_world,coordinates_all_ind_unique$pixels_under_coordinates)
    pixels_ind_ids_merged = cbind(coordinates_all_ind_unique,pixels)
    
    # put the coordinates x and y back into a list according to the individual IDs
    #    coord_unique_pixels <- split(pixels_ind_ids_merged[,list(x,y)],pixels_ind_ids_merged[,ids])
    
    a2 = Sys.time()
    
    # list of df with x and y coordinates of each unique pixel of the circle of each individual
    #    return(coord_unique_pixels) 
    
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
