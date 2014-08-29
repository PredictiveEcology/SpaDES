############################################################################
############################################################################
# First Section - Make dummy data, maps etc. for practicing code
# Map size
ny = 5e2#2e3#3332#1000
nx = 5e2#2e3#1964#500

hab <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn = -ny/2, ymx = ny/2)
hab <- round(GaussMap(hab, speedup.index=10, cov.pars=c(0.3, 200)), 1)
plot(hab, maxpixels=1e4)

best = max(hab@data@values)
worst = min(hab@data@values)
good = Which(hab>0.8*best)
plot(good)

############################################################################
############################################################################
# SELES emulators - several scenarios - these are mostly just trying out 
#  different ways of initializing... i.e., no real simulations
#  Well, the first one has simple CRW


## SELES scenario 1
#inits
N = 1e2
dir.sd = 30
ntimesteps = 10  #365 * 4

al = AgentLocation(good) # good habitat, from above
pri = ProbInit(hab, al)
na = NumAgents(N) # why use this function instead of just N? error checking?

# initialize caribou agents
caribou = new("mobileAgent", agentlocation=al, numagents= na, probinit=pri)

plot(hab)
points(caribou, pch=19, cex=0.1)
for(i in 1:ntimesteps) {
    ex =  hab[position(caribou)] # find out what pixels the individuals are on now
    wh = which(!is.na(ex))
    if (length(wh)==0) stop(paste("all agents off map at time",i))
    sl = ex/10
    sl[-wh] = 1
    
    ln = rlnorm(length(ex),sl,0.02) # log normal step length
    caribou = crw(caribou, step.len = ln , dir.sd = dir.sd)
    points(caribou, pch=19, cex = 0.1)
    
#    caribou = Transitions(p, caribou) # in theory, this would kill some off
    
    # Determine the rings around agents, at given step lengths.
    #  This is in the spirit of Sarah's model
    rads = sample(10:30, length(caribou), replace=TRUE)
    rings = cir(caribou, radiuses=rads, hab, 1)
    points(rings$x, rings$y, col=rings$ids, pch = 19, cex = 0.1)
}
    
    
    




######################################################################
## Scenario 2 = a specific number of agents to start for specific patches

# Arbitrarily pick places with good habitat and cluster them into discrete patches
best = max(hab@data@values)
worst = min(hab@data@values)
good = Which(hab>0.8*best)
al = good

# create patches with patchid
patches = clump(al)

#start exactly n.p agents in each patch
pops = as.numeric(na.omit(unique(getValues(patches))))
num.pops =length(pops)
init.per.pop = data.table(pops, num.in.pop=sample(1:(num.pops*3), num.pops, replace=TRUE))
setkey(init.per.pop, pops)
# make map with patch id representing the number to start
# NOT USED BELOW... IT WAS USED
patches.num = reclassify(patches, matrix(c(pops, init.per.pop$num.in.pop), ncol=2, byrow=FALSE))


# the function == two options, table or map method
al1 = spec.num.per.patch(patches, num.per.patch.table=init.per.pop)
#al = spec.num.per.patch(patches, num.per.patch.map = patches.num)

caribou = new("mobileAgent", agentlocation=al1, probinit=NULL, numagents=NULL)
caribou@other[["population"]] = rep(init.per.pop$pops, init.per.pop$num.in.pop)

plot(patches)
points(caribou, col=extract(patches, position(caribou)), pch=19, cex=0.3)



######################################################################
## Scenario 3 = place 100 agents in proportion to the area of patches
na = NumAgents(100)
al = AgentLocation(al)
caribou = new("mobileAgent", agentlocation=al, numagents=na)
plot(patches)
points(caribou, col=extract(patches, position(caribou)), pch=19, cex = 0.8)




############################################################################
############################################################################
#### Potentially Useful commands

# Create a distance surface from the agents outwards
dfp = distanceFromPoints(hab, position(caribou))
plot(dfp, maxpixels=1e4)

# Started fire things. But this is entirely non functional
big.fire.years = cumsum(rpois(10, 10))
fire(timestep = big.fire.years)

# plotting
dev.flush() # makes each time step visualized. Remove this for a slightly faster simulation
#  arrow(caribou,length = 0.03) # not working right now ... needs calculate.last.position function built

# Create a raster stack of identical sized rasters
hab.stack = stack(hab, dfp)#, quick = T, native = T)

# Identify patch edges
patch.edge = raster::boundaries(patches)
plot(patch.edge)


# nearest neighbour stuff
library(spatstat)
agent.ppp = as.ppp(X=na.omit(as.data.frame(agent$pos)),W=as.owin( W=c(xl=-nx, xu=nx, yl=-ny, yu=ny)))
nnd = nndist(agent.ppp, k=2) # find distance of closest agent to each agent
nnw = nnwhich(agent.ppp) # identify which one is the closest ... complements the previous


